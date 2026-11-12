package io.forge.jam.node

import java.util.concurrent.{ConcurrentHashMap, Executors, LinkedBlockingQueue, ScheduledExecutorService, TimeUnit}

import scala.collection.mutable

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.LittleEndian
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash}
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamHandler, StreamKind}
import io.forge.jam.node.GrandpaMessages.*


/** GRANDPA finality over JAMNP-S: CE 149 votes, CE 150 commits, CE 151 state, CE 152 catch-up, CE 130
  * justification requests and CE 153 warp sync. The voter runs on one thread; justifications and warp sync fragments
  * are kept in the block store's metadata.
  */
final class GrandpaService(
    chain: ChainManager,
    sync: SyncService,
    distribution: DistributionService,
    keys: Option[ValidatorKeySet],
    gossipMillis: Long = 1000L,
    /** We vote only for blocks we judge audited. */
    audited: Hash => Boolean = _ => true
) extends LazyLogging:

  private val exec: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "jam-grandpa"); t.setDaemon(true); t
  }

  /** Peers' votes, commits, state and catch-up requests waiting for the voter thread are bounded; a message over
    * the limit is dropped (votes are resent every gossip period, and a commit can be had again by catch-up). The
    * thread's own timers and import callbacks are not counted.
    */
  private val intake = new WorkGate("grandpa intake", GrandpaService.MaxQueuedMessages, GrandpaService.MaxQueuedBytes)

  /** Peer messages waiting for the voter thread. */
  private[node] def queuedMessages: Int = intake.queued

  private val adapter = new GrandpaChain:
    def finalized: Vote = toVote(chain.finalized)
    def best: Vote = toVote(chain.best)
    def slotOf(h: Hash): Option[Long] = chain.slotOf(h).orElse(Option.when(h == genesisHash)(0L))
    def parentOf(h: Hash): Option[Hash] = if h == genesisHash then None else chain.parentHash(h)
    def stateRootOf(h: Hash): Option[Hash] = chain.postStateRoot(h)
    def headerBytes(h: Hash): Option[Array[Byte]] = chain.headerBytes(h)
    def epochMarkerKeys(h: Hash): Option[List[Array[Byte]]] =
      if h == genesisHash then None
      else chain.header(h).flatMap(_.epochMark).map(_.validators.map(_.ed25519.bytes))
    def finalizeBlock(h: Hash): Either[String, Unit] = chain.finalizeAdopting(h).left.map(_.message).map(_ => ())
    override def isAudited(h: Hash): Boolean = audited(h)

  private def toVote(h: chain.Head): Vote = Vote(h.hash, h.stateRoot, h.slot)

  @volatile private var genesisHash: Hash = Hash.zero

  private val outbound = new GrandpaOutbound:
    def broadcastVote(m: VoteMessage): Unit = toVoters(StreamKind.GrandpaVote, encodeVoteMessage(m))
    def broadcastCommit(round: Long, setId: Long, c: Commit): Unit =
      toVoters(StreamKind.GrandpaCommit, encodeCommitMessage(round, setId, c))
    def broadcastState(round: Long, setId: Long, finalizedSlot: Long): Unit =
      toVoters(StreamKind.GrandpaState, encodeState(round, setId, finalizedSlot))

  @volatile private var voter: GrandpaVoter = null

  /** One open connection per voter peer. */
  private def voterPeers: List[JamnpConnection] =
    val members = Option(voter).map(_.voterSet.members).getOrElse(Set.empty)
    distribution.peers.filter(c => members.contains(c.peerPublicKey)).distinctBy(_.peerPublicKey)

  private def toVoters(kind: Byte, payload: Array[Byte]): Unit =
    val peers = voterPeers
    if kind == StreamKind.GrandpaVote then peers.foreach(c => votesSentTo.add(c.peerPublicKey))
    peers.foreach(c => send(c, kind, payload))

  private def send(c: JamnpConnection, kind: Byte, payload: Array[Byte]): Unit =
    distribution.submitSend(() => c.openStream(kind), kind, payload, c)

  /** Voters that have our current round's votes. */
  private val votesSentTo = ConcurrentHashMap.newKeySet[Ed25519PublicKey]()
  @volatile private var votesRound = (0L, 0L)
  @volatile private var roundSince = 0L
  @volatile private var lastCatchUpAsk = 0L

  /** Resend our current round's votes to voters that connected since, and every vote we hold for it to every voter
    * while the round is stalled (votes lost in a partition are not resent by their senders).
    */
  private def resendVotes(): Unit =
    val now = System.currentTimeMillis()
    val round = (voter.voterSet.setId, voter.round)
    var stalled = false
    if round != votesRound then
      votesRound = round
      roundSince = now
      votesSentTo.clear()
    else if now - roundSince > 8 * gossipMillis then
      roundSince = now - 4 * gossipMillis
      votesSentTo.clear()
      stalled = true
    if stalled && now - lastCatchUpAsk > 30 * gossipMillis then
      // A voter back from a restart hears nothing from peers that never resend: ask them.
      lastCatchUpAsk = now
      logger.debug(s"GRANDPA round ${voter.round} of set ${voter.voterSet.setId} stalled: asking voters for a catch-up")
      voterPeers.foreach(c => requestCatchUp(c, voter.round, voter.voterSet.setId))
    val votes = if stalled then voter.roundVotes else voter.ourVotes
    if votes.nonEmpty then
      voterPeers.filterNot(c => votesSentTo.contains(c.peerPublicKey)).foreach { c =>
        votesSentTo.add(c.peerPublicKey)
        votes.foreach(v => send(c, StreamKind.GrandpaVote, encodeVoteMessage(v)))
      }

  /** Commits whose target block we are still fetching, by target. Touched on the voter thread only. */
  private val pendingCommits = new PendingCommits(GrandpaService.MaxPendingCommits)

  /** How long a request that brought nothing is remembered before it may be made again: the stalled-round cadence
    * above, and at the default T no shorter than the 30 s a request may take in [[request]].
    */
  private val retryMillis = 30 * gossipMillis

  /** (block, slot) of the peer finality we asked a justification for (CE 130). Touched on the voter thread only. */
  private val justificationRequests = new RequestTracker[(Hash, Long)](retryMillis)

  /** Start voting from the persisted voter set, or set 0 at genesis with the genesis validators. */
  def start(genesis: Hash, genesisKeys: IndexedSeq[Array[Byte]]): Unit =
    genesisHash = genesis
    val set = chain.meta("grandpa:set").filter(_.length == 40) match
      case Some(b) =>
        val setId = LittleEndian.get(b, 0, 8)
        val base = Hash(b.drop(8))
        val markerKeys = adapter.epochMarkerKeys(base).map(_.toIndexedSeq).getOrElse(genesisKeys)
        val baseVote = Vote(base, adapter.stateRootOf(base).getOrElse(Hash.zero), adapter.slotOf(base).getOrElse(0L))
        VoterSet(setId, baseVote, markerKeys)
      case None =>
        val root = chain.postStateRoot(genesis).getOrElse(chain.finalized.stateRoot)
        VoterSet(0, Vote(genesis, root, adapter.slotOf(genesis).getOrElse(0L)), genesisKeys)
    voter = new GrandpaVoter(
      adapter,
      outbound,
      set,
      keys,
      gossipMillis = gossipMillis,
      onFinalized = persist,
      onSetAdopted = s => chain.putMeta("grandpa:set", LittleEndian.encode(s.setId, 8) ++ s.base.hash.bytes.toArray),
      onProgress = (setId, round, votes) =>
        chain.putMeta(
          "grandpa:round",
          LittleEndian.encode(setId, 8) ++ LittleEndian.encode(round, 8) ++ votes.flatMap(encodeVoteMessage)
        ),
      resume = storedRound(set.setId)
    )
    logger.info(s"GRANDPA voter started: set ${set.setId} round ${voter.round}, ${set.size} voters, voting=${keys
        .exists(k => set.members.contains(k.ed25519Key))}")
    chain.onImported((head, _) =>
      exec.execute(() => { retryPending(head.hash); voter.step() })
    )
    exec.scheduleAtFixedRate(
      () => guarded { voter.step(); resendVotes() },
      gossipMillis / 4,
      gossipMillis / 4,
      TimeUnit.MILLISECONDS
    )
    exec.scheduleAtFixedRate(() => guarded(followPeers()), 2, 2, TimeUnit.SECONDS)

  def shutdown(): Unit = exec.shutdownNow()

  private def guarded(f: => Unit): Unit =
    try f
    catch case e: Exception => logger.error("GRANDPA step failed", e)

  /** The round and our votes in it persisted for `setId` before a restart. */
  private def storedRound(setId: Long): Option[(Long, List[VoteMessage])] =
    chain.meta("grandpa:round").filter(b => b.length >= 16 && LittleEndian.get(b, 0, 8) == setId).map { b =>
      val votes = b.drop(16).grouped(VoteMessageSize).flatMap(v => decodeVoteMessage(v).toOption).toList
      (LittleEndian.get(b, 8, 8), votes)
    }

  private def persist(target: Vote, j: Justification, setChanged: Boolean): Unit =
    val encoded = encodeJustification(j)
    chain.putMeta(s"grandpa:just:${target.hash.toHex}", encoded)
    if setChanged then
      chain.headerBytes(target.hash).foreach(h => chain.putMeta(s"grandpa:warp:${j.setId}", h ++ encoded))
      chain.putMeta("grandpa:set", LittleEndian.encode(j.setId + 1, 8) ++ target.hash.bytes.toArray)

  private def retryPending(imported: Hash): Unit =
    pendingCommits.take(imported).foreach((round, setId, c, anc) => applyCommit(round, setId, c, anc, None): Unit)

  /** True when the voter took the commit. */
  private def applyCommit(
      round: Long,
      setId: Long,
      c: Commit,
      ancestries: List[Array[Byte]],
      from: Option[JamnpConnection]
  ): Boolean =
    voter.onCommit(round, setId, c, ancestries) match
      case Right(Some(missing)) =>
        logger.debug(s"GRANDPA commit for ${c.target.short} waits for the block")
        pendingCommits.prune(chain.finalized.slot, voter.voterSet.setId)
        pendingCommits.put(missing, round, setId, c, ancestries)
        from.foreach(sync.fetchBlock(_, missing))
        true
      case Right(None) => true
      case Left(err) =>
        logger.debug(s"GRANDPA commit for ${c.target.short} rejected: $err")
        false

  /** Ask a voter whose UP 0 finality is ahead of ours for its justification (CE 130). */
  private def followPeers(): Unit =
    val ours = chain.finalized.slot
    pendingCommits.prune(ours, voter.voterSet.setId)
    justificationRequests.retain((_, slot) => slot > ours)
    // Any peer will do, voter or not: a justification is checked against the voter set, not against who sent it.
    distribution.peers.distinctBy(_.peerPublicKey).foreach { conn =>
      sync.peerFinal(conn).filter(f => f.slot > ours + 1).foreach { f =>
        // A node that joined late is voter sets behind: the peer's justification is signed by a set we have not
        // reached, so the set changes in between are asked for first (CE 153) and applied in order.
        val ourSet = voter.voterSet.setId
        warpRequests.retain(_ >= ourSet)
        if chain.hasBlock(f.hash) && warpRequests.tryStart(ourSet, conn.peerPublicKey) then
          logger.debug(s"GRANDPA: peer final slot ${f.slot} is ahead of ours ($ours); asking for set changes from $ourSet")
          requestWarpSync(conn, ourSet)
        if justificationRequests.tryStart((f.hash, f.slot), conn.peerPublicKey) then
          requestJustification(conn, f.hash, f.slot)
      }
    }

  // ---------------------------------------------------------------------------
  // handlers
  // ---------------------------------------------------------------------------

  private def onSingleMessage(f: (JamnpConnection, JamnpStream, Array[Byte]) => Unit): StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) => stream.onMessage(msg => f(conn, stream, msg))

  /** The set we asked a warp sync from (ours at the time: the request is the same whatever later set the peer names),
    * and the (set, round) pairs we asked a catch-up to. Touched on the voter thread only.
    */
  private val warpRequests = new RequestTracker[Long](retryMillis)
  private val catchUpRequests = new RequestTracker[(Long, Long)](retryMillis)

  /** A peer in a later set or round than ours: ask it for warp sync (CE 153) or a catch-up (CE 152) once, since some
    * peers never send CE 151 state.
    */
  private def catchUpWith(conn: JamnpConnection, round: Long, setId: Long): Unit =
    val ourSet = voter.voterSet.setId
    if setId > ourSet then
      warpRequests.retain(_ >= ourSet)
      if warpRequests.tryStart(ourSet, conn.peerPublicKey) then requestWarpSync(conn, ourSet)
    // Within our set only a voter can be ahead of us in a way a catch-up would settle, and its key is the
    // connection's: anyone else naming a later round would otherwise have us ask them for it.
    else if voter.voterSet.members.contains(conn.peerPublicKey) && voter.wantsCatchUp(round, setId) then
      catchUpRequests.retain((s, r) => s == ourSet && r > voter.round + 1)
      if catchUpRequests.tryStart((setId, round), conn.peerPublicKey) then
        requestCatchUp(conn, round - 1, setId, () => catchUpRequests.succeeded((setId, round)))

  /** CE 149. */
  def voteHandler: StreamHandler = onSingleMessage { (conn, stream, msg) =>
    stream.finish()
    decodeVoteMessage(msg) match
      case Right(m) =>
        intake.tryExecute(exec, msg.length.toLong, conn) {
          guarded {
            catchUpWith(conn, m.round, m.setId)
            voter.onVote(m)
          }
        }
      case Left(err) => logger.warn(s"bad CE149 payload: $err")
  }

  /** CE 150. */
  def commitHandler: StreamHandler = onSingleMessage { (conn, stream, msg) =>
    stream.finish()
    decodeCommitMessage(msg) match
      case Right((round, setId, c)) =>
        intake.tryExecute(exec, msg.length.toLong, conn) {
          guarded {
            catchUpWith(conn, round + 1, setId)
            applyCommit(round, setId, c, Nil, Some(conn))
          }
        }
      case Left(err)                => logger.warn(s"bad CE150 payload: $err")
  }

  /** CE 151. */
  def stateHandler: StreamHandler = onSingleMessage { (conn, stream, msg) =>
    stream.finish()
    decodeState(msg) match
      case Right((round, setId, _)) =>
        intake.tryExecute(exec, msg.length.toLong, conn) {
          guarded(catchUpWith(conn, round, setId))
        }
      case Left(err) => logger.warn(s"bad CE151 payload: $err")
  }

  /** CE 152. */
  def catchUpHandler: StreamHandler = onSingleMessage { (conn, stream, msg) =>
    decodeCatchUpRequest(msg) match
      case Right((round, setId)) =>
        val admitted = intake.tryExecute(exec, msg.length.toLong, conn) {
          voter.catchUpFor(round, setId) match
            case Some(c) => stream.send(encodeCatchUp(c)); stream.finish()
            case None    => stream.reset()
        }
        if !admitted then stream.reset()
      case Left(_) => stream.reset()
  }

  /** CE 130. */
  def justificationHandler: StreamHandler = onSingleMessage { (_, stream, msg) =>
    val j = Option.when(msg.length == 32)(Hash(msg)).flatMap(h => chain.meta(s"grandpa:just:${h.toHex}"))
    j match
      case Some(bytes) => stream.send(bytes); stream.finish()
      case None        => stream.reset()
  }

  /** CE 153. */
  def warpSyncHandler: StreamHandler = onSingleMessage { (_, stream, msg) =>
    val fragments =
      if msg.length != 4 then Nil
      else
        val from = LittleEndian.get(msg, 0, 4)
        Iterator
          .iterate(from)(_ + 1)
          .map(id => chain.meta(s"grandpa:warp:$id"))
          .takeWhile(_.isDefined)
          .take(MaxWarpFragments)
          .flatten
          .toList
    if fragments.isEmpty then stream.reset()
    else
      val w = new io.forge.jam.core.codec.ByteWriter(1024)
      w.compact(fragments.size.toLong)
      fragments.foreach(w.bytes(_))
      stream.send(w.toArray)
      stream.finish()
  }

  def register(network: io.forge.jam.network.JamnpNode): Unit =
    network
      .registerHandler(StreamKind.GrandpaVote, voteHandler)
      .registerHandler(StreamKind.GrandpaCommit, commitHandler)
      .registerHandler(StreamKind.GrandpaState, stateHandler)
      .registerHandler(StreamKind.GrandpaCatchUp, catchUpHandler)
      .registerHandler(StreamKind.GrandpaJustificationRequest, justificationHandler)
      .registerHandler(StreamKind.WarpSyncRequest, warpSyncHandler)
    ()

  // ---------------------------------------------------------------------------
  // requests
  // ---------------------------------------------------------------------------

  /** Send one request and collect the response, off the voter thread; `f` then runs on it. */
  private def request(conn: JamnpConnection, kind: Byte, payload: Array[Byte])(f: Array[Byte] => Unit): Unit =
    distribution.submitRequest { () =>
      val stream = conn.openStream(kind).get(10, TimeUnit.SECONDS)
      // One response message is expected; whatever else the peer sends on the stream is not kept.
      val received = new LinkedBlockingQueue[Array[Byte]](1)
      val done = new LinkedBlockingQueue[java.lang.Boolean]()
      stream.onMessage(received.offer(_): Unit)
      stream.onClosed(() => done.offer(true))
      try
        stream.send(payload)
        stream.finish()
        if done.poll(20, TimeUnit.SECONDS) != null then
          val answer = Option(received.poll())
          if answer.isEmpty then logger.debug(s"${StreamKind.name(kind)}: the peer closed the stream without an answer")
          answer.foreach(bytes => exec.execute(() => guarded(f(bytes))))
        else
          logger.debug(s"${StreamKind.name(kind)}: no answer in 20 s")
          stream.reset() // timed out: give the stream up on both sides
      catch
        case e: Exception =>
          stream.reset()
          throw e
    }

  private def requestJustification(conn: JamnpConnection, hash: Hash, slot: Long): Unit =
    request(conn, StreamKind.GrandpaJustificationRequest, hash.bytes.toArray) { bytes =>
      decodeJustification(bytes, chain.headerCodec) match
        case Right(j) if j.commit.target.hash == hash =>
          if applyCommit(j.round, j.setId, j.commit, j.ancestries, Some(conn)) then
            justificationRequests.succeeded((hash, slot))
        case Right(_)  => logger.warn("CE130 justification for another block")
        case Left(err) => logger.warn(s"bad CE130 response: $err")
    }

  private def requestCatchUp(conn: JamnpConnection, round: Long, setId: Long, onApplied: () => Unit = () => ()): Unit =
    request(conn, StreamKind.GrandpaCatchUp, encodeCatchUpRequest(round, setId)) { bytes =>
      decodeCatchUp(bytes).flatMap(voter.onCatchUp(setId, _)) match
        case Right(()) => onApplied()
        case Left(e)   => logger.debug(s"catch-up: $e")
    }

  private def requestWarpSync(conn: JamnpConnection, fromSet: Long): Unit =
    request(conn, StreamKind.WarpSyncRequest, LittleEndian.encode(fromSet, 4)) { bytes =>
      decodeWarpFragments(bytes, chain.headerCodec) match
        case Right(fragments) =>
          logger.debug(s"CE153: ${fragments.size} set-change fragment(s) from set $fromSet")
          val applied = fragments.map { f =>
            val j = f.justification
            applyCommit(j.round, j.setId, j.commit, j.ancestries, Some(conn))
          }
          if applied.contains(true) then warpRequests.succeeded(fromSet)
        case Left(err) => logger.warn(s"bad CE153 response: $err")
    }

object GrandpaService:

  /** Commits kept while their target block is fetched. Each one carries a supermajority of valid precommits of the
    * current set (checked before it is kept) for a block above our finalized one, so honest voters produce at most
    * one target per round, and finalizing the highest target finalizes the rest: when more than this many wait, the
    * lowest targets are the ones dropped.
    */
  val MaxPendingCommits: Int = 64

  /** CE 149-152 messages from peers that may wait for the voter thread, and the bytes they may hold. */
  val MaxQueuedMessages: Int = 4096
  val MaxQueuedBytes: Long = 32L * 1024 * 1024

/** Verified commits waiting for their target block, at most `max`: the highest targets are kept. Not thread safe. */
private[node] final class PendingCommits(max: Int):
  private val byTarget = mutable.HashMap.empty[Hash, (Long, Long, Commit, List[Array[Byte]])]

  def size: Int = byTarget.size

  def put(target: Hash, round: Long, setId: Long, c: Commit, ancestries: List[Array[Byte]]): Unit =
    byTarget.put(target, (round, setId, c, ancestries))
    while byTarget.size > max do byTarget.remove(byTarget.minBy(_._2._3.target.slot)._1)

  def take(target: Hash): Option[(Long, Long, Commit, List[Array[Byte]])] = byTarget.remove(target)

  /** Drop commits that can no longer finalize anything: at or below the finalized slot, or of another set. */
  def prune(finalizedSlot: Long, setId: Long): Unit =
    byTarget.filterInPlace((_, v) => v._3.target.slot > finalizedSlot && v._2 == setId)

/** Requests made to peers, so that each is made once and none is remembered for good. A request is forgotten
  * `retryMillis` after it was made, which covers one that failed, was answered with something useless or timed
  * out (the wait is the back-off before asking again), or earlier through [[retain]] once the state it was to bring
  * is there. While a request is unanswered its peer gets no other of this kind, so the entries are at most one per
  * peer plus the answered ones, and an answer counts only when it was valid ([[succeeded]]). Not thread safe.
  */
private[node] final class RequestTracker[K](retryMillis: Long, clock: () => Long = () => System.currentTimeMillis()):
  private final class Entry(val peer: Ed25519PublicKey, val expires: Long, var answered: Boolean)
  private val entries = mutable.LinkedHashMap.empty[K, Entry]

  def size: Int = entries.size

  /** True when `key` should be requested from `peer` now; the request is then tracked. */
  def tryStart(key: K, peer: Ed25519PublicKey): Boolean =
    val now = clock()
    // Entries are in request order and live equally long, so the expired ones lead.
    while entries.headOption.exists(_._2.expires <= now) do entries.remove(entries.head._1)
    if entries.contains(key) || entries.valuesIterator.exists(e => !e.answered && e.peer == peer) then false
    else
      entries.put(key, new Entry(peer, now + retryMillis, false))
      true

  /** The peer answered `key` with something valid: it may be asked for something else. */
  def succeeded(key: K): Unit = entries.get(key).foreach(_.answered = true)

  /** Forget the requests whose key no longer needs asking. */
  def retain(keep: K => Boolean): Unit = entries.filterInPlace((k, _) => keep(k))
