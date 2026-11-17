package io.forge.jam.node

import java.util.concurrent.{ConcurrentHashMap, ScheduledExecutorService, TimeUnit}

import scala.collection.mutable

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.LittleEndian
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash}
import io.forge.jam.network.{PeerConnection, MessageStream, PeerStreamHandler, StreamKind}
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
    audited: Hash => Boolean = _ => true,
    serving: InboundWork = InboundWork.direct,
    /** Owner of this service's threads: the node's, or its own when made alone. */
    threads: NodeThreads = new NodeThreads,
    misbehaved: (PeerConnection, Offence) => Unit = (_, _) => (),
    paused: () => Boolean = () => false,
    clock: () => Long = () => System.currentTimeMillis()
) extends LazyLogging:

  private val exec: ScheduledExecutorService = threads.scheduled("jam-grandpa")

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
    override def genesisNextKeys: Option[List[Array[Byte]]] = genesisNext
    def finalizeBlock(h: Hash): Either[String, Unit] = chain.finalizeAdopting(h).left.map(_.message).map(_ => ())
    override def isAudited(h: Hash): Boolean = audited(h)

  private def toVote(h: ChainHead): Vote = Vote(h.hash, h.stateRoot, h.slot)

  @volatile private var genesisHash: Hash = Hash.zero
  @volatile private var genesisNext: Option[List[Array[Byte]]] = None

  private val outbound = new GrandpaOutbound:
    def broadcastVote(m: VoteMessage): Unit = toVoters(StreamKind.GrandpaVote, encodeVoteMessage(m))
    def broadcastCommit(round: Long, setId: Long, c: Commit): Unit =
      toVoters(StreamKind.GrandpaCommit, encodeCommitMessage(round, setId, c))
    def broadcastState(round: Long, setId: Long, finalizedSlot: Long): Unit =
      toVoters(StreamKind.GrandpaState, encodeState(round, setId, finalizedSlot))

  @volatile private var voter: GrandpaVoter = null

  private[node] def voterSet: VoterSet = voter.voterSet

  /** One open connection per voter peer. */
  private def voterPeers: List[PeerConnection] =
    val members = Option(voter).map(_.voterSet.members).getOrElse(Set.empty)
    distribution.peers.filter(c => members.contains(c.peerPublicKey)).distinctBy(_.peerPublicKey)

  private def toVoters(kind: Byte, payload: Array[Byte]): Unit =
    val peers = voterPeers
    if kind == StreamKind.GrandpaVote then peers.foreach(c => votesSentTo.add(c.peerPublicKey))
    peers.foreach(c => send(c, kind, payload))

  private def send(c: PeerConnection, kind: Byte, payload: Array[Byte]): Unit =
    distribution.submitSend(() => c.openStream(kind), kind, payload, c)

  /** Voters that have our current round's votes. */
  private val votesSentTo = ConcurrentHashMap.newKeySet[Ed25519PublicKey]()
  @volatile private var votesRound = (0L, 0L)
  @volatile private var lastCatchUpAsk = 0L
  private val stall = new StallClock(gossipMillis)

  private val forwardedTo = mutable.Set.empty[Ed25519PublicKey]

  /** Resend our current round's votes to voters that connected since, and to every voter while the round is
    * stalled; on a stall, forward the others' votes we hold for it to a few voters at a time (votes lost in a
    * partition are not resent by their senders). Each voter's votes go as one egress entry: a stalled round must not
    * crowd out what is queued with them.
    */
  private def resendVotes(): Unit =
    if silent then return
    val now = clock()
    val round = (voter.voterSet.setId, voter.round)
    if round != votesRound then
      votesRound = round
      stall.restart(now)
      votesSentTo.clear()
      forwardedTo.clear()
    val stalled = stall.due(now)
    if stalled then votesSentTo.clear()
    // An observer has no round to complete: it follows by justification (CE 130), and voters refuse it a catch-up.
    if stalled && keys.isDefined && now - lastCatchUpAsk > 30 * gossipMillis then
      // A voter back from a restart hears nothing from peers that never resend: ask them.
      val heard = voter.votersHeard
      val asked = voterPeers.filterNot(c => heard.contains(c.peerPublicKey))
      if asked.nonEmpty then
        lastCatchUpAsk = now
        logger.debug(
          s"GRANDPA round ${voter.round} of set ${voter.voterSet.setId} stalled: asking ${asked.size} voter(s) not heard from since the start for a catch-up"
        )
        asked.foreach(c => requestCatchUp(c, voter.round, voter.voterSet.setId))
    val peers = voterPeers
    val own = voter.ourVotes
    if own.nonEmpty then
      peers.filterNot(c => votesSentTo.contains(c.peerPublicKey)).foreach { c =>
        votesSentTo.add(c.peerPublicKey)
        sendVotes(c, own)
      }
    if stalled then
      val ours = keys.map(_.ed25519Key)
      val others = voter.roundVotes.filterNot(m => ours.contains(m.message.signed.signerKey))
      if others.nonEmpty then forwardTargets(peers).foreach(sendVotes(_, others))

  private def forwardTargets(peers: List[PeerConnection]): List[PeerConnection] =
    val waiting = peers.filterNot(c => forwardedTo.contains(c.peerPublicKey))
    val pool =
      if waiting.nonEmpty then waiting
      else
        forwardedTo.clear()
        peers
    val heard = voter.votersHeard
    val chosen = pool
      .sortWith((a, b) => java.util.Arrays.compareUnsigned(a.peerKey, b.peerKey) < 0)
      .sortBy(c => heard.contains(c.peerPublicKey))
      .take(GrandpaService.ForwardPeersPerStall)
    chosen.foreach(c => forwardedTo += c.peerPublicKey)
    chosen

  private def sendVotes(c: PeerConnection, votes: List[VoteMessage]): Unit =
    distribution.submitSends(() => c.openStream(StreamKind.GrandpaVote), StreamKind.GrandpaVote, votes.map(encodeVoteMessage), c)

  /** Commits whose target block we are still fetching, by target. Touched on the voter thread only. */
  private val pendingCommits = new PendingCommits(GrandpaService.MaxPendingCommits)

  /** How long a request that brought nothing is remembered before it may be made again: the stalled-round cadence
    * above, and at the default T no shorter than the 30 s a request may take in [[request]].
    */
  private val retryMillis = 30 * gossipMillis

  /** (block, slot) of the peer finality we asked a justification for (CE 130). Touched on the voter thread only. */
  private val justificationRequests = new RequestTracker[(Hash, Long)](retryMillis)

  /** Start voting from the persisted voter set, or set 0 at genesis with the genesis validators.
    *
    * @param genesisKeys
    *   Ed25519 keys of the genesis state's validators (κ): set 0
    * @param genesisNextKeys
    *   of its next-epoch validators (γ_P): the set after the first epoch marker
    */
  def start(genesis: Hash, genesisKeys: IndexedSeq[Array[Byte]], genesisNextKeys: IndexedSeq[Array[Byte]]): Unit =
    genesisHash = genesis
    genesisNext = Some(genesisNextKeys.toList)
    val set = chain.meta.grandpaSet.filter(b => b.length >= 40 && (b.length - 40) % 32 == 0) match
      case Some(b) =>
        val setId = LittleEndian.get(b, 0, 8)
        val base = Hash(b.slice(8, 40))
        val stored = b.drop(40).grouped(32).toIndexedSeq
        // A record written before the keys were kept names only the base: its voters are the validators of the
        // epoch the base begins, read from its state or from the marker before it.
        val voters =
          if stored.nonEmpty then stored
          else if base == genesis then genesisKeys
          else
            chain
              .stateViewAt(base)
              .map(_.validators.current.map(_.ed25519.bytes))
              .orElse(markerBefore(base).map(_.toIndexedSeq))
              .getOrElse(genesisNextKeys)
        val baseVote = Vote(base, adapter.stateRootOf(base).getOrElse(Hash.zero), adapter.slotOf(base).getOrElse(0L))
        VoterSet(setId, baseVote, voters)
      case None =>
        val root = chain.postStateRoot(genesis).getOrElse(chain.finalized.stateRoot)
        VoterSet(0, Vote(genesis, root, adapter.slotOf(genesis).getOrElse(0L)), genesisKeys)
    voter = new GrandpaVoter(
      adapter,
      outbound,
      set,
      keys,
      clock = clock,
      gossipMillis = gossipMillis,
      onFinalized = persist,
      onSetAdopted = s =>
        chain.meta.putGrandpaSet(
          LittleEndian.encode(s.setId, 8) ++ s.base.hash.bytes.toArray ++ s.keys.flatten,
          settlesCheckpoint = rebased && s.setId != GrandpaVoter.UnknownSet
        ),
      onProgress = (setId, round, votes, durable) =>
        chain.meta.putGrandpaRound(
          LittleEndian.encode(setId, 8) ++ LittleEndian.encode(round, 8) ++ votes.flatMap(encodeVoteMessage),
          durable
        ),
      resume = storedRound(set.setId),
      paused = () => silent
    )
    if chain.meta.grandpaCheckpoint.isDefined then rebaseAtFinalized()
    logger.info(s"GRANDPA voter started: set ${voter.voterSet.setId} round ${voter.round}, ${voter.voterSet.size} " +
      s"voters, voting=${keys.exists(k => voter.voterSet.members.contains(k.ed25519Key))}")
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

  private[node] def onVoterThread[A](f: GrandpaVoter => A): A =
    exec.submit(() => f(voter)).get(10, TimeUnit.SECONDS)

  private[node] def resendNow(): Unit = onVoterThread(_ => resendVotes())

  private def guarded(f: => Unit): Unit =
    try f
    catch case e: Exception => logger.error("GRANDPA step failed", e)

  /** The round and our votes in it persisted for `setId` before a restart. */
  /** Keys of the nearest epoch marker strictly below `h` on its chain, while the headers are held. */
  private def markerBefore(h: Hash): Option[List[Array[Byte]]] =
    Iterator
      .iterate(adapter.parentOf(h))(_.flatMap(adapter.parentOf))
      .takeWhile(_.isDefined)
      .flatten
      .map(adapter.epochMarkerKeys)
      .collectFirst { case Some(keys) => keys }

  private def storedRound(setId: Long): Option[(Long, List[VoteMessage])] =
    chain.meta.grandpaRound.filter(b => b.length >= 16 && LittleEndian.get(b, 0, 8) == setId).map { b =>
      val votes = b.drop(16).grouped(VoteMessageSize).flatMap(v => decodeVoteMessage(v).toOption).toList
      (LittleEndian.get(b, 8, 8), votes)
    }

  private def persist(target: Vote, j: Justification, setChanged: Boolean): Unit =
    val encoded = encodeJustification(j)
    chain.meta.putGrandpaJustification(target.hash, encoded)
    if setChanged then
      chain.headerBytes(target.hash).foreach(h => chain.meta.putGrandpaWarp(j.setId, h ++ encoded))

  private def retryPending(imported: Hash): Unit =
    pendingCommits.take(imported).foreach((round, setId, c, anc) => applyCommit(round, setId, c, anc, None): Unit)

  @volatile private var rebasing = false

  @volatile private var rebased = false

  private def silent: Boolean = paused() || rebasing

  def restartAtFinalized(): Unit =
    rebasing = true
    exec.execute(() =>
      try guarded(if chain.meta.grandpaCheckpoint.isDefined then rebaseAtFinalized())
      finally rebasing = false
    )

  private def rebaseAtFinalized(): Unit =
    val f = chain.finalized
    val setId = chain.meta.grandpaCheckpoint.getOrElse(GrandpaVoter.UnknownSet)
    chain.stateViewAt(f.hash).foreach { view =>
      val epoch = f.slot / chain.config.epochLength
      var base = f.hash
      var cursor: Option[Hash] = Some(f.hash)
      var found = false
      while !found && cursor.isDefined do
        chain.header(cursor.get) match
          case Some(h) if h.slot.toLong / chain.config.epochLength == epoch =>
            if h.epochMark.isDefined then
              base = cursor.get
              found = true
            else cursor = Some(h.parent)
          case _ => cursor = None
      if !found then
        logger.warn("GRANDPA: the block that began the checkpoint's epoch is not held; the next voter set may be wrong")
      val baseVote = Vote(base, adapter.stateRootOf(base).getOrElse(Hash.zero), adapter.slotOf(base).getOrElse(f.slot))
      pendingCommits.prune(Long.MaxValue, Long.MaxValue)
      rebased = true
      voter.restartAt(VoterSet(setId, baseVote, view.validators.current.map(_.ed25519.bytes)))
    }

  private def applyCommit(
      round: Long,
      setId: Long,
      c: Commit,
      ancestries: List[Array[Byte]],
      from: Option[PeerConnection]
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
        if err == GrandpaVoter.BadPrecommitSignature then from.foreach(misbehaved(_, Offence.BadSignature))
        false

  /** Slots a peer's finality may be ahead before we ask for its justification. A voter is sent each commit (CE 150)
    * and waits one slot for it; an observer is sent none, so every block ahead is asked for.
    */
  private val commitsExpected = if keys.isDefined then 1 else 0

  /** Ask a voter whose UP 0 finality is ahead of ours for its justification (CE 130). */
  private def followPeers(): Unit =
    val ours = chain.finalized.slot
    pendingCommits.prune(ours, voter.voterSet.setId)
    justificationRequests.retain((_, slot) => slot > ours)
    // Any peer will do, voter or not: a justification is checked against the voter set, not against who sent it.
    distribution.peers.distinctBy(_.peerPublicKey).foreach { conn =>
      sync.peerFinal(conn).filter(f => f.slot > ours + commitsExpected).foreach { f =>
        // A node that joined late is voter sets behind: the peer's justification is signed by a set we have not
        // reached, so the set changes in between are asked for first (CE 153) and applied in order.
        val ourSet = voter.voterSet.setId
        warpRequests.retain(_ >= ourSet)
        if ourSet != GrandpaVoter.UnknownSet && chain.hasBlock(f.hash) && warpRequests.tryStart(ourSet, conn.peerPublicKey) then
          logger.debug(s"GRANDPA: peer final slot ${f.slot} is ahead of ours ($ours); asking for set changes from $ourSet")
          requestWarpSync(conn, ourSet)
        if justificationRequests.tryStart((f.hash, f.slot), conn.peerPublicKey) then
          requestJustification(conn, f.hash, f.slot)
        if ourSet == GrandpaVoter.UnknownSet then
          firstMarkerAbove(chain.finalized).filter(_._2 <= f.slot).foreach { (h, slot) =>
            if justificationRequests.tryStart((h, slot), conn.peerPublicKey) then requestJustification(conn, h, slot)
          }
      }
    }

  private def firstMarkerAbove(finalized: ChainHead): Option[(Hash, Long)] =
    var found: Option[(Hash, Long)] = None
    var cursor: Option[Hash] = Some(chain.best.hash)
    while cursor.exists(_ != finalized.hash) do
      chain.header(cursor.get) match
        case Some(h) if h.slot.toLong > finalized.slot =>
          if h.epochMark.isDefined then found = Some(cursor.get -> h.slot.toLong)
          cursor = Some(h.parent)
        case _ => cursor = None
    found

  // ---------------------------------------------------------------------------
  // handlers
  // ---------------------------------------------------------------------------

  private def onSingleMessage(f: (PeerConnection, MessageStream, Array[Byte]) => Unit): PeerStreamHandler =
    (conn: PeerConnection, stream: MessageStream) => stream.onMessage(msg => f(conn, stream, msg))

  /** The set we asked a warp sync from (ours at the time: the request is the same whatever later set the peer names),
    * and the (set, round) pairs we asked a catch-up to. Touched on the voter thread only.
    */
  private val warpRequests = new RequestTracker[Long](retryMillis)
  private val catchUpRequests = new RequestTracker[(Long, Long)](retryMillis)

  /** A peer in a later set or round than ours: ask it for warp sync (CE 153) or a catch-up (CE 152) once, since some
    * peers never send CE 151 state.
    */
  private def catchUpWith(conn: PeerConnection, round: Long, setId: Long): Unit =
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
  def voteHandler: PeerStreamHandler = onSingleMessage { (conn, stream, msg) =>
    stream.finish()
    decodeVoteMessage(msg) match
      case Right(m) =>
        intake.tryExecute(exec, msg.length.toLong, conn) {
          guarded {
            catchUpWith(conn, m.round, m.setId)
            voter.onVote(m)
          }
        }
      case Left(err) =>
        logger.warn(s"bad CE149 payload: $err")
        misbehaved(conn, Offence.Malformed)
  }

  /** CE 150. */
  def commitHandler: PeerStreamHandler = onSingleMessage { (conn, stream, msg) =>
    stream.finish()
    decodeCommitMessage(msg) match
      case Right((round, setId, c)) =>
        intake.tryExecute(exec, msg.length.toLong, conn) {
          guarded {
            catchUpWith(conn, round + 1, setId)
            applyCommit(round, setId, c, Nil, Some(conn))
          }
        }
      case Left(err) =>
        logger.warn(s"bad CE150 payload: $err")
        misbehaved(conn, Offence.Malformed)
  }

  /** CE 151. */
  def stateHandler: PeerStreamHandler = onSingleMessage { (conn, stream, msg) =>
    stream.finish()
    decodeState(msg) match
      case Right((round, setId, _)) =>
        intake.tryExecute(exec, msg.length.toLong, conn) {
          guarded(catchUpWith(conn, round, setId))
        }
      case Left(err) => logger.warn(s"bad CE151 payload: $err")
  }

  /** CE 152. */
  def catchUpHandler: PeerStreamHandler = onSingleMessage { (conn, stream, msg) =>
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
  def justificationHandler: PeerStreamHandler = onSingleMessage { (conn, stream, msg) =>
    served(conn, stream, msg) {
      val j = Option.when(msg.length == 32)(Hash(msg)).flatMap(h => chain.meta.grandpaJustification(h))
      j match
        case Some(bytes) => stream.send(bytes); stream.finish()
        case None        => stream.reset()
    }
  }

  private def served(conn: PeerConnection, stream: MessageStream, msg: Array[Byte])(answer: => Unit): Unit =
    val taken = serving.submit(conn, msg.length.toLong) {
      try answer
      catch
        case e: Exception =>
          logger.warn(s"${StreamKind.name(stream.kind)} handler failed: ${e.getMessage}")
          stream.reset()
    }
    if !taken then stream.reset()

  /** CE 153. */
  def warpSyncHandler: PeerStreamHandler = onSingleMessage { (conn, stream, msg) =>
    served(conn, stream, msg)(answerWarpSync(stream, msg))
  }

  private def answerWarpSync(stream: MessageStream, msg: Array[Byte]): Unit =
    val fragments =
      if msg.length != 4 then Nil
      else
        val from = LittleEndian.get(msg, 0, 4)
        Iterator
          .iterate(from)(_ + 1)
          .map(id => chain.meta.grandpaWarp(id))
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

  def register(network: io.forge.jam.network.JamnpNode): Unit =
    network
      .registerPeerHandler(StreamKind.GrandpaVote, voteHandler)
      .registerPeerHandler(StreamKind.GrandpaCommit, commitHandler)
      .registerPeerHandler(StreamKind.GrandpaState, stateHandler)
      .registerPeerHandler(StreamKind.GrandpaCatchUp, catchUpHandler)
      .registerPeerHandler(StreamKind.GrandpaJustificationRequest, justificationHandler)
      .registerPeerHandler(StreamKind.WarpSyncRequest, warpSyncHandler)
    ()

  // ---------------------------------------------------------------------------
  // requests
  // ---------------------------------------------------------------------------

  /** Send one request and collect the response, off the voter thread; `f` then runs on it. */
  private def request(conn: PeerConnection, kind: Byte, payload: Array[Byte])(f: Array[Byte] => Unit): Unit =
    distribution.submitRequest { () =>
      // One response message is expected; whatever else the peer sends on the stream is not kept.
      StreamRequest.exchange(conn, kind, Seq(payload), 10_000, () => 20_000, maxMessages = 1) match
        case Right(answer :: _) => exec.execute(() => guarded(f(answer)))
        case Right(Nil) => logger.debug(s"${StreamKind.name(kind)}: the peer closed the stream without an answer")
        case Left(failure) => logger.debug(s"${StreamKind.name(kind)}: ${failure.message}")
    }

  private def requestJustification(conn: PeerConnection, hash: Hash, slot: Long): Unit =
    request(conn, StreamKind.GrandpaJustificationRequest, hash.bytes.toArray) { bytes =>
      decodeJustification(bytes, chain.headerCodec) match
        case Right(j) if j.commit.target.hash == hash =>
          if applyCommit(j.round, j.setId, j.commit, j.ancestries, Some(conn)) then
            justificationRequests.succeeded((hash, slot))
        case Right(_)  => logger.warn("CE130 justification for another block")
        case Left(err) =>
          logger.warn(s"bad CE130 response: $err")
          misbehaved(conn, Offence.Malformed)
    }

  private def requestCatchUp(conn: PeerConnection, round: Long, setId: Long, onApplied: () => Unit = () => ()): Unit =
    request(conn, StreamKind.GrandpaCatchUp, encodeCatchUpRequest(round, setId)) { bytes =>
      decodeCatchUp(bytes).flatMap(voter.onCatchUp(setId, _)) match
        case Right(()) => onApplied()
        case Left(e)   => logger.debug(s"catch-up: $e")
    }

  private def requestWarpSync(conn: PeerConnection, fromSet: Long): Unit =
    request(conn, StreamKind.WarpSyncRequest, LittleEndian.encode(fromSet, 4)) { bytes =>
      decodeWarpFragments(bytes, chain.headerCodec) match
        case Right(fragments) =>
          logger.debug(s"CE153: ${fragments.size} set-change fragment(s) from set $fromSet")
          val applied = fragments.map { f =>
            val j = f.justification
            applyCommit(j.round, j.setId, j.commit, j.ancestries, Some(conn))
          }
          if applied.contains(true) then warpRequests.succeeded(fromSet)
        case Left(err) =>
          logger.warn(s"bad CE153 response: $err")
          misbehaved(conn, Offence.Malformed)
    }

object GrandpaService:

  val ForwardPeersPerStall: Int = 4

  /** Commits kept while their target block is fetched. Each one carries a supermajority of valid precommits of the
    * current set (checked before it is kept) for a block above our finalized one, so honest voters produce at most
    * one target per round, and finalizing the highest target finalizes the rest: when more than this many wait, the
    * lowest targets are the ones dropped.
    */
  val MaxPendingCommits: Int = 64

  /** CE 149-152 messages from peers that may wait for the voter thread, and the bytes they may hold. */
  val MaxQueuedMessages: Int = 4096
  val MaxQueuedBytes: Long = 32L * 1024 * 1024

private[node] final class StallClock(gossipMillis: Long):
  private var next = Long.MaxValue
  private var interval = 0L

  def restart(now: Long): Unit =
    next = now + 8 * gossipMillis
    interval = 4 * gossipMillis

  def due(now: Long): Boolean =
    if now < next then false
    else
      next = now + interval
      interval = math.min(interval * 2, 64 * gossipMillis)
      true

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
