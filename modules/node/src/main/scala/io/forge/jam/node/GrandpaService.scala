package io.forge.jam.node

import java.util.concurrent.{ConcurrentHashMap, Executors, LinkedBlockingQueue, ScheduledExecutorService, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.LittleEndian
import io.forge.jam.core.primitives.Hash
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
    gossipMillis: Long = 1000L
) extends LazyLogging:

  private val exec: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "jam-grandpa"); t.setDaemon(true); t
  }

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
    def finalizeBlock(h: Hash): Either[String, Unit] = chain.finalizeAdopting(h).map(_ => ())

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
    distribution.peers.filter(c => members.contains(c.peerKey.toSeq)).distinctBy(_.peerKey.toSeq)

  private def toVoters(kind: Byte, payload: Array[Byte]): Unit =
    val peers = voterPeers
    if kind == StreamKind.GrandpaVote then peers.foreach(c => votesSentTo.add(c.peerKey.toSeq))
    peers.foreach(c => send(c, kind, payload))

  private def send(c: JamnpConnection, kind: Byte, payload: Array[Byte]): Unit =
    distribution.submitSend(() => c.openStream(kind), kind, payload)

  /** Voters that have our current round's votes. */
  private val votesSentTo = ConcurrentHashMap.newKeySet[Seq[Byte]]()
  @volatile private var votesRound = (0L, 0L)
  @volatile private var roundSince = 0L

  /** Resend our current round's votes to voters that connected since, and to every voter while the round is stalled
    * (votes lost in a partition are not resent by their senders).
    */
  private def resendVotes(): Unit =
    val now = System.currentTimeMillis()
    val round = (voter.voterSet.setId, voter.round)
    if round != votesRound then
      votesRound = round
      roundSince = now
      votesSentTo.clear()
    else if now - roundSince > 8 * gossipMillis then
      roundSince = now - 4 * gossipMillis
      votesSentTo.clear()
    val votes = voter.ourVotes
    if votes.nonEmpty then
      voterPeers.filterNot(c => votesSentTo.contains(c.peerKey.toSeq)).foreach { c =>
        votesSentTo.add(c.peerKey.toSeq)
        votes.foreach(v => send(c, StreamKind.GrandpaVote, encodeVoteMessage(v)))
      }

  /** Commits whose target block we are still fetching, by target. */
  private val pendingCommits = new ConcurrentHashMap[Hash, (Long, Long, Commit, List[Array[Byte]])]()
  private val requestedJustifications = ConcurrentHashMap.newKeySet[Hash]()

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
    voter = new GrandpaVoter(adapter, outbound, set, keys, gossipMillis = gossipMillis, onFinalized = persist)
    logger.info(s"GRANDPA voter started: set ${set.setId}, ${set.size} voters, voting=${keys.exists(k =>
        set.members.contains(k.ed25519Public.toSeq))}")
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

  private def persist(target: Vote, j: Justification, setChanged: Boolean): Unit =
    val encoded = encodeJustification(j)
    chain.putMeta(s"grandpa:just:${target.hash.toHex}", encoded)
    if setChanged then
      chain.headerBytes(target.hash).foreach(h => chain.putMeta(s"grandpa:warp:${j.setId}", h ++ encoded))
      chain.putMeta("grandpa:set", LittleEndian.encode(j.setId + 1, 8) ++ target.hash.bytes.toArray)

  private def retryPending(imported: Hash): Unit =
    Option(pendingCommits.remove(imported)).foreach((round, setId, c, anc) => applyCommit(round, setId, c, anc, None))

  private def applyCommit(
      round: Long,
      setId: Long,
      c: Commit,
      ancestries: List[Array[Byte]],
      from: Option[JamnpConnection]
  ): Unit =
    voter.onCommit(round, setId, c, ancestries) match
      case Right(Some(missing)) =>
        logger.debug(s"GRANDPA commit for ${c.target.short} waits for the block")
        pendingCommits.put(missing, (round, setId, c, ancestries))
        from.foreach(sync.fetchBlock(_, missing))
      case Right(None) => ()
      case Left(err)   => logger.debug(s"GRANDPA commit for ${c.target.short} rejected: $err")

  /** Ask a voter whose UP 0 finality is ahead of ours for its justification (CE 130). */
  private def followPeers(): Unit =
    val ours = chain.finalized.slot
    voterPeers.foreach { conn =>
      sync.peerFinal(conn).filter(f => f.slot > ours + 1).foreach { f =>
        if requestedJustifications.add(f.hash) then requestJustification(conn, f.hash)
      }
    }

  // ---------------------------------------------------------------------------
  // handlers
  // ---------------------------------------------------------------------------

  private def onSingleMessage(f: (JamnpConnection, JamnpStream, Array[Byte]) => Unit): StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) => stream.onMessage(msg => f(conn, stream, msg))

  /** CE 149. */
  def voteHandler: StreamHandler = onSingleMessage { (_, stream, msg) =>
    stream.finish()
    decodeVoteMessage(msg) match
      case Right(m)  => exec.execute(() => guarded(voter.onVote(m)))
      case Left(err) => logger.warn(s"bad CE149 payload: $err")
  }

  /** CE 150. */
  def commitHandler: StreamHandler = onSingleMessage { (conn, stream, msg) =>
    stream.finish()
    decodeCommitMessage(msg) match
      case Right((round, setId, c)) => exec.execute(() => guarded(applyCommit(round, setId, c, Nil, Some(conn))))
      case Left(err)                => logger.warn(s"bad CE150 payload: $err")
  }

  /** CE 151. */
  def stateHandler: StreamHandler = onSingleMessage { (conn, stream, msg) =>
    stream.finish()
    decodeState(msg) match
      case Right((round, setId, _)) =>
        exec.execute(() =>
          guarded {
            if setId > voter.voterSet.setId then requestWarpSync(conn, voter.voterSet.setId)
            else if voter.wantsCatchUp(round, setId) then requestCatchUp(conn, round - 1, setId)
          }
        )
      case Left(err) => logger.warn(s"bad CE151 payload: $err")
  }

  /** CE 152. */
  def catchUpHandler: StreamHandler = onSingleMessage { (_, stream, msg) =>
    decodeCatchUpRequest(msg) match
      case Right((round, setId)) =>
        exec.execute(() =>
          voter.catchUpFor(round, setId) match
            case Some(c) => stream.send(encodeCatchUp(c)); stream.finish()
            case None    => stream.reset()
        )
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
      val received = new LinkedBlockingQueue[Array[Byte]]()
      val done = new LinkedBlockingQueue[java.lang.Boolean]()
      stream.onMessage(received.offer(_))
      stream.onClosed(() => done.offer(true))
      stream.send(payload)
      stream.finish()
      if done.poll(20, TimeUnit.SECONDS) != null then
        Option(received.poll()).foreach(bytes => exec.execute(() => guarded(f(bytes))))
    }

  private def requestJustification(conn: JamnpConnection, hash: Hash): Unit =
    request(conn, StreamKind.GrandpaJustificationRequest, hash.bytes.toArray) { bytes =>
      decodeJustification(bytes, chain.headerCodec) match
        case Right(j) if j.commit.target.hash == hash =>
          applyCommit(j.round, j.setId, j.commit, j.ancestries, Some(conn))
        case Right(_)  => logger.warn("CE130 justification for another block")
        case Left(err) => logger.warn(s"bad CE130 response: $err")
    }

  private def requestCatchUp(conn: JamnpConnection, round: Long, setId: Long): Unit =
    request(conn, StreamKind.GrandpaCatchUp, encodeCatchUpRequest(round, setId)) { bytes =>
      decodeCatchUp(bytes).flatMap(voter.onCatchUp(setId, _)).left.foreach(e => logger.debug(s"catch-up: $e"))
    }

  private def requestWarpSync(conn: JamnpConnection, fromSet: Long): Unit =
    request(conn, StreamKind.WarpSyncRequest, LittleEndian.encode(fromSet, 4)) { bytes =>
      decodeWarpFragments(bytes, chain.headerCodec) match
        case Right(fragments) =>
          fragments.foreach { f =>
            val j = f.justification
            applyCommit(j.round, j.setId, j.commit, j.ancestries, Some(conn))
          }
        case Left(err) => logger.warn(s"bad CE153 response: $err")
    }
