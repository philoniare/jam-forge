package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{RangeProof, StateTrie, StateTrieBackend, TrieNode}
import io.forge.jam.network.{PeerConnection, StreamKind}

import scala.collection.mutable

final class StateDownload(
    headerHash: Hash,
    stateRoot: Hash,
    backend: StateTrieBackend,
    ask: (PeerConnection, JamBytes) => Option[(List[Array[Byte]], IndexedSeq[(JamBytes, JamBytes)])],
    saving: (() => Unit) => Unit = f => f(),
    lied: PeerConnection => Unit = _ => (),
    requestedBytes: Long = StateDownload.MaxResponseBytes,
    clock: () => Long = () => System.nanoTime() / 1_000_000,
    stallMillis: Long = StateDownload.StallMillis
) extends LazyLogging:

  def run(peers: Iterable[PeerConnection]): Either[String, Long] =
    val trie = StateTrie.empty(backend)
    val untried = mutable.Queue.from(peers)
    val slowPeers = mutable.Queue.empty[PeerConnection]
    def nextPeer(): Option[PeerConnection] =
      if untried.nonEmpty then Some(untried.dequeue())
      else if slowPeers.nonEmpty then Some(slowPeers.dequeue())
      else None
    var conn = nextPeer()
    var slowInTurn = 0
    var start: Option[JamBytes] = Some(JamBytes(new Array[Byte](31)))
    var keys = 0L
    var bytes = 0L
    var done = false
    var lastProgress = clock()
    var stalled = false
    while !done && !stalled && conn.isDefined do
      val peer = conn.get
      val asked = clock()
      val verified = ask(peer, start.get).filter { (boundary, pairs) =>
        RangeProof.verify(stateRoot, start.get, boundary, pairs) match
          case Right(()) => true
          case Left(why) =>
            logger.warn(s"CE129: peer ${JamNode.hex(peer.peerKey).take(16)} sent a state range that does not verify ($why)")
            lied(peer)
            false
      }
      val took = clock() - asked
      verified match
        case None =>
          conn = nextPeer()
          slowInTurn = 0
        case Some((boundary, pairs)) =>
          if pairs.nonEmpty then
            saving { () =>
              trie.update(pairs.map((k, v) => k -> Some(v)))
              trie.save()
            }
            keys += pairs.size
            bytes += pairs.iterator.map(_._2.length.toLong).sum
            lastProgress = clock()
          start = pairs.lastOption.flatMap(p => StateDownload.keyAfter(p._1))
          done = pairs.isEmpty || start.isEmpty
          if !done && StateDownload.isSlow(StateDownload.responseSize(boundary, pairs), took, requestedBytes) then
            slowInTurn += 1
            if slowInTurn >= StateDownload.SlowResponsesPerPeer then
              logger.info(
                s"CE129: peer ${JamNode.hex(peer.peerKey).take(16)} answered $slowInTurn time(s) slowly or with little; " +
                  s"going on from key ${start.get.toHex.take(16)} with the next peer"
              )
              slowPeers.enqueue(peer)
              conn = nextPeer()
              slowInTurn = 0
      if !done && clock() - lastProgress > stallMillis then stalled = true
    if stalled then
      Left(s"state of ${headerHash.toHex.take(18)}: nothing written for over $stallMillis ms, after $keys key(s)")
    else if !done then Left(s"state of ${headerHash.toHex.take(18)}: no peer left to ask after $keys key(s)")
    else if trie.rootHash != stateRoot then
      Left(s"state of ${headerHash.toHex.take(18)}: downloaded root ${trie.rootHash.toHex.take(18)} is not ${stateRoot.toHex.take(18)}")
    else
      logger.info(s"state of ${headerHash.toHex.take(18)} downloaded: $keys key(s), $bytes value byte(s)")
      Right(keys)

object StateDownload:

  val MaxResponseBytes: Long = 4L * 1024 * 1024
  val TimeoutMillis: Long = 20000

  val SlowMillis: Long = 5000

  val SlowResponsesPerPeer: Int = 3

  val StallMillis: Long = 6 * TimeoutMillis

  def isSlow(size: Long, tookMillis: Long, requestedBytes: Long): Boolean =
    size * 4 < requestedBytes || tookMillis > SlowMillis

  def responseSize(boundary: List[Array[Byte]], pairs: IndexedSeq[(JamBytes, JamBytes)]): Long =
    boundary.size.toLong * TrieNode.GpEncodingBytes +
      pairs.iterator.map((k, v) => k.length.toLong + JamCodecs.encodeCompactInteger(v.length.toLong).length + v.length).sum

  val MaxValueBytes: Long = 32L * 1024 * 1024

  val MaxBoundaryBytes: Long = 2L * (31 * 8 + 1) * TrieNode.GpEncodingBytes

  def responseBound(maxBytes: Long): Long = maxBytes + MaxBoundaryBytes + 31 + 9 + MaxValueBytes

  def keyAfter(key: JamBytes): Option[JamBytes] =
    val a = key.toArray
    var i = a.length - 1
    while i >= 0 && a(i) == 0xff.toByte do
      a(i) = 0
      i -= 1
    if i < 0 then None
    else
      a(i) = (a(i) + 1).toByte
      Some(JamBytes(a))

  private val log = com.typesafe.scalalogging.Logger(classOf[StateDownload])

  private val LastKey = JamBytes(Array.fill[Byte](31)(0xff.toByte))

  def request(
      headerHash: Hash,
      maxBytes: Long = MaxResponseBytes,
      timeoutMs: Long = TimeoutMillis,
      malformed: PeerConnection => Unit = _ => (),
      bound: Long => Long = responseBound
  )(
      conn: PeerConnection,
      start: JamBytes
  ): Option[(List[Array[Byte]], IndexedSeq[(JamBytes, JamBytes)])] =
    def refuse(why: String): None.type =
      log.warn(s"CE129: peer ${JamNode.hex(conn.peerKey).take(16)} sent a malformed state response ($why)")
      malformed(conn)
      None
    StreamRequest.exchange(
      conn,
      StreamKind.StateRequest,
      Seq(SyncCodec.encodeStateRequest(headerHash, start, LastKey, maxBytes)),
      timeoutMs,
      () => timeoutMs,
      until = StreamRequest.Until.MaxMessages,
      maxMessages = 2,
      excessFails = true,
      maxBytes = bound(maxBytes)
    ) match
      case Left(StreamRequest.Failure.Excess(why)) => refuse(why)
      case Left(_)                                  => None
      case Right(Nil)                               => None
      case Right(List(boundary, pairs)) =>
        try
          SyncCodec.decodeBoundaryNodes(boundary) match
            case Right(nodes) => Some(nodes -> SyncCodec.decodeStatePairs(pairs).toIndexedSeq)
            case Left(why)    => refuse(why)
        catch case e: Exception => refuse(Option(e.getMessage).getOrElse("undecodable pairs"))
      case Right(other) => refuse(s"${other.size} message(s), not 2")
