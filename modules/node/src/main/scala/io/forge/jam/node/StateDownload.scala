package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{RangeProof, StateTrie, StateTrieBackend}
import io.forge.jam.network.{PeerConnection, StreamKind}

final class StateDownload(
    headerHash: Hash,
    stateRoot: Hash,
    backend: StateTrieBackend,
    ask: (PeerConnection, JamBytes) => Option[(List[Array[Byte]], IndexedSeq[(JamBytes, JamBytes)])],
    saving: (() => Unit) => Unit = f => f(),
    lied: PeerConnection => Unit = _ => ()
) extends LazyLogging:

  def run(peers: Iterable[PeerConnection]): Either[String, Long] =
    val trie = StateTrie.empty(backend)
    val left = peers.iterator
    var conn: Option[PeerConnection] = if left.hasNext then Some(left.next()) else None
    var start: Option[JamBytes] = Some(JamBytes(new Array[Byte](31)))
    var keys = 0L
    var bytes = 0L
    var done = false
    while !done && conn.isDefined do
      val peer = conn.get
      val verified = ask(peer, start.get).filter { (boundary, pairs) =>
        RangeProof.verify(stateRoot, start.get, boundary, pairs) match
          case Right(()) => true
          case Left(why) =>
            logger.warn(s"CE129: peer ${JamNode.hex(peer.peerKey).take(16)} sent a state range that does not verify ($why)")
            lied(peer)
            false
      }
      verified match
        case None =>
          conn = if left.hasNext then Some(left.next()) else None
        case Some((_, pairs)) =>
          if pairs.nonEmpty then
            saving { () =>
              trie.update(pairs.map((k, v) => k -> Some(v)))
              trie.save()
            }
            keys += pairs.size
            bytes += pairs.iterator.map(_._2.length.toLong).sum
          start = pairs.lastOption.flatMap(p => StateDownload.keyAfter(p._1))
          done = pairs.isEmpty || start.isEmpty
    if !done then Left(s"state of ${headerHash.toHex.take(18)}: no peer left to ask after $keys key(s)")
    else if trie.rootHash != stateRoot then
      Left(s"state of ${headerHash.toHex.take(18)}: downloaded root ${trie.rootHash.toHex.take(18)} is not ${stateRoot.toHex.take(18)}")
    else
      logger.info(s"state of ${headerHash.toHex.take(18)} downloaded: $keys key(s), $bytes value byte(s)")
      Right(keys)

object StateDownload:

  val MaxResponseBytes: Long = 4L * 1024 * 1024
  val TimeoutMillis: Long = 20000

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

  private val LastKey = JamBytes(Array.fill[Byte](31)(0xff.toByte))

  def request(headerHash: Hash, maxBytes: Long = MaxResponseBytes, timeoutMs: Long = TimeoutMillis)(
      conn: PeerConnection,
      start: JamBytes
  ): Option[(List[Array[Byte]], IndexedSeq[(JamBytes, JamBytes)])] =
    StreamRequest
      .collect(conn, StreamKind.StateRequest, SyncCodec.encodeStateRequest(headerHash, start, LastKey, maxBytes), timeoutMs)
      .flatMap {
        case List(boundary, pairs) =>
          try SyncCodec.decodeBoundaryNodes(boundary).toOption.map(_ -> SyncCodec.decodeStatePairs(pairs).toIndexedSeq)
          catch case _: Exception => None // a truncated or oversized pair list
        case _ => None
      }
