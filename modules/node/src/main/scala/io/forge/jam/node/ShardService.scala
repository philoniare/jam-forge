package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.db.ShardStore
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamKind}
import io.forge.jam.protocol.refine.{DaShards, ValidatorShards}

import scala.collection.mutable

/** DA shard custody protocols */
final class ShardService(
    store: ShardStore,
    config: ChainConfig
) extends LazyLogging:

  // =========================================================================
  // server side
  // =========================================================================

  /** CE 137/138/140 handler: full custody unit for (erasure-root, index). */
  def custodyHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        parseRequest(msg) match
          case Some((root, index, _)) =>
            store.get(root, index) match
              case Some(bytes) => stream.send(bytes)
              case None => logger.debug(s"no shard ${index} for ${root.toHex.take(18)}")
            stream.finish()
          case None =>
            logger.warn("bad shard request")
            stream.close()
      }

  /** CE 139 handler: the requested segment shards only. */
  def segmentShardHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        parseRequest(msg) match
          case Some((root, index, segmentIndexes)) =>
            store.get(root, index).flatMap(b => DaShards.decode(b).toOption) match
              case Some(shards) =>
                val selected = segmentIndexes.flatMap(shards.segmentShards.lift)
                val out = new java.io.ByteArrayOutputStream()
                out.write(JamCodecs.encodeCompactInteger(selected.length.toLong))
                selected.foreach { s =>
                  out.write(JamCodecs.encodeCompactInteger(s.length.toLong))
                  out.write(s)
                }
                stream.send(out.toByteArray)
              case None => ()
            stream.finish()
          case None => stream.close()
      }

  // =========================================================================
  // client side
  // =========================================================================

  /** Fetch and verify validator `index`'s custody unit from `conn`. Returns
    * None on timeout, absence, or verification failure.
    */
  def fetchShards(
      conn: JamnpConnection,
      kind: Byte,
      erasureRoot: Hash,
      index: Int,
      timeoutMs: Long = 10000
  ): Option[ValidatorShards] =
    request(conn, kind, encodeRequest(erasureRoot, index, Nil), timeoutMs)
      .flatMap(msgs => msgs.headOption)
      .flatMap(bytes => DaShards.decode(bytes).toOption)
      .filter { shards =>
        val ok = shards.validatorIndex == index &&
          DaShards.verify(erasureRoot, config.validatorCount, shards)
        if !ok then logger.warn(s"shard $index failed verification against ${erasureRoot.toHex.take(18)}")
        ok
      }

  /** Fetch (unverified) segment shards via CE 139. */
  def fetchSegmentShards(
      conn: JamnpConnection,
      erasureRoot: Hash,
      index: Int,
      segmentIndexes: List[Int],
      timeoutMs: Long = 10000
  ): Option[IndexedSeq[Array[Byte]]] =
    request(
      conn,
      StreamKind.SegmentShardRequest,
      encodeRequest(erasureRoot, index, segmentIndexes),
      timeoutMs
    ).flatMap(_.headOption).flatMap { bytes =>
      try
        var offset = 0
        def readCompact(): Long =
          val (v, c) = JamCodecs.decodeCompactInteger(bytes, offset)
          offset += c
          v
        val n = readCompact().toInt
        Some(IndexedSeq.fill(n) {
          val len = readCompact().toInt
          val b = java.util.Arrays.copyOfRange(bytes, offset, offset + len)
          offset += len
          b
        })
      catch case _: Exception => None
    }

  /** Fetch enough bundle shards from `peers` (local store first) to
    * reconstruct the audit bundle for a report.
    */
  def reconstructBundle(
      erasureRoot: Hash,
      bundleLength: Int,
      peers: Iterable[JamnpConnection]
  ): Either[String, Array[Byte]] =
    val needed = config.ecPieceSize / 2
    val collected = mutable.Map.empty[Int, Array[Byte]]

    store.heldIndexes(erasureRoot).foreach { idx =>
      store.get(erasureRoot, idx).flatMap(b => DaShards.decode(b).toOption).foreach { s =>
        collected(idx) = s.bundleShard
      }
    }

    // Sweep each peer for every still-missing index until we have enough.
    val peerList = peers.filter(_.isOpen).toList
    var pi = 0
    while collected.size < needed && pi < peerList.length do
      val conn = peerList(pi)
      var idx = 0
      while collected.size < needed && idx < config.validatorCount do
        if !collected.contains(idx) then
          fetchShards(conn, StreamKind.AuditShardRequest, erasureRoot, idx).foreach { s =>
            collected(idx) = s.bundleShard
            store.put(erasureRoot, idx, s.encode)
          }
        idx += 1
      pi += 1

    if collected.size < needed then
      Left(s"only ${collected.size}/$needed bundle shards recoverable")
    else DaShards.reconstructBundle(bundleLength, collected.toSeq, config)

  // =========================================================================
  // helpers
  // =========================================================================

  private def encodeRequest(root: Hash, index: Int, segmentIndexes: List[Int]): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream(40)
    out.write(root.bytes.toArray)
    out.write(index & 0xff)
    out.write((index >> 8) & 0xff)
    if segmentIndexes.nonEmpty then
      out.write(JamCodecs.encodeCompactInteger(segmentIndexes.length.toLong))
      segmentIndexes.foreach(i => out.write(JamCodecs.encodeCompactInteger(i.toLong)))
    out.toByteArray

  private def parseRequest(msg: Array[Byte]): Option[(Hash, Int, List[Int])] =
    if msg.length < 34 then None
    else
      try
        val root = Hash(java.util.Arrays.copyOfRange(msg, 0, 32))
        val index = (msg(32) & 0xff) | ((msg(33) & 0xff) << 8)
        var offset = 34
        val segs =
          if offset >= msg.length then Nil
          else
            val (n, c) = JamCodecs.decodeCompactInteger(msg, offset)
            offset += c
            List.fill(n.toInt) {
              val (v, c2) = JamCodecs.decodeCompactInteger(msg, offset)
              offset += c2
              v.toInt
            }
        Some((root, index, segs))
      catch case _: Exception => None

  /** Open an ephemeral stream, send one request, collect response messages
    * until the peer closes, with a deadline.
    */
  private def request(
      conn: JamnpConnection,
      kind: Byte,
      payload: Array[Byte],
      timeoutMs: Long
  ): Option[List[Array[Byte]]] =
    try
      val stream = conn.openStream(kind).get(timeoutMs, TimeUnit.MILLISECONDS)
      val messages = mutable.ListBuffer.empty[Array[Byte]]
      val done = new CompletableFuture[List[Array[Byte]]]()
      stream.onMessage(messages += _)
      stream.onClosed(() => done.complete(messages.toList))
      stream.send(payload)
      stream.finish()
      Some(done.get(timeoutMs, TimeUnit.MILLISECONDS))
    catch
      case e: Exception =>
        logger.debug(s"shard request on ${StreamKind.name(kind)} failed: ${e.getMessage}")
        None
