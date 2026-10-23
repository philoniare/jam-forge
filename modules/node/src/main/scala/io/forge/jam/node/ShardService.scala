package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{ChainConfig, LittleEndian}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.db.ShardStore
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamKind}
import io.forge.jam.protocol.refine.{DaShards, ValidatorShards}

import scala.collection.mutable

/** DA shard protocols (JAMNP-S CE 137, 138, 139 and 140). Custody units are
  * stored per (erasure-root, shard index); validator v of a report on core c
  * holds shard (c·R + v) mod V ([[DaShards.shardIndexFor]]).
  */
final class ShardService(
    store: ShardStore,
    config: ChainConfig
) extends LazyLogging:
  import ShardService.*

  private val segmentShardSize = DaShards.segmentShardSize(config)

  // =========================================================================
  // server side
  // =========================================================================

  /** CE 137: `Erasure-Root ++ Shard Index` → bundle shard, the exported and
    * proof segment shards, then the justification, as three messages.
    */
  def shardDistributionHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        withCustody(stream, msg) { shards =>
          stream.send(shards.bundleShard)
          stream.send(concat(shards.segmentShards))
          stream.send(encodeJustification(shards.justification, segmentShardSize))
        }
      }

  /** CE 138: `Erasure-Root ++ Shard Index` → bundle shard, then the CE 137
    * justification with the segment-shard root appended. Served to any node;
    * non-validators are not throttled.
    */
  def auditShardHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        withCustody(stream, msg) { shards =>
          stream.send(shards.bundleShard)
          stream.send(encodeJustification(DaShards.auditJustification(shards), segmentShardSize))
        }
      }

  /** Look up the custody unit a CE 137/138 request names and answer with
    * `respond`, then FIN; a malformed request or a shard we don't hold is
    * rejected with a reset.
    */
  private def withCustody(stream: JamnpStream, msg: Array[Byte])(respond: ValidatorShards => Unit): Unit =
    val shards =
      if msg.length != 34 then None
      else
        val root = Hash(java.util.Arrays.copyOfRange(msg, 0, 32))
        val index = LittleEndian.get(msg, 32, 2).toInt
        store.get(root, index).flatMap(b => DaShards.decode(b).toOption)
    shards match
      case Some(s) =>
        respond(s)
        stream.finish()
      case None =>
        logger.debug(s"${StreamKind.name(stream.kind)}: no shard for request of ${msg.length} bytes")
        stream.reset()

  /** CE 139 (`verified` false) and CE 140 (`verified` true): every requested
    * segment shard in one message, then for CE 140 one justification message
    * per shard. Any unknown root, missing index or more than 2·W_M shards
    * rejects the whole request.
    */
  def segmentShardHandler(verified: Boolean): io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        val served =
          for
            groups <- decodeSegmentShardRequest(msg)
            if groups.map(_._3.size).sum <= MaxSegmentShardsPerStream
            empty = Option(Vector.empty[(ValidatorShards, Int)])
            selected <- groups.foldLeft(empty) { case (acc, (root, index, segs)) =>
              acc.flatMap { done =>
                store.get(root, index).flatMap(b => DaShards.decode(b).toOption).flatMap { shards =>
                  if segs.forall(i => i >= 0 && i < shards.segmentShards.size) then Some(done ++ segs.map(shards -> _))
                  else None
                }
              }
            }
          yield selected
        served match
          case Some(selected) =>
            stream.send(concat(selected.map((s, i) => s.segmentShards(i))))
            if verified then
              selected.foreach { (s, i) =>
                stream.send(encodeJustification(DaShards.segmentJustification(s, i), segmentShardSize))
              }
            stream.finish()
          case None =>
            logger.debug(s"${StreamKind.name(stream.kind)}: request not servable")
            stream.reset()
      }

  // =========================================================================
  // client side
  // =========================================================================

  /** CE 137: fetch and verify shard `shardIndex`'s custody unit from `conn`.
    * None on timeout, rejection, or verification failure.
    */
  def fetchShards(
      conn: JamnpConnection,
      erasureRoot: Hash,
      shardIndex: Int,
      timeoutMs: Long = 10000
  ): Option[ValidatorShards] =
    request(conn, StreamKind.ShardDistribution, encodeShardRequest(erasureRoot, shardIndex), timeoutMs)
      .flatMap {
        case List(bundle, segments, justification) if segments.length % segmentShardSize == 0 =>
          decodeJustification(justification, segmentShardSize).map { j =>
            ValidatorShards(shardIndex, bundle, segments.grouped(segmentShardSize).toIndexedSeq, j)
          }
        case _ => None
      }
      .filter { shards =>
        val ok = DaShards.verify(erasureRoot, config.validatorCount, shards)
        if !ok then logger.warn(s"shard $shardIndex failed verification against ${erasureRoot.toHex.take(18)}")
        ok
      }

  /** CE 138: fetch and verify bundle shard `shardIndex` from `conn`. */
  def fetchAuditShard(
      conn: JamnpConnection,
      erasureRoot: Hash,
      shardIndex: Int,
      timeoutMs: Long = 10000
  ): Option[Array[Byte]] =
    request(conn, StreamKind.AuditShardRequest, encodeShardRequest(erasureRoot, shardIndex), timeoutMs)
      .flatMap {
        case List(bundle, justification) =>
          decodeJustification(justification, segmentShardSize)
            .filter(j => DaShards.verifyAuditShard(erasureRoot, config.validatorCount, shardIndex, bundle, j))
            .map(_ => bundle)
        case _ => None
      }

  /** CE 139/140: fetch segment shards `segmentIndexes` of shard `shardIndex`.
    * With `verified`, each shard's justification is checked against the
    * erasure-root before the shards are returned; that needs the report's
    * `segmentCount` (exported plus paged-proof segments).
    */
  def fetchSegmentShards(
      conn: JamnpConnection,
      erasureRoot: Hash,
      shardIndex: Int,
      segmentIndexes: List[Int],
      verified: Boolean,
      segmentCount: Int = 0,
      timeoutMs: Long = 10000
  ): Option[IndexedSeq[Array[Byte]]] =
    val kind = if verified then StreamKind.SegmentShardRequestVerified else StreamKind.SegmentShardRequest
    request(conn, kind, encodeSegmentShardRequest(List((erasureRoot, shardIndex, segmentIndexes))), timeoutMs)
      .flatMap {
        case shardsMsg :: justifications if shardsMsg.length == segmentIndexes.size * segmentShardSize =>
          val shards = shardsMsg.grouped(segmentShardSize).toIndexedSeq
          if !verified then Option.when(justifications.isEmpty)(shards)
          else if justifications.size != shards.size then None
          else
            val ok = shards.indices.forall { k =>
              decodeJustification(justifications(k), segmentShardSize).exists(j =>
                ShardService.verifySegmentShard(
                  erasureRoot,
                  config.validatorCount,
                  shardIndex,
                  segmentIndexes(k),
                  segmentCount,
                  shards(k),
                  j
                )
              )
            }
            Option.when(ok)(shards)
        case _ => None
      }

  /** Fetch enough bundle shards (local store first, then CE 138 from `peers`)
    * to reconstruct the audit bundle for a report.
    */
  def reconstructBundle(
      erasureRoot: Hash,
      bundleLength: Int,
      peers: Iterable[JamnpConnection]
  ): Either[String, Array[Byte]] =
    val needed = DaShards.recoveryThreshold(config)
    val collected = mutable.Map.empty[Int, Array[Byte]]

    store.heldIndexes(erasureRoot).foreach { idx =>
      store.get(erasureRoot, idx).flatMap(b => DaShards.decode(b).toOption).foreach { s =>
        collected(idx) = s.bundleShard
      }
    }

    // Sweep each peer for every still-missing index until we have enough.
    // Fetched audit shards are not stored: they lack the segment shards a
    // CE 137 custody unit carries.
    val peerList = peers.filter(_.isOpen).toList
    var pi = 0
    while collected.size < needed && pi < peerList.length do
      val conn = peerList(pi)
      var idx = 0
      while collected.size < needed && idx < config.validatorCount do
        if !collected.contains(idx) then fetchAuditShard(conn, erasureRoot, idx).foreach(collected(idx) = _)
        idx += 1
      pi += 1

    if collected.size < needed then
      Left(s"${ShardService.TooFewShards}: only ${collected.size}/$needed recoverable")
    else DaShards.reconstructBundle(bundleLength, collected.toSeq, config)

  // =========================================================================
  // helpers
  // =========================================================================

  private def concat(parts: Iterable[Array[Byte]]): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream()
    parts.foreach(p => out.write(p))
    out.toByteArray

  /** Open an ephemeral stream, send one request, collect response messages
    * until the peer finishes; None on reset (no messages), error or timeout.
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
      stream.onMessage(m => messages.synchronized(messages += m))
      stream.onClosed(() => done.complete(messages.synchronized(messages.toList)))
      stream.send(payload)
      stream.finish()
      Some(done.get(timeoutMs, TimeUnit.MILLISECONDS)).filter(_.nonEmpty)
    catch
      case e: Exception =>
        logger.debug(s"shard request on ${StreamKind.name(kind)} failed: ${e.getMessage}")
        None

object ShardService:

  /** Prefix of the error when too few peers answered to rebuild a bundle, as opposed to a bundle that fails to decode. */
  val TooFewShards = "too few bundle shards"

  /** Most segment shards one CE 139/140 stream may request: 2·W_M with
    * W_M = 3072 (JAMNP-S CE 139/140, GP 0.8.0 W_M).
    */
  val MaxSegmentShardsPerStream: Int = 2 * 3072

  /** CE 137/138 request: `Erasure-Root ++ Shard Index(u16)`. */
  def encodeShardRequest(root: Hash, shardIndex: Int): Array[Byte] =
    root.bytes ++ LittleEndian.encode(shardIndex.toLong, 2)

  /** CE 139/140 request: `[Erasure-Root ++ Shard Index ++ len++[Segment Index(u16)]]`. */
  def encodeSegmentShardRequest(groups: List[(Hash, Int, List[Int])]): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream()
    groups.foreach { (root, shardIndex, segs) =>
      out.write(encodeShardRequest(root, shardIndex))
      out.write(JamCodecs.encodeCompactInteger(segs.size.toLong))
      segs.foreach(i => out.write(LittleEndian.encode(i.toLong, 2)))
    }
    out.toByteArray

  def decodeSegmentShardRequest(msg: Array[Byte]): Option[List[(Hash, Int, List[Int])]] =
    try
      val groups = mutable.ListBuffer.empty[(Hash, Int, List[Int])]
      var offset = 0
      while offset < msg.length do
        if offset + 34 > msg.length then return None
        val root = Hash(java.util.Arrays.copyOfRange(msg, offset, offset + 32))
        val shardIndex = LittleEndian.get(msg, offset + 32, 2).toInt
        offset += 34
        val (n, c) = JamCodecs.decodeCompactInteger(msg, offset)
        offset += c
        if n < 0 || offset + 2 * n > msg.length then return None
        val segs = List.tabulate(n.toInt)(k => LittleEndian.get(msg, offset + 2 * k, 2).toInt)
        offset += 2 * n.toInt
        groups += ((root, shardIndex, segs))
      Option.when(groups.nonEmpty)(groups.toList)
    catch case _: Exception => None

  /** A justification as JAMNP-S sends it: each co-path entry is `0 ++ Hash`,
    * `1 ++ Hash ++ Hash` (a 64-byte leaf of the erasure-root tree) or
    * `2 ++ Segment Shard` (a leaf of a segment-shard tree).
    */
  def encodeJustification(entries: List[Array[Byte]], segmentShardSize: Int): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream()
    entries.foreach { e =>
      e.length match
        case 32 => out.write(0)
        case 64 => out.write(1)
        case n if n == segmentShardSize => out.write(2)
        case n => throw new IllegalArgumentException(s"justification entry of $n bytes")
      out.write(e)
    }
    out.toByteArray

  def decodeJustification(bytes: Array[Byte], segmentShardSize: Int): Option[List[Array[Byte]]] =
    val out = mutable.ListBuffer.empty[Array[Byte]]
    var offset = 0
    while offset < bytes.length do
      val size = bytes(offset) match
        case 0 => 32
        case 1 => 64
        case 2 => segmentShardSize
        case _ => return None
      if offset + 1 + size > bytes.length then return None
      out += java.util.Arrays.copyOfRange(bytes, offset + 1, offset + 1 + size)
      offset += 1 + size
    Some(out.toList)

  /** Verify a CE 140 segment shard: its justification is `j ++ [b] ++ T(s, i)`
    * where `j` (the CE 137 co-path) is as long as leaf `shardIndex` is deep
    * in the erasure-root tree. The shard climbs T to the segment-shard root,
    * which with the bundle shard hash `b` forms the erasure-root leaf.
    */
  def verifySegmentShard(
      erasureRoot: Hash,
      validatorCount: Int,
      shardIndex: Int,
      segmentIndex: Int,
      segmentCount: Int,
      shard: Array[Byte],
      justification: List[Array[Byte]]
  ): Boolean =
    import io.forge.jam.core.merkle.BinaryMerkle
    val depth = pathLength(validatorCount, shardIndex)
    if justification.size <= depth || segmentIndex < 0 || segmentIndex >= segmentCount then false
    else
      val (outer, rest) = justification.splitAt(depth)
      val bundleShardHash = rest.head
      val segmentRoot =
        if segmentCount == 1 then Option.when(rest.tail.isEmpty)(BinaryMerkle.blake2b(shard))
        else recomputeRoot(shard, segmentIndex, segmentCount, rest.tail)
      segmentRoot.exists(r =>
        BinaryMerkle.verifyTrace(erasureRoot.bytes, bundleShardHash ++ r, shardIndex, validatorCount, outer)
      )

  /** Depth of leaf `index` in a well-balanced tree of `count` leaves. */
  private def pathLength(count: Int, index: Int): Int =
    var c = count
    var i = index
    var d = 0
    while c > 1 do
      val mid = (c + 1) / 2
      if i < mid then c = mid
      else
        i -= mid
        c -= mid
      d += 1
    d

  private def recomputeRoot(leaf: Array[Byte], index: Int, count: Int, path: List[Array[Byte]]): Option[Array[Byte]] =
    import io.forge.jam.core.merkle.BinaryMerkle
    val node = "node".getBytes("UTF-8")
    def go(idx: Int, c: Int, p: List[Array[Byte]]): Option[Array[Byte]] =
      if c == 1 then Option.when(p.isEmpty)(leaf)
      else
        p match
          case Nil => None
          case sibling :: tail =>
            val mid = (c + 1) / 2
            if idx < mid then go(idx, mid, tail).map(own => BinaryMerkle.blake2b(node ++ own ++ sibling))
            else go(idx - mid, c - mid, tail).map(own => BinaryMerkle.blake2b(node ++ sibling ++ own))
    go(index, count, path)
