package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.LittleEndian
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.primitives.Hash
import io.forge.jam.network.{JamnpConnection, StreamKind}
import io.forge.jam.protocol.refine.ImportResolver
import io.forge.jam.protocol.refine.ImportResolver.Import

/** Gets a work-package's imported segments for a guarantor: from the segments we exported ourselves when we hold
  * them, otherwise by CE 148 from peers (the exporting package's guarantors keep its segments), and failing that
  * rebuilt from the assurers' shards (`recover`), every segment checked against its segments-root by its
  * justification before it is used. A peer that sends a wrong segment or proof is not trusted for that root and the
  * next is asked. Blocks on the network: runs on I/O
  * threads.
  */
final class ImportFetcher(
    segments: SegmentPool,
    peers: () => Iterable[JamnpConnection],
    timeoutMs: Long = ImportFetcher.TimeoutMillis,
    maxPeers: Int = ImportFetcher.MaxPeersAsked,
    /** Last resort: rebuild the segments from erasure-coded shards (CE 139/140), verified. */
    recover: (Hash, Seq[Int]) => Option[Map[Int, (Array[Byte], List[Array[Byte]])]] = (_, _) => None
) extends LazyLogging:

  type Fetched = (IndexedSeq[IndexedSeq[Array[Byte]]], IndexedSeq[IndexedSeq[List[Array[Byte]]]])

  /** Per work item, the imported segments and their justifications, in import order. */
  def fetch(imports: IndexedSeq[IndexedSeq[Import]]): Either[String, Fetched] =
    val byRoot = imports.flatten.groupBy(_.segmentsRoot).view.mapValues(_.map(_.index).distinct.sorted).toMap
    val got = byRoot.map { (root, indices) => root -> fetchRoot(root, indices) }
    got.collectFirst { case (root, None) => root } match
      case Some(root) => Left(s"imported segments of ${root.toHex.take(18)} not available from us or any peer")
      case None =>
        val table = got.map((root, found) => root -> found.get)
        Right((
          imports.map(_.map(i => table(i.segmentsRoot)(i.index)._1)),
          imports.map(_.map(i => table(i.segmentsRoot)(i.index)._2))
        ))

  /** The segments at `indices` of `root`, each with its proof, verified; None when no source has them all. */
  private def fetchRoot(root: Hash, indices: Seq[Int]): Option[Map[Int, (Array[Byte], List[Array[Byte]])]] =
    def verified(found: Seq[(Array[Byte], List[Array[Byte]])]): Option[Map[Int, (Array[Byte], List[Array[Byte]])]] =
      Option.when(
        found.size == indices.size &&
          indices.zip(found).forall { case (i, (seg, proof)) => ImportResolver.verify(root, i, seg, proof) }
      )(indices.zip(found).toMap)
    segments.getWithProofs(root, indices).flatMap(verified).orElse {
      peers().iterator.filter(_.isOpen).take(maxPeers).map { conn =>
        StreamRequest
          .collect(conn, StreamKind.SegmentRequest, ImportFetcher.encodeRequest(root, indices), timeoutMs)
          .flatMap(ImportFetcher.decodeResponse(_, indices.size))
          .flatMap { found =>
            val ok = verified(found)
            if ok.isEmpty then logger.warn(s"CE148: peer ${JamNode.hex(conn.peerKey).take(16)} sent segments of ${root.toHex.take(18)} that do not verify")
            ok
          }
      }.collectFirst { case Some(found) => found }
    }.orElse(recover(root, indices))

object ImportFetcher:
  val TimeoutMillis: Long = 5000
  val MaxPeersAsked: Int = 6

  /** CE 148 request, one group: `Segments-Root ++ len++[Segment Index (u16)]`. */
  def encodeRequest(root: Hash, indices: Seq[Int]): Array[Byte] =
    root.bytes ++ JamCodecs.encodeCompactInteger(indices.size.toLong) ++
      indices.flatMap(i => LittleEndian.encode(i.toLong, 2)).toArray

  /** CE 148 response for `count` segments: the segments concatenated, then `len++[Hash]` per segment. */
  def decodeResponse(messages: List[Array[Byte]], count: Int): Option[Seq[(Array[Byte], List[Array[Byte]])]] =
    val size = Csegmentsize.toInt
    messages match
      case List(segs, proofs) if segs.length == count * size =>
        try
          var off = 0
          val paths = (0 until count).map { _ =>
            val (n, used) = JamCodecs.decodeCompactInteger(proofs, off)
            off += used
            if n < 0 || n > 32 || off + n * 32 > proofs.length then throw new IllegalArgumentException("bad proof")
            val path = (0 until n.toInt).map(k => java.util.Arrays.copyOfRange(proofs, off + 32 * k, off + 32 * (k + 1))).toList
            off += 32 * n.toInt
            path
          }
          Option.when(off == proofs.length)(
            (0 until count).map(i => java.util.Arrays.copyOfRange(segs, i * size, (i + 1) * size)).zip(paths)
          )
        catch case _: Exception => None
      case _ => None
