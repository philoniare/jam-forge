package io.forge.jam.protocol.refine

import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.types.workpackage.WorkPackage
import scodec.Codec
import scodec.bits.BitVector

/** The auditable work-package bundle: the encoded work package followed by each
  * work item's extrinsic data, imported segments, and import justifications.
  *
  * Per the spec, only the justification Merkle paths carry a length prefix —
  * every other sequence length is determined by the work package itself.
  *
  * @param extrinsicData per work item, the extrinsic blobs in declaration
  *   order (lengths must match the item's (hash, len) references)
  * @param importSegments per work item, the imported segments (Csegmentsize
  *   octets each) in declaration order
  * @param justifications per work item, per imported segment, the Merkle path
  *   (32-byte hashes) justifying the segment against its segments-root
  */
final case class WorkPackageBundle(
    workPackage: WorkPackage,
    extrinsicData: IndexedSeq[IndexedSeq[Array[Byte]]],
    importSegments: IndexedSeq[IndexedSeq[Array[Byte]]],
    justifications: IndexedSeq[IndexedSeq[List[Array[Byte]]]]
):
  /** Serialize the bundle */
  def encode: Array[Byte] =
    val estimatedSize =
      64 +
        extrinsicData.iterator.flatten.map(_.length).sum +
        importSegments.iterator.flatten.map(_.length).sum +
        justifications.iterator.flatten.map(path => 4 + path.length * 32).sum
    val out = new java.io.ByteArrayOutputStream(estimatedSize)
    out.write(RefineFetch.encodeWorkPackage(workPackage))
    extrinsicData.foreach(_.foreach(out.write))
    importSegments.foreach(_.foreach(out.write))
    justifications.foreach(_.foreach { path =>
      out.write(JamCodecs.encodeCompactInteger(path.length.toLong))
      path.foreach(out.write)
    })
    out.toByteArray

object WorkPackageBundle:

  /** Decode a bundle blob back into its parts, driven by the embedded work
    * package's declared extrinsic lengths and import counts (used by auditors
    * reconstructing a bundle from erasure-coded shards).
    */
  def decode(bytes: Array[Byte]): Either[String, WorkPackageBundle] =
    val bits = BitVector(bytes)
    summon[Codec[WorkPackage]].decode(bits) match
      case scodec.Attempt.Failure(err) => Left(s"work package: ${err.message}")
      case scodec.Attempt.Successful(result) =>
        val wp = result.value
        var offset = (bits.size - result.remainder.size).toInt / 8
        def take(n: Int): Either[String, Array[Byte]] =
          if offset + n > bytes.length then Left("bundle truncated")
          else
            val out = java.util.Arrays.copyOfRange(bytes, offset, offset + n)
            offset += n
            Right(out)

        try
          val extrinsics = wp.items.map { item =>
            item.extrinsic.map { ref =>
              take(ref.len.toInt).fold(e => throw new RuntimeException(e), identity)
            }.toIndexedSeq
          }.toIndexedSeq

          val imports = wp.items.map { item =>
            item.importSegments.map { _ =>
              take(Csegmentsize.toInt).fold(e => throw new RuntimeException(e), identity)
            }.toIndexedSeq
          }.toIndexedSeq

          val justifications = wp.items.map { item =>
            item.importSegments.map { _ =>
              if offset >= bytes.length then
                throw new RuntimeException("bad justification length")
              val (count, consumed) = JamCodecs.decodeCompactInteger(bytes, offset)
              offset += consumed
              (0L until count).map { _ =>
                take(32).fold(e => throw new RuntimeException(e), identity)
              }.toList
            }.toIndexedSeq
          }.toIndexedSeq

          if offset != bytes.length then Left("trailing bytes in bundle")
          else Right(WorkPackageBundle(wp, extrinsics, imports, justifications))
        catch case e: RuntimeException => Left(e.getMessage)
