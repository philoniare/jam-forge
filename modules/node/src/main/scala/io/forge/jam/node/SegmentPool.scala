package io.forge.jam.node

import io.forge.jam.core.merkle.BinaryMerkle
import io.forge.jam.core.primitives.Hash

/** Bounded (128 roots, `maxBytes` of segments and proofs) insertion-evicting cache of a guaranteed report's
  * exported segments, keyed by the report's exports-root
  * (`packageSpec.exportsRoot`), so a peer's CE 148 segment request can be
  * served directly instead of re-refining the work package. Each segment's
  * import proof J_0 (GP 0.8.0 §14, `T(C(s), i)`) is kept alongside it.
  */
final class SegmentPool(maxBytes: Long = SegmentPool.DefaultMaxBytes):
  private final case class Entry(segments: IndexedSeq[Array[Byte]], proofs: IndexedSeq[List[Array[Byte]]]):
    val bytes: Long = segments.iterator.map(_.length.toLong).sum + proofs.iterator.flatten.map(_.length.toLong).sum

  private val bySegmentsRoot = new ByteBoundedCache[Hash, Entry](128, maxBytes, _.bytes)

  /** Record `segments` (each exactly `Csegmentsize` bytes) under
    * `segmentsRoot`, evicting the least-recently-inserted roots past 128 or past `maxBytes`.
    */
  def put(segmentsRoot: Hash, segments: IndexedSeq[Array[Byte]]): Unit =
    val proofs = BinaryMerkle.pagedJustifications(segments, 0).map(_._1)
    bySegmentsRoot.put(segmentsRoot, Entry(segments, proofs))

  /** The segments at `indices` (in request order) for `segmentsRoot`, or
    * `None` if the root is unknown or any requested index is out of range.
    */
  def get(segmentsRoot: Hash, indices: Seq[Int]): Option[IndexedSeq[Array[Byte]]] =
    getWithProofs(segmentsRoot, indices).map(_.map(_._1))

  /** As [[get]], each segment paired with its import proof. */
  def getWithProofs(segmentsRoot: Hash, indices: Seq[Int]): Option[IndexedSeq[(Array[Byte], List[Array[Byte]])]] =
    bySegmentsRoot.get(segmentsRoot).flatMap { e =>
      if indices.forall(i => i >= 0 && i < e.segments.length) then
        Some(indices.map(i => (e.segments(i), e.proofs(i))).toIndexedSeq)
      else None
    }

object SegmentPool:
  val DefaultMaxBytes: Long = 512L << 20
