package io.forge.jam.node

import io.forge.jam.core.merkle.BinaryMerkle
import io.forge.jam.core.primitives.Hash

/** Bounded (128-root) insertion-evicting cache of a guaranteed report's
  * exported segments, keyed by the report's exports-root
  * (`packageSpec.exportsRoot`), so a peer's CE 148 segment request can be
  * served directly instead of re-refining the work package. Each segment's
  * import proof J_0 (GP 0.8.0 §14, `T(C(s), i)`) is kept alongside it.
  */
final class SegmentPool:
  private final case class Entry(segments: IndexedSeq[Array[Byte]], proofs: IndexedSeq[List[Array[Byte]]])

  private val bySegmentsRoot =
    java.util.Collections.synchronizedMap(
      new java.util.LinkedHashMap[Hash, Entry](128, 0.75f, false) {
        override def removeEldestEntry(e: java.util.Map.Entry[Hash, Entry]) = size > 128
      }
    )

  /** Record `segments` (each exactly `Csegmentsize` bytes) under
    * `segmentsRoot`, evicting the least-recently-inserted root past 128.
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
    Option(bySegmentsRoot.get(segmentsRoot)).flatMap { e =>
      if indices.forall(i => i >= 0 && i < e.segments.length) then
        Some(indices.map(i => (e.segments(i), e.proofs(i))).toIndexedSeq)
      else None
    }
