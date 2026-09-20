package io.forge.jam.node

import io.forge.jam.core.primitives.Hash

/** Bounded (128-root) insertion-evicting cache of a guaranteed report's
  * exported segments, keyed by the report's exports-root
  * (`packageSpec.exportsRoot`), so a peer's CE 148 segment request can be
  * served directly instead of re-refining the work package.
  */
final class SegmentPool:
  private val bySegmentsRoot =
    java.util.Collections.synchronizedMap(
      new java.util.LinkedHashMap[Hash, IndexedSeq[Array[Byte]]](128, 0.75f, false) {
        override def removeEldestEntry(
            e: java.util.Map.Entry[Hash, IndexedSeq[Array[Byte]]]
        ) = size > 128
      }
    )

  /** Record `segments` (each exactly `Csegmentsize` bytes) under
    * `segmentsRoot`, evicting the least-recently-inserted root past 128.
    */
  def put(segmentsRoot: Hash, segments: IndexedSeq[Array[Byte]]): Unit =
    bySegmentsRoot.put(segmentsRoot, segments)

  /** The segments at `indices` (in request order) for `segmentsRoot`, or
    * `None` if the root is unknown or any requested index is out of range.
    */
  def get(segmentsRoot: Hash, indices: Seq[Int]): Option[IndexedSeq[Array[Byte]]] =
    Option(bySegmentsRoot.get(segmentsRoot)).flatMap { segments =>
      if indices.forall(i => i >= 0 && i < segments.length) then
        Some(indices.map(segments(_)).toIndexedSeq)
      else None
    }
