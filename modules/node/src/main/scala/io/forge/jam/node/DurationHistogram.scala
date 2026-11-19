package io.forge.jam.node

import java.util.concurrent.atomic.LongAdder

final class DurationHistogram(boundsSeconds: IndexedSeq[Double]):
  require(boundsSeconds.nonEmpty && boundsSeconds.sliding(2).forall(w => w.size < 2 || w(0) < w(1)), "bounds must ascend")

  private val boundsNanos: Array[Long] = boundsSeconds.map(b => math.round(b * 1e9)).toArray
  private val buckets: Array[LongAdder] = Array.fill(boundsNanos.length + 1)(new LongAdder)
  private val sumNanos = new LongAdder

  def observeNanos(nanos: Long): Unit =
    var i = 0
    while i < boundsNanos.length && nanos > boundsNanos(i) do i += 1
    buckets(i).increment()
    sumNanos.add(nanos)

  def snapshot: DurationHistogram.Snapshot =
    val counts = buckets.map(_.sum)
    val cumulative = counts.init.scanLeft(0L)(_ + _).tail.toVector
    DurationHistogram.Snapshot(boundsSeconds.toVector, cumulative, counts.sum, sumNanos.sum / 1e9)

object DurationHistogram:
  final case class Snapshot(bounds: Vector[Double], cumulative: Vector[Long], count: Long, sumSeconds: Double)

  object Snapshot:
    val empty: Snapshot = Snapshot(Vector.empty, Vector.empty, 0L, 0.0)

  val ImportBounds: Vector[Double] = Vector(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0)

final class ChainTimings:
  val blockImport = new DurationHistogram(DurationHistogram.ImportBounds)
  val walSync = new DurationHistogram(DurationHistogram.ImportBounds)
  val metaWrite = new DurationHistogram(DurationHistogram.ImportBounds)

  def snapshot: ChainTimings.Snapshot =
    ChainTimings.Snapshot(blockImport.snapshot, walSync.snapshot, metaWrite.snapshot)

object ChainTimings:
  final case class Snapshot(
      blockImport: DurationHistogram.Snapshot,
      walSync: DurationHistogram.Snapshot,
      metaWrite: DurationHistogram.Snapshot
  )
  object Snapshot:
    val empty: Snapshot =
      Snapshot(DurationHistogram.Snapshot.empty, DurationHistogram.Snapshot.empty, DurationHistogram.Snapshot.empty)
