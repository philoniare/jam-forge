package io.forge.jam.crypto.benchmark

import io.forge.jam.core.ChainConfig
import io.forge.jam.crypto.ErasureCoding

import java.util.Locale
import scala.util.Random

/**
 * Erasure coding benchmark at full config: 1023 validators, 684-byte pieces, so 342 shards
 * rebuild the data. Bundle encode at W_B, 342-of-1023 reconstruct from parity shards only (the
 * worst case) and from the systematic shards, one segment, and 3120 segments through the batch API the DA path uses.
 *
 * Args: optional subset of `bundle`, `segment`, `segments`. Prints `RESULT name value unit` lines, each the median
 * of 5 runs after one warmup run.
 */
object ErasureCodingBench:
  private val BundleSize = 13_791_360 // W_B
  private val SegmentSize = 4104 // W_G
  private val SegmentCount = 3120 // as in PolkajamComparisonBenchmark
  private val Runs = 5

  private val config = ChainConfig.FULL
  private val Validators = config.validatorCount
  private val Piece = config.ecPieceSize
  private val Needed = Piece / 2 // 342

  private def result(name: String, value: Double, unit: String): Unit =
    println(f"RESULT $name $value%.3f $unit")

  private def timeMs[A](f: => A): (A, Double) =
    val t0 = System.nanoTime()
    val a = f
    (a, (System.nanoTime() - t0) / 1e6)

  /** Median ms of [[Runs]] runs of `f`, after one warmup run. */
  private def bench(f: => Unit): Double =
    f
    Seq.fill(Runs)(timeMs(f)._2).sorted.apply(Runs / 2)

  private def encode(data: Array[Byte]): Array[Array[Byte]] =
    ErasureCoding.chunk(data, Piece, Validators).fold(e => throw e, identity)

  private def rebuild(shards: Array[ErasureCoding.Shard]): Unit =
    ErasureCoding.reconstruct(shards, Piece, Validators).fold(e => throw e, _ => ())

  private def bundle(rng: Random): Unit =
    val data = rng.nextBytes(BundleSize)
    result("ec_bundle_encode", bench(encode(data)), "ms")
    val shards = encode(data).zipWithIndex.map((d, i) => ErasureCoding.Shard(d, i))
    result("ec_bundle_reconstruct_parity", bench(rebuild(shards.takeRight(Needed))), "ms")
    result("ec_bundle_reconstruct_systematic", bench(rebuild(shards.take(Needed))), "ms")

  private def segment(rng: Random): Unit =
    val data = rng.nextBytes(SegmentSize)
    val n = 200
    result("ec_segment_encode", bench((1 to n).foreach(_ => encode(data))) * 1000 / n, "us")
    val shards = encode(data).zipWithIndex.map((d, i) => ErasureCoding.Shard(d, i)).takeRight(Needed)
    result("ec_segment_reconstruct_parity", bench((1 to n).foreach(_ => rebuild(shards))) * 1000 / n, "us")

  private def segments(rng: Random): Unit =
    val data = Vector.fill(SegmentCount)(rng.nextBytes(SegmentSize))
    def encodeAll() = ErasureCoding.chunkSegments(data, Piece, Validators).fold(e => throw e, identity)
    result(s"ec_segments_${SegmentCount}_encode", bench(encodeAll()), "ms")
    val encoded = encodeAll()
    val held = (Validators - Needed until Validators).map(v => ErasureCoding.Shard(encoded.joinedColumn(v), v)).toArray
    result(
      s"ec_segments_${SegmentCount}_reconstruct_parity",
      bench(ErasureCoding.reconstructSegments(held, SegmentCount, Piece, Validators).fold(e => throw e, _ => ())),
      "ms"
    )

  def main(args: Array[String]): Unit =
    Locale.setDefault(Locale.US)
    org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME) match
      case l: ch.qos.logback.classic.Logger => l.setLevel(ch.qos.logback.classic.Level.OFF)
      case _ => ()
    val only = args.toSet
    def enabled(name: String) = only.isEmpty || only.contains(name)
    println(s"ErasureCodingBench validators=$Validators piece=$Piece needed=$Needed")
    val rng = new Random(7)
    if enabled("bundle") then bundle(rng)
    if enabled("segment") then segment(rng)
    if enabled("segments") then segments(rng)
