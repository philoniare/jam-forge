package io.forge.jam.protocol.benchmark

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrie, StateTrieStore}
import io.forge.jam.crypto.ErasureCoding
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.traces.{BlockImporter, TraceStep}
import io.circe.Decoder
import io.forge.jam.core.types.block.Block.given

import java.util.Locale
import scala.util.Random

object PolkajamComparisonBenchmark:
  System.setProperty("logback.statusListenerClass", "ch.qos.logback.core.status.NopStatusListener")

  given Decoder[TraceStep] = TraceStep.decoder

  private val BundleSize = 13_791_360 // W_B
  private val SegmentSize = 4104 // W_G
  private val SegmentCount = 3120
  private val InsertBatch = 10_000

  private def result(name: String, value: Double, unit: String): Unit =
    println(f"RESULT $name $value%.2f $unit")

  private def timeMs[A](f: => A): (A, Double) =
    val t0 = System.nanoTime()
    val a = f
    (a, (System.nanoTime() - t0) / 1e6)

  private def randomEntries(rng: Random, n: Int): Vector[(JamBytes, JamBytes)] =
    Vector.fill(n)((JamBytes(rng.nextBytes(31)), JamBytes(rng.nextBytes(36))))

  private def trieBenchmark(n: Int): Unit =
    val rng = new Random(42)
    val first = randomEntries(rng, n)
    val second = randomEntries(rng, n)
    val store = new StateTrieStore(new InMemoryTrieBackend)
    val (_, populateMs) = timeMs(store.bootstrap(first))
    result("trie_populate_1m", populateMs, "ms")
    // Saved every InsertBatch keys: StateTrie keeps every superseded branch in its pending set until save(), so a
    // single 1M-key batch into a 1M-key trie runs out of a 12 GB heap.
    val (_, insertMs) = timeMs {
      var root = store.currentRoot
      second.grouped(InsertBatch).foreach { batch =>
        val trie = StateTrie.at(store.backend, root)
        trie.update(batch.map((k, v) => (k, Some(v))))
        trie.save()
        root = trie.rootHash
      }
      root
    }
    result("trie_insert_1m", insertMs, "ms")

  private def ecRoundTrip(name: String, pieces: Seq[Array[Byte]], config: ChainConfig): Unit =
    val recoveryOnly = config.validatorCount - config.ecPieceSize / 2
    val (chunked, encodeMs) = timeMs(pieces.map(p => ErasureCoding.chunk(p, config.ecPieceSize, config.validatorCount)))
    val shardSets = chunked.map {
      case Right(shards) =>
        shards.zipWithIndex.drop(shards.length - recoveryOnly).map((d, i) => ErasureCoding.Shard(d, i))
      case Left(err) => throw err
    }
    val (_, reconstructMs) = timeMs(
      shardSets.foreach(s =>
        ErasureCoding.reconstruct(s, config.ecPieceSize, config.validatorCount).fold(e => throw e, identity)
      )
    )
    result(s"${name}_encode", encodeMs, "ms")
    result(s"${name}_reconstruct", reconstructMs, "ms")

  private def ecBenchmark(): Unit =
    val config = ChainConfig.FULL
    val rng = new Random(7)
    val bundle = rng.nextBytes(BundleSize)
    val segments = Vector.fill(SegmentCount)(rng.nextBytes(SegmentSize))
    // One untimed pass so JIT and native-library loading are out of the measurement.
    ecRoundTrip("ec_warmup", Seq(bundle), config)
    ecRoundTrip("ec_bundle", Seq(bundle), config)
    ecRoundTrip("ec_segments", segments, config)

  private def importBenchmark(rounds: Int): Unit =
    val config = ChainConfig.TINY
    for name <- List("fallback", "safrole", "storage", "storage_light") do
      TestFileLoader.getTraceStepFilenames(name).toOption.foreach { files =>
        val steps = files.flatMap(f => TestFileLoader.loadJsonFromTestVectors[TraceStep](s"traces/$name", f).toOption)
        def round(): Double =
          val importer = new BlockImporter(config)
          timeMs(steps.foreach(s => importer.importBlock(s.block, s.preState)))._2
        for _ <- 1 to 3 do round()
        val total = (1 to rounds).map(_ => round()).sum
        result(s"import_$name", total / rounds / steps.size, "ms/block")
      }

  def main(args: Array[String]): Unit =
    Locale.setDefault(Locale.US)
    org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME) match
      case l: ch.qos.logback.classic.Logger => l.setLevel(ch.qos.logback.classic.Level.OFF)
      case _ => ()
    val only = args.toSet
    def enabled(name: String) = only.isEmpty || only.contains(name)
    if enabled("trie") then trieBenchmark(1_000_000)
    if enabled("ec") then ecBenchmark()
    if enabled("import") && TestFileLoader.canLocateTestVectors then importBenchmark(rounds = 5)
