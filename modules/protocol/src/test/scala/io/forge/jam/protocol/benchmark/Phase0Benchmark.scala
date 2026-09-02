package io.forge.jam.protocol.benchmark

import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.traces.{TraceStep, BlockImporter, ImportResult}
import io.circe.Decoder
import io.forge.jam.core.types.block.Block.given

import java.util.Locale

object Phase0Benchmark:
  System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "OFF")
  System.setProperty("logback.statusListenerClass", "ch.qos.logback.core.status.NopStatusListener")

  val config: ChainConfig = ChainConfig.TINY

  given Decoder[TraceStep] = TraceStep.decoder

  private def suppressLogging(): Unit =
    org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME) match
      case logback: ch.qos.logback.classic.Logger =>
        logback.setLevel(ch.qos.logback.classic.Level.OFF)
      case _ => ()

  private def loadTrace(name: String): Option[Vector[TraceStep]] =
    TestFileLoader.getTraceStepFilenames(name) match
      case Left(_) => None
      case Right(names) if names.isEmpty => None
      case Right(names) =>
        val steps = names.flatMap { n =>
          TestFileLoader.loadJsonFromTestVectors[TraceStep](s"traces/$name", n).toOption
        }.toVector
        if steps.isEmpty then None else Some(steps)

  private case class Stats(steps: Int, rounds: Int, mean: Double, p50: Double, p90: Double, p99: Double, max: Double, errors: Int)

  private def percentile(sorted: Array[Double], p: Double): Double =
    if sorted.isEmpty then 0.0
    else sorted(math.min(sorted.length - 1, math.ceil(p / 100.0 * sorted.length).toInt - 1).max(0))

  private def runRounds(steps: Vector[TraceStep], rounds: Int, record: Boolean): (Array[Double], Int) =
    val times = Array.newBuilder[Double]
    var errors = 0
    for _ <- 1 to rounds do
      val importer = new BlockImporter(config)
      for step <- steps do
        val t0 = System.nanoTime()
        val ok =
          try
            importer.importBlock(step.block, step.preState) match
              case _: ImportResult.Success => true
              case _: ImportResult.Failure => false
          catch case _: Throwable => false
        val t1 = System.nanoTime()
        if ok then { if record then times += (t1 - t0) / 1e6 }
        else errors += 1
    (times.result(), errors)

  def main(args: Array[String]): Unit =
    Locale.setDefault(Locale.US)
    suppressLogging()

    if !TestFileLoader.canLocateTestVectors then
      println("ERROR: Test vectors not available"); System.exit(1)

    val useJfr = args.contains("--jfr")
    val warmupRounds = 3
    val measuredRounds = 5

    val traces = List("fallback", "safrole", "storage", "storage_light")
    val loaded = traces.flatMap(t => loadTrace(t).map(t -> _))
    loaded.foreach { (n, s) => println(s"loaded $n: ${s.size} steps") }

    // Warmup (untimed) — get C2 to steady state before any measurement
    println(s"\nwarmup: $warmupRounds rounds per trace ...")
    for (_, steps) <- loaded do runRounds(steps, warmupRounds, record = false)

    // Optional JFR over the measured section only
    val recording: Option[jdk.jfr.Recording] =
      if useJfr then
        val r = new jdk.jfr.Recording(jdk.jfr.Configuration.getConfiguration("profile"))
        r.start(); Some(r)
      else None

    println(s"measuring: $measuredRounds rounds per trace ...\n")
    val results = loaded.map { (name, steps) =>
      val (times, errors) = runRounds(steps, measuredRounds, record = true)
      java.util.Arrays.sort(times)
      val mean = if times.nonEmpty then times.sum / times.length else 0.0
      name -> Stats(steps.size, measuredRounds, mean,
        percentile(times, 50), percentile(times, 90), percentile(times, 99),
        if times.nonEmpty then times.last else 0.0, errors)
    }

    recording.foreach { r =>
      val out = java.nio.file.Path.of(sys.props.getOrElse("jam.phase0.jfr", "/tmp/phase0.jfr"))
      r.stop(); r.dump(out); r.close()
      println(s"JFR profile written to $out\n")
    }

    println("=" * 84)
    println("PHASE 0 — steady-state full block import, per-step latency (ms)")
    println("=" * 84)
    println(f"${"trace"}%-14s ${"steps"}%6s ${"mean"}%9s ${"p50"}%9s ${"p90"}%9s ${"p99"}%9s ${"max"}%9s ${"err"}%5s")
    println("-" * 84)
    for (name, s) <- results do
      println(f"$name%-14s ${s.steps}%6d ${s.mean}%9.2f ${s.p50}%9.2f ${s.p90}%9.2f ${s.p99}%9.2f ${s.max}%9.2f ${s.errors}%5d")
    println("=" * 84)
    println("compare: jam-conformance/fuzz-perf/0.7.2 (W3F hardware, includes warmup+harness)")
