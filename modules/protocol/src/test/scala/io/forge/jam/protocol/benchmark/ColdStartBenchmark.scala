package io.forge.jam.protocol.benchmark

import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.traces.{TraceStep, BlockImporter, ImportResult}
import io.circe.Decoder
import io.forge.jam.core.types.block.Block.given

import java.lang.management.ManagementFactory
import java.util.Locale
import scala.jdk.CollectionConverters.*

object ColdStartBenchmark:
  System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "OFF")
  System.setProperty("logback.statusListenerClass", "ch.qos.logback.core.status.NopStatusListener")

  val config: ChainConfig = ChainConfig.TINY
  given Decoder[TraceStep] = TraceStep.decoder

  private def suppressLogging(): Unit =
    org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME) match
      case logback: ch.qos.logback.classic.Logger =>
        logback.setLevel(ch.qos.logback.classic.Level.OFF)
      case _ => ()

  private def loadTrace(name: String): Vector[TraceStep] =
    TestFileLoader.getTraceStepFilenames(name) match
      case Left(err) => sys.error(s"trace $name unavailable: $err")
      case Right(names) =>
        names.flatMap { n =>
          TestFileLoader.loadJsonFromTestVectors[TraceStep](s"traces/$name", n).toOption
        }.toVector

  private def importAll(importer: BlockImporter, steps: Vector[TraceStep], times: Array[Double] | Null): Int =
    var errors = 0
    var i = 0
    for step <- steps do
      val t0 = System.nanoTime()
      val ok =
        try
          importer.importBlock(step.block, step.preState) match
            case _: ImportResult.Success => true
            case _: ImportResult.Failure => false
        catch case _: Throwable => false
      val t1 = System.nanoTime()
      if times != null then times(i) = (t1 - t0) / 1e6
      if !ok then errors += 1
      i += 1
    errors

  private def pct(sorted: Array[Double], p: Double): Double =
    if sorted.isEmpty then 0.0
    else sorted(math.min(sorted.length - 1, math.max(0, math.ceil(p / 100.0 * sorted.length).toInt - 1)))

  def main(args: Array[String]): Unit =
    Locale.setDefault(Locale.US)
    suppressLogging()
    val jvmStartMs = ManagementFactory.getRuntimeMXBean.getStartTime
    def sinceJvmStart: Double = (System.currentTimeMillis() - jvmStartMs) / 1000.0

    def argVal(flag: String): Option[String] =
      args.sliding(2).collectFirst { case Array(`flag`, v) => v }
    val traceName = argVal("--trace").getOrElse("storage")
    val warmupRounds = argVal("--warmup-rounds").map(_.toInt).getOrElse(0)
    val label = argVal("--label").getOrElse(if warmupRounds > 0 then "warmed" else "cold")

    if !TestFileLoader.canLocateTestVectors then
      println("ERROR: test vectors not found (run from repo root)"); System.exit(1)

    val gcNames = ManagementFactory.getGarbageCollectorMXBeans.asScala.map(_.getName).mkString(", ")
    val jvmArgs = ManagementFactory.getRuntimeMXBean.getInputArguments.asScala
      .filter(a => a.startsWith("-X") || a.startsWith("-XX")).mkString(" ")
    println(s"[$label] gc=[$gcNames] flags=[$jvmArgs]")

    // ---- warmup pipeline: imports over the OTHER traces only -----------------
    val tLoad0 = sinceJvmStart
    if warmupRounds > 0 then
      val warmupTraces = List("fallback", "safrole", "storage", "storage_light")
        .filterNot(_ == traceName)
        .map(n => loadTrace(n))
      val w0 = sinceJvmStart
      for _ <- 1 to warmupRounds; steps <- warmupTraces do
        importAll(new BlockImporter(config), steps, null)
      println(f"[$label] warmup: $warmupRounds rounds over ${warmupTraces.size} traces " +
        f"in ${sinceJvmStart - w0}%.1f s (ends at t=+${sinceJvmStart}%.1f s from JVM start)")

    // ---- the measured pass: ONE sequential run, W3F-style --------------------
    val steps = loadTrace(traceName)
    val times = new Array[Double](steps.size)
    val m0 = sinceJvmStart
    val errors = importAll(new BlockImporter(config), steps, times)
    val m1 = sinceJvmStart

    val firstTen = times.take(10).map(t => f"$t%.0f").mkString(" ")
    val sorted = times.clone(); java.util.Arrays.sort(sorted)
    val mean = if times.nonEmpty then times.sum / times.length else 0.0

    println(f"[$label] first 10 step times (ms): $firstTen")
    println(f"[$label] measured pass: ${steps.size} steps in ${(m1 - m0)}%.1f s " +
      f"(starts t=+$m0%.1f s, JSON preload ${m0 - tLoad0}%.1f s excluded)")
    println(f"RESULT label=$label trace=$traceName mean=$mean%.2f p50=${pct(sorted, 50)}%.2f " +
      f"p90=${pct(sorted, 90)}%.2f p99=${pct(sorted, 99)}%.2f max=${sorted.lastOption.getOrElse(0.0)}%.2f " +
      f"errors=$errors total_from_jvm_start=${m1}%.1f s")
