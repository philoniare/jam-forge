package io.forge.jam.protocol.benchmark

/**
 * CLI entry point for the JAM stress test.
 *
 * Usage: sbt stressTest
 * Or:    sbt "protocol/Test/runMain io.forge.jam.protocol.benchmark.StressTestApp --blocks=10000"
 *
 * Options:
 *   --blocks=N       Total blocks to process (default: 100000)
 *   --window=N       Blocks per metric window (default: 1000)
 *   --force-gc=N     Force GC every N windows (default: 10, 0 = disabled)
 *   --heap-threshold=N  Fail if retained heap grows > N% (default: 50)
 *   --throughput-threshold=N  Fail if throughput drops > N% (default: 30)
 */
object StressTestApp:
  // Suppress logging before any logger initialization
  System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "OFF")
  System.setProperty("logback.statusListenerClass", "ch.qos.logback.core.status.NopStatusListener")

  def main(args: Array[String]): Unit =
    suppressLogging()

    val config = parseArgs(args)
    val harness = new StressTestHarness(config)
    val report = harness.run()

    System.exit(if report.passed then 0 else 1)

  private def parseArgs(args: Array[String]): StressTestConfig =
    var config = StressTestConfig()
    for arg <- args do
      arg match
        case s if s.startsWith("--blocks=") =>
          config = config.copy(totalBlocks = s.stripPrefix("--blocks=").toLong)
        case s if s.startsWith("--window=") =>
          config = config.copy(windowSize = s.stripPrefix("--window=").toInt)
        case s if s.startsWith("--force-gc=") =>
          config = config.copy(forceGcEveryNWindows = s.stripPrefix("--force-gc=").toInt)
        case s if s.startsWith("--heap-threshold=") =>
          config = config.copy(heapGrowthThresholdPct = s.stripPrefix("--heap-threshold=").toInt)
        case s if s.startsWith("--throughput-threshold=") =>
          config = config.copy(throughputDropThresholdPct = s.stripPrefix("--throughput-threshold=").toInt)
        case _ =>
          System.err.println(s"Unknown argument: $arg")
    config

  private def suppressLogging(): Unit =
    try {
      val rootLogger = org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      rootLogger match
        case logback: ch.qos.logback.classic.Logger =>
          logback.setLevel(ch.qos.logback.classic.Level.OFF)
        case _ => ()
    } catch { case _: Exception => () }
