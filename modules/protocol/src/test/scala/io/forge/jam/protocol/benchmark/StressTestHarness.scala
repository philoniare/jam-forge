package io.forge.jam.protocol.benchmark

import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.traces.{BlockImporter, ImportResult, RawState, Genesis}
import io.forge.jam.protocol.TestFileLoader
import io.circe.Decoder
import io.forge.jam.core.types.block.Block.given

import java.lang.management.ManagementFactory

case class StressTestConfig(
  totalBlocks: Long = 100_000L,
  windowSize: Int = 1000,
  forceGcEveryNWindows: Int = 10,
  baselineWindows: Int = 5,
  heapGrowthThresholdPct: Int = 50,
  throughputDropThresholdPct: Int = 30,
  chainConfig: ChainConfig = ChainConfig.TINY
)

case class MetricSnapshot(
  windowNum: Int,
  heapUsedMB: Double,
  gcCount: Long,
  gcTimeMs: Long,
  blocksProcessed: Int,
  windowDurationMs: Long,
  blocksPerSec: Double,
  avgBlockTimeMs: Double,
  p99BlockTimeMs: Double,
  retainedHeapMB: Option[Double]
)

case class StressTestReport(
  totalBlocks: Long,
  totalTimeMs: Long,
  windows: List[MetricSnapshot],
  baselineThroughput: Double,
  baselineRetainedMB: Double,
  finalThroughput: Double,
  finalRetainedMB: Double,
  passed: Boolean,
  failureReason: Option[String]
)

class StressTestHarness(config: StressTestConfig):

  private val memoryBean = ManagementFactory.getMemoryMXBean
  private val gcBeans = ManagementFactory.getGarbageCollectorMXBeans

  def run(): StressTestReport =
    // Load genesis
    val genesis = loadGenesis()
    val validators = DevKeyStore.getAllTinyValidators()
    val generator = new BlockGenerator(config.chainConfig, validators)
    val importer = new BlockImporter(config.chainConfig)

    println(s"=== JAM Stress Test ===")
    println(s"Total blocks: ${config.totalBlocks}")
    println(s"Window size: ${config.windowSize}")
    println()

    var currentState = genesis
    var slot = 1L
    val windows = scala.collection.mutable.ListBuffer[MetricSnapshot]()
    val blockTimes = new Array[Long](config.windowSize) // ring buffer for p99
    var windowBlockCount = 0
    var windowStartTime = System.nanoTime()
    var windowNum = 0
    val startGcCount = totalGcCount()
    val startGcTime = totalGcTimeMs()
    val overallStart = System.nanoTime()
    var errors = 0

    // Global tracking for avg/max/hangs
    var totalBlockTimeMs = 0L
    var maxBlockTimeMs = 0L
    var maxBlockNum = 0L
    var successCount = 0L
    val HANG_THRESHOLD_MS = 5000L // blocks taking >5s considered "hangs"
    var hangCount = 0L

    printHeader()

    for blockNum <- 1L to config.totalBlocks do
      val blockStart = System.nanoTime()

      try
        val block = generator.generateBlock(currentState, slot)
        val result = importer.importBlock(block, currentState)
        result match
          case ImportResult.Success(postState, _, _) =>
            currentState = postState
            slot += 1
          case ImportResult.Failure(err, msg) =>
            errors += 1
            if errors <= 5 then
              System.err.println(s"  Block $blockNum (slot $slot) failed: $err - $msg")
            // Skip this slot
            slot += 1
      catch
        case e: Exception =>
          errors += 1
          if errors <= 5 then
            System.err.println(s"  Block $blockNum exception: ${e.getMessage}")
          slot += 1

      val blockEnd = System.nanoTime()
      val blockMs = (blockEnd - blockStart) / 1_000_000
      blockTimes(windowBlockCount % config.windowSize) = blockMs
      windowBlockCount += 1

      // Global stats
      totalBlockTimeMs += blockMs
      if blockMs > maxBlockTimeMs then
        maxBlockTimeMs = blockMs
        maxBlockNum = blockNum
      if blockMs > HANG_THRESHOLD_MS then
        hangCount += 1
        if hangCount <= 10 then
          System.err.println(f"  SLOW block $blockNum: ${blockMs}ms")
      successCount += 1

      // Window checkpoint
      if windowBlockCount >= config.windowSize then
        windowNum += 1
        val windowEnd = System.nanoTime()
        val windowMs = (windowEnd - windowStartTime) / 1_000_000

        val forceGc = config.forceGcEveryNWindows > 0 && windowNum % config.forceGcEveryNWindows == 0
        val retained = if forceGc then
          System.gc()
          Thread.sleep(100)
          Some(heapUsedMB())
        else None

        val snapshot = MetricSnapshot(
          windowNum = windowNum,
          heapUsedMB = heapUsedMB(),
          gcCount = totalGcCount() - startGcCount,
          gcTimeMs = totalGcTimeMs() - startGcTime,
          blocksProcessed = windowBlockCount,
          windowDurationMs = windowMs,
          blocksPerSec = if windowMs > 0 then windowBlockCount.toDouble * 1000 / windowMs else 0,
          avgBlockTimeMs = if windowBlockCount > 0 then windowMs.toDouble / windowBlockCount else 0,
          p99BlockTimeMs = computeP99(blockTimes, windowBlockCount),
          retainedHeapMB = retained
        )
        windows += snapshot
        printSnapshot(snapshot)

        windowBlockCount = 0
        windowStartTime = System.nanoTime()

    val overallEnd = System.nanoTime()
    val totalTimeMs = (overallEnd - overallStart) / 1_000_000

    // Compute results
    val baselineSnapshots = windows.take(config.baselineWindows).toList
    val finalSnapshots = windows.takeRight(config.baselineWindows).toList

    val baselineThroughput = avg(baselineSnapshots.map(_.blocksPerSec))
    val finalThroughput = avg(finalSnapshots.map(_.blocksPerSec))

    val baselineRetained = avgOpt(baselineSnapshots.flatMap(_.retainedHeapMB))
    val finalRetained = avgOpt(finalSnapshots.flatMap(_.retainedHeapMB))

    val throughputDrop = if baselineThroughput > 0 then
      (1.0 - finalThroughput / baselineThroughput) * 100
    else 0.0

    val heapGrowth = if baselineRetained > 0 then
      (finalRetained / baselineRetained - 1.0) * 100
    else 0.0

    val adjacentDrops = windows.toList.sliding(2).collect {
      case List(a, b) if a.blocksPerSec > 0 => (1.0 - b.blocksPerSec / a.blocksPerSec) * 100
    }.toList
    val maxAdjacentDrop = if adjacentDrops.isEmpty then 0.0 else adjacentDrops.max
    val throughputFailed = maxAdjacentDrop > config.throughputDropThresholdPct

    val heapFailed = heapGrowth > config.heapGrowthThresholdPct
    val passed = !throughputFailed && !heapFailed && errors == 0

    val failureReason =
      if errors > 0 then Some(s"$errors blocks failed to import")
      else if throughputFailed then
        Some(
          f"Max adjacent window throughput drop $maxAdjacentDrop%.1f%% (threshold: ${config.throughputDropThresholdPct}%%)"
        )
      else if heapFailed then Some(f"Heap grew $heapGrowth%.1f%% (threshold: ${config.heapGrowthThresholdPct}%%)")
      else None

    // Print summary
    val avgBlockMs = if successCount > 0 then totalBlockTimeMs.toDouble / successCount else 0.0
    println()
    println("=" * 75)
    println(
      f"Total: ${config.totalBlocks} blocks in ${totalTimeMs}ms (${config.totalBlocks.toDouble * 1000 / totalTimeMs}%.1f blk/s)"
    )
    println(f"Successes: $successCount, Errors: $errors")
    println(f"Avg block time: $avgBlockMs%.2f ms")
    println(f"Max block time: $maxBlockTimeMs ms (block #$maxBlockNum)")
    println(f"Hangs (>${HANG_THRESHOLD_MS}ms): $hangCount")
    println(
      f"Baseline (windows 1-${config.baselineWindows}): $baselineThroughput%.1f blk/s, $baselineRetained%.0f MB retained"
    )
    println(
      f"Final (last ${config.baselineWindows}):          $finalThroughput%.1f blk/s, $finalRetained%.0f MB retained"
    )
    println(f"Overall throughput change: ${-throughputDrop}%.1f%% (expected with state growth)")
    println(f"Max adjacent window drop: $maxAdjacentDrop%.1f%% (threshold: ${config.throughputDropThresholdPct}%%)")
    if heapGrowth.abs > 1 then println(f"Heap change: $heapGrowth%.1f%%")
    println(s"RESULT: ${if passed then "PASS" else "FAIL"}")
    failureReason.foreach(r => println(s"  Reason: $r"))
    println("=" * 75)

    StressTestReport(
      totalBlocks = config.totalBlocks,
      totalTimeMs = totalTimeMs,
      windows = windows.toList,
      baselineThroughput = baselineThroughput,
      baselineRetainedMB = baselineRetained,
      finalThroughput = finalThroughput,
      finalRetainedMB = finalRetained,
      passed = passed,
      failureReason = failureReason
    )

  private def loadGenesis(): RawState =
    given Decoder[Genesis] = Genesis.decoder
    TestFileLoader
      .loadJsonFromTestVectors[Genesis]("traces/fuzzy", "genesis")
      .map(_.state)
      .getOrElse(throw new RuntimeException("Cannot load genesis from jamtestvectors/traces/fuzzy/genesis.json"))

  private def heapUsedMB(): Double =
    memoryBean.getHeapMemoryUsage.getUsed.toDouble / (1024 * 1024)

  private def totalGcCount(): Long =
    import scala.jdk.CollectionConverters.*
    gcBeans.asScala.map(_.getCollectionCount).filter(_ >= 0).sum

  private def totalGcTimeMs(): Long =
    import scala.jdk.CollectionConverters.*
    gcBeans.asScala.map(_.getCollectionTime).filter(_ >= 0).sum

  private def computeP99(times: Array[Long], count: Int): Double =
    if count == 0 then 0.0
    else
      val n = math.min(count, times.length)
      val sorted = times.take(n).sorted
      sorted((n * 0.99).toInt.min(n - 1)).toDouble

  private def avg(values: List[Double]): Double =
    if values.isEmpty then 0.0 else values.sum / values.size

  private def avgOpt(values: List[Double]): Double =
    if values.isEmpty then 0.0 else values.sum / values.size

  private def printHeader(): Unit =
    println(
      f"${"Window"}%7s ${"Blocks"}%7s ${"Time(ms)"}%9s ${"Blk/s"}%8s ${"Heap(MB)"}%9s ${"GC(ms)"}%8s ${"RetainedMB"}%11s ${"p99(ms)"}%8s"
    )
    println("-" * 75)

  private def printSnapshot(s: MetricSnapshot): Unit =
    val retained = s.retainedHeapMB.map(r => f"$r%.0f").getOrElse("-")
    println(
      f"${s.windowNum}%7d ${s.blocksProcessed}%7d ${s.windowDurationMs}%9d ${s.blocksPerSec}%8.1f ${s.heapUsedMB}%9.0f ${s.gcTimeMs}%8d $retained%11s ${s.p99BlockTimeMs}%8.0f"
    )
