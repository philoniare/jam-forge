package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig
import org.crac.{Context, Core, Resource}

import java.nio.file.{Files, Paths}

/**
 * CRaC warmup runner for creating checkpoints with pre-warmed JVM.
 *
 * This runner uses the existing JsonTraceRunner to import real blocks from
 * fuzz-reports traces, warming up all hot paths before triggering a CRaC checkpoint.
 *
 * Hot paths warmed up:
 * - BlockImporter.importBlock()
 * - FullJamState.fromKeyvals()
 * - StateMerklization.stateMerklize()
 * - All 9 STFs in BlockPipeline
 * - PVM execution (for accumulation blocks)
 * - InstructionExecutor array dispatch
 * - BasicMemory region caching
 *
 * Usage:
 *   java -XX:CRaCCheckpointTo=/path/to/cr -jar jam-conformance.jar warmup --iterations=100
 *
 * After warmup completes, the JVM state is checkpointed. Restore with:
 *   java -XX:CRaCRestoreFrom=/path/to/cr -jar jam-conformance.jar fuzz /tmp/socket
 */
object WarmupRunner extends Resource:

  /** Default number of blocks to import for warmup */
  private val DefaultIterations = 100

  /**
   * Run warmup using existing trace files and trigger CRaC checkpoint.
   *
   * @param args Command line arguments: --iterations=N (default 100)
   */
  def run(args: List[String]): Unit =
    val iterations = args
      .find(_.startsWith("--iterations="))
      .map(_.stripPrefix("--iterations=").toInt)
      .getOrElse(DefaultIterations)

    val baseDir = sys.props.getOrElse("jam.base.dir", System.getProperty("user.dir"))
    val tracesDir = Paths.get(baseDir, "jam-conformance", "fuzz-reports", "0.7.2", "traces")

    println("=== CRaC Warmup Runner ===")
    println(s"Base directory: $baseDir")
    println(s"Traces directory: $tracesDir")
    println(s"Target iterations: $iterations blocks")

    if !Files.exists(tracesDir) then
      println(s"ERROR: Traces directory not found: $tracesDir")
      println("Make sure jam-conformance submodule is initialized:")
      println("  git submodule update --init --recursive")
      System.exit(1)

    // Count available trace files
    val traceSubdirs = Option(tracesDir.toFile.listFiles())
      .getOrElse(Array.empty[java.io.File])
      .filter(_.isDirectory)
      .sortBy(_.getName)

    val totalFiles = traceSubdirs.flatMap { subdir =>
      Option(subdir.listFiles())
        .getOrElse(Array.empty[java.io.File])
        .filter(f => f.isFile && f.getName.endsWith(".json") && f.getName != "genesis.json")
    }.length

    println(s"Available traces: ${traceSubdirs.length} directories, $totalFiles total files")

    // Calculate how many trace directories to process
    // Each trace directory has ~100 blocks
    val maxTraces = ((iterations / 100.0).ceil.toInt).max(1)
    println(s"Processing first $maxTraces trace directories...")

    // Use existing JsonTraceRunner - this warms up all hot paths
    val runner = new JsonTraceRunner(ChainConfig.TINY, verbose = false, compareKeyvals = false)

    val startTime = System.nanoTime()
    val results = runner.runAllTraces(tracesDir, maxTraces = maxTraces)
    val elapsed = (System.nanoTime() - startTime) / 1_000_000

    val successes = results.count(_.isInstanceOf[JsonTraceResult.Success])
    val failures = results.count(_.isInstanceOf[JsonTraceResult.Failure])
    val errors = results.count(_.isInstanceOf[JsonTraceResult.Error])

    println(s"\n=== Warmup Complete ===")
    println(s"Processed: ${results.size} blocks in ${elapsed}ms")
    println(s"Success: $successes, Failures: $failures, Errors: $errors")
    println(s"Throughput: ${if elapsed > 0 then results.size * 1000 / elapsed else 0} blocks/sec")

    if successes == 0 then
      println("WARNING: No successful block imports during warmup!")
      println("JIT may not be fully warmed. Check trace files.")

    // Register for CRaC lifecycle callbacks
    Core.getGlobalContext.register(WarmupRunner)

    // Trigger checkpoint
    println("\nTriggering CRaC checkpoint...")
    println("(JVM state will be saved to checkpoint directory)")

    try
      Core.checkpointRestore()
      // This code runs AFTER restore
      println("Restored from checkpoint successfully!")
    catch
      case e: UnsupportedOperationException =>
        println(s"CRaC checkpoint not available: ${e.getMessage}")
        println("Running without checkpoint (for local testing)")
      case e: Exception =>
        println(s"Checkpoint failed: ${e.getMessage}")
        e.printStackTrace()
        System.exit(1)

  // CRaC Resource interface implementation

  override def beforeCheckpoint(context: Context[_ <: Resource]): Unit =
    println("Preparing for checkpoint...")
    // Close any resources that can't be checkpointed
    // (file handles, sockets, etc. are handled automatically by CRaC)

  override def afterRestore(context: Context[_ <: Resource]): Unit =
    println("Restored from CRaC checkpoint!")
    println("JVM is pre-warmed and ready for high-performance operation.")
