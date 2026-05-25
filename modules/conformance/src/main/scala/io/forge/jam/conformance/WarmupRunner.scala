package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig
import org.crac.{Context, Core, Resource}

import java.nio.file.{Files, Path, Paths}

/** CRaC warmup + checkpoint runner.
  */
object WarmupRunner extends Resource:

  /** Warm the JVM by replaying all available traces, take a CRaC checkpoint,
    * and on restore launch the fuzz server. Returns only after restore (the
    * original process exits inside Core.checkpointRestore()).
    *
    * @param socketPath
    *   socket the restored server binds (must exist at restore time)
    * @return
    *   true if restore-path executed cleanly; false if checkpoint was
    *   unavailable
    */
  def warmupAndCheckpoint(socketPath: Path): Boolean =
    runWarmup()

    Core.getGlobalContext.register(WarmupRunner)

    println("\nTriggering CRaC checkpoint...")
    println(s"Post-restore the server will bind: $socketPath")

    try
      Core.checkpointRestore()
      // Resumes here on restore.
      println("Restored from CRaC checkpoint.")
      true
    catch
      case e: UnsupportedOperationException =>
        println(s"CRaC checkpoint not available: ${e.getMessage}")
        println(
          "Continuing without checkpoint (JIT is still warm in this process)."
        )
        false
      case e: Exception =>
        println(
          s"Checkpoint failed: ${e.getClass.getSimpleName}: ${e.getMessage}"
        )
        e.printStackTrace()
        throw e

  /** Standalone warmup for the legacy `warmup` CLI. Warms then checkpoints,
    * then exits — does not start a server post-restore.
    */
  def run(_args: List[String]): Unit =
    runWarmup()
    Core.getGlobalContext.register(WarmupRunner)
    println("\nTriggering CRaC checkpoint...")
    try
      Core.checkpointRestore()
      println("Restored from checkpoint.")
    catch
      case e: UnsupportedOperationException =>
        println(s"CRaC checkpoint not available: ${e.getMessage}")
      case e: Exception =>
        println(s"Checkpoint failed: ${e.getMessage}")
        e.printStackTrace()
        sys.exit(1)

  private def runWarmup(): Unit =
    val baseDir =
      sys.props.getOrElse("jam.base.dir", System.getProperty("user.dir"))
    val tracesDir = resolveTracesDir(baseDir)

    println("=== CRaC Warmup Runner ===")
    println(s"Base directory: $baseDir")
    println(s"Traces directory: $tracesDir")

    if !Files.exists(tracesDir) then
      println(s"ERROR: Traces directory not found: $tracesDir")
      println(
        "Set JAM_TRACES_DIR or place traces under jam-conformance/fuzz-reports/0.7.2/traces"
      )
      sys.exit(1)

    println("Processing all available trace directories...")

    val runner = new JsonTraceRunner(
      ChainConfig.TINY,
      verbose = false,
      compareKeyvals = false
    )

    val startTime = System.nanoTime()
    val results = runner.runAllTraces(tracesDir, maxTraces = 3)
    val elapsed = (System.nanoTime() - startTime) / 1_000_000

    val successes = results.count(_.isInstanceOf[JsonTraceResult.Success])
    val failures = results.count(_.isInstanceOf[JsonTraceResult.Failure])
    val errors = results.count(_.isInstanceOf[JsonTraceResult.Error])

    println(s"\n=== Warmup Complete ===")
    println(s"Processed: ${results.size} blocks in ${elapsed}ms")
    println(s"Success: $successes, Failures: $failures, Errors: $errors")
    println(s"Throughput: ${
        if elapsed > 0 then results.size * 1000 / elapsed else 0
      } blocks/sec")

    if successes == 0 then
      println(
        "WARNING: No successful block imports during warmup; JIT may not be fully warmed."
      )

  private def resolveTracesDir(baseDir: String): Path =
    sys.env
      .get("JAM_TRACES_DIR")
      .map(Paths.get(_))
      .getOrElse(
        Paths.get(baseDir, "jam-conformance", "fuzz-reports", "0.7.2", "traces")
      )

  override def beforeCheckpoint(context: Context[_ <: Resource]): Unit =
    println("Preparing for CRaC checkpoint...")

  override def afterRestore(context: Context[_ <: Resource]): Unit =
    println("CRaC restore: JVM resumed with warm JIT.")
