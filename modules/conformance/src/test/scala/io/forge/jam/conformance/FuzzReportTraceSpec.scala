package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.accumulation.NativeRunner
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

/**
 * Conformance tests for JAM fuzz report traces.
 *
 * These tests load JSON trace files from jam-conformance/fuzz-reports/0.8.0/traces/
 * and verify that our implementation produces the expected post-state after importing each block.
 *
 * Each trace file contains:
 * - pre_state: Initial state with keyvals and state root
 * - block: Block to import with header and extrinsic
 * - post_state: Expected state after import with keyvals and state root
 *
 * The test flow for each file is:
 * 1. Parse JSON to get pre_state, block, and expected post_state
 * 2. Verify pre_state root matches computed root (sanity check)
 * 3. Import block using BlockImporter
 * 4. Compare actual post_state root with expected
 */
class FuzzReportTraceSpec extends AnyFunSpec with Matchers with BeforeAndAfterAll:
  override def afterAll(): Unit =
    NativeRunner.logSummary()

  // Base directory for test vectors
  private val baseDir = sys.props.getOrElse("jam.base.dir", System.getProperty("user.dir"))
  // TINY-config traces
  private val tracesDir = Paths.get(baseDir, "jam-conformance", "fuzz-reports", "0.8.0", "traces")
  // FULL-config traces
  private val fullTracesDir = Paths.get(baseDir, "jam-conformance", "fuzz-reports", "0.8.0", "full-traces")

  private val requireCorpus = sys.props.get("jam.fuzz.requireCorpus").exists(_.toBoolean)
  private val compareKeyvals = sys.props.get("jam.fuzz.compareKeyvals").exists(_.toBoolean)
  private def missingCorpus(msg: String): Nothing =
    if requireCorpus then fail(s"$msg (jam.fuzz.requireCorpus=true → treating missing corpus as failure)")
    else
      println(msg)
      cancel(msg)

  private def requireCorpusPath(path: java.nio.file.Path, label: String): Boolean =
    if Files.exists(path) then true
    else missingCorpus(s"[FUZZ-CORPUS MISSING] $label not found, skipping (1 suite skipped): $path")

  /** Trace directories under `dir`, sorted by name; a directory with none (empty or unreadable) is a missing corpus. */
  private def requireTraceDirs(dir: java.nio.file.Path, label: String): List[java.io.File] =
    val dirs =
      Option(dir.toFile.listFiles()).getOrElse(Array.empty[java.io.File]).filter(_.isDirectory).sortBy(_.getName)
    if dirs.isEmpty then
      missingCorpus(s"[FUZZ-CORPUS MISSING] $label has no trace directories, skipping (1 suite skipped): $dir")
    dirs.toList

  /** Step files of a trace directory, as [[JsonTraceRunner.runTraceDirectory]] selects them. */
  private def stepFiles(traceDir: java.io.File): List[java.io.File] =
    Option(traceDir.listFiles()).getOrElse(Array.empty[java.io.File])
      .filter(f => f.isFile && f.getName.endsWith(".json") && f.getName != "genesis.json" && f.getName != "report.json")
      .sortBy(_.getName)
      .toList

  describe("v0.8.0 Fuzz Report Traces"):

    describe("single trace validation"):

      it("should successfully import the configured target trace file"):
        requireCorpusPath(tracesDir, "TINY traces directory")

        val runner = new JsonTraceRunner(ChainConfig.TINY, verbose = true, compareKeyvals = compareKeyvals)

        // A debugging aid: -Djam.fuzz.target=<traceId>/<fileName> names the step to replay, and that file is then
        // required. Without it, replay the first step of the first trace directory in the corpus.
        val targetFile = sys.props.get("jam.fuzz.target") match
          case Some(target) =>
            val (targetTraceId, targetFileName) = target.split("/", 2) match
              case Array(t, f) if t.nonEmpty && f.nonEmpty => (t, f)
              case _ => fail(s"Invalid -Djam.fuzz.target='$target' — expected <traceId>/<fileName>")
            val file = tracesDir.resolve(targetTraceId).resolve(targetFileName)
            requireCorpusPath(file, s"Target trace file $targetTraceId/$targetFileName (-Djam.fuzz.target)")
            file
          case None =>
            requireTraceDirs(tracesDir, "TINY traces directory").flatMap(stepFiles).headOption.map(_.toPath).getOrElse(
              missingCorpus(s"[FUZZ-CORPUS MISSING] no trace step files found, skipping (1 suite skipped): $tracesDir")
            )
        info(s"target: ${tracesDir.relativize(targetFile)}")

        val result = runner.runSingleTrace(targetFile)

        result match
          case JsonTraceResult.Success(traceId, fileName, slot) =>
            info(s"SUCCESS: trace=$traceId file=$fileName slot=$slot")
          case JsonTraceResult.Failure(traceId, fileName, slot, expected, actual, diffs) =>
            fail(
              s"FAILURE: trace=$traceId file=$fileName slot=$slot\n  Expected: $expected\n  Actual: $actual\n  Diffs: ${diffs.getOrElse("N/A")}"
            )
          case JsonTraceResult.Error(traceId, fileName, errorMessage) =>
            fail(s"ERROR: trace=$traceId file=$fileName\n  $errorMessage")

    describe("full trace directory validation"):

      it("should pass all traces in a single trace directory"):
        requireCorpusPath(tracesDir, "TINY traces directory")

        val runner = new JsonTraceRunner(ChainConfig.TINY, verbose = false, compareKeyvals = compareKeyvals)

        val firstTraceDir = requireTraceDirs(tracesDir, "TINY traces directory").headOption

        val results = runner.runTraceDirectory(firstTraceDir.get.toPath)

        val successes = results.collect { case s: JsonTraceResult.Success => s }
        val failures = results.collect { case f: JsonTraceResult.Failure => f }
        val errors = results.collect { case e: JsonTraceResult.Error => e }

        // Report results
        println(s"\n=== Trace ${firstTraceDir.get.getName} Results ===")
        println(s"Total: ${results.size}, Passed: ${successes.size}, Failed: ${failures.size}, Errors: ${errors.size}")

        // Print failures
        failures.take(5).foreach { f =>
          println(s"\nFAILED [${f.fileName}] slot=${f.slot}:")
          println(s"  Expected: ${f.expectedRoot.take(32)}...")
          println(s"  Actual:   ${f.actualRoot.take(32)}...")
          f.keyvalDiffs.foreach(d => println(s"  Diffs: $d"))
        }

        // Print errors
        errors.take(5).foreach(e => println(s"\nERROR [${e.fileName}]: ${e.errorMessage}"))

        failures shouldBe empty
        errors shouldBe empty

    describe("all traces validation"):

      it("should pass all v0.8.0 fuzz report traces"):
        requireCorpusPath(tracesDir, "TINY traces directory")
        requireTraceDirs(tracesDir, "TINY traces directory")

        val runner = new JsonTraceRunner(ChainConfig.TINY, verbose = false, compareKeyvals = compareKeyvals)
        val results = runner.runAllTraces(tracesDir)

        val successes = results.collect { case s: JsonTraceResult.Success => s }
        val failures = results.collect { case f: JsonTraceResult.Failure => f }
        val errors = results.collect { case e: JsonTraceResult.Error => e }

        // Group by trace ID
        val failuresByTrace = failures.groupBy(_.traceId)
        val errorsByTrace = errors.groupBy(_.traceId)

        // Report summary
        println(s"\n=== v0.8.0 Fuzz Report Traces Summary ===")
        println(s"Total files: ${results.size}")
        println(s"Passed: ${successes.size}")
        println(s"Failed: ${failures.size} (in ${failuresByTrace.size} traces)")
        println(s"Errors: ${errors.size} (in ${errorsByTrace.size} traces)")

        // Print first few failures
        if failures.nonEmpty then
          println(s"\n--- First 10 Failures ---")
          failures.take(10).foreach { f =>
            println(s"[${f.traceId}/${f.fileName}] slot=${f.slot}")
            println(s"  Expected: ${f.expectedRoot.take(32)}...")
            println(s"  Actual:   ${f.actualRoot.take(32)}...")
          }

        // Print first few errors
        if errors.nonEmpty then
          println(s"\n--- First 10 Errors ---")
          errors.take(10).foreach(e => println(s"[${e.traceId}/${e.fileName}]: ${e.errorMessage.take(100)}"))

        // Assert all passed
        withClue(s"Failed traces: ${failuresByTrace.keys.mkString(", ")}") {
          failures shouldBe empty
        }
        withClue(s"Error traces: ${errorsByTrace.keys.mkString(", ")}") {
          errors shouldBe empty
        }

    describe("selective trace validation"):

      it("should pass first 5 traces (quick check)"):
        requireCorpusPath(tracesDir, "TINY traces directory")

        val runner = new JsonTraceRunner(ChainConfig.TINY, verbose = true, compareKeyvals = compareKeyvals)
        val results = runner.runAllTraces(tracesDir, maxTraces = 5)

        val successes = results.collect { case s: JsonTraceResult.Success => s }
        val failures = results.collect { case f: JsonTraceResult.Failure => f }
        val errors = results.collect { case e: JsonTraceResult.Error => e }

        println(s"\n=== Quick Check (5 traces) ===")
        println(s"Total: ${results.size}, Passed: ${successes.size}, Failed: ${failures.size}, Errors: ${errors.size}")

        failures.foreach(f => println(s"\nFAILED [${f.traceId}/${f.fileName}] slot=${f.slot}"))

        errors.foreach(e => println(s"\nERROR [${e.traceId}/${e.fileName}]: ${e.errorMessage}"))

        failures shouldBe empty
        errors shouldBe empty

    describe("FULL config traces validation"):

      it("should pass all FULL-config (1023-validator) fuzz report traces"):
        // jam-conformance publishes no FULL-config corpus for 0.8.0 (no fuzz-reports/0.8.0/full-traces), so its
        // absence from a present 0.8.0 corpus is not a missing corpus. The submodule not being
        // checked out, or a full-traces directory without traces, still is.
        requireCorpusPath(fullTracesDir.getParent, "fuzz-reports/0.8.0 directory")
        if !Files.exists(fullTracesDir) then
          cancel(
            s"no FULL corpus published for 0.8.0: jam-conformance has no ${fullTracesDir.getFileName} directory " +
              s"(not a missing corpus; add traces at $fullTracesDir to run this test)"
          )
        requireTraceDirs(fullTracesDir, "FULL traces directory")

        val runner = new JsonTraceRunner(ChainConfig.FULL, verbose = false, compareKeyvals = compareKeyvals)
        val results = runner.runAllTraces(fullTracesDir)

        val successes = results.collect { case s: JsonTraceResult.Success => s }
        val failures = results.collect { case f: JsonTraceResult.Failure => f }
        val errors = results.collect { case e: JsonTraceResult.Error => e }

        val failuresByTrace = failures.groupBy(_.traceId)
        val errorsByTrace = errors.groupBy(_.traceId)

        println(s"\n=== v0.8.0 FULL Config Traces Summary ===")
        println(s"Total files: ${results.size}")
        println(s"Passed: ${successes.size}")
        println(s"Failed: ${failures.size} (in ${failuresByTrace.size} traces)")
        println(s"Errors: ${errors.size} (in ${errorsByTrace.size} traces)")

        if failures.nonEmpty then
          println(s"\n--- First 10 Failures ---")
          failures.take(10).foreach { f =>
            println(s"[${f.traceId}/${f.fileName}] slot=${f.slot}")
            println(s"  Expected: ${f.expectedRoot.take(32)}...")
            println(s"  Actual:   ${f.actualRoot.take(32)}...")
          }

        if errors.nonEmpty then
          println(s"\n--- First 10 Errors ---")
          errors.take(10).foreach(e => println(s"[${e.traceId}/${e.fileName}]: ${e.errorMessage.take(100)}"))

        withClue(s"Failed traces: ${failuresByTrace.keys.mkString(", ")}") {
          failures shouldBe empty
        }
        withClue(s"Error traces: ${errorsByTrace.keys.mkString(", ")}") {
          errors shouldBe empty
        }
