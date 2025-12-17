package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

/**
 * Conformance tests for JAM fuzz report traces.
 *
 * These tests load JSON trace files from jam-conformance/fuzz-reports/0.7.1/traces/
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
 *
 * NOTE: Disabled until v0.7.2 traces are available.
 */
@org.scalatest.Ignore
class FuzzReportTraceSpec extends AnyFunSpec with Matchers:

  // Base directory for test vectors
  private val baseDir = sys.props.getOrElse("jam.base.dir", System.getProperty("user.dir"))
  private val tracesDir = Paths.get(baseDir, "jam-conformance", "fuzz-reports", "0.7.1", "traces")

  describe("v0.7.1 Fuzz Report Traces"):

    describe("single trace validation"):

      it("should successfully import first trace file"):
        assume(Files.exists(tracesDir), s"Trace directory not found: $tracesDir")

        val runner = new JsonTraceRunner(ChainConfig.TINY, verbose = true, compareKeyvals = true)

        // Target a specific trace for debugging
        val targetTraceId = "1763488465"
        val targetFileName = "00001705.json"

        val targetFile = tracesDir.resolve(targetTraceId).resolve(targetFileName)
        assume(Files.exists(targetFile), s"Target trace file not found: $targetFile")

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
        assume(Files.exists(tracesDir), s"Trace directory not found: $tracesDir")

        val runner = new JsonTraceRunner(ChainConfig.TINY, verbose = false, compareKeyvals = true)

        // Get first trace directory
        val firstTraceDir = Option(tracesDir.toFile.listFiles())
          .getOrElse(Array.empty[java.io.File])
          .filter(_.isDirectory)
          .sortBy(_.getName)
          .headOption

        assume(firstTraceDir.isDefined, "No trace directories found")

        val results = runner.runTraceDirectory(firstTraceDir.get.toPath)

        val successes = results.collect { case s: JsonTraceResult.Success => s }
        val failures = results.collect { case f: JsonTraceResult.Failure => f }
        val errors = results.collect { case e: JsonTraceResult.Error => e }

        // Report results
        println(s"\n=== Trace ${firstTraceDir.get.getName} Results ===")
        println(s"Total: ${results.size}, Passed: ${successes.size}, Failed: ${failures.size}, Errors: ${errors.size}")

        // Print failures
        failures.take(5).foreach { f =>
          println(s"\nFAILED [$f.fileName] slot=${f.slot}:")
          println(s"  Expected: ${f.expectedRoot.take(32)}...")
          println(s"  Actual:   ${f.actualRoot.take(32)}...")
          f.keyvalDiffs.foreach(d => println(s"  Diffs: $d"))
        }

        // Print errors
        errors.take(5).foreach(e => println(s"\nERROR [${e.fileName}]: ${e.errorMessage}"))

        failures shouldBe empty
        errors shouldBe empty

    describe("all traces validation"):

      it("should pass all v0.7.1 fuzz report traces"):
        assume(Files.exists(tracesDir), s"Trace directory not found: $tracesDir")

        val runner = new JsonTraceRunner(ChainConfig.TINY, verbose = false, compareKeyvals = true)
        val results = runner.runAllTraces(tracesDir)

        val successes = results.collect { case s: JsonTraceResult.Success => s }
        val failures = results.collect { case f: JsonTraceResult.Failure => f }
        val errors = results.collect { case e: JsonTraceResult.Error => e }

        // Group by trace ID
        val failuresByTrace = failures.groupBy(_.traceId)
        val errorsByTrace = errors.groupBy(_.traceId)

        // Report summary
        println(s"\n=== v0.7.1 Fuzz Report Traces Summary ===")
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
        assume(Files.exists(tracesDir), s"Trace directory not found: $tracesDir")

        val runner = new JsonTraceRunner(ChainConfig.TINY, verbose = true, compareKeyvals = true)
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
