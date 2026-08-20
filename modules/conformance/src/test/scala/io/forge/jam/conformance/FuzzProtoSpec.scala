package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

/**
 * Integration tests for fuzz-proto conformance test vectors.
 *
 * These tests load test vectors from jam-conformance/fuzz-proto/examples/v1/
 * and verify that our implementation produces the expected outputs.
 */
class FuzzProtoSpec extends AnyFunSpec with Matchers:

  // Base directory for test vectors
  private val baseDir = sys.props.getOrElse("jam.base.dir", System.getProperty("user.dir"))
  private val examplesDir = Paths.get(baseDir, "jam-conformance", "fuzz-proto", "examples", "0.7.2")

  describe("FuzzProto Conformance Tests"):

    describe("no_forks (features=0x02, ancestry only)"):
      val noForksDir = examplesDir.resolve("no_forks")

      it("should pass all no_forks test cases"):
        assume(Files.exists(noForksDir), s"Test directory not found: $noForksDir")

        val runner = new ConformanceTestRunner(ChainConfig.TINY)
        val results = runner.runTests(noForksDir)

        val failures = results.collect { case f: TestResult.Failure => f }
        val errors = results.collect { case e: TestResult.Error => e }
        val successes = results.collect { case s: TestResult.Success => s }

        // Report results
        println(s"\n=== no_forks Results ===")
        println(s"Total: ${results.size}, Passed: ${successes.size}, Failed: ${failures.size}, Errors: ${errors.size}")

        // Print failures
        failures.foreach { f =>
          println(s"\nFAILED [${f.index}] ${f.messageType}:")
          println(s"  Expected: ${f.expected}")
          println(s"  Actual:   ${f.actual}")
        }

        // Print errors
        errors.foreach(e => println(s"\nERROR [${e.index}] ${e.messageType}: ${e.errorMessage}"))

        failures shouldBe empty
        errors shouldBe empty

    describe("forks (features=0x03, ancestry + forks)"):
      val forksDir = examplesDir.resolve("forks")

      it("should pass all forks test cases"):
        assume(Files.exists(forksDir), s"Test directory not found: $forksDir")

        val runner = new ConformanceTestRunner(ChainConfig.TINY)
        val results = runner.runTests(forksDir)

        val failures = results.collect { case f: TestResult.Failure => f }
        val errors = results.collect { case e: TestResult.Error => e }
        val successes = results.collect { case s: TestResult.Success => s }

        // Report results
        println(s"\n=== forks Results ===")
        println(s"Total: ${results.size}, Passed: ${successes.size}, Failed: ${failures.size}, Errors: ${errors.size}")

        // Print failures
        failures.foreach { f =>
          println(s"\nFAILED [${f.index}] ${f.messageType}:")
          println(s"  Expected: ${f.expected}")
          println(s"  Actual:   ${f.actual}")
        }

        // Print errors
        errors.foreach(e => println(s"\nERROR [${e.index}] ${e.messageType}: ${e.errorMessage}"))

        failures shouldBe empty
        errors shouldBe empty
