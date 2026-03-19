package io.forge.jam.protocol.benchmark

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class StressTestSpec extends AnyFunSpec with Matchers:

  // Suppress logging for stress tests
  System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "OFF")

  describe("Memory leak detection"):
    it("should not leak memory over 1000 blocks"):
      val config = StressTestConfig(
        totalBlocks = 1000,
        windowSize = 200,
        forceGcEveryNWindows = 2,
        baselineWindows = 2,
        heapGrowthThresholdPct = 100,
        throughputDropThresholdPct = 50
      )
      val harness = new StressTestHarness(config)
      val report = harness.run()

      report.passed shouldBe true
      report.finalRetainedMB should be > 0.0

  describe("Throughput stability"):
    it("should maintain throughput over 500 blocks"):
      val config = StressTestConfig(
        totalBlocks = 500,
        windowSize = 100,
        forceGcEveryNWindows = 0,
        baselineWindows = 2,
        throughputDropThresholdPct = 50
      )
      val harness = new StressTestHarness(config)
      val report = harness.run()

      report.passed shouldBe true
      report.windows.size should be >= 4
