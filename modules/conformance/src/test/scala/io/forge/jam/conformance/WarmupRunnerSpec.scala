package io.forge.jam.conformance

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

class WarmupRunnerSpec extends AnyFunSpec with Matchers:

  describe("WarmupRunner.warmupIfEnabled") {

    it("returns without warming when JAM_WARMUP=0") {
      noException should be thrownBy WarmupRunner.warmupIfEnabled(Map("JAM_WARMUP" -> "0").get)
    }

    it("starts cold instead of exiting when the warmup traces directory is missing") {
      val missing = Files.createTempDirectory("jam-warmup").resolve("absent")
      noException should be thrownBy WarmupRunner.warmupIfEnabled(Map("JAM_TRACES_DIR" -> missing.toString).get)
    }

    it("warms on a trace directory and returns") {
      val traces = Files.createTempDirectory("jam-warmup")
      Files.createDirectories(traces.resolve("empty-trace"))
      noException should be thrownBy WarmupRunner.warmupIfEnabled(Map("JAM_TRACES_DIR" -> traces.toString).get)
    }
  }
