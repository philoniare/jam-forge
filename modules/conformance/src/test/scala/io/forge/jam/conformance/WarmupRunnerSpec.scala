package io.forge.jam.conformance

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}

class WarmupRunnerSpec extends AnyFunSpec with Matchers:

  private val baseDir = sys.props.getOrElse("jam.base.dir", System.getProperty("user.dir"))
  private val preimagesLight = Paths.get(baseDir, "jamtestvectors", "traces", "preimages_light")

  /** A warmup corpus dir holding the genesis and first `n` steps of preimages_light, laid out as in the image. */
  private def smallCorpus(n: Int): Path =
    assume(Files.isRegularFile(preimagesLight.resolve("genesis.bin")), s"$preimagesLight not found")
    val corpus = Files.createTempDirectory("jam-warmup")
    val trace = Files.createDirectories(corpus.resolve("preimages_light"))
    val source = FuzzClient.TraceDir.find(preimagesLight).get
    for file <- source.genesis +: source.steps.take(n) do Files.copy(file, trace.resolve(file.getFileName))
    corpus

  describe("WarmupRunner.warmupIfEnabled") {

    it("returns without warming when JAM_WARMUP=0") {
      WarmupRunner.warmupIfEnabled(Map("JAM_WARMUP" -> "0").get) shouldBe None
    }

    it("starts cold instead of exiting when the warmup traces directory is missing") {
      val missing = Files.createTempDirectory("jam-warmup").resolve("absent")
      WarmupRunner.warmupIfEnabled(Map("JAM_TRACES_DIR" -> missing.toString).get) shouldBe None
    }

    it("starts cold when the directory has no binary traces") {
      val traces = Files.createTempDirectory("jam-warmup")
      Files.writeString(Files.createDirectories(traces.resolve("json-only")).resolve("00000001.json"), "{}")
      WarmupRunner.warmupIfEnabled(Map("JAM_TRACES_DIR" -> traces.toString).get) shouldBe None
    }

    it("replays the binary corpus through the server's socket path, every round, with matching roots") {
      val corpus = smallCorpus(5)
      val env = Map("JAM_TRACES_DIR" -> corpus.toString, "JAM_WARMUP_ROUNDS" -> "2")
      val summary = WarmupRunner.warmupIfEnabled(env.get).getOrElse(fail("warmup did not run"))
      summary.traces shouldBe 2
      summary.imported shouldBe 10
      summary.mismatches shouldBe 0
    }

    it("accepts a single trace directory as the corpus") {
      val corpus = smallCorpus(2)
      WarmupRunner.findTraces(corpus.resolve("preimages_light")).map(_.steps.size) shouldBe Vector(2)
      WarmupRunner.findTraces(corpus).map(_.steps.size) shouldBe Vector(2)
    }
  }
