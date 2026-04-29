package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Paths

class FuzzEnvSpec extends AnyFunSpec with Matchers:

  /** Build an env reader from a Map. */
  private def envOf(entries: (String, String)*): String => Option[String] =
    val m = entries.toMap
    name => m.get(name)

  describe("FuzzEnv.fromEnv") {

    it("returns Right(None) when JAM_FUZZ is not set") {
      FuzzEnv.fromEnv(envOf()) shouldBe Right(None)
    }

    it("returns Right(None) when JAM_FUZZ is empty") {
      FuzzEnv.fromEnv(envOf("JAM_FUZZ" -> "")) shouldBe Right(None)
    }

    it("parses tiny spec into ChainConfig.TINY") {
      val res = FuzzEnv.fromEnv(envOf(
        "JAM_FUZZ"           -> "1",
        "JAM_FUZZ_SPEC"      -> "tiny",
        "JAM_FUZZ_DATA_PATH" -> "/tmp/jam/data",
        "JAM_FUZZ_SOCK_PATH" -> "/tmp/jam/fuzz.sock"
      ))
      val cfg = res.toOption.flatten.getOrElse(fail("expected Some(cfg)"))
      cfg.chainConfig shouldBe ChainConfig.TINY
      cfg.socketPath  shouldBe Paths.get("/tmp/jam/fuzz.sock")
      cfg.dataPath    shouldBe Some(Paths.get("/tmp/jam/data"))
    }

    it("parses full spec into ChainConfig.FULL (case-insensitive)") {
      val res = FuzzEnv.fromEnv(envOf(
        "JAM_FUZZ"           -> "1",
        "JAM_FUZZ_SPEC"      -> "FULL",
        "JAM_FUZZ_DATA_PATH" -> "/var/jam",
        "JAM_FUZZ_SOCK_PATH" -> "/var/jam.sock"
      ))
      res.toOption.flatten.map(_.chainConfig) shouldBe Some(ChainConfig.FULL)
    }

    it("rejects an unknown spec value") {
      val res = FuzzEnv.fromEnv(envOf(
        "JAM_FUZZ"           -> "1",
        "JAM_FUZZ_SPEC"      -> "medium",
        "JAM_FUZZ_DATA_PATH" -> "/tmp",
        "JAM_FUZZ_SOCK_PATH" -> "/tmp/x.sock"
      ))
      res.left.toOption.getOrElse("") should include("tiny")
    }

    it("rejects when JAM_FUZZ_SPEC is missing") {
      val res = FuzzEnv.fromEnv(envOf(
        "JAM_FUZZ"           -> "1",
        "JAM_FUZZ_DATA_PATH" -> "/tmp",
        "JAM_FUZZ_SOCK_PATH" -> "/tmp/x.sock"
      ))
      res.left.toOption.getOrElse("") should include("JAM_FUZZ_SPEC")
    }

    it("rejects when JAM_FUZZ_DATA_PATH is missing") {
      val res = FuzzEnv.fromEnv(envOf(
        "JAM_FUZZ"           -> "1",
        "JAM_FUZZ_SPEC"      -> "tiny",
        "JAM_FUZZ_SOCK_PATH" -> "/tmp/x.sock"
      ))
      res.left.toOption.getOrElse("") should include("JAM_FUZZ_DATA_PATH")
    }

    it("rejects when JAM_FUZZ_SOCK_PATH is missing") {
      val res = FuzzEnv.fromEnv(envOf(
        "JAM_FUZZ"           -> "1",
        "JAM_FUZZ_SPEC"      -> "tiny",
        "JAM_FUZZ_DATA_PATH" -> "/tmp"
      ))
      res.left.toOption.getOrElse("") should include("JAM_FUZZ_SOCK_PATH")
    }

    it("treats empty required vars as missing") {
      val res = FuzzEnv.fromEnv(envOf(
        "JAM_FUZZ"           -> "1",
        "JAM_FUZZ_SPEC"      -> "tiny",
        "JAM_FUZZ_DATA_PATH" -> "/tmp",
        "JAM_FUZZ_SOCK_PATH" -> ""
      ))
      res.left.toOption.getOrElse("") should include("JAM_FUZZ_SOCK_PATH")
    }

    it("applies JAM_FUZZ_LOG_LEVEL to the LOG_LEVEL system property") {
      val key  = "LOG_LEVEL"
      val prev = Option(System.getProperty(key))
      try
        System.clearProperty(key)
        val res = FuzzEnv.fromEnv(envOf(
          "JAM_FUZZ"           -> "1",
          "JAM_FUZZ_SPEC"      -> "tiny",
          "JAM_FUZZ_DATA_PATH" -> "/tmp",
          "JAM_FUZZ_SOCK_PATH" -> "/tmp/x.sock",
          "JAM_FUZZ_LOG_LEVEL" -> "debug"
        ))
        res.isRight shouldBe true
        System.getProperty(key) shouldBe "DEBUG"
      finally
        prev match
          case Some(v) => System.setProperty(key, v)
          case None    => System.clearProperty(key)
    }
  }
