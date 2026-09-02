package io.forge.jam.conformance

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FileLoggerVerbositySpec extends AnyFunSuite with Matchers:

  private def withLogLevelProp[A](body: => A): A =
    val prev = sys.props.get("LOG_LEVEL")
    try body
    finally
      prev match
        case Some(v) => System.setProperty("LOG_LEVEL", v)
        case None    => System.clearProperty("LOG_LEVEL")

  test("LOG_LEVEL system property drives verbosity and wins over the env var") {
    withLogLevelProp {
      System.setProperty("LOG_LEVEL", "debug")
      FileLogger.isVerbose shouldBe true
      System.setProperty("LOG_LEVEL", "TRACE")
      FileLogger.isVerbose shouldBe true
      // A non-verbose property must mask any OS-level LOG_LEVEL env var
      // (property first, env only as the plain-CLI fallback).
      System.setProperty("LOG_LEVEL", "warn")
      FileLogger.isVerbose shouldBe false
    }
  }

  test("FuzzEnv propagates JAM_FUZZ_LOG_LEVEL into the LOG_LEVEL property") {
    withLogLevelProp {
      System.clearProperty("LOG_LEVEL")
      val env = Map(
        "JAM_FUZZ" -> "1",
        "JAM_FUZZ_SPEC" -> "tiny",
        "JAM_FUZZ_DATA_PATH" -> "/tmp/jam-fuzz-data",
        "JAM_FUZZ_SOCK_PATH" -> "/tmp/jam-fuzz.sock",
        "JAM_FUZZ_LOG_LEVEL" -> "debug"
      )
      FuzzEnv.fromEnv(env.get).isRight shouldBe true
      sys.props.get("LOG_LEVEL") shouldBe Some("DEBUG")
      FileLogger.isVerbose shouldBe true
    }
  }
