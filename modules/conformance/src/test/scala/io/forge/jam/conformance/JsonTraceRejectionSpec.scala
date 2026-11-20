package io.forge.jam.conformance

import io.forge.jam.protocol.importing.{ImportError, ImportResult}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class JsonTraceRejectionSpec extends AnyFunSuite with Matchers:

  private val invalid = ImportResult.Invalid(ImportError.SafroleError, "bad slot", None)
  private val ours = ImportResult.Failed("jump table entry size 5", new IllegalStateException("jump table entry size 5"))

  test("a rejected block passes a step that expects no state change, and fails one that expects a new state") {
    JsonTraceRunner.afterRejection("t", "1.json", 7L, expectsRejection = true, invalid) shouldBe
      JsonTraceResult.Success("t", "1.json", 7L)
    JsonTraceRunner.afterRejection("t", "1.json", 7L, expectsRejection = false, invalid) shouldBe a[JsonTraceResult.Error]
  }

  test("an import that failed on our side never passes, not even where a rejection was expected") {
    for expected <- List(true, false) do
      JsonTraceRunner.afterRejection("t", "1.json", 7L, expectsRejection = expected, ours) match
        case JsonTraceResult.Error(_, _, why) => why should include("jump table entry size 5")
        case other                            => fail(s"expected an error, got $other")
  }
