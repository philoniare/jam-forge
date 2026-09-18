package io.forge.jam.pvm.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PvmInternalErrorSpec extends AnyFunSuite with Matchers:

  test("PvmInternalError preserves the original throwable, including message-less ones") {
    val npe = new NullPointerException()
    val err = PvmInternalError(npe)
    err.cause shouldBe theSameInstanceAs(npe)
    err.describe should include("NullPointerException")
  }

  test("PvmInternalError.describe includes the message when there is one") {
    val err = PvmInternalError(new IllegalStateException("boom"))
    err.describe should include("IllegalStateException")
    err.describe should include("boom")
  }
