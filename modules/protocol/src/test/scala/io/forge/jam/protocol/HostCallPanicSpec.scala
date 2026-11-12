package io.forge.jam.protocol

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HostCallPanicSpec extends AnyFunSuite with Matchers:

  test("HostCallPanic carries no stack trace") {
    new HostCallPanic("x").getStackTrace shouldBe empty
  }
