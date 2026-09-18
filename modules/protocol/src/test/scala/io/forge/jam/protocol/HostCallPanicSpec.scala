package io.forge.jam.protocol

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HostCallPanicSpec extends AnyFunSuite with Matchers:

  test("HostCallPanic is a RuntimeException and preserves its message") {
    val e = new HostCallPanic("Write PANIC: key not readable")
    e shouldBe a[RuntimeException]
    e.getMessage shouldBe "Write PANIC: key not readable"
  }

  test("HostCallPanic is distinguishable from a generic RuntimeException") {
    val generic: RuntimeException = new RuntimeException("boom")
    generic match
      case _: HostCallPanic => fail("generic RuntimeException must not match HostCallPanic")
      case _                => succeed
  }
