package io.forge.jam.pvm

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ExecutionModePropertySpec extends AnyFunSuite with Matchers:

  test("property absent (None) resolves to Interpreted") {
    ExecutionMode.fromProperty(None) shouldBe ExecutionMode.Interpreted
  }

  test("property = \"recompiled\" resolves to Recompiled") {
    ExecutionMode.fromProperty(Some("recompiled")) shouldBe ExecutionMode.Recompiled
  }

  test("property with any other value resolves to Interpreted") {
    ExecutionMode.fromProperty(Some("interpreted")) shouldBe ExecutionMode.Interpreted
    ExecutionMode.fromProperty(Some("Recompiled")) shouldBe ExecutionMode.Interpreted // case-sensitive
    ExecutionMode.fromProperty(Some("RECOMPILED")) shouldBe ExecutionMode.Interpreted
    ExecutionMode.fromProperty(Some("")) shouldBe ExecutionMode.Interpreted
    ExecutionMode.fromProperty(Some("garbage")) shouldBe ExecutionMode.Interpreted
  }
