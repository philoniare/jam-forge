package io.forge.jam.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParallelSpec extends AnyFunSuite with Matchers:

  test("map keeps input order") {
    Parallel.map((1 to 50).toList)(i => { Thread.sleep((50 - i) % 3); i * 2 }) shouldBe (1 to 50).map(_ * 2).toList
  }

  test("map rethrows a task's own exception") {
    val e = intercept[IllegalStateException] {
      Parallel.map(List(1, 2, 3))(i => if i == 1 then throw new IllegalStateException("boom") else i)
    }
    e.getMessage shouldBe "boom"
  }

  test("await rethrows the cause, not a CompletionException") {
    an[ArithmeticException] should be thrownBy Parallel.await(Parallel.async(1 / 0))
  }
