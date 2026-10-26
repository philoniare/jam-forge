package io.forge.jam.node

import io.forge.jam.core.primitives.Hash
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ChainErrorSpec extends AnyFunSuite with Matchers:

  test("a chain error prints as its message, so log lines keep their text") {
    ChainError.PreviouslyInvalid.toString shouldBe "block failed validation before"
    ChainError.StoredOnSideBranch(1, 2, 3, 4).toString shouldBe "stored on side branch (weight 1 height 2 vs best 3/4)"
    s"${ChainError.ReplayFailed(ChainError.UnknownParent(Hash.zero))}" shouldBe
      s"reorg replay failed: unknown parent ${Hash.zero.toHex.take(18)}"
  }
