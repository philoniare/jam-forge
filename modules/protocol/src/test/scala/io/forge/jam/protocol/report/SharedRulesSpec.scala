package io.forge.jam.protocol.report

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{Ed25519Signature, Hash, ValidatorIndex}
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.protocol.assurance.AssuranceTransition
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Rules the node asks the protocol for rather than restating: the rotation window of a guarantee and which cores a
  * block's assurances make available.
  */
class SharedRulesSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY // rotation period 4

  test("a guarantee is reportable in its own rotation and the next, and the oldest reportable slot agrees") {
    import GuarantorAssignment.{oldestReportableSlot, rotationInWindow}
    rotationInWindow(reportSlot = 8, blockSlot = 11, config) shouldBe true
    rotationInWindow(reportSlot = 8, blockSlot = 15, config) shouldBe true
    rotationInWindow(reportSlot = 7, blockSlot = 11, config) shouldBe true
    rotationInWindow(reportSlot = 7, blockSlot = 12, config) shouldBe false
    (8L to 64L).foreach { block =>
      val oldest = oldestReportableSlot(block, config)
      rotationInWindow(oldest, block, config) shouldBe true
      rotationInWindow(oldest - 1, block, config) shouldBe false
    }
  }

  test("a core is assured by more than two thirds of the validators") {
    def assurance(v: Int, bits: Int): AssuranceExtrinsic =
      AssuranceExtrinsic(Hash.zero, JamBytes(Array(bits.toByte)), ValidatorIndex(v), Ed25519Signature(new Array[Byte](64)))
    // Six validators: four assurances are exactly two thirds, five are enough. Core 1 gets five, core 0 four.
    val assurances = (0 until 5).map(v => assurance(v, if v < 4 then 3 else 2))
    AssuranceTransition.assuredCores(assurances, coresCount = 2, validatorCount = 6) shouldBe Set(1)
    AssuranceTransition.assuredCores(assurances, coresCount = 2, validatorCount = 5) shouldBe Set(0, 1)
    // A bitfield shorter than the cores assures none of the cores beyond it.
    AssuranceTransition.assuredCores(assurances, coresCount = 16, validatorCount = 5) shouldBe Set(0, 1)
  }
