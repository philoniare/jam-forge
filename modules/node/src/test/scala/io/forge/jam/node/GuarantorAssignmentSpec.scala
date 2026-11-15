package io.forge.jam.node

import io.forge.jam.core.{ChainConfig, Hashing}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Which η₂ and κ a guarantor signs with, from the head state, for the next slot. */
class GuarantorAssignmentSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private val eta = (0 until 4).map(i => Hashing.blake2b256(Array(i.toByte))).toList
  private val kappa = Vector("k0", "k1")
  private val gammaP = Vector("p0", "p1")

  test("guarantor assignment at the first slot of an epoch uses η′₂ = η₁ and κ′ = γ_P") {
    val last = config.epochLength - 1L
    GuarantorService.rotatedForSlot(last, eta, kappa, gammaP, last + 1, config) shouldBe (eta(1), gammaP)
  }

  test("GP: cores are assigned over the validator set in force, three validators to each active core") {
    // The fuzz configuration has 255 validator slots and 85 cores; designate may shrink the set, here to 9.
    val fuzz = ChainConfig.FUZZ
    val nine = (0 until 9).map(i => s"k$i").toVector
    for slot <- List(61L, 66L, 119L) do
      val (cores, validators) = GuarantorService.assignmentsFor(60L, eta, nine, gammaP, slot, fuzz)
      validators shouldBe nine
      cores should have size 9
      withClue(s"slot $slot: ")(cores.groupBy(identity).view.mapValues(_.size).toMap shouldBe Map(0 -> 3, 1 -> 3, 2 -> 3))
  }

  test("guarantor assignment within an epoch uses the head state's η₂ and κ") {
    GuarantorService.rotatedForSlot(13L, eta, kappa, gammaP, 14L, config) shouldBe (eta(2), kappa)
    GuarantorService.rotatedForSlot(12L, eta, kappa, gammaP, 12L, config) shouldBe (eta(2), kappa)
  }
