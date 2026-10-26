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

  test("guarantor assignment within an epoch uses the head state's η₂ and κ") {
    GuarantorService.rotatedForSlot(13L, eta, kappa, gammaP, 14L, config) shouldBe (eta(2), kappa)
    GuarantorService.rotatedForSlot(12L, eta, kappa, gammaP, 12L, config) shouldBe (eta(2), kappa)
  }
