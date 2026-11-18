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

  test("a slot of the epoch before the state's is assigned over the previous set λ with η₃ (M* in an epoch's first rotation)") {
    val lambda = Vector("l0", "l1", "l2", "l3", "l4", "l5")
    val six = Vector("k0", "k1", "k2", "k3", "k4", "k5")
    val pending = Vector("p0", "p1", "p2", "p3", "p4", "p5")
    val (cores, validators) = GuarantorService.assignmentsAt(12L, eta, lambda, six, pending, 11L, config)
    validators shouldBe lambda
    cores shouldBe io.forge.jam.protocol.report.GuarantorAssignment.coreAssignments(eta(3), 11L, 6, config).toIndexedSeq
    GuarantorService.assignmentsAt(12L, eta, lambda, six, pending, 13L, config) shouldBe
      GuarantorService.assignmentsFor(12L, eta, six, pending, 13L, config)
    GuarantorService.assignmentsAt(23L, eta, lambda, six, pending, 24L, config) shouldBe
      GuarantorService.assignmentsFor(23L, eta, six, pending, 24L, config)
  }

  test("a guarantee is timed at the next block's slot when assigned in its rotation, else at the last slot of the rotation before") {
    GuarantorService.guaranteeSlot(8L, config.rotationPeriod)(_ => true) shouldBe Some(8L)
    GuarantorService.guaranteeSlot(8L, config.rotationPeriod)(_ < 8L) shouldBe Some(7L)
    GuarantorService.guaranteeSlot(10L, config.rotationPeriod)(_ < 8L) shouldBe Some(7L)
    GuarantorService.guaranteeSlot(10L, config.rotationPeriod)(_ < 4L) shouldBe None
    GuarantorService.guaranteeSlot(2L, config.rotationPeriod)(_ => false) shouldBe None
    GuarantorService.guaranteeSlot(1L, config.rotationPeriod)(_ < 0L) shouldBe None
  }
