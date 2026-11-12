package io.forge.jam.protocol.dispute

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.codec.Codec
import io.forge.jam.core.primitives.{Hash, Ed25519Signature, ValidatorIndex, Timeslot}
import io.forge.jam.core.types.extrinsic.{Dispute, GuaranteeExtrinsic, Verdict}
import io.forge.jam.core.types.work.Vote
import io.forge.jam.core.types.workpackage.{WorkReport, AvailabilityAssignment}
import io.forge.jam.protocol.generators.StfGenerators.*
import io.forge.jam.protocol.dispute.DisputeTypes.*
import spire.math.{UInt, UShort}

/**
 * Disputes STF behaviour not exercised by the stf/disputes vectors (DisputeTest
 * runs them all): empty disputes over random non-empty ψ/ρ, duplicate verdict
 * targets, and ρ retention for reports judged in an earlier block (ADV-4).
 */
class DisputesSTFSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  private val tinyConfig = ChainConfig.TINY

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  private val emptyDisputes = DisputeInput(Dispute(List.empty, List.empty, List.empty))

  /** Deterministic draw from an StfGenerators generator. */
  private def sample[A](gen: Gen[A], seed: Long): A =
    gen.pureApply(Gen.Parameters.default, Seed(seed))

  test("GP: empty disputes succeed and leave ψ, τ, κ, λ and |ρ| unchanged") {
    forAll(genDisputeState(tinyConfig)) { preState =>
      val (postState, output) = DisputeTransition.stfInternal(emptyDisputes, preState, tinyConfig)

      output.isRight shouldBe true
      DisputeTransition.stfInternal(emptyDisputes, preState, tinyConfig)._2 shouldBe output

      postState.psi.good shouldBe preState.psi.good
      postState.psi.bad shouldBe preState.psi.bad
      postState.psi.wonky shouldBe preState.psi.wonky
      postState.psi.offenders shouldBe preState.psi.offenders

      postState.tau shouldBe preState.tau

      postState.kappa.size shouldBe preState.kappa.size
      postState.kappa.zip(preState.kappa).foreach { case (post, pre) =>
        post.bandersnatch.bytes shouldBe pre.bandersnatch.bytes
        post.ed25519.bytes shouldBe pre.ed25519.bytes
      }
      postState.lambda.size shouldBe preState.lambda.size
      postState.lambda.zip(preState.lambda).foreach { case (post, pre) =>
        post.bandersnatch.bytes shouldBe pre.bandersnatch.bytes
        post.ed25519.bytes shouldBe pre.ed25519.bytes
      }

      preState.rho.size shouldBe tinyConfig.coresCount
      postState.rho.size shouldBe tinyConfig.coresCount
    }
  }

  test("GP: duplicate verdict targets are rejected with VerdictsNotSortedUnique") {
    val preState = sample(genDisputeState(tinyConfig), seed = 1L)
    val target = Hash(Array.fill(32)(0x42.toByte))
    val votes = (0 until tinyConfig.votesPerVerdict).toList.map { i =>
      Vote(vote = true, validatorIndex = ValidatorIndex(UShort(i)), signature = Ed25519Signature(new Array[Byte](64)))
    }
    val verdict = Verdict(target = target, age = Timeslot(UInt(0)), votes = votes)

    val input = DisputeInput(Dispute(List(verdict, verdict), List.empty, List.empty))
    val (postState, output) = DisputeTransition.stfInternal(input, preState, tinyConfig)

    output shouldBe Left(DisputeErrorCode.VerdictsNotSortedUnique)
    postState shouldBe preState
  }

  test("GP: a report judged bad, wonky or good in an earlier block is NOT cleared from ρ (eq:removenonpositive, ADV-4)") {
    val baseState = sample(genDisputeState(tinyConfig), seed = 2L)
    val workReport = sample(genWorkReport(tinyConfig), seed = 3L)
    val reportHash = Hashing.blake2b256(summon[Codec[WorkReport]].encode(workReport))
    val assignment = AvailabilityAssignment(GuaranteeExtrinsic(workReport, Timeslot(100), List.empty), 100L)

    val cases = List(
      ("bad", 0, Psi(good = List.empty, bad = List(reportHash), wonky = List.empty, offenders = List.empty)),
      ("wonky", 1, Psi(good = List.empty, bad = List.empty, wonky = List(reportHash), offenders = List.empty)),
      ("good", 0, Psi(good = List(reportHash), bad = List.empty, wonky = List.empty, offenders = List.empty))
    )

    for (label, core, psi) <- cases do
      val rho = List.tabulate(tinyConfig.coresCount)(i => if i == core then Some(assignment) else None)
      val (postState, output) =
        DisputeTransition.stfInternal(emptyDisputes, baseState.copy(psi = psi, rho = rho), tinyConfig)

      withClue(s"psi.$label: ") {
        output.isRight shouldBe true
        postState.rho(core) shouldBe Some(assignment)
        postState.rho shouldBe rho
      }
  }
