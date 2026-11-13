package io.forge.jam.node

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes, constants}
import io.forge.jam.core.primitives.{BandersnatchPublicKey, BlsPublicKey, Ed25519PublicKey, Ed25519Signature, Hash}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.crypto.Ed25519ZebraWrapper
import io.forge.jam.protocol.dispute.DisputeBuilder
import io.forge.jam.protocol.dispute.DisputeBuilder.{Credential, Evidence, Judgment}
import io.forge.jam.protocol.dispute.DisputeTransition
import io.forge.jam.protocol.dispute.DisputeTypes.{DisputeInput, DisputeState, Psi}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** TINY: six validators, so a verdict has five votes; good needs five positive, bad five negative, wonky two
  * positive and three negative.
  */
class DisputeBuilderSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private val keys = (0 until 6).map(ValidatorKeySet.dev)
  private val kappa = keys.map(k =>
    ValidatorKey(BandersnatchPublicKey(k.bandersnatchPublic), k.ed25519Key, BlsPublicKey.zero, JamBytes(new Array[Byte](128)))
  )
  private val epoch = 3L
  private val state = DisputeState(Psi.empty, List.fill(config.coresCount)(None), epoch * config.epochLength + 1, kappa, kappa)
  private val target = Hashing.blake2b256("report".getBytes("UTF-8"))

  private def judge(i: Int, valid: Boolean, t: Hash = target) =
    val context = if valid then constants.JAM_VALID_BYTES else constants.JAM_INVALID_BYTES
    Judgment(i, valid, Ed25519Signature(Ed25519ZebraWrapper.sign(keys(i).ed25519Secret, context ++ t.bytes)))
  private def guarantor(i: Int, t: Hash = target) =
    Credential(keys(i).ed25519Key, Ed25519Signature(Ed25519ZebraWrapper.sign(keys(i).ed25519Secret, constants.JAM_GUARANTEE_BYTES ++ t.bytes)))

  private def accepted(b: DisputeBuilder.Built): Boolean =
    DisputeTransition.stfInternal(DisputeInput(b.dispute), state, config)._2.isRight

  test("nothing to dispute: all positive, or too few judgments") {
    DisputeBuilder.build(Seq(Evidence(target, epoch, (0 until 6).map(judge(_, true)), Nil)), state, config).isEmpty shouldBe
      true
    DisputeBuilder.build(Seq(Evidence(target, epoch, (0 until 3).map(judge(_, false)), Nil)), state, config).isEmpty shouldBe
      true
  }

  test("good verdict: five positive votes, the negative judge a fault and an offender") {
    val js = (0 until 5).map(judge(_, true)) :+ judge(5, false)
    val b = DisputeBuilder.build(Seq(Evidence(target, epoch, js, Nil)), state, config)
    b.dispute.verdicts.map(_.votes.count(_.vote)) shouldBe List(5)
    b.dispute.faults.map(_.key) shouldBe List(keys(5).ed25519Key)
    b.dispute.culprits shouldBe Nil
    b.offendersMark shouldBe List(keys(5).ed25519Key)
    accepted(b) shouldBe true
  }

  test("bad verdict: five negative votes, guarantors culprits and the positive judge a fault") {
    val js = (0 until 5).map(judge(_, false)) :+ judge(5, true)
    val b = DisputeBuilder.build(Seq(Evidence(target, epoch, js, Seq(guarantor(1), guarantor(4)))), state, config)
    b.dispute.verdicts.map(_.votes.count(_.vote)) shouldBe List(0)
    b.dispute.culprits.map(_.key).toSet shouldBe Set(keys(1).ed25519Key, keys(4).ed25519Key)
    b.dispute.faults.map(_.key) shouldBe List(keys(5).ed25519Key)
    b.offendersMark.toSet shouldBe Set(keys(1).ed25519Key, keys(4).ed25519Key, keys(5).ed25519Key)
    accepted(b) shouldBe true
    // A bad verdict goes in without the guarantors when we do not know them (the test vectors allow it).
    val bare = DisputeBuilder.build(Seq(Evidence(target, epoch, js, Nil)), state, config)
    bare.dispute.culprits shouldBe Nil
    accepted(bare) shouldBe true
    // A forged credential is not named.
    val forged = guarantor(2).copy(key = keys(3).ed25519Key)
    DisputeBuilder.build(Seq(Evidence(target, epoch, js, Seq(forged))), state, config).dispute.culprits shouldBe Nil
  }

  test("wonky verdict once everyone has judged and neither side has five") {
    val js = (0 until 3).map(judge(_, true)) ++ (3 until 6).map(judge(_, false))
    val b = DisputeBuilder.build(Seq(Evidence(target, epoch, js, Nil)), state, config)
    b.dispute.verdicts.map(_.votes.map(_.vote)) shouldBe List(List(true, true, false, false, false))
    accepted(b) shouldBe true
    DisputeBuilder.build(Seq(Evidence(target, epoch, js.dropRight(1), Nil)), state, config).isEmpty shouldBe true
  }

  test("forged and two-sided judgments do not count; judged targets and stale epochs are skipped") {
    val forged = judge(0, true).copy(validatorIndex = 1) // validator 0's signature claimed for 1
    val js = Seq(judge(0, true), forged, judge(2, true), judge(3, true), judge(4, true), judge(5, false))
    DisputeBuilder.build(Seq(Evidence(target, epoch, js, Nil)), state, config).isEmpty shouldBe true
    val twoSided = (0 until 5).map(judge(_, true)) ++ Seq(judge(4, false), judge(5, false))
    DisputeBuilder.build(Seq(Evidence(target, epoch, twoSided, Nil)), state, config).isEmpty shouldBe true

    val good = (0 until 5).map(judge(_, true)) :+ judge(5, false)
    val judged = state.copy(psi = Psi(List(target), Nil, Nil, Nil))
    DisputeBuilder.build(Seq(Evidence(target, epoch, good, Nil)), judged, config).isEmpty shouldBe true
    DisputeBuilder.build(Seq(Evidence(target, epoch - 2, good, Nil)), state, config).isEmpty shouldBe true
    // The previous epoch's judgments verify against the previous validator set.
    accepted(DisputeBuilder.build(Seq(Evidence(target, epoch - 1, good, Nil)), state, config)) shouldBe true
  }

  test("several reports: verdicts sorted by target, offenders unique and sorted, the STF accepts the whole") {
    val targets = (0 until 4).map(i => Hashing.blake2b256(s"r$i".getBytes("UTF-8")))
    val evidence = targets.map(t => Evidence(t, epoch, (0 until 5).map(judge(_, false, t)), Seq(guarantor(0, t), guarantor(1, t))))
    val b = DisputeBuilder.build(evidence, state, config)
    b.dispute.verdicts.size shouldBe 4
    b.dispute.verdicts.map(_.target) shouldBe targets.sortWith((x, y) => io.forge.jam.core.JamBytes.compareUnsigned(x.bytes, y.bytes) < 0)
    // Keys 0 and 1 are culprits once each, though they guaranteed every report.
    b.dispute.culprits.map(_.key).toSet shouldBe Set(keys(0).ed25519Key, keys(1).ed25519Key)
    b.dispute.culprits.size shouldBe 2
    accepted(b) shouldBe true
    // Past the limit of 16 verdicts the rest wait for a later block.
    val many = (0 until 20).map(i => Hashing.blake2b256(s"m$i".getBytes("UTF-8")))
      .map(t => Evidence(t, epoch, (0 until 5).map(judge(_, false, t)), Nil))
    DisputeBuilder.build(many, state, config).dispute.verdicts.size shouldBe constants.Cmaxextrinsicverdicts
  }
