package io.forge.jam.protocol.statistics

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.AppendedClues.convertToClueful
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey, ValidatorIndex, ServiceId, Ed25519Signature, Timeslot, CoreIndex, Gas}
import _root_.scodec.Codec
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.extrinsic.{Preimage, AssuranceExtrinsic, Dispute, GuaranteeExtrinsic, Verdict}
import io.forge.jam.core.types.tickets.TicketEnvelope
import io.forge.jam.core.types.dispute.{Culprit, Fault, GuaranteeSignature}
import io.forge.jam.core.types.work.{Vote, PackageSpec}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workresult.{WorkResult, RefineLoad}
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.statistics.StatisticsTypes.*
import io.forge.jam.protocol.statistics.StatisticsTransition
import spire.math.{UByte, UInt, UShort}

/**
 * Tests for the Statistics State Transition Function.
 */
class StatisticsTest extends AnyFunSuite with Matchers:

  val TinyConfig = ChainConfig.TINY
  val FullConfig = ChainConfig.FULL

  // Helper to create a validator key filled with a specific byte value
  private def validatorKeyFilled(value: Int): ValidatorKey =
    ValidatorKey(
      BandersnatchPublicKey(Array.fill(32)(value.toByte)),
      Ed25519PublicKey(Array.fill(32)(value.toByte)),
      BlsPublicKey(Array.fill(144)(value.toByte)),
      JamBytes(Array.fill(128)(value.toByte))
    )

  // Helper to create empty extrinsic
  private def emptyExtrinsic: StatExtrinsic =
    StatExtrinsic(
      tickets = List.empty,
      preimages = List.empty,
      guarantees = List.empty,
      assurances = List.empty,
      disputes = Dispute(List.empty, List.empty, List.empty)
    )

  // Helper to create initial state with zero stats
  private def initialState(validatorCount: Int, slot: Long): StatState =
    StatState(
      valsCurrStats = (0 until validatorCount).map(_ => StatCount.zero).toList,
      valsLastStats = (0 until validatorCount).map(_ => StatCount.zero).toList,
      slot = slot,
      currValidators = (0 until validatorCount).map(i => validatorKeyFilled(i)).toList
    )

  test("epoch transition detection and stats rotation") {
    // Start at slot 10, transition to slot 13 (crosses epoch at 12)
    val preState = StatState(
      valsCurrStats = List(
        StatCount(blocks = 5, tickets = 3, preImages = 2, preImagesSize = 100, guarantees = 4, assurances = 6),
        StatCount(blocks = 3, tickets = 1, preImages = 1, preImagesSize = 50, guarantees = 2, assurances = 3)
      ),
      valsLastStats = List(StatCount.zero, StatCount.zero),
      slot = 10,
      currValidators = List(validatorKeyFilled(0), validatorKeyFilled(1))
    )

    val input = StatInput(
      slot = 13, // Crosses epoch boundary
      authorIndex = 0,
      extrinsic = emptyExtrinsic
    )

    val config = ChainConfig.TINY.copy(validatorCount = 2)
    val (postState, _) = StatisticsTransition.stfInternal(input, preState, config)

    // Current stats should have moved to last stats
    postState.valsLastStats(0).blocks shouldBe 5
    postState.valsLastStats(0).tickets shouldBe 3
    postState.valsLastStats(1).blocks shouldBe 3

    // Current stats should be reset (except author's block count which is +1)
    postState.valsCurrStats(0).blocks shouldBe 1 // Author got +1
    postState.valsCurrStats(0).tickets shouldBe 0
    postState.valsCurrStats(1).blocks shouldBe 0
  }

  test("author stats update (blocks, tickets)") {
    val preState = initialState(validatorCount = 3, slot = 5)

    // Create tickets
    val tickets = List(
      TicketEnvelope(UByte(0), JamBytes(Array.fill(784)(0.toByte))),
      TicketEnvelope(UByte(1), JamBytes(Array.fill(784)(0.toByte)))
    )

    // Create preimages
    val preimages = List(
      Preimage(ServiceId(UInt(1)), JamBytes(Array.fill(50)(1.toByte))),
      Preimage(ServiceId(UInt(2)), JamBytes(Array.fill(75)(2.toByte)))
    )

    val input = StatInput(
      slot = 6,
      authorIndex = 1, // Validator 1 is the author
      extrinsic = StatExtrinsic(
        tickets = tickets,
        preimages = preimages,
        guarantees = List.empty,
        assurances = List.empty,
        disputes = Dispute(List.empty, List.empty, List.empty)
      )
    )

    val config = ChainConfig.TINY.copy(validatorCount = 3)
    val (postState, _) = StatisticsTransition.stfInternal(input, preState, config)

    // Author (validator 1) should have updated stats
    postState.valsCurrStats(1).blocks shouldBe 1
    postState.valsCurrStats(1).tickets shouldBe 2
    postState.valsCurrStats(1).preImages shouldBe 2
    postState.valsCurrStats(1).preImagesSize shouldBe 125 // 50 + 75

    // Other validators should have unchanged stats
    postState.valsCurrStats(0).blocks shouldBe 0
    postState.valsCurrStats(2).blocks shouldBe 0
  }

  test("guarantor stats update") {
    val preState = initialState(validatorCount = 4, slot = 5)

    // Create a simple work report
    val workReport = WorkReport(
      PackageSpec(Hash.zero, UInt(100), Hash.zero, Hash.zero, UShort(1)),
      Context(Hash.zero, Hash.zero, Hash.zero, Hash.zero, Timeslot(0), List.empty),
      CoreIndex(0),
      Hash.zero,
      Gas(0L),
      JamBytes.empty,
      List.empty,
      List.empty
    )

    val guarantees = List(
      GuaranteeExtrinsic(
        workReport,
        Timeslot(5),
        List(
          GuaranteeSignature(ValidatorIndex(0), Ed25519Signature(Array.fill(64)(0.toByte))),
          GuaranteeSignature(ValidatorIndex(2), Ed25519Signature(Array.fill(64)(0.toByte)))
        )
      ),
      GuaranteeExtrinsic(
        workReport,
        Timeslot(5),
        List(
          GuaranteeSignature(ValidatorIndex(2), Ed25519Signature(Array.fill(64)(0.toByte))), // Same validator as above
          GuaranteeSignature(ValidatorIndex(3), Ed25519Signature(Array.fill(64)(0.toByte)))
        )
      )
    )

    val input = StatInput(
      slot = 6,
      authorIndex = 1,
      extrinsic = StatExtrinsic(
        tickets = List.empty,
        preimages = List.empty,
        guarantees = guarantees,
        assurances = List.empty,
        disputes = Dispute(List.empty, List.empty, List.empty)
      )
    )

    val config = ChainConfig.TINY.copy(validatorCount = 4)
    val (postState, _) = StatisticsTransition.stfInternal(input, preState, config)

    // Each unique validator who signed gets +1 (not per signature)
    postState.valsCurrStats(0).guarantees shouldBe 1
    postState.valsCurrStats(2).guarantees shouldBe 1 // Only +1 even though signed twice
    postState.valsCurrStats(3).guarantees shouldBe 1
    postState.valsCurrStats(1).guarantees shouldBe 0 // Not a guarantor, just author
    postState.valsCurrStats(1).blocks shouldBe 1 // But is the author
  }

  test("assurance stats update") {
    val preState = initialState(validatorCount = 4, slot = 5)

    val assurances = List(
      AssuranceExtrinsic(
        Hash.zero,
        JamBytes(Array(0x03.toByte)), // bitfield for 2 cores
        ValidatorIndex(0),
        Ed25519Signature(Array.fill(64)(0.toByte))
      ),
      AssuranceExtrinsic(
        Hash.zero,
        JamBytes(Array(0x01.toByte)),
        ValidatorIndex(2),
        Ed25519Signature(Array.fill(64)(0.toByte))
      ),
      AssuranceExtrinsic(
        Hash.zero,
        JamBytes(Array(0x02.toByte)),
        ValidatorIndex(0), // Same validator as first one
        Ed25519Signature(Array.fill(64)(0.toByte))
      )
    )

    val input = StatInput(
      slot = 6,
      authorIndex = 1,
      extrinsic = StatExtrinsic(
        tickets = List.empty,
        preimages = List.empty,
        guarantees = List.empty,
        assurances = assurances,
        disputes = Dispute(List.empty, List.empty, List.empty)
      )
    )

    val config = ChainConfig.TINY.copy(validatorCount = 4)
    val (postState, _) = StatisticsTransition.stfInternal(input, preState, config)

    // Each assurance increments the validator's count
    postState.valsCurrStats(0).assurances shouldBe 2 // Two assurances from validator 0
    postState.valsCurrStats(2).assurances shouldBe 1
    postState.valsCurrStats(1).assurances shouldBe 0
    postState.valsCurrStats(3).assurances shouldBe 0
  }

  test("tiny config state transition") {
    val folderPath = "stf/statistics/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadTestDataFromTestVectors[StatCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right((testCase, expectedBinaryData)) =>
          // Test encoding
          given statCaseCodec: Codec[StatCase] = StatCase.codec(ChainConfig.TINY)
          val encoded = testCase.encode
          encoded.toArray shouldBe expectedBinaryData withClue s"Encoding mismatch for $testCaseName"

          // Test state transition
          val (postState, _) = StatisticsTransition.stfInternal(
            testCase.input,
            testCase.preState,
            TinyConfig
          )
          assertStatStateEquals(testCase.postState, postState, testCaseName)
  }

  test("full config state transition") {
    val folderPath = "stf/statistics/full"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadTestDataFromTestVectors[StatCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right((testCase, expectedBinaryData)) =>
          // Test encoding
          given statCaseCodec: Codec[StatCase] = StatCase.codec(ChainConfig.FULL)
          val encoded = testCase.encode
          encoded.toArray shouldBe expectedBinaryData withClue s"Encoding mismatch for $testCaseName"

          // Test state transition
          val (postState, _) = StatisticsTransition.stfInternal(
            testCase.input,
            testCase.preState,
            FullConfig
          )
          assertStatStateEquals(testCase.postState, postState, testCaseName)
  }

  // Helper method to compare StatState instances with detailed error messages
  private def assertStatStateEquals(
    expected: StatState,
    actual: StatState,
    testCaseName: String
  ): Unit =
    expected.slot shouldBe actual.slot withClue
      s"Slot mismatch in test case: $testCaseName"

    expected.valsCurrStats.size shouldBe actual.valsCurrStats.size withClue
      s"Current stats size mismatch in test case: $testCaseName"

    expected.valsLastStats.size shouldBe actual.valsLastStats.size withClue
      s"Last stats size mismatch in test case: $testCaseName"

    // Compare current stats
    expected.valsCurrStats.zip(actual.valsCurrStats).zipWithIndex.foreach { case ((exp, act), index) =>
      exp.blocks shouldBe act.blocks withClue
        s"Current blocks at index $index mismatch in test case: $testCaseName"
      exp.tickets shouldBe act.tickets withClue
        s"Current tickets at index $index mismatch in test case: $testCaseName"
      exp.preImages shouldBe act.preImages withClue
        s"Current preImages at index $index mismatch in test case: $testCaseName"
      exp.preImagesSize shouldBe act.preImagesSize withClue
        s"Current preImagesSize at index $index mismatch in test case: $testCaseName"
      exp.guarantees shouldBe act.guarantees withClue
        s"Current guarantees at index $index mismatch in test case: $testCaseName"
      exp.assurances shouldBe act.assurances withClue
        s"Current assurances at index $index mismatch in test case: $testCaseName"
    }

    // Compare last stats
    expected.valsLastStats.zip(actual.valsLastStats).zipWithIndex.foreach { case ((exp, act), index) =>
      exp.blocks shouldBe act.blocks withClue
        s"Last blocks at index $index mismatch in test case: $testCaseName"
      exp.tickets shouldBe act.tickets withClue
        s"Last tickets at index $index mismatch in test case: $testCaseName"
      exp.preImages shouldBe act.preImages withClue
        s"Last preImages at index $index mismatch in test case: $testCaseName"
      exp.preImagesSize shouldBe act.preImagesSize withClue
        s"Last preImagesSize at index $index mismatch in test case: $testCaseName"
      exp.guarantees shouldBe act.guarantees withClue
        s"Last guarantees at index $index mismatch in test case: $testCaseName"
      exp.assurances shouldBe act.assurances withClue
        s"Last assurances at index $index mismatch in test case: $testCaseName"
    }
