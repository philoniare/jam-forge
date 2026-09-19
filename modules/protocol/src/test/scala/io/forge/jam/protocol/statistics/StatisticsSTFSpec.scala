package io.forge.jam.protocol.statistics

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.generators.StfGenerators.*
import io.forge.jam.protocol.statistics.StatisticsTypes.*
import io.forge.jam.protocol.report.ReportTypes.{
  CoreStatisticsRecord,
  ServiceActivityRecord,
  ServiceStatisticsEntry
}

/**
 * - Activity statistics tracking increments correctly
 * - Validator statistics updates are deterministic
 */
class StatisticsSTFSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  private val testConfig = ChainConfig.TINY

  // Override default ScalaCheck configuration for faster tests
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  test("property: StatCount has all non-negative values") {
    forAll(genStatCount) { statCount =>
      // Property: all counters should be non-negative
      statCount.blocks should be >= 0L
      statCount.tickets should be >= 0L
      statCount.preImages should be >= 0L
      statCount.preImagesSize should be >= 0L
      statCount.guarantees should be >= 0L
      statCount.assurances should be >= 0L
    }
  }

  test("property: StatCount.zero returns all zeros") {
    val zero = StatCount.zero

    // Property: zero should have all zeros
    zero.blocks shouldBe 0L
    zero.tickets shouldBe 0L
    zero.preImages shouldBe 0L
    zero.preImagesSize shouldBe 0L
    zero.guarantees shouldBe 0L
    zero.assurances shouldBe 0L
  }

  test("property: StatCount size is 24 bytes") {
    // Property: StatCount should be 6 x 4 bytes = 24 bytes
    StatCount.Size shouldBe 24
  }

  test("property: valsCurrStats size matches validator count") {
    forAll(genStatState(testConfig)) { state =>
      // Property: valsCurrStats should have one entry per validator
      state.valsCurrStats.size shouldBe testConfig.validatorCount
    }
  }

  test("property: valsLastStats size matches validator count") {
    forAll(genStatState(testConfig)) { state =>
      // Property: valsLastStats should have one entry per validator
      state.valsLastStats.size shouldBe testConfig.validatorCount
    }
  }

  test("property: currValidators size matches validator count") {
    forAll(genStatState(testConfig)) { state =>
      // Property: currValidators should have correct count
      state.currValidators.size shouldBe testConfig.validatorCount
    }
  }

  test("property: STF is deterministic") {
    forAll(genStatState(testConfig), genStatInput(testConfig)) { (preState, input) =>
      val (postState1, output1) = StatisticsTransition.stfInternal(input, preState, testConfig)
      val (postState2, output2) = StatisticsTransition.stfInternal(input, preState, testConfig)

      // Property: same inputs should produce same output
      output1 shouldBe output2

      // Property: same inputs should produce same state
      postState1.valsCurrStats.size shouldBe postState2.valsCurrStats.size
      postState1.valsCurrStats.zip(postState2.valsCurrStats).foreach {
        case (s1, s2) =>
          s1.blocks shouldBe s2.blocks
          s1.tickets shouldBe s2.tickets
          s1.preImages shouldBe s2.preImages
          s1.preImagesSize shouldBe s2.preImagesSize
          s1.guarantees shouldBe s2.guarantees
          s1.assurances shouldBe s2.assurances
      }

      postState1.valsLastStats.size shouldBe postState2.valsLastStats.size
    }
  }

  test("property: author index increments block count") {
    forAll(genStatState(testConfig)) { preState =>
      // Use a valid author index
      val authorIndex = 0L

      val input = StatInput(
        slot = preState.slot + 1,
        authorIndex = authorIndex,
        extrinsic = StatExtrinsic(
          tickets = List.empty,
          preimages = List.empty,
          guarantees = List.empty,
          assurances = List.empty,
          disputes = io.forge.jam.core.types.extrinsic.Dispute(List.empty, List.empty, List.empty)
        )
      )

      val (postState, _) = StatisticsTransition.stfInternal(input, preState, testConfig)

      // Property: author's block count should increase by 1
      val preBlocks = preState.valsCurrStats(authorIndex.toInt).blocks
      val postBlocks = postState.valsCurrStats(authorIndex.toInt).blocks

      // Account for epoch rotation which resets stats
      val preEpoch = preState.slot / testConfig.epochLength
      val postEpoch = input.slot / testConfig.epochLength

      if postEpoch > preEpoch then
        // Epoch rotated - stats were reset, so block count is 1
        postBlocks shouldBe 1L
      else
        postBlocks shouldBe (preBlocks + 1)
    }
  }

  test("property: epoch transition rotates stats") {
    forAll(genStatState(testConfig)) { preState =>
      // Create input that crosses epoch boundary
      val preEpoch = preState.slot / testConfig.epochLength
      val newSlot = (preEpoch + 1) * testConfig.epochLength // First slot of next epoch

      val input = StatInput(
        slot = newSlot,
        authorIndex = 0L,
        extrinsic = StatExtrinsic(
          tickets = List.empty,
          preimages = List.empty,
          guarantees = List.empty,
          assurances = List.empty,
          disputes = io.forge.jam.core.types.extrinsic.Dispute(List.empty, List.empty, List.empty)
        )
      )

      val (postState, _) = StatisticsTransition.stfInternal(input, preState, testConfig)

      // Property: valsLastStats should now contain previous valsCurrStats
      postState.valsLastStats.zip(preState.valsCurrStats).foreach {
        case (postLast, preCurr) =>
          postLast.blocks shouldBe preCurr.blocks
          postLast.tickets shouldBe preCurr.tickets
          postLast.preImages shouldBe preCurr.preImages
          postLast.preImagesSize shouldBe preCurr.preImagesSize
          postLast.guarantees shouldBe preCurr.guarantees
          postLast.assurances shouldBe preCurr.assurances
      }
    }
  }

  test("property: non-epoch transition preserves valsLastStats") {
    forAll(genStatState(testConfig)) { preState =>
      // Create input within same epoch
      val preEpoch = preState.slot / testConfig.epochLength
      val newSlot = preState.slot + 1
      val postEpoch = newSlot / testConfig.epochLength

      whenever(preEpoch == postEpoch) {
        val input = StatInput(
          slot = newSlot,
          authorIndex = 0L,
          extrinsic = StatExtrinsic(
            tickets = List.empty,
            preimages = List.empty,
            guarantees = List.empty,
            assurances = List.empty,
            disputes = io.forge.jam.core.types.extrinsic.Dispute(List.empty, List.empty, List.empty)
          )
        )

        val (postState, _) = StatisticsTransition.stfInternal(input, preState, testConfig)

        // Property: valsLastStats should remain unchanged
        postState.valsLastStats.zip(preState.valsLastStats).foreach {
          case (postLast, preLast) =>
            postLast.blocks shouldBe preLast.blocks
            postLast.tickets shouldBe preLast.tickets
            postLast.preImages shouldBe preLast.preImages
            postLast.preImagesSize shouldBe preLast.preImagesSize
            postLast.guarantees shouldBe preLast.guarantees
            postLast.assurances shouldBe preLast.assurances
        }
      }
    }
  }

  test("property: ticket count increments by number of tickets") {
    forAll(genStatState(testConfig), Gen.choose(0, 5)) { (preState, ticketCount) =>
      // Generate tickets
      val tickets =
        (0 until ticketCount).map(_ => genTicketEnvelope(testConfig.ticketsPerValidator).sample.get).toList

      val authorIndex = 0L
      val preEpoch = preState.slot / testConfig.epochLength
      val newSlot = preState.slot + 1
      val postEpoch = newSlot / testConfig.epochLength

      whenever(preEpoch == postEpoch) {
        val input = StatInput(
          slot = newSlot,
          authorIndex = authorIndex,
          extrinsic = StatExtrinsic(
            tickets = tickets,
            preimages = List.empty,
            guarantees = List.empty,
            assurances = List.empty,
            disputes = io.forge.jam.core.types.extrinsic.Dispute(List.empty, List.empty, List.empty)
          )
        )

        val (postState, _) = StatisticsTransition.stfInternal(input, preState, testConfig)

        // Property: ticket count should increase by ticket count
        val preTickets = preState.valsCurrStats(authorIndex.toInt).tickets
        val postTickets = postState.valsCurrStats(authorIndex.toInt).tickets
        postTickets shouldBe (preTickets + ticketCount)
      }
    }
  }

  test("property: author index is within validator bounds") {
    forAll(genStatInput(testConfig)) { input =>
      // Property: author index should be less than validator count
      input.authorIndex.toInt should be < testConfig.validatorCount
      input.authorIndex.toInt should be >= 0
    }
  }

  test("property: slot is positive") {
    forAll(genStatInput(testConfig)) { input =>
      // Property: slot should be positive
      input.slot should be > 0L
    }
  }

  test("property: CoreStatisticsRecord.zero returns all zeros") {
    val zero = CoreStatisticsRecord.zero

    // Property: zero should have all zeros
    zero.daLoad shouldBe 0L
    zero.popularity shouldBe 0L
    zero.imports shouldBe 0L
    zero.extrinsicCount shouldBe 0L
    zero.extrinsicSize shouldBe 0L
    zero.exports shouldBe 0L
    zero.bundleSize shouldBe 0L
    zero.gasUsed shouldBe 0L
  }

  test("property: ServiceActivityRecord default returns all zeros") {
    val zero = ServiceActivityRecord()

    // Property: zero should have all zeros
    zero.providedCount shouldBe 0
    zero.providedSize shouldBe 0L
    zero.refinementCount shouldBe 0L
    zero.refinementGasUsed shouldBe 0L
    zero.imports shouldBe 0L
    zero.extrinsicCount shouldBe 0L
    zero.extrinsicSize shouldBe 0L
    zero.exports shouldBe 0L
    zero.accumulateCount shouldBe 0L
    zero.accumulateGasUsed shouldBe 0L
  }

  test("property: empty activity statistics round-trip through the canonical codecs") {
    val accumulator = List.fill(testConfig.validatorCount)(StatCount.zero)
    val previous = List.fill(testConfig.validatorCount)(StatCount.zero)
    val core = List.fill(testConfig.coresCount)(CoreStatisticsRecord.zero)
    val service = List.empty[ServiceStatisticsEntry]

    val codec =
      ActivityStatisticsCodec.activityStatisticsCodec(testConfig.validatorCount, testConfig.coresCount)
    val (decAcc, decPrev, decCore, decSvc) =
      codec.decodeValue(codec.encode((accumulator, previous, core, service)).require).require

    // Property: accumulator/previous keep their size and stay zeroed
    decAcc.size shouldBe testConfig.validatorCount
    decAcc.foreach(stat => stat shouldBe StatCount.zero)
    decPrev.size shouldBe testConfig.validatorCount
    decPrev.foreach(stat => stat shouldBe StatCount.zero)

    // Property: core keeps its size and stays zeroed
    decCore.size shouldBe testConfig.coresCount
    decCore.foreach(stat => stat shouldBe CoreStatisticsRecord.zero)

    // Property: service should be empty
    decSvc shouldBe List.empty
  }

  test("GP: guarantee counter increments for each unique guarantor validator") {
    // GP: guarantees++ for reporters (each validator in guarantee signatures)
    import io.forge.jam.core.types.extrinsic.{GuaranteeExtrinsic, AssuranceExtrinsic, Dispute}
    import io.forge.jam.core.types.dispute.GuaranteeSignature
    import io.forge.jam.core.types.workpackage._
    import io.forge.jam.core.types.work.PackageSpec
    import io.forge.jam.core.types.context.Context
    import io.forge.jam.core.primitives._
    import io.forge.jam.core.JamBytes
    import spire.math.{UInt, UShort}

    forAll(genStatState(testConfig)) { preState =>
      val preEpoch = preState.slot / testConfig.epochLength
      val newSlot = preState.slot + 1
      val postEpoch = newSlot / testConfig.epochLength

      whenever(preEpoch == postEpoch && preState.valsCurrStats.size >= 3) {
        // Create a minimal work report
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
            Timeslot(newSlot.toInt),
            List(
              GuaranteeSignature(ValidatorIndex(0), Ed25519Signature(Array.fill(64)(0.toByte))),
              GuaranteeSignature(ValidatorIndex(2), Ed25519Signature(Array.fill(64)(0.toByte)))
            )
          )
        )

        val input = StatInput(
          slot = newSlot,
          authorIndex = 1L,
          extrinsic = StatExtrinsic(
            tickets = List.empty,
            preimages = List.empty,
            guarantees = guarantees,
            assurances = List.empty,
            disputes = Dispute(List.empty, List.empty, List.empty)
          )
        )

        val (postState, _) = StatisticsTransition.stfInternal(input, preState, testConfig)

        // GP: Each validator who signed a guarantee gets +1 to guarantees
        val preGuarantees0 = preState.valsCurrStats(0).guarantees
        val preGuarantees2 = preState.valsCurrStats(2).guarantees
        val postGuarantees0 = postState.valsCurrStats(0).guarantees
        val postGuarantees2 = postState.valsCurrStats(2).guarantees

        postGuarantees0 shouldBe (preGuarantees0 + 1)
        postGuarantees2 shouldBe (preGuarantees2 + 1)
      }
    }
  }

  test("GP: assurance counter increments for each assuring validator") {
    // GP: assurances++ for assurers (eq:activityspec)
    import io.forge.jam.core.types.extrinsic.{GuaranteeExtrinsic, AssuranceExtrinsic, Dispute}
    import io.forge.jam.core.primitives._
    import io.forge.jam.core.JamBytes

    forAll(genStatState(testConfig)) { preState =>
      val preEpoch = preState.slot / testConfig.epochLength
      val newSlot = preState.slot + 1
      val postEpoch = newSlot / testConfig.epochLength

      whenever(preEpoch == postEpoch && preState.valsCurrStats.size >= 3) {
        val bitfieldSize = (testConfig.coresCount + 7) / 8

        val assurances = List(
          AssuranceExtrinsic(
            Hash.zero,
            JamBytes(Array.fill(bitfieldSize)(0x01.toByte)),
            ValidatorIndex(0),
            Ed25519Signature(Array.fill(64)(0.toByte))
          ),
          AssuranceExtrinsic(
            Hash.zero,
            JamBytes(Array.fill(bitfieldSize)(0x02.toByte)),
            ValidatorIndex(2),
            Ed25519Signature(Array.fill(64)(0.toByte))
          )
        )

        val input = StatInput(
          slot = newSlot,
          authorIndex = 1L,
          extrinsic = StatExtrinsic(
            tickets = List.empty,
            preimages = List.empty,
            guarantees = List.empty,
            assurances = assurances,
            disputes = Dispute(List.empty, List.empty, List.empty)
          )
        )

        val (postState, _) = StatisticsTransition.stfInternal(input, preState, testConfig)

        // GP: Each assurance from a validator increments their assurances count
        val preAssurances0 = preState.valsCurrStats(0).assurances
        val preAssurances2 = preState.valsCurrStats(2).assurances
        val postAssurances0 = postState.valsCurrStats(0).assurances
        val postAssurances2 = postState.valsCurrStats(2).assurances

        postAssurances0 shouldBe (preAssurances0 + 1)
        postAssurances2 shouldBe (preAssurances2 + 1)
      }
    }
  }

  test("GP: multiple assurances from same validator increment count multiple times") {
    // GP: assurances++ for assurers - each assurance entry increments
    import io.forge.jam.core.types.extrinsic.{GuaranteeExtrinsic, AssuranceExtrinsic, Dispute}
    import io.forge.jam.core.primitives._
    import io.forge.jam.core.JamBytes

    forAll(genStatState(testConfig)) { preState =>
      val preEpoch = preState.slot / testConfig.epochLength
      val newSlot = preState.slot + 1
      val postEpoch = newSlot / testConfig.epochLength

      whenever(preEpoch == postEpoch && preState.valsCurrStats.nonEmpty) {
        val bitfieldSize = (testConfig.coresCount + 7) / 8

        // Same validator submits two assurances
        val assurances = List(
          AssuranceExtrinsic(
            Hash.zero,
            JamBytes(Array.fill(bitfieldSize)(0x01.toByte)),
            ValidatorIndex(0),
            Ed25519Signature(Array.fill(64)(0.toByte))
          ),
          AssuranceExtrinsic(
            Hash.zero,
            JamBytes(Array.fill(bitfieldSize)(0x02.toByte)),
            ValidatorIndex(0), // Same validator
            Ed25519Signature(Array.fill(64)(1.toByte))
          )
        )

        val input = StatInput(
          slot = newSlot,
          authorIndex = 0L,
          extrinsic = StatExtrinsic(
            tickets = List.empty,
            preimages = List.empty,
            guarantees = List.empty,
            assurances = assurances,
            disputes = Dispute(List.empty, List.empty, List.empty)
          )
        )

        val (postState, _) = StatisticsTransition.stfInternal(input, preState, testConfig)

        // GP: Two assurances from validator 0 should increment by 2
        val preAssurances = preState.valsCurrStats(0).assurances
        val postAssurances = postState.valsCurrStats(0).assurances

        postAssurances shouldBe (preAssurances + 2)
      }
    }
  }

  test("GP: guarantee counter increments once per validator even with multiple signatures") {
    // GP: guarantees++ for reporters - unique validator set
    import io.forge.jam.core.types.extrinsic.{GuaranteeExtrinsic, AssuranceExtrinsic, Dispute}
    import io.forge.jam.core.types.dispute.GuaranteeSignature
    import io.forge.jam.core.types.workpackage._
    import io.forge.jam.core.types.work.PackageSpec
    import io.forge.jam.core.types.context.Context
    import io.forge.jam.core.primitives._
    import io.forge.jam.core.JamBytes
    import spire.math.{UInt, UShort}

    forAll(genStatState(testConfig)) { preState =>
      val preEpoch = preState.slot / testConfig.epochLength
      val newSlot = preState.slot + 1
      val postEpoch = newSlot / testConfig.epochLength

      whenever(preEpoch == postEpoch && preState.valsCurrStats.size >= 3) {
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

        // Validator 2 signs BOTH guarantees
        val guarantees = List(
          GuaranteeExtrinsic(
            workReport,
            Timeslot(newSlot.toInt),
            List(
              GuaranteeSignature(ValidatorIndex(0), Ed25519Signature(Array.fill(64)(0.toByte))),
              GuaranteeSignature(ValidatorIndex(2), Ed25519Signature(Array.fill(64)(0.toByte)))
            )
          ),
          GuaranteeExtrinsic(
            workReport.copy(coreIndex = CoreIndex(1)),
            Timeslot(newSlot.toInt),
            List(
              GuaranteeSignature(ValidatorIndex(2), Ed25519Signature(Array.fill(64)(1.toByte))),
              GuaranteeSignature(ValidatorIndex(1), Ed25519Signature(Array.fill(64)(0.toByte)))
            )
          )
        )

        val input = StatInput(
          slot = newSlot,
          authorIndex = 0L,
          extrinsic = StatExtrinsic(
            tickets = List.empty,
            preimages = List.empty,
            guarantees = guarantees,
            assurances = List.empty,
            disputes = Dispute(List.empty, List.empty, List.empty)
          )
        )

        val (postState, _) = StatisticsTransition.stfInternal(input, preState, testConfig)

        // GP: Validator 2 signed both guarantees but should only get +1
        val preGuarantees2 = preState.valsCurrStats(2).guarantees
        val postGuarantees2 = postState.valsCurrStats(2).guarantees

        // Note: This tests that validators are deduplicated across guarantees
        postGuarantees2 shouldBe (preGuarantees2 + 1)
      }
    }
  }

  test("GP: empty guarantees and assurances do not change counters") {
    // GP: Zero guarantees/assurances should not modify stats
    import io.forge.jam.core.types.extrinsic.Dispute

    forAll(genStatState(testConfig)) { preState =>
      val preEpoch = preState.slot / testConfig.epochLength
      val newSlot = preState.slot + 1
      val postEpoch = newSlot / testConfig.epochLength

      whenever(preEpoch == postEpoch) {
        val input = StatInput(
          slot = newSlot,
          authorIndex = 0L,
          extrinsic = StatExtrinsic(
            tickets = List.empty,
            preimages = List.empty,
            guarantees = List.empty,
            assurances = List.empty,
            disputes = Dispute(List.empty, List.empty, List.empty)
          )
        )

        val (postState, _) = StatisticsTransition.stfInternal(input, preState, testConfig)

        // GP: All guarantee counters should be unchanged (except author blocks)
        preState.valsCurrStats.zipWithIndex.foreach { case (preStat, idx) =>
          val postStat = postState.valsCurrStats(idx)
          postStat.guarantees shouldBe preStat.guarantees
          postStat.assurances shouldBe preStat.assurances
        }
      }
    }
  }
