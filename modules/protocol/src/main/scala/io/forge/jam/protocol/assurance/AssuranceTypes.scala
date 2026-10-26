package io.forge.jam.protocol.assurance

import io.circe.derivation.ConfiguredDecoder
import io.forge.jam.core.json.JsonConfig.given
import io.forge.jam.core.StfResult
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.workpackage.{WorkReport, AvailabilityAssignment}
import io.forge.jam.core.json.JsonHelpers.parseHexBytesFixed
import io.forge.jam.core.json.JsonHelpers
import io.circe.Decoder
import spire.math.UByte
import io.forge.jam.core.codec.Codec
import io.forge.jam.core.codec.codecs.*
import io.forge.jam.core.codec.JamCodecs.{hashCodec, ubyteCodec, compactPrefixedList, fixedSizeList, optionCodec, stfResultCodec}
import io.forge.jam.core.codec.FullJamStateCodecs

/**
 * Types for the Assurances State Transition Function.
 *
 * The Assurances STF processes availability assurances from validators,
 * tracking which work reports have achieved sufficient attestations (2/3 supermajority)
 * for availability confirmation.
 */
object AssuranceTypes:

  /**
   * Input to the Assurances STF.
   */
  final case class AssuranceInput(
    assurances: List[AssuranceExtrinsic],
    slot: Long,
    parent: Hash
  )

  object AssuranceInput:
    /** Create a codec that knows the cores count */
    def codec(coreCount: Int): Codec[AssuranceInput] =
      (compactPrefixedList(AssuranceExtrinsic.codec(coreCount)) :: uint32L :: hashCodec).xmap(
        { case (assurances, slot, parent) =>
          AssuranceInput(assurances, slot & 0xFFFFFFFFL, parent)
        },
        ai => (ai.assurances, ai.slot & 0xFFFFFFFFL, ai.parent)
      )

    given Decoder[AssuranceInput] =
      Decoder.instance { cursor =>
        for
          assurances <- cursor.get[List[AssuranceExtrinsic]]("assurances")
          slot <- cursor.get[Long]("slot")
          parentHex <- cursor.get[String]("parent")
          parent <- parseHexBytesFixed(parentHex, Hash.Size).map(Hash(_))
        yield AssuranceInput(assurances, slot, parent)
      }

  /**
   * Assurance state containing availability assignments and validators.
   */
  final case class AssuranceState(
    availAssignments: List[Option[AvailabilityAssignment]],
    currValidators: IndexedSeq[ValidatorKey],
    postValidators: IndexedSeq[ValidatorKey]
  )

  object AssuranceState:
    /**
     * Create a codec that knows the cores and validator counts.
     */
    def codec(coreCount: Int): Codec[AssuranceState] =
      (fixedSizeList(optionCodec(summon[Codec[AvailabilityAssignment]]), coreCount) ::
       FullJamStateCodecs.validatorListCodec ::
       FullJamStateCodecs.validatorListCodec).xmap(
        { case (availAssignments, currValidators, postValidators) =>
          AssuranceState(availAssignments, currValidators, postValidators)
        },
        as => (as.availAssignments, as.currValidators, as.postValidators)
      )

    given Decoder[AssuranceState] = ConfiguredDecoder.derived

  /**
   * Error codes for the Assurances STF.
   */
  enum AssuranceErrorCode:
    case BadAttestationParent
    case BadValidatorIndex
    case CoreNotEngaged
    case BadSignature
    case NotSortedOrUniqueAssurers

  object AssuranceErrorCode:
    given Codec[AssuranceErrorCode] =
      ubyteCodec.emap(
        b =>
          val ordinal = b.toInt
          if ordinal < AssuranceErrorCode.values.length then Right(AssuranceErrorCode.fromOrdinal(ordinal))
          else Left(s"invalid AssuranceErrorCode ordinal: $ordinal"),
        e => UByte(e.ordinal)
      )

    given Decoder[AssuranceErrorCode] =
      Decoder.instance { cursor =>
        cursor.as[String].map {
          case "bad_attestation_parent" => AssuranceErrorCode.BadAttestationParent
          case "bad_validator_index" => AssuranceErrorCode.BadValidatorIndex
          case "core_not_engaged" => AssuranceErrorCode.CoreNotEngaged
          case "bad_signature" => AssuranceErrorCode.BadSignature
          case "not_sorted_or_unique_assurers" => AssuranceErrorCode.NotSortedOrUniqueAssurers
        }
      }

  /**
   * Output marks containing reported work reports.
   */
  final case class AssuranceOutputMarks(
    reported: List[WorkReport]
  )

  object AssuranceOutputMarks:
    given Codec[AssuranceOutputMarks] =
      compactPrefixedList(summon[Codec[WorkReport]]).xmap(
        reported => AssuranceOutputMarks(reported),
        aom => aom.reported
      )

    given Decoder[AssuranceOutputMarks] = ConfiguredDecoder.derived

  /**
   * Output from the Assurances STF.
   */
  type AssuranceOutput = StfResult[AssuranceOutputMarks, AssuranceErrorCode]

  object AssuranceOutput:
    given Codec[AssuranceOutput] =
      stfResultCodec[AssuranceOutputMarks, AssuranceErrorCode](
        using summon[Codec[AssuranceOutputMarks]],
        summon[Codec[AssuranceErrorCode]]
      )

    given circeDecoder: Decoder[AssuranceOutput] = JsonHelpers.stfResultDecoder[AssuranceOutputMarks, AssuranceErrorCode]

  /**
   * Test case for Assurances STF.
   */
  final case class AssuranceCase(
    input: AssuranceInput,
    preState: AssuranceState,
    output: AssuranceOutput,
    postState: AssuranceState
  )

  object AssuranceCase:
    import AssuranceOutput.circeDecoder
    import AssuranceOutput.given Codec[AssuranceOutput]

    /** Create a config-aware codec for AssuranceCase */
    def codec(coreCount: Int): Codec[AssuranceCase] =
      (AssuranceInput.codec(coreCount) ::
       AssuranceState.codec(coreCount) ::
       summon[Codec[AssuranceOutput]] ::
       AssuranceState.codec(coreCount)).xmap(
        { case (input, preState, output, postState) =>
          AssuranceCase(input, preState, output, postState)
        },
        ac => (ac.input, ac.preState, ac.output, ac.postState)
      )

    given Decoder[AssuranceCase] = ConfiguredDecoder.derived
