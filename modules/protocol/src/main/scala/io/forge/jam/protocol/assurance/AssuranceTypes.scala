package io.forge.jam.protocol.assurance

import io.forge.jam.core.StfResult
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.workpackage.{WorkReport, AvailabilityAssignment}
import io.forge.jam.core.json.JsonHelpers.parseHexBytesFixed
import io.circe.Decoder
import spire.math.UByte
import _root_.scodec.Codec
import _root_.scodec.codecs.*
import io.forge.jam.core.scodec.JamCodecs.{hashCodec, ubyteCodec, compactPrefixedList, fixedSizeList, optionCodec, stfResultCodec}

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
    currValidators: List[ValidatorKey]
  )

  object AssuranceState:
    /** Create a codec that knows the cores and validator counts */
    def codec(coreCount: Int, validatorCount: Int): Codec[AssuranceState] =
      (fixedSizeList(optionCodec(summon[Codec[AvailabilityAssignment]]), coreCount) ::
       fixedSizeList(summon[Codec[ValidatorKey]], validatorCount)).xmap(
        { case (availAssignments, currValidators) =>
          AssuranceState(availAssignments, currValidators)
        },
        as => (as.availAssignments, as.currValidators)
      )

    given Decoder[AssuranceState] =
      Decoder.instance { cursor =>
        for
          availAssignments <- cursor.get[List[Option[AvailabilityAssignment]]]("avail_assignments")
          currValidators <- cursor.get[List[ValidatorKey]]("curr_validators")
        yield AssuranceState(availAssignments, currValidators)
      }

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
      ubyteCodec.xmap(
        b => AssuranceErrorCode.fromOrdinal(b.toInt),
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

    given Decoder[AssuranceOutputMarks] =
      Decoder.instance { cursor =>
        for
          reported <- cursor.get[List[WorkReport]]("reported")
        yield AssuranceOutputMarks(reported)
      }

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

    given circeDecoder: Decoder[AssuranceOutput] =
      Decoder.instance { cursor =>
        val okResult = cursor.get[AssuranceOutputMarks]("ok")
        val errResult = cursor.get[AssuranceErrorCode]("err")
        (okResult, errResult) match
          case (Right(ok), _) => Right(StfResult.success(ok))
          case (_, Right(err)) => Right(StfResult.error(err))
          case (Left(_), Left(_)) =>
            // Try to determine which field is present
            cursor.downField("ok").focus match
              case Some(_) =>
                cursor.get[AssuranceOutputMarks]("ok").map(ok => StfResult.success(ok))
              case None =>
                cursor.get[AssuranceErrorCode]("err").map(err => StfResult.error(err))
      }

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
    def codec(coreCount: Int, validatorCount: Int): Codec[AssuranceCase] =
      (AssuranceInput.codec(coreCount) ::
       AssuranceState.codec(coreCount, validatorCount) ::
       summon[Codec[AssuranceOutput]] ::
       AssuranceState.codec(coreCount, validatorCount)).xmap(
        { case (input, preState, output, postState) =>
          AssuranceCase(input, preState, output, postState)
        },
        ac => (ac.input, ac.preState, ac.output, ac.postState)
      )

    given Decoder[AssuranceCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[AssuranceInput]("input")
          preState <- cursor.get[AssuranceState]("pre_state")
          output <- cursor.get[AssuranceOutput]("output")
          postState <- cursor.get[AssuranceState]("post_state")
        yield AssuranceCase(input, preState, output, postState)
      }
