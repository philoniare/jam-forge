package io.forge.jam.protocol.assurance

import io.forge.jam.core.{JamBytes, codec, CodecDerivation, StfResult}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode, decodeAs, encodeFixedList, decodeFixedList}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.workpackage.{WorkReport, AvailabilityAssignment}
import io.forge.jam.core.json.JsonHelpers.parseHexBytesFixed
import io.circe.Decoder
import spire.math.UInt

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
    given JamEncoder[AssuranceInput] with
      def encode(a: AssuranceInput): JamBytes =
        val builder = JamBytes.newBuilder
        // assurances - compact length prefix + items (uses listEncoder)
        builder ++= a.assurances.encode
        // slot - 4 bytes
        builder ++= codec.encodeU32LE(UInt(a.slot.toInt))
        // parent - 32 bytes
        builder ++= a.parent.bytes
        builder.result()

    /** Create a decoder that knows the cores count */
    def decoder(coreCount: Int): JamDecoder[AssuranceInput] = new JamDecoder[AssuranceInput]:
      def decode(bytes: JamBytes, offset: Int): (AssuranceInput, Int) =
        val arr = bytes.toArray
        var pos = offset

        // assurances - compact length prefix + fixed-size items
        val (assurancesLength, assurancesLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += assurancesLengthBytes
        val assuranceDecoder = AssuranceExtrinsic.decoder(coreCount)
        val assurances = (0 until assurancesLength.toInt).map { _ =>
          val (assurance, consumed) = assuranceDecoder.decode(bytes, pos)
          pos += consumed
          assurance
        }.toList

        // slot - 4 bytes
        val slot = codec.decodeU32LE(arr, pos).toLong
        pos += 4

        // parent - 32 bytes
        val parent = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        (AssuranceInput(assurances, slot, parent), pos - offset)

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
    given JamEncoder[AssuranceState] with
      def encode(a: AssuranceState): JamBytes =
        val builder = JamBytes.newBuilder
        // availAssignments - fixed size list (coreCount) of optional AvailabilityAssignment
        builder ++= encodeFixedList(a.availAssignments)
        // currValidators - fixed size list (validatorCount)
        builder ++= encodeFixedList(a.currValidators)
        builder.result()

    /** Create a decoder that knows the cores and validator counts */
    def decoder(coreCount: Int, validatorCount: Int): JamDecoder[AssuranceState] = new JamDecoder[AssuranceState]:
      def decode(bytes: JamBytes, offset: Int): (AssuranceState, Int) =
        var pos = offset

        // availAssignments - fixed size list (coreCount) of optional AvailabilityAssignment
        val (availAssignments, availConsumed) = decodeFixedList[Option[AvailabilityAssignment]](bytes, pos, coreCount)
        pos += availConsumed

        // currValidators - fixed size list (validatorCount)
        val (currValidators, validatorsConsumed) = decodeFixedList[ValidatorKey](bytes, pos, validatorCount)
        pos += validatorsConsumed

        (AssuranceState(availAssignments, currValidators), pos - offset)

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
    given JamEncoder[AssuranceErrorCode] = CodecDerivation.enumEncoder(_.ordinal)
    given JamDecoder[AssuranceErrorCode] = CodecDerivation.enumDecoder(AssuranceErrorCode.fromOrdinal)

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
    given JamEncoder[AssuranceOutputMarks] with
      def encode(a: AssuranceOutputMarks): JamBytes =
        // Uses listEncoder for compact length prefix + items
        a.reported.encode

    given JamDecoder[AssuranceOutputMarks] with
      def decode(bytes: JamBytes, offset: Int): (AssuranceOutputMarks, Int) =
        // Uses listDecoder for compact length prefix + items
        val (reported, consumed) = bytes.decodeAs[List[WorkReport]](offset)
        (AssuranceOutputMarks(reported), consumed)

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
    given JamEncoder[AssuranceOutput] = StfResult.stfResultEncoder[AssuranceOutputMarks, AssuranceErrorCode]
    given JamDecoder[AssuranceOutput] = StfResult.stfResultDecoder[AssuranceOutputMarks, AssuranceErrorCode]

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

    /** Create a config-aware decoder for AssuranceCase */
    def decoder(coreCount: Int, validatorCount: Int): JamDecoder[AssuranceCase] = new JamDecoder[AssuranceCase]:
      def decode(bytes: JamBytes, offset: Int): (AssuranceCase, Int) =
        var pos = offset
        val inputDecoder = AssuranceInput.decoder(coreCount)
        val (input, inputBytes) = inputDecoder.decode(bytes, pos)
        pos += inputBytes
        val stateDecoder = AssuranceState.decoder(coreCount, validatorCount)
        val (preState, preStateBytes) = stateDecoder.decode(bytes, pos)
        pos += preStateBytes
        val (output, outputBytes) = bytes.decodeAs[AssuranceOutput](pos)
        pos += outputBytes
        val (postState, postStateBytes) = stateDecoder.decode(bytes, pos)
        pos += postStateBytes
        (AssuranceCase(input, preState, output, postState), pos - offset)

    /** Create a config-aware encoder for AssuranceCase */
    given JamEncoder[AssuranceCase] with
      def encode(a: AssuranceCase): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.input.encode
        builder ++= a.preState.encode
        builder ++= a.output.encode
        builder ++= a.postState.encode
        builder.result()

    given Decoder[AssuranceCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[AssuranceInput]("input")
          preState <- cursor.get[AssuranceState]("pre_state")
          output <- cursor.get[AssuranceOutput]("output")
          postState <- cursor.get[AssuranceState]("post_state")
        yield AssuranceCase(input, preState, output, postState)
      }
