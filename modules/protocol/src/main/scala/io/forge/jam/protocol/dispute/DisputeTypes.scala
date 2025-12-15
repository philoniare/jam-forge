package io.forge.jam.protocol.dispute

import io.forge.jam.core.{ChainConfig, JamBytes, codec, CodecDerivation, StfResult}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode, decodeAs}
import io.forge.jam.core.primitives.{Hash, Ed25519PublicKey, Ed25519Signature}
import io.forge.jam.core.types.extrinsic.Dispute
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.workpackage.AvailabilityAssignment
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder
import spire.math.UInt

/**
 * Types for the Disputes State Transition Function.
 *
 * The Disputes STF processes dispute verdicts, culprits, and faults,
 * maintaining the judgment state (psi) with good/bad/wonky report hashes
 * and tracking offending validators.
 */
object DisputeTypes:

  /**
   * Psi state containing judgment results.
   * Tracks good, bad, and wonky report hashes, plus offending validator keys.
   */
  final case class Psi(
    good: List[Hash],
    bad: List[Hash],
    wonky: List[Hash],
    offenders: List[Ed25519PublicKey]
  )

  object Psi:
    def empty: Psi = Psi(List.empty, List.empty, List.empty, List.empty)

    given JamEncoder[Psi] with
      def encode(a: Psi): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.good.encode
        builder ++= a.bad.encode
        builder ++= a.wonky.encode
        builder ++= a.offenders.encode
        builder.result()

    given JamDecoder[Psi] with
      def decode(bytes: JamBytes, offset: Int): (Psi, Int) =
        var pos = offset
        val (good, goodBytes) = bytes.decodeAs[List[Hash]](pos)
        pos += goodBytes
        val (bad, badBytes) = bytes.decodeAs[List[Hash]](pos)
        pos += badBytes
        val (wonky, wonkyBytes) = bytes.decodeAs[List[Hash]](pos)
        pos += wonkyBytes
        val (offenders, offendersBytes) = bytes.decodeAs[List[Ed25519PublicKey]](pos)
        pos += offendersBytes
        (Psi(good, bad, wonky, offenders), pos - offset)

    given Decoder[Psi] =
      Decoder.instance { cursor =>
        for
          good <- cursor.get[List[String]]("good").map(_.map(h => Hash(parseHex(h))))
          bad <- cursor.get[List[String]]("bad").map(_.map(h => Hash(parseHex(h))))
          wonky <- cursor.get[List[String]]("wonky").map(_.map(h => Hash(parseHex(h))))
          offenders <- cursor.get[List[String]]("offenders").map(_.map(h => Ed25519PublicKey(parseHex(h))))
        yield Psi(good, bad, wonky, offenders)
      }

  /**
   * Dispute state containing psi judgment state, validator sets, and availability assignments.
   */
  final case class DisputeState(
    psi: Psi,
    rho: List[Option[AvailabilityAssignment]],
    tau: Long,
    kappa: List[ValidatorKey],
    lambda: List[ValidatorKey]
  )

  object DisputeState:
    given JamEncoder[DisputeState] with
      def encode(a: DisputeState): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.psi.encode
        // rho - fixed size list (coreCount) of optional AvailabilityAssignment
        for assignment <- a.rho do
          assignment match
            case None =>
              builder += 0.toByte
            case Some(aa) =>
              builder += 1.toByte
              builder ++= aa.encode
        builder ++= codec.encodeU32LE(UInt(a.tau.toInt))
        builder ++= codec.encodeFixedList(a.kappa)
        builder ++= codec.encodeFixedList(a.lambda)
        builder.result()

    /** Create a decoder that knows the cores and validator counts */
    def decoder(coreCount: Int, validatorCount: Int): JamDecoder[DisputeState] = new JamDecoder[DisputeState]:
      def decode(bytes: JamBytes, offset: Int): (DisputeState, Int) =
        val arr = bytes.toArray
        var pos = offset

        val (psi, psiBytes) = bytes.decodeAs[Psi](pos)
        pos += psiBytes

        // rho - fixed size list (coreCount) of optional AvailabilityAssignment
        val rho = (0 until coreCount).map { _ =>
          val discriminator = arr(pos).toInt & 0xff
          pos += 1
          if discriminator == 0 then
            None
          else
            val (assignment, consumed) = bytes.decodeAs[AvailabilityAssignment](pos)
            pos += consumed
            Some(assignment)
        }.toList

        val tau = codec.decodeU32LE(arr, pos).toLong
        pos += 4

        val (kappa, kappaBytes) = codec.decodeFixedList[ValidatorKey](bytes, pos, validatorCount)
        pos += kappaBytes

        val (lambda, lambdaBytes) = codec.decodeFixedList[ValidatorKey](bytes, pos, validatorCount)
        pos += lambdaBytes

        (DisputeState(psi, rho, tau, kappa, lambda), pos - offset)

    given Decoder[DisputeState] =
      Decoder.instance { cursor =>
        for
          psi <- cursor.get[Psi]("psi")
          rho <- cursor.get[List[Option[AvailabilityAssignment]]]("rho")
          tau <- cursor.get[Long]("tau")
          kappa <- cursor.get[List[ValidatorKey]]("kappa")
          lambda <- cursor.get[List[ValidatorKey]]("lambda")
        yield DisputeState(psi, rho, tau, kappa, lambda)
      }

  /**
   * Input to the Disputes STF.
   */
  final case class DisputeInput(
    disputes: Dispute
  )

  object DisputeInput:
    given JamEncoder[DisputeInput] with
      def encode(a: DisputeInput): JamBytes =
        a.disputes.encode

    /** Create a decoder that knows the votes per verdict (typically 2/3 of validators + 1) */
    def decoder(votesPerVerdict: Int): JamDecoder[DisputeInput] = new JamDecoder[DisputeInput]:
      def decode(bytes: JamBytes, offset: Int): (DisputeInput, Int) =
        val disputeDecoder = Dispute.decoder(votesPerVerdict)
        val (disputes, consumed) = disputeDecoder.decode(bytes, offset)
        (DisputeInput(disputes), consumed)

    given Decoder[DisputeInput] =
      Decoder.instance { cursor =>
        for
          disputes <- cursor.get[Dispute]("disputes")
        yield DisputeInput(disputes)
      }

  /**
   * Error codes for the Disputes STF.
   */
  enum DisputeErrorCode:
    case BadSignature
    case BadGuarantorKey
    case BadAuditorKey
    case BadJudgementAge
    case BadVoteSplit
    case NotEnoughFaults
    case NotEnoughCulprits
    case OffenderAlreadyReported
    case AlreadyJudged
    case CulpritsNotSortedUnique
    case VerdictsNotSortedUnique
    case FaultsNotSortedUnique
    case JudgementsNotSortedUnique
    case FaultVerdictWrong
    case CulpritsVerdictNotBad

  object DisputeErrorCode:
    given JamEncoder[DisputeErrorCode] = CodecDerivation.enumEncoder(_.ordinal)
    given JamDecoder[DisputeErrorCode] = CodecDerivation.enumDecoder(DisputeErrorCode.fromOrdinal)

    given Decoder[DisputeErrorCode] =
      Decoder.instance { cursor =>
        cursor.as[String].map {
          case "bad_signature" => DisputeErrorCode.BadSignature
          case "bad_guarantor_key" => DisputeErrorCode.BadGuarantorKey
          case "bad_auditor_key" => DisputeErrorCode.BadAuditorKey
          case "bad_judgement_age" => DisputeErrorCode.BadJudgementAge
          case "bad_vote_split" => DisputeErrorCode.BadVoteSplit
          case "not_enough_faults" => DisputeErrorCode.NotEnoughFaults
          case "not_enough_culprits" => DisputeErrorCode.NotEnoughCulprits
          case "offender_already_reported" => DisputeErrorCode.OffenderAlreadyReported
          case "already_judged" => DisputeErrorCode.AlreadyJudged
          case "culprits_not_sorted_unique" => DisputeErrorCode.CulpritsNotSortedUnique
          case "verdicts_not_sorted_unique" => DisputeErrorCode.VerdictsNotSortedUnique
          case "faults_not_sorted_unique" => DisputeErrorCode.FaultsNotSortedUnique
          case "judgements_not_sorted_unique" => DisputeErrorCode.JudgementsNotSortedUnique
          case "fault_verdict_wrong" => DisputeErrorCode.FaultVerdictWrong
          case "culprits_verdict_not_bad" => DisputeErrorCode.CulpritsVerdictNotBad
        }
      }

  /**
   * Output marks containing newly identified offenders.
   */
  final case class DisputeOutputMarks(
    offenders: List[Ed25519PublicKey]
  )

  object DisputeOutputMarks:
    given JamEncoder[DisputeOutputMarks] with
      def encode(a: DisputeOutputMarks): JamBytes = a.offenders.encode

    given JamDecoder[DisputeOutputMarks] with
      def decode(bytes: JamBytes, offset: Int): (DisputeOutputMarks, Int) =
        val (offenders, consumed) = bytes.decodeAs[List[Ed25519PublicKey]](offset)
        (DisputeOutputMarks(offenders), consumed)

    // JSON uses "offenders_mark" field name
    given Decoder[DisputeOutputMarks] =
      Decoder.instance { cursor =>
        for
          offenders <- cursor.get[List[String]]("offenders_mark")
        yield DisputeOutputMarks(offenders.map(h => Ed25519PublicKey(parseHex(h))))
      }

  /**
   * Output from the Disputes STF.
   */
  type DisputeOutput = StfResult[DisputeOutputMarks, DisputeErrorCode]

  object DisputeOutput:
    given JamEncoder[DisputeOutput] = StfResult.stfResultEncoder[DisputeOutputMarks, DisputeErrorCode]
    given JamDecoder[DisputeOutput] = StfResult.stfResultDecoder[DisputeOutputMarks, DisputeErrorCode]

    // Circe JSON decoder for DisputeOutput
    given circeDecoder: Decoder[DisputeOutput] =
      Decoder.instance { cursor =>
        val okResult = cursor.get[DisputeOutputMarks]("ok")
        val errResult = cursor.get[DisputeErrorCode]("err")
        (okResult, errResult) match
          case (Right(ok), _) => Right(StfResult.success(ok))
          case (_, Right(err)) => Right(StfResult.error(err))
          case (Left(_), Left(_)) =>
            cursor.downField("ok").focus match
              case Some(_) =>
                cursor.get[DisputeOutputMarks]("ok").map(ok => StfResult.success(ok))
              case None =>
                cursor.get[DisputeErrorCode]("err").map(err => StfResult.error(err))
      }

  /**
   * Test case for Disputes STF.
   */
  final case class DisputeCase(
    input: DisputeInput,
    preState: DisputeState,
    output: DisputeOutput,
    postState: DisputeState
  )

  object DisputeCase:
    import DisputeOutput.circeDecoder

    /** Create a config-aware decoder for DisputeCase */
    def decoder(coreCount: Int, validatorCount: Int, votesPerVerdict: Int): JamDecoder[DisputeCase] =
      new JamDecoder[DisputeCase]:
        def decode(bytes: JamBytes, offset: Int): (DisputeCase, Int) =
          var pos = offset
          val inputDecoder = DisputeInput.decoder(votesPerVerdict)
          val (input, inputBytes) = inputDecoder.decode(bytes, pos)
          pos += inputBytes
          val stateDecoder = DisputeState.decoder(coreCount, validatorCount)
          val (preState, preStateBytes) = stateDecoder.decode(bytes, pos)
          pos += preStateBytes
          val (output, outputBytes) = bytes.decodeAs[DisputeOutput](pos)
          pos += outputBytes
          val (postState, postStateBytes) = stateDecoder.decode(bytes, pos)
          pos += postStateBytes
          (DisputeCase(input, preState, output, postState), pos - offset)

    /** Create a config-aware encoder for DisputeCase */
    given JamEncoder[DisputeCase] with
      def encode(a: DisputeCase): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.input.encode
        builder ++= a.preState.encode
        builder ++= a.output.encode
        builder ++= a.postState.encode
        builder.result()

    given Decoder[DisputeCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[DisputeInput]("input")
          preState <- cursor.get[DisputeState]("pre_state")
          output <- cursor.get[DisputeOutput]("output")
          postState <- cursor.get[DisputeState]("post_state")
        yield DisputeCase(input, preState, output, postState)
      }
