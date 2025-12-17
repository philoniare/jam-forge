package io.forge.jam.protocol.dispute

import io.forge.jam.core.{ChainConfig, JamBytes, StfResult}
import io.forge.jam.core.primitives.{Hash, Ed25519PublicKey, Ed25519Signature}
import io.forge.jam.core.types.extrinsic.Dispute
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.workpackage.AvailabilityAssignment
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder
import spire.math.UInt
import _root_.scodec.{Codec, Attempt, DecodeResult}
import _root_.scodec.bits.BitVector
import _root_.scodec.codecs.*
import io.forge.jam.core.scodec.JamCodecs

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

    given Codec[Psi] =
      (JamCodecs.compactPrefixedList(JamCodecs.hashCodec) ::
       JamCodecs.compactPrefixedList(JamCodecs.hashCodec) ::
       JamCodecs.compactPrefixedList(JamCodecs.hashCodec) ::
       JamCodecs.compactPrefixedList(JamCodecs.ed25519PublicKeyCodec)).xmap(
        { case (good, bad, wonky, offenders) =>
          Psi(good, bad, wonky, offenders)
        },
        psi => (psi.good, psi.bad, psi.wonky, psi.offenders)
      )

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
    /** Create a config-aware codec for DisputeState */
    def codec(coreCount: Int, validatorCount: Int): Codec[DisputeState] =
      given availabilityAssignmentCodec: Codec[AvailabilityAssignment] = summon[Codec[AvailabilityAssignment]]

      val psiCodec = summon[Codec[Psi]]
      val rhoCodec = JamCodecs.fixedSizeList(JamCodecs.optionCodec(availabilityAssignmentCodec), coreCount)
      val tauCodec = uint32L.xmap(
        l => l & 0xFFFFFFFFL,
        tau => tau & 0xFFFFFFFFL
      )
      val validatorListCodec = JamCodecs.fixedSizeList(summon[Codec[ValidatorKey]], validatorCount)

      (psiCodec :: rhoCodec :: tauCodec :: validatorListCodec :: validatorListCodec).xmap(
        { case (psi, rho, tau, kappa, lambda) =>
          DisputeState(psi, rho, tau, kappa, lambda)
        },
        state => (state.psi, state.rho, state.tau, state.kappa, state.lambda)
      )

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
    /** Create a config-aware codec for DisputeInput */
    def codec(votesPerVerdict: Int): Codec[DisputeInput] =
      Dispute.codec(votesPerVerdict).xmap(
        disputes => DisputeInput(disputes),
        input => input.disputes
      )

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
    given Codec[DisputeErrorCode] = byte.xmap(
      b => DisputeErrorCode.fromOrdinal(b.toInt & 0xFF),
      e => e.ordinal.toByte
    )

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
    given Codec[DisputeOutputMarks] =
      JamCodecs.compactPrefixedList(JamCodecs.ed25519PublicKeyCodec).xmap(
        offenders => DisputeOutputMarks(offenders),
        marks => marks.offenders
      )

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
    given Codec[DisputeOutput] = JamCodecs.stfResultCodec[DisputeOutputMarks, DisputeErrorCode](
      using summon[Codec[DisputeOutputMarks]],
      summon[Codec[DisputeErrorCode]]
    )

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
    import DisputeOutput.given

    /** Create a config-aware codec for DisputeCase */
    def codec(coreCount: Int, validatorCount: Int, votesPerVerdict: Int): Codec[DisputeCase] =
      val inputCodec = DisputeInput.codec(votesPerVerdict)
      val stateCodec = DisputeState.codec(coreCount, validatorCount)
      val outputCodec = summon[Codec[DisputeOutput]]

      (inputCodec :: stateCodec :: outputCodec :: stateCodec).xmap(
        { case (input, preState, output, postState) =>
          DisputeCase(input, preState, output, postState)
        },
        testCase => (testCase.input, testCase.preState, testCase.output, testCase.postState)
      )

    given Decoder[DisputeCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[DisputeInput]("input")
          preState <- cursor.get[DisputeState]("pre_state")
          output <- cursor.get[DisputeOutput]("output")
          postState <- cursor.get[DisputeState]("post_state")
        yield DisputeCase(input, preState, output, postState)
      }
