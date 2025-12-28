package io.forge.jam.protocol.safrole

import io.forge.jam.core.{JamBytes, StfResult}
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey}
import io.forge.jam.core.types.epoch.{ValidatorKey, EpochMark}
import io.forge.jam.core.types.tickets.{TicketEnvelope, TicketMark}
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.{Decoder, DecodingFailure}
import _root_.scodec.Codec
import _root_.scodec.codecs.*
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.scodec.JamCodecs.{given Codec[BandersnatchPublicKey]}

/**
 * Types for the Safrole State Transition Function.
 */
object SafroleTypes:

  val BandersnatchRingCommitmentSize: Int = 144

  /**
   * Either tickets or fallback Bandersnatch keys for the sealing sequence.
   * Binary encoding: discriminator byte (0 for tickets, 1 for keys) + fixed-size list (epochLength items)
   */
  sealed trait TicketsOrKeys

  object TicketsOrKeys:
    /** Sealing sequence from validated tickets (outside-in interleaved) */
    final case class Tickets(tickets: List[TicketMark]) extends TicketsOrKeys

    /** Fallback sealing sequence from Bandersnatch public keys */
    final case class Keys(keys: List[BandersnatchPublicKey]) extends TicketsOrKeys

    /** Create a codec that knows the expected epoch length */
    def codec(epochLength: Int): Codec[TicketsOrKeys] =
      val ticketsCodec: Codec[List[TicketMark]] =
        JamCodecs.fixedSizeList(summon[Codec[TicketMark]], epochLength)
      val keysCodec: Codec[List[BandersnatchPublicKey]] =
        JamCodecs.fixedSizeList(summon[Codec[BandersnatchPublicKey]], epochLength)

      discriminated[TicketsOrKeys]
        .by(byte)
        .subcaseP(0) { case t: Tickets => t }(
          ticketsCodec.xmap(Tickets.apply, _.tickets)
        )
        .subcaseP(1) { case k: Keys => k }(
          keysCodec.xmap(Keys.apply, _.keys)
        )

    given Decoder[TicketsOrKeys] = Decoder.instance { cursor =>
      val ticketsResult = cursor.downField("tickets").as[List[TicketMark]]
      val keysResult = cursor.downField("keys").as[List[String]]

      ticketsResult match
        case Right(tickets) => Right(Tickets(tickets))
        case Left(_) =>
          keysResult match
            case Right(keyHexes) =>
              val keys = keyHexes.map(hex => BandersnatchPublicKey(parseHex(hex)))
              Right(Keys(keys))
            case Left(_) =>
              Left(DecodingFailure("TicketsOrKeys must have either 'tickets' or 'keys' field", cursor.history))
    }

  /**
   * Safrole state containing all Safrole-related state components.
   */
  final case class SafroleState(
    tau: Long,
    eta: List[Hash],
    lambda: List[ValidatorKey],
    kappa: List[ValidatorKey],
    gammaK: List[ValidatorKey],
    iota: List[ValidatorKey],
    gammaA: List[TicketMark],
    gammaS: TicketsOrKeys,
    gammaZ: JamBytes,
    postOffenders: List[Ed25519PublicKey]
  ):
    require(eta.size == 4, s"Entropy buffer must have 4 elements, got ${eta.size}")
    require(
      gammaZ.length == BandersnatchRingCommitmentSize,
      s"Ring commitment must be $BandersnatchRingCommitmentSize bytes, got ${gammaZ.length}"
    )

  object SafroleState:
    /** Create a codec that knows the validator count and epoch length */
    def codec(validatorCount: Int, epochLength: Int): Codec[SafroleState] =
      val tauCodec: Codec[Long] = uint32L.xmap(_ & 0xFFFFFFFFL, _ & 0xFFFFFFFFL)
      val etaCodec: Codec[List[Hash]] = JamCodecs.fixedSizeList(JamCodecs.hashCodec, 4)
      val lambdaCodec: Codec[List[ValidatorKey]] = JamCodecs.fixedSizeList(summon[Codec[ValidatorKey]], validatorCount)
      val kappaCodec: Codec[List[ValidatorKey]] = JamCodecs.fixedSizeList(summon[Codec[ValidatorKey]], validatorCount)
      val gammaKCodec: Codec[List[ValidatorKey]] = JamCodecs.fixedSizeList(summon[Codec[ValidatorKey]], validatorCount)
      val iotaCodec: Codec[List[ValidatorKey]] = JamCodecs.fixedSizeList(summon[Codec[ValidatorKey]], validatorCount)
      val gammaACodec: Codec[List[TicketMark]] = JamCodecs.compactPrefixedList(summon[Codec[TicketMark]])
      val gammaSCodec: Codec[TicketsOrKeys] = TicketsOrKeys.codec(epochLength)
      val gammaZCodec: Codec[JamBytes] = fixedSizeBytes(BandersnatchRingCommitmentSize.toLong, bytes).xmap(
        bv => JamBytes.fromByteVector(bv),
        jb => jb.toByteVector
      )
      val postOffendersCodec: Codec[List[Ed25519PublicKey]] = JamCodecs.compactPrefixedList(JamCodecs.ed25519PublicKeyCodec)

      (tauCodec :: etaCodec :: lambdaCodec :: kappaCodec :: gammaKCodec :: iotaCodec ::
       gammaACodec :: gammaSCodec :: gammaZCodec :: postOffendersCodec).xmap(
        { case (tau, eta, lambda, kappa, gammaK, iota, gammaA, gammaS, gammaZ, postOffenders) =>
          SafroleState(tau, eta, lambda, kappa, gammaK, iota, gammaA, gammaS, gammaZ, postOffenders)
        },
        ss => (ss.tau, ss.eta, ss.lambda, ss.kappa, ss.gammaK, ss.iota, ss.gammaA, ss.gammaS, ss.gammaZ, ss.postOffenders)
      )

    given Decoder[SafroleState] = Decoder.instance { cursor =>
      for
        tau <- cursor.get[Long]("tau")
        eta <- cursor.get[List[String]]("eta").map(_.map(h => Hash(parseHex(h))))
        lambda <- cursor.get[List[ValidatorKey]]("lambda")
        kappa <- cursor.get[List[ValidatorKey]]("kappa")
        gammaK <- cursor.get[List[ValidatorKey]]("gamma_k")
        iota <- cursor.get[List[ValidatorKey]]("iota")
        gammaA <- cursor.get[List[TicketMark]]("gamma_a")
        gammaS <- cursor.get[TicketsOrKeys]("gamma_s")
        gammaZ <- cursor.get[String]("gamma_z").map(h => JamBytes(parseHex(h)))
        postOffenders <- cursor.get[List[String]]("post_offenders").map(_.map(h => Ed25519PublicKey(parseHex(h))))
      yield SafroleState(tau, eta, lambda, kappa, gammaK, iota, gammaA, gammaS, gammaZ, postOffenders)
    }

  /**
   * Input to the Safrole STF.
   */
  final case class SafroleInput(
    /** Current time slot from block header */
    slot: Long,
    /** Per-block entropy source */
    entropy: Hash,
    /** Tickets extrinsic (list of TicketEnvelope) */
    extrinsic: List[TicketEnvelope]
  )

  object SafroleInput:
    given Codec[SafroleInput] =
      val slotCodec: Codec[Long] = uint32L.xmap(_ & 0xFFFFFFFFL, _ & 0xFFFFFFFFL)
      val entropyCodec: Codec[Hash] = JamCodecs.hashCodec
      val extrinsicCodec: Codec[List[TicketEnvelope]] = JamCodecs.compactPrefixedList(summon[Codec[TicketEnvelope]])

      (slotCodec :: entropyCodec :: extrinsicCodec).xmap(
        { case (slot, entropy, extrinsic) => SafroleInput(slot, entropy, extrinsic) },
        si => (si.slot, si.entropy, si.extrinsic)
      )

    given Decoder[SafroleInput] = Decoder.instance { cursor =>
      for
        slot <- cursor.get[Long]("slot")
        entropy <- cursor.get[String]("entropy").map(h => Hash(parseHex(h)))
        extrinsic <- cursor.get[List[TicketEnvelope]]("extrinsic")
      yield SafroleInput(slot, entropy, extrinsic)
    }

  /**
   * Error codes for the Safrole STF.
   */
  enum SafroleErrorCode:
    case BadSlot
    case UnexpectedTicket
    case BadTicketOrder
    case BadTicketProof
    case BadTicketAttempt
    case Reserved
    case DuplicateTicket

  object SafroleErrorCode:
    given Codec[SafroleErrorCode] = byte.xmap(
      b => SafroleErrorCode.fromOrdinal(b & 0xff),
      e => e.ordinal.toByte
    )

    given Decoder[SafroleErrorCode] = Decoder.instance { cursor =>
      cursor.as[String].map {
        case "bad_slot" => SafroleErrorCode.BadSlot
        case "unexpected_ticket" => SafroleErrorCode.UnexpectedTicket
        case "bad_ticket_order" => SafroleErrorCode.BadTicketOrder
        case "bad_ticket_proof" => SafroleErrorCode.BadTicketProof
        case "bad_ticket_attempt" => SafroleErrorCode.BadTicketAttempt
        case "reserved" => SafroleErrorCode.Reserved
        case "duplicate_ticket" => SafroleErrorCode.DuplicateTicket
      }
    }

  /**
   * Output data from a successful Safrole STF execution.
   */
  final case class SafroleOutputData(
    /** New epoch marker (generated on epoch boundary crossing) */
    epochMark: Option[EpochMark],
    /** Winning tickets marker (generated when transitioning to epoch tail with full accumulator) */
    ticketsMark: Option[List[TicketMark]]
  )

  object SafroleOutputData:
    /** Create a codec that knows the validator count and epoch length */
    def codec(validatorCount: Int, epochLength: Int): Codec[SafroleOutputData] =
      val epochMarkCodec: Codec[Option[EpochMark]] =
        JamCodecs.optionCodec(EpochMark.epochMarkCodec(validatorCount))
      val ticketsMarkCodec: Codec[Option[List[TicketMark]]] =
        JamCodecs.optionCodec(JamCodecs.fixedSizeList(summon[Codec[TicketMark]], epochLength))

      (epochMarkCodec :: ticketsMarkCodec).xmap(
        { case (epochMark, ticketsMark) => SafroleOutputData(epochMark, ticketsMark) },
        sod => (sod.epochMark, sod.ticketsMark)
      )

    given Decoder[SafroleOutputData] = Decoder.instance { cursor =>
      for
        epochMark <- cursor.get[Option[EpochMark]]("epoch_mark")
        ticketsMark <- cursor.get[Option[List[TicketMark]]]("tickets_mark")
      yield SafroleOutputData(epochMark, ticketsMark)
    }

  /**
   * Output from the Safrole STF.
   */
  type SafroleOutput = StfResult[SafroleOutputData, SafroleErrorCode]

  object SafroleOutput:
    /**
     * Create a config-aware codec for SafroleOutput.
     */
    def codec(validatorCount: Int, epochLength: Int): Codec[SafroleOutput] =
      JamCodecs.stfResultCodec(
        using SafroleOutputData.codec(validatorCount, epochLength),
        summon[Codec[SafroleErrorCode]]
      )

    // Circe JSON decoder for SafroleOutput
    given circeDecoder: Decoder[SafroleOutput] = Decoder.instance { cursor =>
      val okResult = cursor.get[SafroleOutputData]("ok")
      val errResult = cursor.get[SafroleErrorCode]("err")
      (okResult, errResult) match
        case (Right(ok), _) => Right(StfResult.success(ok))
        case (_, Right(err)) => Right(StfResult.error(err))
        case (Left(_), Left(_)) =>
          cursor.downField("ok").focus match
            case Some(_) =>
              cursor.get[SafroleOutputData]("ok").map(ok => StfResult.success(ok))
            case None =>
              cursor.get[SafroleErrorCode]("err").map(err => StfResult.error(err))
    }

  /**
   * Test case for Safrole STF.
   */
  final case class SafroleCase(
    input: SafroleInput,
    preState: SafroleState,
    output: SafroleOutput,
    postState: SafroleState
  )

  object SafroleCase:
    import SafroleOutput.circeDecoder

    /** Create a config-aware codec for SafroleCase */
    def codec(validatorCount: Int, epochLength: Int): Codec[SafroleCase] =
      val inputCodec: Codec[SafroleInput] = summon[Codec[SafroleInput]]
      val stateCodec: Codec[SafroleState] = SafroleState.codec(validatorCount, epochLength)
      val outputCodec: Codec[SafroleOutput] = SafroleOutput.codec(validatorCount, epochLength)

      (inputCodec :: stateCodec :: outputCodec :: stateCodec).xmap(
        { case (input, preState, output, postState) =>
          SafroleCase(input, preState, output, postState)
        },
        sc => (sc.input, sc.preState, sc.output, sc.postState)
      )

    given Decoder[SafroleCase] = Decoder.instance { cursor =>
      for
        input <- cursor.get[SafroleInput]("input")
        preState <- cursor.get[SafroleState]("pre_state")
        output <- cursor.get[SafroleOutput]("output")
        postState <- cursor.get[SafroleState]("post_state")
      yield SafroleCase(input, preState, output, postState)
    }
