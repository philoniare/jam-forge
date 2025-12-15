package io.forge.jam.protocol.safrole

import io.forge.jam.core.{ChainConfig, JamBytes, codec, CodecDerivation, StfResult}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode, decodeAs, encodeFixedList, decodeFixedList}
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey}
import io.forge.jam.core.types.epoch.{ValidatorKey, EpochMark, EpochValidatorKey}
import io.forge.jam.core.types.tickets.{TicketEnvelope, TicketMark}
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.{Decoder, DecodingFailure}

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

    given JamEncoder[TicketsOrKeys] with
      def encode(a: TicketsOrKeys): JamBytes =
        val builder = JamBytes.newBuilder
        a match
          case Tickets(tickets) =>
            builder += 0.toByte
            builder ++= encodeFixedList(tickets)
          case Keys(keys) =>
            builder += 1.toByte
            for key <- keys do
              builder ++= key.bytes
        builder.result()

    /** Create a decoder that knows the expected epoch length */
    def decoder(epochLength: Int): JamDecoder[TicketsOrKeys] = new JamDecoder[TicketsOrKeys]:
      def decode(bytes: JamBytes, offset: Int): (TicketsOrKeys, Int) =
        val discriminator = bytes(offset).toInt & 0xff
        var pos = offset + 1
        if discriminator == 0 then
          // Tickets variant
          val (tickets, ticketsConsumed) = decodeFixedList[TicketMark](bytes, pos, epochLength)
          (Tickets(tickets), 1 + ticketsConsumed)
        else
          // Keys variant
          val keys = (0 until epochLength).map { i =>
            val keyOffset = pos + i * BandersnatchPublicKey.Size
            BandersnatchPublicKey(bytes.toArray.slice(keyOffset, keyOffset + BandersnatchPublicKey.Size))
          }.toList
          (Keys(keys), 1 + epochLength * BandersnatchPublicKey.Size)

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
            case Left(err) =>
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
    given JamEncoder[SafroleState] with
      def encode(a: SafroleState): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeU32LE(spire.math.UInt(a.tau.toInt))
        for hash <- a.eta do
          builder ++= hash.bytes
        builder ++= encodeFixedList(a.lambda)
        builder ++= encodeFixedList(a.kappa)
        builder ++= encodeFixedList(a.gammaK)
        builder ++= encodeFixedList(a.iota)
        builder ++= a.gammaA.encode
        builder ++= a.gammaS.encode
        builder ++= a.gammaZ
        builder ++= codec.encodeCompactInteger(a.postOffenders.size.toLong)
        for offender <- a.postOffenders do
          builder ++= offender.bytes
        builder.result()

    /** Create a decoder that knows the validator count and epoch length */
    def decoder(validatorCount: Int, epochLength: Int): JamDecoder[SafroleState] = new JamDecoder[SafroleState]:
      def decode(bytes: JamBytes, offset: Int): (SafroleState, Int) =
        val arr = bytes.toArray
        var pos = offset
        val tau = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        val eta = (0 until 4).map { i =>
          val hashOffset = pos + i * Hash.Size
          Hash(arr.slice(hashOffset, hashOffset + Hash.Size))
        }.toList
        pos += 4 * Hash.Size
        val (lambda, lambdaConsumed) = decodeFixedList[ValidatorKey](bytes, pos, validatorCount)
        pos += lambdaConsumed
        val (kappa, kappaConsumed) = decodeFixedList[ValidatorKey](bytes, pos, validatorCount)
        pos += kappaConsumed
        val (gammaK, gammaKConsumed) = decodeFixedList[ValidatorKey](bytes, pos, validatorCount)
        pos += gammaKConsumed
        val (iota, iotaConsumed) = decodeFixedList[ValidatorKey](bytes, pos, validatorCount)
        pos += iotaConsumed
        val (gammaA, gammaAConsumed) = bytes.decodeAs[List[TicketMark]](pos)
        pos += gammaAConsumed
        val ticketsOrKeysDecoder = TicketsOrKeys.decoder(epochLength)
        val (gammaS, gammaSConsumed) = ticketsOrKeysDecoder.decode(bytes, pos)
        pos += gammaSConsumed
        val gammaZ = bytes.slice(pos, pos + BandersnatchRingCommitmentSize)
        pos += BandersnatchRingCommitmentSize
        val (offendersLength, offendersLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += offendersLengthBytes
        val postOffenders = (0 until offendersLength.toInt).map { i =>
          val keyOffset = pos + i * Ed25519PublicKey.Size
          Ed25519PublicKey(arr.slice(keyOffset, keyOffset + Ed25519PublicKey.Size))
        }.toList
        pos += offendersLength.toInt * Ed25519PublicKey.Size

        (SafroleState(tau, eta, lambda, kappa, gammaK, iota, gammaA, gammaS, gammaZ, postOffenders), pos - offset)

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
    given JamEncoder[SafroleInput] with
      def encode(a: SafroleInput): JamBytes =
        val builder = JamBytes.newBuilder
        // slot - 4 bytes
        builder ++= codec.encodeU32LE(spire.math.UInt(a.slot.toInt))
        // entropy - 32 bytes
        builder ++= a.entropy.bytes
        // extrinsic - compact length prefix + items
        builder ++= a.extrinsic.encode
        builder.result()

    given JamDecoder[SafroleInput] with
      def decode(bytes: JamBytes, offset: Int): (SafroleInput, Int) =
        val arr = bytes.toArray
        var pos = offset

        // slot - 4 bytes
        val slot = codec.decodeU32LE(arr, pos).toLong
        pos += 4

        // entropy - 32 bytes
        val entropy = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // extrinsic - compact length prefix + items
        val (extrinsic, extrinsicConsumed) = bytes.decodeAs[List[TicketEnvelope]](pos)
        pos += extrinsicConsumed

        (SafroleInput(slot, entropy, extrinsic), pos - offset)

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
    given JamEncoder[SafroleErrorCode] = CodecDerivation.enumEncoder(_.ordinal)
    given JamDecoder[SafroleErrorCode] = CodecDerivation.enumDecoder(SafroleErrorCode.fromOrdinal)

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
    given JamEncoder[SafroleOutputData] with
      def encode(a: SafroleOutputData): JamBytes =
        val builder = JamBytes.newBuilder
        // epochMark - optional
        builder ++= a.epochMark.encode
        // ticketsMark - optional list
        a.ticketsMark match
          case None => builder += 0.toByte
          case Some(tickets) =>
            builder += 1.toByte
            builder ++= encodeFixedList(tickets)
        builder.result()

    /** Create a decoder that knows the validator count and epoch length */
    def decoder(validatorCount: Int, epochLength: Int): JamDecoder[SafroleOutputData] =
      new JamDecoder[SafroleOutputData]:
        def decode(bytes: JamBytes, offset: Int): (SafroleOutputData, Int) =
          var pos = offset

          // epochMark - optional (discriminator byte + content)
          val epochMarkDiscriminator = bytes(pos).toInt & 0xff
          pos += 1
          val epochMark = if epochMarkDiscriminator == 0 then
            None
          else
            val epochMarkDecoder = EpochMark.decoder(validatorCount)
            val (mark, markConsumed) = epochMarkDecoder.decode(bytes, pos)
            pos += markConsumed
            Some(mark)

          // ticketsMark - optional (discriminator byte + fixed list)
          val ticketsMarkDiscriminator = bytes(pos).toInt & 0xff
          pos += 1
          val ticketsMark = if ticketsMarkDiscriminator == 0 then
            None
          else
            val (tickets, ticketsConsumed) = decodeFixedList[TicketMark](bytes, pos, epochLength)
            pos += ticketsConsumed
            Some(tickets)

          (SafroleOutputData(epochMark, ticketsMark), pos - offset)

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
     * Create a config-aware decoder for SafroleOutput.
     */
    def decoder(validatorCount: Int, epochLength: Int): JamDecoder[SafroleOutput] = new JamDecoder[SafroleOutput]:
      def decode(bytes: JamBytes, offset: Int): (SafroleOutput, Int) =
        var pos = offset
        val discriminator = bytes.toArray(pos).toInt & 0xff
        pos += 1
        if discriminator == 0 then
          val outputDataDecoder = SafroleOutputData.decoder(validatorCount, epochLength)
          val (data, dataBytes) = outputDataDecoder.decode(bytes, pos)
          (StfResult.success(data), 1 + dataBytes)
        else
          val (err, errBytes) = bytes.decodeAs[SafroleErrorCode](pos)
          (StfResult.error(err), 1 + errBytes)

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

    /** Create a config-aware decoder for SafroleCase */
    def decoder(validatorCount: Int, epochLength: Int): JamDecoder[SafroleCase] = new JamDecoder[SafroleCase]:
      def decode(bytes: JamBytes, offset: Int): (SafroleCase, Int) =
        var pos = offset

        // input
        val (input, inputBytes) = bytes.decodeAs[SafroleInput](pos)
        pos += inputBytes

        // preState
        val stateDecoder = SafroleState.decoder(validatorCount, epochLength)
        val (preState, preStateBytes) = stateDecoder.decode(bytes, pos)
        pos += preStateBytes

        // output - use config-aware decoder
        val outputDecoder = SafroleOutput.decoder(validatorCount, epochLength)
        val (output, outputBytes) = outputDecoder.decode(bytes, pos)
        pos += outputBytes

        // postState
        val (postState, postStateBytes) = stateDecoder.decode(bytes, pos)
        pos += postStateBytes

        (SafroleCase(input, preState, output, postState), pos - offset)

    /** Create a config-aware encoder for SafroleCase */
    given JamEncoder[SafroleCase] with
      def encode(a: SafroleCase): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.input.encode
        builder ++= a.preState.encode
        a.output.ok match
          case Some(data) =>
            builder += 0.toByte
            builder ++= data.encode
          case None =>
            builder += 1.toByte
            builder ++= a.output.err.get.encode
        builder ++= a.postState.encode
        builder.result()

    given Decoder[SafroleCase] = Decoder.instance { cursor =>
      for
        input <- cursor.get[SafroleInput]("input")
        preState <- cursor.get[SafroleState]("pre_state")
        output <- cursor.get[SafroleOutput]("output")
        postState <- cursor.get[SafroleState]("post_state")
      yield SafroleCase(input, preState, output, postState)
    }
