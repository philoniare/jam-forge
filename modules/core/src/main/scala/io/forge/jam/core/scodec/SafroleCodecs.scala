package io.forge.jam.core.scodec

import scodec.*
import scodec.bits.*
import scodec.codecs.*
import spire.math.{UByte, UInt}
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey}

/**
 * scodec codecs for Safrole State Transition Function types.
 * Provides config-parameterized codecs for TicketsOrKeys, SafroleState, SafroleInput, etc.
 */
object SafroleCodecs:

  val BandersnatchRingCommitmentSize: Int = 144
  val ValidatorKeySize: Int = 336
  val MetadataSize: Int = 128
  val RingVrfSignatureSize: Int = 784
  val TicketEnvelopeSize: Int = 1 + RingVrfSignatureSize
  val TicketMarkSize: Int = Hash.Size + 1
  val EpochValidatorKeySize: Int = 64

  final case class ValidatorKeyData(
    bandersnatch: BandersnatchPublicKey,
    ed25519: Ed25519PublicKey,
    bls: BlsPublicKey,
    metadata: JamBytes
  )

  final case class TicketMarkData(
    id: ByteVector,
    attempt: UByte
  )

  final case class TicketEnvelopeData(
    attempt: UByte,
    signature: ByteVector
  )

  final case class EpochValidatorKeyData(
    bandersnatch: BandersnatchPublicKey,
    ed25519: Ed25519PublicKey
  )

  final case class EpochMarkData(
    entropy: Hash,
    ticketsEntropy: Hash,
    validators: List[EpochValidatorKeyData]
  )

  sealed trait TicketsOrKeys

  object TicketsOrKeys:
    final case class Tickets(tickets: List[TicketMarkData]) extends TicketsOrKeys
    final case class Keys(keys: List[BandersnatchPublicKey]) extends TicketsOrKeys

  final case class SafroleStateData(
    tau: Long,
    eta: List[Hash],
    lambda: List[ValidatorKeyData],
    kappa: List[ValidatorKeyData],
    gammaK: List[ValidatorKeyData],
    iota: List[ValidatorKeyData],
    gammaA: List[TicketMarkData],
    gammaS: TicketsOrKeys,
    gammaZ: ByteVector,
    postOffenders: List[Ed25519PublicKey]
  )

  final case class SafroleInputData(
    slot: Long,
    entropy: Hash,
    extrinsic: List[TicketEnvelopeData]
  )

  final case class SafroleOutputDataData(
    epochMark: Option[EpochMarkData],
    ticketsMark: Option[List[TicketMarkData]]
  )

  enum SafroleErrorCode:
    case BadSlot
    case UnexpectedTicket
    case BadTicketOrder
    case BadTicketProof
    case BadTicketAttempt
    case Reserved
    case DuplicateTicket

  val validatorKeyCodec: Codec[ValidatorKeyData] =
    (JamCodecs.bandersnatchPublicKeyCodec ::
     JamCodecs.ed25519PublicKeyCodec ::
     JamCodecs.blsPublicKeyCodec ::
     fixedSizeBytes(MetadataSize.toLong, bytes)).xmap(
      { case (bandersnatch, ed25519, bls, metadataBytes) =>
        ValidatorKeyData(bandersnatch, ed25519, bls, JamBytes(metadataBytes.toArray))
      },
      vk => (vk.bandersnatch, vk.ed25519, vk.bls, ByteVector(vk.metadata.toArray))
    )

  val ticketMarkCodec: Codec[TicketMarkData] =
    (fixedSizeBytes(Hash.Size.toLong, bytes) :: JamCodecs.ubyteCodec).xmap(
      { case (idBytes, attempt) => TicketMarkData(idBytes, attempt) },
      tm => (tm.id, tm.attempt)
    )

  val ticketEnvelopeCodec: Codec[TicketEnvelopeData] =
    (JamCodecs.ubyteCodec :: fixedSizeBytes(RingVrfSignatureSize.toLong, bytes)).xmap(
      { case (attempt, signature) => TicketEnvelopeData(attempt, signature) },
      te => (te.attempt, te.signature)
    )

  val epochValidatorKeyCodec: Codec[EpochValidatorKeyData] =
    (JamCodecs.bandersnatchPublicKeyCodec :: JamCodecs.ed25519PublicKeyCodec).xmap(
      { case (bandersnatch, ed25519) => EpochValidatorKeyData(bandersnatch, ed25519) },
      evk => (evk.bandersnatch, evk.ed25519)
    )

  def epochMarkCodec(validatorCount: Int): Codec[EpochMarkData] =
    (JamCodecs.hashCodec ::
     JamCodecs.hashCodec ::
     JamCodecs.fixedSizeList(epochValidatorKeyCodec, validatorCount)).xmap(
      { case (entropy, ticketsEntropy, validators) =>
        EpochMarkData(entropy, ticketsEntropy, validators)
      },
      em => (em.entropy, em.ticketsEntropy, em.validators)
    )

  def ticketsOrKeysCodec(epochLength: Int): Codec[TicketsOrKeys] =
    val ticketsListCodec: Codec[List[TicketMarkData]] = JamCodecs.fixedSizeList(ticketMarkCodec, epochLength)
    val keysListCodec: Codec[List[BandersnatchPublicKey]] =
      JamCodecs.fixedSizeList(JamCodecs.bandersnatchPublicKeyCodec, epochLength)

    discriminated[TicketsOrKeys]
      .by(byte)
      .subcaseP(0) { case t: TicketsOrKeys.Tickets => t }(
        ticketsListCodec.xmap(TicketsOrKeys.Tickets.apply, _.tickets)
      )
      .subcaseP(1) { case k: TicketsOrKeys.Keys => k }(
        keysListCodec.xmap(TicketsOrKeys.Keys.apply, _.keys)
      )

  def safroleStateCodec(validatorCount: Int, epochLength: Int): Codec[SafroleStateData] =
    // tau - 4 bytes little-endian
    val tauCodec: Codec[Long] = JamCodecs.uintCodec.xmap(_.toLong, l => UInt(l.toInt))

    // eta - exactly 4 hashes
    val etaCodec: Codec[List[Hash]] = JamCodecs.fixedSizeList(JamCodecs.hashCodec, 4)

    // validator lists - fixed size
    val validatorListCodec: Codec[List[ValidatorKeyData]] = JamCodecs.fixedSizeList(validatorKeyCodec, validatorCount)

    // gammaA - compact length prefix + ticket marks
    val gammaACodec: Codec[List[TicketMarkData]] = JamCodecs.compactPrefixedList(ticketMarkCodec)

    // gammaS - discriminated tickets or keys
    val gammaSCodec: Codec[TicketsOrKeys] = ticketsOrKeysCodec(epochLength)

    // gammaZ - fixed 144 bytes
    val gammaZCodec: Codec[ByteVector] = fixedSizeBytes(BandersnatchRingCommitmentSize.toLong, bytes)

    // postOffenders - compact length prefix + Ed25519 keys
    val postOffendersCodec: Codec[List[Ed25519PublicKey]] =
      JamCodecs.compactPrefixedList(JamCodecs.ed25519PublicKeyCodec)

    // Compose all fields
    (tauCodec ::
     etaCodec ::
     validatorListCodec :: // lambda
     validatorListCodec :: // kappa
     validatorListCodec :: // gammaK
     validatorListCodec :: // iota
     gammaACodec ::
     gammaSCodec ::
     gammaZCodec ::
     postOffendersCodec).xmap(
      { case (tau, eta, lambda, kappa, gammaK, iota, gammaA, gammaS, gammaZ, postOffenders) =>
        SafroleStateData(tau, eta, lambda, kappa, gammaK, iota, gammaA, gammaS, gammaZ, postOffenders)
      },
      s => (s.tau, s.eta, s.lambda, s.kappa, s.gammaK, s.iota, s.gammaA, s.gammaS, s.gammaZ, s.postOffenders)
    )

  val safroleInputCodec: Codec[SafroleInputData] =
    val slotCodec: Codec[Long] = JamCodecs.uintCodec.xmap(_.toLong, l => UInt(l.toInt))
    val extrinsicCodec: Codec[List[TicketEnvelopeData]] = JamCodecs.compactPrefixedList(ticketEnvelopeCodec)

    (slotCodec :: JamCodecs.hashCodec :: extrinsicCodec).xmap(
      { case (slot, entropy, extrinsic) => SafroleInputData(slot, entropy, extrinsic) },
      i => (i.slot, i.entropy, i.extrinsic)
    )

  def safroleOutputDataCodec(validatorCount: Int, epochLength: Int): Codec[SafroleOutputDataData] =
    // epochMark with config-dependent decoder
    val epochMarkOptCodec: Codec[Option[EpochMarkData]] =
      JamCodecs.optionCodec(epochMarkCodec(validatorCount))

    // ticketsMark - optional fixed-size list
    val ticketsMarkCodec: Codec[List[TicketMarkData]] =
      JamCodecs.fixedSizeList(ticketMarkCodec, epochLength)
    val ticketsMarkOptCodec: Codec[Option[List[TicketMarkData]]] =
      JamCodecs.optionCodec(ticketsMarkCodec)

    (epochMarkOptCodec :: ticketsMarkOptCodec).xmap(
      { case (epochMark, ticketsMark) => SafroleOutputDataData(epochMark, ticketsMark) },
      o => (o.epochMark, o.ticketsMark)
    )

  val safroleErrorCodeCodec: Codec[SafroleErrorCode] =
    JamCodecs.ubyteCodec.xmap(
      b => SafroleErrorCode.fromOrdinal(b.toInt),
      e => UByte(e.ordinal)
    )

  def safroleOutputCodec(validatorCount: Int, epochLength: Int): Codec[Either[SafroleErrorCode, SafroleOutputDataData]] =
    JamCodecs.stfResultCodec(
      using safroleOutputDataCodec(validatorCount, epochLength),
      safroleErrorCodeCodec
    )

  final case class SafroleCaseData(
    input: SafroleInputData,
    preState: SafroleStateData,
    output: Either[SafroleErrorCode, SafroleOutputDataData],
    postState: SafroleStateData
  )

  def safroleCaseCodec(validatorCount: Int, epochLength: Int): Codec[SafroleCaseData] =
    val stateCodec = safroleStateCodec(validatorCount, epochLength)
    val outputCodec = safroleOutputCodec(validatorCount, epochLength)

    (safroleInputCodec :: stateCodec :: outputCodec :: stateCodec).xmap(
      { case (input, preState, output, postState) =>
        SafroleCaseData(input, preState, output, postState)
      },
      c => (c.input, c.preState, c.output, c.postState)
    )
