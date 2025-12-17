package io.forge.jam.protocol.traces

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.protocol.safrole.SafroleTypes.*
import _root_.scodec.{Codec, Attempt, DecodeResult}
import _root_.scodec.bits.BitVector
import _root_.scodec.codecs.*

/**
 * State key encoding/decoding for JAM protocol.
 *
 * State keys are 31 bytes and identify which state component a value belongs to.
 * The first byte identifies the component type, with remaining bytes used for
 * component-specific indexing.
 */
object StateKeys:
  // State component prefixes (byte 0)
  val CORE_AUTHORIZATION_POOL: Byte = 1 // phi_c - core authorizations
  val AUTHORIZATION_QUEUE: Byte = 2 // phi queue
  val RECENT_HISTORY: Byte = 3 // beta - recent blocks
  val SAFROLE_STATE: Byte = 4 // gamma - safrole gamma state
  val JUDGEMENTS: Byte = 5 // psi - judgements
  val ENTROPY_POOL: Byte = 6 // eta - entropy accumulator
  val VALIDATOR_QUEUE: Byte = 7 // iota - pending validators
  val CURRENT_VALIDATORS: Byte = 8 // kappa - current validators
  val PREVIOUS_VALIDATORS: Byte = 9 // lambda - previous validators
  val REPORTS: Byte = 10 // rho - pending reports
  val TIMESLOT: Byte = 11 // tau - current timeslot
  val PRIVILEGED_SERVICES: Byte = 12 // chi - privileged services
  val ACTIVITY_STATISTICS: Byte = 13 // activity stats
  val ACCUMULATION_QUEUE: Byte = 14 // accumulation queue
  val ACCUMULATION_HISTORY: Byte = 15 // accumulation history
  val LAST_ACCUMULATION_OUTPUTS: Byte = 16 // last accumulation outputs
  val SERVICE_STATISTICS: Byte = 17 // service statistics
  val SERVICE_ACCOUNT: Byte = 0xff.toByte // 255 - service account details (delta)

  /**
   * Creates a simple state key with just the component prefix.
   */
  def simpleKey(component: Byte): JamBytes =
    val data = new Array[Byte](31)
    data(0) = component
    JamBytes(data)

  /**
   * Known non-service key prefixes.
   */
  val KNOWN_PREFIXES: Set[Int] = Set(
    CORE_AUTHORIZATION_POOL.toInt & 0xff,
    AUTHORIZATION_QUEUE.toInt & 0xff,
    RECENT_HISTORY.toInt & 0xff,
    SAFROLE_STATE.toInt & 0xff,
    JUDGEMENTS.toInt & 0xff,
    ENTROPY_POOL.toInt & 0xff,
    VALIDATOR_QUEUE.toInt & 0xff,
    CURRENT_VALIDATORS.toInt & 0xff,
    PREVIOUS_VALIDATORS.toInt & 0xff,
    REPORTS.toInt & 0xff,
    TIMESLOT.toInt & 0xff,
    PRIVILEGED_SERVICES.toInt & 0xff,
    ACTIVITY_STATISTICS.toInt & 0xff,
    ACCUMULATION_QUEUE.toInt & 0xff,
    ACCUMULATION_HISTORY.toInt & 0xff,
    LAST_ACCUMULATION_OUTPUTS.toInt & 0xff,
    SERVICE_STATISTICS.toInt & 0xff,
    0xff
  )

  /**
   * Checks if a key byte indicates a service data key (interleaved encoding).
   */
  def isServiceDataKey(keyByte: Int): Boolean =
    !KNOWN_PREFIXES.contains(keyByte)

  /**
   * Extracts service index from a key with prefix 255.
   * Service bytes at positions 1, 3, 5, 7.
   */
  def extractServiceIndex255(key: JamBytes): Int =
    val bytes = key.toArray
    (bytes(1).toInt & 0xff) |
      ((bytes(3).toInt & 0xff) << 8) |
      ((bytes(5).toInt & 0xff) << 16) |
      ((bytes(7).toInt & 0xff) << 24)

  /**
   * Extracts service index from a service data key.
   * Service bytes at positions 0, 2, 4, 6.
   */
  def extractServiceIndexInterleaved(key: JamBytes): Int =
    val bytes = key.toArray
    (bytes(0).toInt & 0xff) |
      ((bytes(2).toInt & 0xff) << 8) |
      ((bytes(4).toInt & 0xff) << 16) |
      ((bytes(6).toInt & 0xff) << 24)

/**
 * Codec for encoding/decoding state between raw keyvals and typed state structures.
 */
object StateCodec:
  // Import scodec codec instances (exclude ticketMarkCodec to avoid ambiguity)
  import JamCodecs.{ticketMarkCodec as _, given, *}
  import io.forge.jam.core.types.epoch.ValidatorKey.given_Codec_ValidatorKey

  // Bandersnatch ring commitment size (144 bytes)
  private val RING_COMMITMENT_SIZE: Int = TinyConfig.BANDERSNATCH_RING_COMMITMENT_SIZE

  /**
   * Decodes SafroleState from keyvals.
   */
  def decodeSafroleState(keyvals: List[KeyValue], config: ChainConfig = ChainConfig.TINY): SafroleState =
    var tau: Long = 0
    var eta: List[Hash] = List.fill(4)(Hash.zero)
    var kappa: List[ValidatorKey] = List.empty
    var lambda: List[ValidatorKey] = List.empty
    var gammaK: List[ValidatorKey] = List.empty
    var iota: List[ValidatorKey] = List.empty
    var gammaA: List[TicketMark] = List.empty
    var gammaS: io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys =
      io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Keys(List.fill(config.epochLength)(BandersnatchPublicKey.zero))
    var gammaZ: JamBytes = JamBytes.zeros(RING_COMMITMENT_SIZE)
    var postOffenders: List[Ed25519PublicKey] = List.empty

    for kv <- keyvals do
      val keyByte = kv.key.toArray(0).toInt & 0xff
      val value = kv.value.toArray

      keyByte match
        case k if k == (StateKeys.TIMESLOT.toInt & 0xff) =>
          // Timeslot: 4 bytes little-endian
          val bits = BitVector(value)
          uint32L.decode(bits) match
            case Attempt.Successful(DecodeResult(v, _)) => tau = v & 0xFFFFFFFFL
            case Attempt.Failure(_) => () // Keep default value

        case k if k == (StateKeys.ENTROPY_POOL.toInt & 0xff) =>
          // Entropy pool: 4 x 32-byte hashes
          eta = decodeEntropyPool(value)

        case k if k == (StateKeys.CURRENT_VALIDATORS.toInt & 0xff) =>
          // Current validators (kappa)
          kappa = decodeValidatorList(value, config.validatorCount)

        case k if k == (StateKeys.PREVIOUS_VALIDATORS.toInt & 0xff) =>
          // Previous validators (lambda)
          lambda = decodeValidatorList(value, config.validatorCount)

        case k if k == (StateKeys.VALIDATOR_QUEUE.toInt & 0xff) =>
          // Pending validators (iota)
          iota = decodeValidatorList(value, config.validatorCount)

        case k if k == (StateKeys.SAFROLE_STATE.toInt & 0xff) =>
          // Safrole gamma state (gamma_k, gamma_z, gamma_s, gamma_a)
          val (decoded, _) = decodeSafroleGammaState(value, config.validatorCount, config.epochLength)
          gammaK = decoded.gammaK
          gammaA = decoded.gammaA
          gammaS = decoded.gammaS
          gammaZ = decoded.gammaZ

        case _ => () // Ignore other keys

    SafroleState(tau, eta, lambda, kappa, gammaK, iota, gammaA, gammaS, gammaZ, postOffenders)

  /**
   * Decodes entropy pool from raw bytes.
   */
  private def decodeEntropyPool(value: Array[Byte]): List[Hash] =
    (0 until 4).map { i =>
      val offset = i * 32
      if offset + 32 <= value.length then
        Hash(value.slice(offset, offset + 32))
      else
        Hash.zero
    }.toList

  /**
   * Decodes a list of validators from raw bytes.
   */
  private def decodeValidatorList(value: Array[Byte], expectedCount: Int): List[ValidatorKey] =
    val codec = JamCodecs.fixedSizeList(summon[Codec[ValidatorKey]], expectedCount)
    val bits = BitVector(value)
    codec.decode(bits) match
      case Attempt.Successful(DecodeResult(validators, _)) => validators
      case Attempt.Failure(_) =>
        // Create default validator key with all zero bytes
        val zeroValidator = ValidatorKey(
          BandersnatchPublicKey.zero,
          Ed25519PublicKey(Array.fill(32)(0.toByte)),
          BlsPublicKey(Array.fill(144)(0.toByte)),
          JamBytes.zeros(128)
        )
        List.fill(expectedCount)(zeroValidator)

  /**
   * Represents decoded safrole gamma state.
   */
  case class SafroleGammaDecoded(
    gammaK: List[ValidatorKey],
    gammaZ: JamBytes,
    gammaS: io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys,
    gammaA: List[TicketMark]
  )

  /**
   * Decodes safrole gamma state from raw bytes.
   */
  private def decodeSafroleGammaState(
    value: Array[Byte],
    validatorCount: Int,
    epochLength: Int
  ): (SafroleGammaDecoded, Int) =
    val bits = BitVector(value)
    var remainingBits = bits
    var totalConsumed = 0

    // gammaK - fixed list of ValidatorKey
    val gammaKCodec = JamCodecs.fixedSizeList(summon[Codec[ValidatorKey]], validatorCount)
    val gammaK = gammaKCodec.decode(remainingBits) match
      case Attempt.Successful(DecodeResult(validators, remainder)) =>
        val consumed = (remainingBits.size - remainder.size) / 8
        totalConsumed += consumed.toInt
        remainingBits = remainder
        validators
      case Attempt.Failure(_) =>
        val zeroValidator = ValidatorKey(
          BandersnatchPublicKey.zero,
          Ed25519PublicKey(Array.fill(32)(0.toByte)),
          BlsPublicKey(Array.fill(144)(0.toByte)),
          JamBytes.zeros(128)
        )
        List.fill(validatorCount)(zeroValidator)

    // gammaZ - 144 bytes ring commitment
    val gammaZCodec = fixedSizeBytes(RING_COMMITMENT_SIZE.toLong, bytes)
    val gammaZ = gammaZCodec.decode(remainingBits) match
      case Attempt.Successful(DecodeResult(bv, remainder)) =>
        totalConsumed += RING_COMMITMENT_SIZE
        remainingBits = remainder
        JamBytes.fromByteVector(bv)
      case Attempt.Failure(_) => JamBytes.zeros(RING_COMMITMENT_SIZE)

    // gammaS - TicketsOrKeys (discriminator + fixed list)
    val gammaSCodec = createTicketsOrKeysCodec(epochLength)
    val gammaS = gammaSCodec.decode(remainingBits) match
      case Attempt.Successful(DecodeResult(toks, remainder)) =>
        val consumed = (remainingBits.size - remainder.size) / 8
        totalConsumed += consumed.toInt
        remainingBits = remainder
        toks
      case Attempt.Failure(_) => io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Keys(List.fill(epochLength)(BandersnatchPublicKey.zero))

    // gammaA - compact length prefix + TicketMark items
    val ticketMarkCodec = io.forge.jam.core.types.tickets.TicketMark.given_Codec_TicketMark
    val gammaACodec = JamCodecs.compactPrefixedList(ticketMarkCodec)
    val gammaA = gammaACodec.decode(remainingBits) match
      case Attempt.Successful(DecodeResult(ticketsList, remainder2)) =>
        val consumed = (remainingBits.size - remainder2.size) / 8
        totalConsumed += consumed.toInt
        remainingBits = remainder2
        ticketsList
      case Attempt.Failure(_) => List.empty

    (SafroleGammaDecoded(gammaK, gammaZ, gammaS, gammaA), totalConsumed)

  /**
   * Creates a codec for TicketsOrKeys with the given epoch length.
   */
  private def createTicketsOrKeysCodec(epochLength: Int): Codec[io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys] =
    val ticketMarkCodec = io.forge.jam.core.types.tickets.TicketMark.given_Codec_TicketMark
    val ticketsListCodec: Codec[List[TicketMark]] = JamCodecs.fixedSizeList(ticketMarkCodec, epochLength)
    val keysListCodec: Codec[List[BandersnatchPublicKey]] = JamCodecs.fixedSizeList(summon[Codec[BandersnatchPublicKey]], epochLength)

    discriminated[io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys]
      .by(byte)
      .subcaseP(0) { case t: io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Tickets => t }(
        ticketsListCodec.xmap(io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Tickets.apply, _.tickets)
      )
      .subcaseP(1) { case k: io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Keys => k }(
        keysListCodec.xmap(io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Keys.apply, _.keys)
      )

  /**
   * Groups keyvals by state key prefix.
   */
  def groupKeyvals(keyvals: List[KeyValue]): Map[Int, List[KeyValue]] =
    keyvals.groupBy(kv => kv.key.toArray(0).toInt & 0xff)

  /**
   * Gets keyvals for a specific state component.
   */
  def getComponentKeyval(keyvals: List[KeyValue], component: Byte): Option[KeyValue] =
    keyvals.find(kv => (kv.key.toArray(0).toInt & 0xff) == (component.toInt & 0xff))
