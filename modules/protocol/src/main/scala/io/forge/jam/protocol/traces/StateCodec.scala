package io.forge.jam.protocol.traces

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{
  Hash,
  BandersnatchPublicKey,
  Ed25519PublicKey,
  BlsPublicKey
}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.codec.FullJamStateCodecs
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.dispute.DisputeTypes.Psi
import io.forge.jam.core.codec.Codec

/** State key encoding/decoding for JAM protocol.
  *
  * State keys are 31 bytes and identify which state component a value belongs
  * to. The first byte identifies the component type, with remaining bytes used
  * for component-specific indexing.
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
  val SERVICE_ACCOUNT: Byte =
    0xff.toByte // 255 - service account details (delta)

  /** Creates a simple state key with just the component prefix.
    */
  def simpleKey(component: Byte): JamBytes =
    val data = new Array[Byte](31)
    data(0) = component
    JamBytes(data)

  /** Known non-service key prefixes.
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

  /** Checks if a key byte indicates a service data key (interleaved encoding).
    *
    * Note: This is a simple check based only on the first byte. For accurate
    * classification, use `isServiceDataKeyFull`.
    */
  def isServiceDataKey(keyByte: Int): Boolean =
    !KNOWN_PREFIXES.contains(keyByte)

  /** Checks if a full 31-byte key is a service data key vs a protocol state
    * key.
    *
    * Protocol state keys have format: [prefix, 0, 0, ..., 0] (only first byte
    * non-zero) Service data keys have interleaved format: [s0, h0, s1, h1, s2,
    * h2, s3, h3, h4..h26]
    *
    * Returns true if this is a service data key (has non-zero bytes after the
    * first).
    */
  def isServiceDataKeyFull(key: JamBytes): Boolean =
    if key.length != 31 then false
    else
      val firstByte = key(0).toInt & 0xff
      if firstByte == 0xff then key(2) != 0
      else if KNOWN_PREFIXES.contains(firstByte) then
        var i = 1
        while i < 31 do
          if key(i) != 0 then return true
          i += 1
        false
      else true

/** Codec for encoding/decoding state between raw keyvals and typed state
  * structures.
  */
object StateCodec:

  // Bandersnatch ring commitment size (144 bytes)
  private val RING_COMMITMENT_SIZE: Int =
    io.forge.jam.protocol.safrole.SafroleTypes.BandersnatchRingCommitmentSize

  /** Checks if a key is a simple protocol state key (prefix + 30 zero bytes).
    */
  private def isSimpleKey(key: JamBytes, prefix: Byte): Boolean =
    if key.length != 31 then false
    else if key(0) != prefix then false
    else
      var i = 1
      while i < 31 do
        if key(i) != 0 then return false
        i += 1
      true

  private def decodeOffenders(value: Array[Byte]): List[Ed25519PublicKey] =
    summon[Codec[Psi]].decode(value) match
      case Right(psi) => psi.offenders
      case Left(_)    => List.empty

  /** Decodes SafroleState from keyvals
    */
  def decodeSafroleState(
      keyvals: List[KeyValue],
      config: ChainConfig
  ): SafroleState =
    var tau: Long = 0
    var eta: List[Hash] = List.fill(4)(Hash.zero)
    var kappa: IndexedSeq[ValidatorKey] = IndexedSeq.empty
    var lambda: IndexedSeq[ValidatorKey] = IndexedSeq.empty
    var gammaK: IndexedSeq[ValidatorKey] = IndexedSeq.empty
    var iota: IndexedSeq[ValidatorKey] = IndexedSeq.empty
    var gammaA: List[TicketMark] = List.empty
    var gammaS: io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys =
      io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Keys(
        List.fill(config.epochLength)(BandersnatchPublicKey.zero)
      )
    var gammaZ: JamBytes = JamBytes.zeros(RING_COMMITMENT_SIZE)
    var postOffenders: List[Ed25519PublicKey] = List.empty

    for kv <- keyvals do
      val key = kv.key
      val value = kv.value.toArray

      // Only process simple protocol state keys (prefix + 30 zero bytes)
      // This avoids confusing service data keys with protocol keys
      if isSimpleKey(key, StateKeys.TIMESLOT) then
        // Timeslot: 4 bytes little-endian
        FullJamStateCodecs.timeslotCodec.decode(value) match
          case Right(v) => tau = v
          case Left(_)  => () // Keep default value
      else if isSimpleKey(key, StateKeys.ENTROPY_POOL) then
        // Entropy pool: 4 x 32-byte hashes
        eta = decodeEntropyPool(value)
      else if isSimpleKey(key, StateKeys.CURRENT_VALIDATORS) then
        // Current validators (kappa)
        kappa = decodeValidatorList(value, config.validatorCount)
      else if isSimpleKey(key, StateKeys.PREVIOUS_VALIDATORS) then
        // Previous validators (lambda)
        lambda = decodeValidatorList(value, config.validatorCount)
      else if isSimpleKey(key, StateKeys.VALIDATOR_QUEUE) then
        // Pending validators (iota)
        iota = decodeValidatorList(value, config.validatorCount)
      else if isSimpleKey(key, StateKeys.SAFROLE_STATE) then
        // Safrole gamma state (gamma_k, gamma_z, gamma_s, gamma_a)
        val (decoded, _) = decodeSafroleGammaState(
          value,
          config.validatorCount,
          config.epochLength
        )
        gammaK = decoded.gammaK
        gammaA = decoded.gammaA
        gammaS = decoded.gammaS
        gammaZ = decoded.gammaZ
      else if isSimpleKey(key, StateKeys.JUDGEMENTS) then
        postOffenders = decodeOffenders(value)

    SafroleState(
      tau,
      eta,
      lambda,
      kappa,
      gammaK,
      iota,
      gammaA,
      gammaS,
      gammaZ,
      postOffenders
    )

  /** Decodes entropy pool from raw bytes.
    */
  private def decodeEntropyPool(value: Array[Byte]): List[Hash] =
    (0 until 4).map { i =>
      val offset = i * 32
      if offset + 32 <= value.length then Hash(value.slice(offset, offset + 32))
      else Hash.zero
    }.toList

  /** Decodes a list of validators from raw bytes.
    */
  private def decodeValidatorList(
      value: Array[Byte],
      expectedCount: Int
  ): IndexedSeq[ValidatorKey] =
    val codec =
      FullJamStateCodecs.validatorListCodec
    codec.decode(value) match
      case Right(validators) => validators
      case Left(_)           =>
        // Create default validator key with all zero bytes
        val zeroValidator = ValidatorKey(
          BandersnatchPublicKey.zero,
          Ed25519PublicKey(Array.fill(32)(0.toByte)),
          BlsPublicKey(Array.fill(144)(0.toByte)),
          JamBytes.zeros(128)
        )
        IndexedSeq.fill(expectedCount)(zeroValidator)

  /** Represents decoded safrole gamma state.
    */
  case class SafroleGammaDecoded(
      gammaK: IndexedSeq[ValidatorKey],
      gammaZ: JamBytes,
      gammaS: io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys,
      gammaA: List[TicketMark]
  )

  /** Decodes safrole gamma state from raw bytes.
    */
  private def decodeSafroleGammaState(
      value: Array[Byte],
      validatorCount: Int,
      epochLength: Int
  ): (SafroleGammaDecoded, Int) =
    var offset = 0

    // gammaK - fixed list of ValidatorKey
    val gammaKCodec =
      FullJamStateCodecs.validatorListCodec
    val gammaK = gammaKCodec.decodeAt(value, offset) match
      case Right((validators, used)) =>
        offset += used
        validators
      case Left(_) =>
        val zeroValidator = ValidatorKey(
          BandersnatchPublicKey.zero,
          Ed25519PublicKey(Array.fill(32)(0.toByte)),
          BlsPublicKey(Array.fill(144)(0.toByte)),
          JamBytes.zeros(128)
        )
        IndexedSeq.fill(validatorCount)(zeroValidator)

    // gammaZ - 144 bytes ring commitment
    val gammaZ = io.forge.jam.core.codec.codecs.bytes(RING_COMMITMENT_SIZE).decodeAt(value, offset) match
      case Right((z, used)) =>
        offset += used
        z
      case Left(_) => JamBytes.zeros(RING_COMMITMENT_SIZE)

    // gammaS - TicketsOrKeys (discriminator + fixed list)
    val gammaSCodec = createTicketsOrKeysCodec(epochLength)
    val gammaS = gammaSCodec.decodeAt(value, offset) match
      case Right((toks, used)) =>
        offset += used
        toks
      case Left(_) =>
        io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Keys(
          List.fill(epochLength)(BandersnatchPublicKey.zero)
        )

    // gammaA - compact length prefix + TicketMark items
    val ticketMarkCodec =
      io.forge.jam.core.types.tickets.TicketMark.given_Codec_TicketMark
    val gammaACodec = JamCodecs.compactPrefixedList(ticketMarkCodec)
    val gammaA = gammaACodec.decodeAt(value, offset) match
      case Right((ticketsList, used)) =>
        offset += used
        ticketsList
      case Left(_) => List.empty

    (SafroleGammaDecoded(gammaK, gammaZ, gammaS, gammaA), offset)

  /** Creates a codec for TicketsOrKeys with the given epoch length.
    */
  private def createTicketsOrKeysCodec(
      epochLength: Int
  ): Codec[io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys] =
    val ticketMarkCodec =
      io.forge.jam.core.types.tickets.TicketMark.given_Codec_TicketMark
    val ticketsListCodec: Codec[List[TicketMark]] =
      JamCodecs.fixedSizeList(ticketMarkCodec, epochLength)
    val keysListCodec: Codec[List[BandersnatchPublicKey]] =
      JamCodecs.fixedSizeList(JamCodecs.bandersnatchPublicKeyCodec, epochLength)

    Codec.of(
      (w, v) =>
        v match
          case io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Tickets(t) =>
            w.u8(0)
            ticketsListCodec.write(w, t)
          case io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Keys(k) =>
            w.u8(1)
            keysListCodec.write(w, k),
      r =>
        r.u8() match
          case 0 => io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Tickets(ticketsListCodec.read(r))
          case 1 => io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Keys(keysListCodec.read(r))
          case other => r.fail(s"invalid TicketsOrKeys discriminator $other")
    )
