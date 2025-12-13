package io.forge.jam.protocol.traces

import io.forge.jam.core.{ChainConfig, JamBytes, codec}
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.protocol.safrole.SafroleTypes.*

/**
 * Unified JAM state container holding all state components.
 */
final case class FullJamState(
  // tau - Current timeslot
  timeslot: Long,

  // eta - Entropy pool (4 x 32-byte hashes)
  entropyPool: List[Hash],

  // kappa - Current validators
  currentValidators: List[ValidatorKey],

  // lambda - Previous validators
  previousValidators: List[ValidatorKey],

  // iota - Validator queue (pending validators)
  validatorQueue: List[ValidatorKey],

  // gamma - Safrole state
  safroleGammaK: List[ValidatorKey], // gamma_k - next epoch validators
  safroleGammaZ: JamBytes, // gamma_z - ring root
  safroleGammaS: TicketsOrKeys, // gamma_s - sealing sequence
  safroleGammaA: List[TicketMark], // gamma_a - ticket accumulator

  // phi_c - Core authorization pools (simplified as raw bytes)
  authPools: List[List[Hash]] = List.empty,

  // phi - Authorization queues (simplified as raw bytes)
  authQueues: List[List[Hash]] = List.empty,

  // rho - Pending reports as raw keyvals (simplified)
  reportsKvs: List[KeyValue] = List.empty,

  // psi - Judgements as raw keyvals (simplified)
  judgementsKvs: List[KeyValue] = List.empty,

  // Post-dispute offenders
  postOffenders: List[Ed25519PublicKey] = List.empty,

  // Raw keyvals for all other state components (for pass-through)
  otherKeyvals: List[KeyValue] = List.empty
):

  /**
   * Convert to SafroleState for Safrole STF.
   */
  def toSafroleState(): SafroleState =
    SafroleState(
      tau = timeslot,
      eta = entropyPool,
      lambda = previousValidators,
      kappa = currentValidators,
      gammaK = safroleGammaK,
      iota = validatorQueue,
      gammaA = safroleGammaA,
      gammaS = safroleGammaS,
      gammaZ = safroleGammaZ,
      postOffenders = postOffenders
    )

  /**
   * Update from SafroleState after STF execution.
   */
  def withSafroleState(safrole: SafroleState): FullJamState =
    copy(
      timeslot = safrole.tau,
      entropyPool = safrole.eta,
      previousValidators = safrole.lambda,
      currentValidators = safrole.kappa,
      safroleGammaK = safrole.gammaK,
      validatorQueue = safrole.iota,
      safroleGammaA = safrole.gammaA,
      safroleGammaS = safrole.gammaS,
      safroleGammaZ = safrole.gammaZ,
      postOffenders = safrole.postOffenders
    )

  /**
   * Convert back to raw keyvals for state root computation.
   */
  def toKeyvals(config: ChainConfig = ChainConfig.TINY): List[KeyValue] =
    val builder = scala.collection.mutable.ListBuffer[KeyValue]()

    // Timeslot (0x0B)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.TIMESLOT),
      encodeTimeslot(timeslot)
    )

    // Entropy pool (0x06)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.ENTROPY_POOL),
      encodeEntropyPool(entropyPool)
    )

    // Current validators (0x08)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.CURRENT_VALIDATORS),
      encodeValidatorList(currentValidators)
    )

    // Previous validators (0x09)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.PREVIOUS_VALIDATORS),
      encodeValidatorList(previousValidators)
    )

    // Validator queue (0x07)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.VALIDATOR_QUEUE),
      encodeValidatorList(validatorQueue)
    )

    // Safrole gamma state (0x04)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.SAFROLE_STATE),
      encodeSafroleGammaState(safroleGammaK, safroleGammaZ, safroleGammaS, safroleGammaA, config)
    )

    // Add other keyvals as-is
    builder ++= otherKeyvals

    builder.toList

  // Encoding helpers

  private def encodeTimeslot(tau: Long): JamBytes =
    JamBytes(codec.encodeU32LE(spire.math.UInt(tau.toInt)))

  private def encodeEntropyPool(eta: List[Hash]): JamBytes =
    val builder = JamBytes.newBuilder
    for hash <- eta do
      builder ++= hash.bytes
    builder.result()

  private def encodeValidatorList(validators: List[ValidatorKey]): JamBytes =
    import io.forge.jam.core.codec.encode
    val builder = JamBytes.newBuilder
    for v <- validators do
      builder ++= v.encode
    builder.result()

  private def encodeSafroleGammaState(
    gammaK: List[ValidatorKey],
    gammaZ: JamBytes,
    gammaS: TicketsOrKeys,
    gammaA: List[TicketMark],
    config: ChainConfig
  ): JamBytes =
    import io.forge.jam.core.codec.encode
    val builder = JamBytes.newBuilder

    // gammaK - fixed list of validators
    for v <- gammaK do
      builder ++= v.encode

    // gammaZ - 144 bytes
    builder ++= gammaZ

    // gammaS - TicketsOrKeys
    builder ++= gammaS.encode

    // gammaA - compact length prefix + TicketMark items
    builder ++= gammaA.encode

    builder.result()

object FullJamState:

  // Bandersnatch ring commitment size (144 bytes)
  private val RING_COMMITMENT_SIZE: Int = TinyConfig.BANDERSNATCH_RING_COMMITMENT_SIZE

  /**
   * Create from raw state keyvals.
   */
  def fromKeyvals(keyvals: List[KeyValue], config: ChainConfig = ChainConfig.TINY): FullJamState =
    val safroleState = StateCodec.decodeSafroleState(keyvals, config)

    // Separate Safrole-related keyvals from others
    val safroleRelatedPrefixes = Set(
      StateKeys.TIMESLOT.toInt & 0xff,
      StateKeys.ENTROPY_POOL.toInt & 0xff,
      StateKeys.CURRENT_VALIDATORS.toInt & 0xff,
      StateKeys.PREVIOUS_VALIDATORS.toInt & 0xff,
      StateKeys.VALIDATOR_QUEUE.toInt & 0xff,
      StateKeys.SAFROLE_STATE.toInt & 0xff
    )

    val otherKvs = keyvals.filterNot(kv => safroleRelatedPrefixes.contains(kv.key.toArray(0).toInt & 0xff))

    FullJamState(
      timeslot = safroleState.tau,
      entropyPool = safroleState.eta,
      currentValidators = safroleState.kappa,
      previousValidators = safroleState.lambda,
      validatorQueue = safroleState.iota,
      safroleGammaK = safroleState.gammaK,
      safroleGammaZ = safroleState.gammaZ,
      safroleGammaS = safroleState.gammaS,
      safroleGammaA = safroleState.gammaA,
      postOffenders = safroleState.postOffenders,
      otherKeyvals = otherKvs
    )

  /**
   * Create an empty/default FullJamState.
   */
  def empty(config: ChainConfig = ChainConfig.TINY): FullJamState =
    val emptyValidatorKey = ValidatorKey(
      BandersnatchPublicKey.zero,
      Ed25519PublicKey(new Array[Byte](Ed25519PublicKey.Size)),
      BlsPublicKey(new Array[Byte](BlsPublicKey.Size)),
      JamBytes.zeros(ValidatorKey.MetadataSize)
    )
    val emptyValidators = List.fill(config.validatorCount)(emptyValidatorKey)
    val emptyEntropy = List.fill(4)(Hash.zero)

    FullJamState(
      timeslot = 0,
      entropyPool = emptyEntropy,
      currentValidators = emptyValidators,
      previousValidators = emptyValidators,
      validatorQueue = emptyValidators,
      safroleGammaK = emptyValidators,
      safroleGammaZ = JamBytes.zeros(RING_COMMITMENT_SIZE),
      safroleGammaS = TicketsOrKeys.Keys(List.fill(config.epochLength)(BandersnatchPublicKey.zero)),
      safroleGammaA = List.empty
    )
