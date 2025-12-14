package io.forge.jam.protocol.traces

import io.forge.jam.core.{ChainConfig, JamBytes, codec}
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.types.workpackage.AvailabilityAssignment
import io.forge.jam.core.types.history.HistoricalBetaContainer
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.dispute.DisputeTypes.Psi
import io.forge.jam.protocol.accumulation.{
  AccumulationState,
  AccumulationServiceItem,
  AccumulationServiceData,
  AccumulationReadyRecord,
  Privileges,
  AlwaysAccItem
}
import io.forge.jam.protocol.history.HistoryTypes.HistoricalState
import io.forge.jam.protocol.authorization.AuthorizationTypes.AuthState
import io.forge.jam.protocol.preimage.PreimageTypes.{PreimageState, PreimageAccount, AccountInfo}
import io.forge.jam.protocol.statistics.StatisticsTypes.{StatState, StatCount}
import io.forge.jam.protocol.report.ReportTypes.{CoreStatisticsRecord, ServiceStatisticsEntry}

import scala.collection.mutable

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

  // phi_c - Core authorization pools (per core, variable-size inner lists)
  authPools: List[List[Hash]] = List.empty,

  // phi - Authorization queues (per core, fixed-size 80 inner lists)
  authQueues: List[List[Hash]] = List.empty,

  // beta - Recent block history
  recentHistory: HistoricalBetaContainer = HistoricalBetaContainer(),

  // rho - Pending work reports (availability assignments per core)
  reports: List[Option[AvailabilityAssignment]] = List.empty,

  // psi - Judgements (disputes resolution state)
  judgements: Psi = Psi.empty,

  // chi - Privileged services configuration
  privilegedServices: Privileges = Privileges(0, List.empty, 0, 0, List.empty),

  // Ready queue for accumulation (epoch-length ring buffer)
  accumulationQueue: List[List[AccumulationReadyRecord]] = List.empty,

  // Accumulated hashes history (epoch-length ring buffer)
  accumulationHistory: List[List[JamBytes]] = List.empty,

  // delta - Service accounts with full data
  serviceAccounts: List[AccumulationServiceItem] = List.empty,

  // pi - Service statistics (per block, fresh each block)
  serviceStatistics: List[ServiceStatisticsEntry] = List.empty,

  // alpha_c - Core statistics (per core)
  coreStatistics: List[CoreStatisticsRecord] = List.empty,

  // alpha_v^curr - Current epoch validator statistics
  activityStatsCurrent: List[StatCount] = List.empty,

  // alpha_v^last - Last epoch validator statistics
  activityStatsLast: List[StatCount] = List.empty,

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
   * Convert to AccumulationState for Accumulation STF.
   */
  def toAccumulationState(config: ChainConfig): AccumulationState =
    // Initialize ready queue and accumulated history if empty
    val readyQueue = if accumulationQueue.isEmpty then
      List.fill(config.epochLength)(List.empty[AccumulationReadyRecord])
    else
      accumulationQueue

    val accumulated = if accumulationHistory.isEmpty then
      List.fill(config.epochLength)(List.empty[JamBytes])
    else
      accumulationHistory

    AccumulationState(
      slot = timeslot,
      entropy = if entropyPool.nonEmpty then JamBytes(entropyPool.head.bytes) else JamBytes.zeros(32),
      readyQueue = readyQueue,
      accumulated = accumulated,
      privileges = privilegedServices,
      statistics = serviceStatistics.map(s =>
        io.forge.jam.protocol.accumulation.ServiceStatisticsEntry(
          s.id,
          io.forge.jam.protocol.accumulation.ServiceActivityRecord(
            s.record.refinementCount.toInt,
            s.record.refinementGasUsed,
            s.record.refinementCount,
            s.record.refinementGasUsed,
            s.record.imports,
            s.record.extrinsicCount,
            s.record.extrinsicSize,
            s.record.exports,
            0,
            0
          )
        )
      ),
      accounts = serviceAccounts
    )

  /**
   * Convert to HistoricalState for History STF.
   */
  def toHistoryState(): HistoricalState =
    HistoricalState(beta = recentHistory)

  /**
   * Convert to AuthState for Authorization STF.
   */
  def toAuthState(): AuthState =
    AuthState(
      authPools = authPools,
      authQueues = authQueues
    )

  /**
   * Convert to PreimageState for Preimage STF.
   */
  def toPreimageState(): PreimageState =
    PreimageState(
      accounts = serviceAccounts.map { item =>
        PreimageAccount(
          id = item.id,
          data = AccountInfo(
            preimages = item.data.preimages.map(p =>
              io.forge.jam.core.types.preimage.PreimageHash(p.hash, p.blob)
            ),
            lookupMeta = item.data.preimagesStatus.map { status =>
              io.forge.jam.protocol.preimage.PreimageTypes.PreimageHistory(
                key = io.forge.jam.protocol.preimage.PreimageTypes.PreimageHistoryKey(status.hash, 0),
                value = status.status
              )
            }
          )
        )
      }
    )

  /**
   * Convert to StatState for Statistics STF.
   */
  def toStatState(): StatState =
    StatState(
      valsCurrStats = activityStatsCurrent,
      valsLastStats = activityStatsLast,
      slot = timeslot,
      currValidators = currentValidators
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

    // Initialize empty reports list (one per core)
    val emptyReports = List.fill(config.coresCount)(Option.empty[AvailabilityAssignment])

    // Initialize empty auth pools and queues (one per core)
    val emptyAuthPools = List.fill(config.coresCount)(List.empty[Hash])
    val emptyAuthQueues = List.fill(config.coresCount)(List.fill(80)(Hash.zero))

    // Initialize empty validator statistics
    val emptyStatCount = List.fill(config.validatorCount)(StatCount.zero)

    // Initialize empty core statistics
    val emptyCoreStats = List.fill(config.coresCount)(CoreStatisticsRecord())

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
      judgements = Psi.empty, // Judgements initialized empty, updated by Disputes STF
      reports = emptyReports,
      authPools = emptyAuthPools,
      authQueues = emptyAuthQueues,
      activityStatsCurrent = emptyStatCount,
      activityStatsLast = emptyStatCount,
      coreStatistics = emptyCoreStats,
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
    val emptyReports = List.fill(config.coresCount)(Option.empty[AvailabilityAssignment])
    val emptyAuthPools = List.fill(config.coresCount)(List.empty[Hash])
    val emptyAuthQueues = List.fill(config.coresCount)(List.fill(80)(Hash.zero))
    val emptyStatCount = List.fill(config.validatorCount)(StatCount.zero)
    val emptyCoreStats = List.fill(config.coresCount)(CoreStatisticsRecord())

    FullJamState(
      timeslot = 0,
      entropyPool = emptyEntropy,
      currentValidators = emptyValidators,
      previousValidators = emptyValidators,
      validatorQueue = emptyValidators,
      safroleGammaK = emptyValidators,
      safroleGammaZ = JamBytes.zeros(RING_COMMITMENT_SIZE),
      safroleGammaS = TicketsOrKeys.Keys(List.fill(config.epochLength)(BandersnatchPublicKey.zero)),
      safroleGammaA = List.empty,
      reports = emptyReports,
      authPools = emptyAuthPools,
      authQueues = emptyAuthQueues,
      activityStatsCurrent = emptyStatCount,
      activityStatsLast = emptyStatCount,
      coreStatistics = emptyCoreStats
    )
