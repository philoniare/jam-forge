package io.forge.jam.protocol.state

import monocle.Lens
import monocle.Focus

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{Hash, Ed25519PublicKey}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.types.workpackage.AvailabilityAssignment
import io.forge.jam.core.types.history.HistoricalBetaContainer
import io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys
import io.forge.jam.protocol.dispute.DisputeTypes.Psi
import io.forge.jam.protocol.accumulation.{AccumulationServiceItem, AccumulationReadyRecord, Privileges}
import io.forge.jam.protocol.report.ReportTypes.{CoreStatisticsRecord, ServiceStatisticsEntry}
import io.forge.jam.protocol.statistics.StatisticsTypes.StatCount
import io.forge.jam.protocol.traces.{FullJamState, KeyValue}

/**
 * Nested state component for validator sets.
 * Groups kappa, lambda, iota, gammaK which always travel together across STFs.
 *
 * @param current Current epoch validators (kappa)
 * @param previous Previous epoch validators (lambda)
 * @param queue Validator queue for next epoch (iota)
 * @param nextEpoch Planned validators for next epoch (gammaK)
 */
final case class ValidatorState(
  current: List[ValidatorKey],
  previous: List[ValidatorKey],
  queue: List[ValidatorKey],
  nextEpoch: List[ValidatorKey]
)

object ValidatorState:
  def empty: ValidatorState =
    ValidatorState(
      current = List.empty,
      previous = List.empty,
      queue = List.empty,
      nextEpoch = List.empty
    )

/**
 * Nested state component for entropy pool.
 * The entropy pool is a 4-element list of hashes (eta).
 *
 * @param pool 4-element entropy pool (eta[0..3])
 */
final case class EntropyBuffer(
  pool: List[Hash]
):
  require(pool.size == 4, s"Entropy buffer must have 4 elements, got ${pool.size}")

  /** Extract first entropy value as JamBytes (used by Accumulation STF) */
  def firstAsBytes: JamBytes = JamBytes(pool.head.bytes)

object EntropyBuffer:
  def empty: EntropyBuffer = EntropyBuffer(List.fill(4)(Hash.zero))

/**
 * Nested state component for Safrole gamma state.
 * Groups gammaZ, gammaS, gammaA which are Safrole-specific.
 *
 * @param z Ring commitment (gammaZ) - 144 bytes
 * @param s Sealing sequence - either tickets or fallback keys (gammaS)
 * @param a Ticket accumulator (gammaA)
 */
final case class SafroleGamma(
  z: JamBytes,
  s: TicketsOrKeys,
  a: List[TicketMark]
)

object SafroleGamma:
  val RingCommitmentSize: Int = 144

/**
 * Nested state component for per-core state.
 * Groups availability assignments and core statistics.
 *
 * @param reports Pending availability assignments per core (rho)
 * @param statistics Per-core statistics
 */
final case class CoreState(
  reports: List[Option[AvailabilityAssignment]],
  statistics: List[CoreStatisticsRecord]
)

object CoreState:
  def empty(coreCount: Int): CoreState = CoreState(
    reports = List.fill(coreCount)(None),
    statistics = List.fill(coreCount)(CoreStatisticsRecord.zero)
  )

/**
 * Nested state component for accumulation-related state.
 * Groups readyQueue, accumulated, privileges, and serviceAccounts.
 *
 * @param readyQueue Ready queue for accumulation (epoch-length ring buffer)
 * @param accumulated Accumulated hashes history (epoch-length ring buffer)
 * @param privileges Privileged service configuration (chi)
 * @param serviceAccounts Service accounts with full data (delta)
 */
final case class AccumulationSubState(
  readyQueue: List[List[AccumulationReadyRecord]],
  accumulated: List[List[JamBytes]],
  privileges: Privileges,
  serviceAccounts: List[AccumulationServiceItem]
)

object AccumulationSubState:
  def empty(epochLength: Int): AccumulationSubState = AccumulationSubState(
    readyQueue = List.fill(epochLength)(List.empty),
    accumulated = List.fill(epochLength)(List.empty),
    privileges = Privileges(0, List.empty, 0, 0, List.empty),
    serviceAccounts = List.empty
  )

/**
 * Nested state component for statistics-related state.
 * Groups current and previous epoch validator statistics.
 *
 * @param current Current epoch validator statistics (alpha_v^curr)
 * @param last Previous epoch validator statistics (alpha_v^last)
 */
final case class StatisticsSubState(
  current: List[StatCount],
  last: List[StatCount]
)

object StatisticsSubState:
  def empty(validatorCount: Int): StatisticsSubState = StatisticsSubState(
    current = List.fill(validatorCount)(StatCount.zero),
    last = List.fill(validatorCount)(StatCount.zero)
  )

/**
 * Unified JAM state container holding all state components.
 */
final case class JamState(
  // Flat primitive: Current timeslot (tau)
  tau: Long,

  // Nested validator state: kappa, lambda, iota, gammaK
  // Used by: Safrole (RW), Dispute (R), Assurance (R), Report (R), Statistics (R)
  validators: ValidatorState,

  // Nested entropy buffer: eta (4-element pool)
  // Used by: Safrole (RW), Report (R), Accumulation (R - eta[0] only)
  entropy: EntropyBuffer,

  // Nested Safrole gamma state: gammaZ, gammaS, gammaA
  // Used by: Safrole (RW)
  gamma: SafroleGamma,

  // Flat authorization pools (per core, variable-size inner lists)
  // Used by: Authorization (RW), Report (R)
  authPools: List[List[Hash]],

  // Flat authorization queues (per core, fixed-size 80 inner lists)
  // Used by: Authorization (RW)
  authQueues: List[List[Hash]],

  // Nested core state: reports (rho), coreStatistics
  // Used by: Dispute (RW), Assurance (RW), Report (RW)
  cores: CoreState,

  // Nested accumulation state: readyQueue, accumulated, privileges, serviceAccounts
  // Used by: Accumulation (RW), Preimage (RW via serviceAccounts)
  accumulation: AccumulationSubState,

  // Nested statistics state: valsCurrStats, valsLastStats
  // Used by: Statistics (RW)
  statistics: StatisticsSubState,

  // Flat judgements state (psi)
  // Used by: Dispute (RW), Report (R - offenders subset)
  psi: Psi,

  // Flat recent block history (beta)
  // Used by: History (RW), Report (R)
  beta: HistoricalBetaContainer,

  // Flat service statistics (per block, fresh each block)
  // Used by: Report (R), Accumulation (W)
  serviceStatistics: List[ServiceStatisticsEntry],

  // Flat post-dispute offenders list
  // Used by: Safrole (RW)
  postOffenders: List[Ed25519PublicKey],

  // Last accumulation outputs (service commitments from accumulation)
  // Used by: Accumulation (W), History (R for state encoding)
  lastAccumulationOutputs: List[(Long, JamBytes)],

  // Raw keyvals for pass-through when unchanged
  otherKeyvals: List[KeyValue],

  // Original keyvals by full key (for preserving exact bytes)
  originalKeyvals: Map[JamBytes, KeyValue],

  // Raw service data by state key (for storage, preimages)
  rawServiceDataByStateKey: Map[JamBytes, JamBytes]
)

/**
 * Companion object providing Monocle lens definitions for JamState field access.
 * Lenses enable composable, type-safe state updates.
 */
object JamState:

  // Flat field lenses
  val tauLens: Lens[JamState, Long] = Focus[JamState](_.tau)
  val psiLens: Lens[JamState, Psi] = Focus[JamState](_.psi)
  val betaLens: Lens[JamState, HistoricalBetaContainer] = Focus[JamState](_.beta)
  val authPoolsLens: Lens[JamState, List[List[Hash]]] = Focus[JamState](_.authPools)
  val authQueuesLens: Lens[JamState, List[List[Hash]]] = Focus[JamState](_.authQueues)
  val serviceStatisticsLens: Lens[JamState, List[ServiceStatisticsEntry]] = Focus[JamState](_.serviceStatistics)
  val postOffendersLens: Lens[JamState, List[Ed25519PublicKey]] = Focus[JamState](_.postOffenders)
  val lastAccumulationOutputsLens: Lens[JamState, List[(Long, JamBytes)]] = Focus[JamState](_.lastAccumulationOutputs)

  // Nested component lenses
  val validatorsLens: Lens[JamState, ValidatorState] = Focus[JamState](_.validators)
  val entropyLens: Lens[JamState, EntropyBuffer] = Focus[JamState](_.entropy)
  val gammaLens: Lens[JamState, SafroleGamma] = Focus[JamState](_.gamma)
  val coresLens: Lens[JamState, CoreState] = Focus[JamState](_.cores)
  val accumulationLens: Lens[JamState, AccumulationSubState] = Focus[JamState](_.accumulation)
  val statisticsLens: Lens[JamState, StatisticsSubState] = Focus[JamState](_.statistics)

  // Deep path lenses for commonly accessed nested fields

  // Validator state deep paths
  val kappaLens: Lens[JamState, List[ValidatorKey]] = Focus[JamState](_.validators.current)
  val lambdaLens: Lens[JamState, List[ValidatorKey]] = Focus[JamState](_.validators.previous)
  val iotaLens: Lens[JamState, List[ValidatorKey]] = Focus[JamState](_.validators.queue)
  val gammaKLens: Lens[JamState, List[ValidatorKey]] = Focus[JamState](_.validators.nextEpoch)

  // Entropy deep path
  val etaLens: Lens[JamState, List[Hash]] = Focus[JamState](_.entropy.pool)

  // Safrole gamma deep paths
  val gammaZLens: Lens[JamState, JamBytes] = Focus[JamState](_.gamma.z)
  val gammaSLens: Lens[JamState, TicketsOrKeys] = Focus[JamState](_.gamma.s)
  val gammaALens: Lens[JamState, List[TicketMark]] = Focus[JamState](_.gamma.a)

  // Core state deep paths
  val rhoLens: Lens[JamState, List[Option[AvailabilityAssignment]]] = Focus[JamState](_.cores.reports)
  val coreStatisticsLens: Lens[JamState, List[CoreStatisticsRecord]] = Focus[JamState](_.cores.statistics)

  // Accumulation deep paths
  val readyQueueLens: Lens[JamState, List[List[AccumulationReadyRecord]]] = Focus[JamState](_.accumulation.readyQueue)
  val accumulatedLens: Lens[JamState, List[List[JamBytes]]] = Focus[JamState](_.accumulation.accumulated)
  val privilegesLens: Lens[JamState, Privileges] = Focus[JamState](_.accumulation.privileges)
  val serviceAccountsLens: Lens[JamState, List[AccumulationServiceItem]] =
    Focus[JamState](_.accumulation.serviceAccounts)

  // Statistics deep paths
  val valsCurrStatsLens: Lens[JamState, List[StatCount]] = Focus[JamState](_.statistics.current)
  val valsLastStatsLens: Lens[JamState, List[StatCount]] = Focus[JamState](_.statistics.last)

  // Raw data lenses
  val otherKeyvalsLens: Lens[JamState, List[KeyValue]] = Focus[JamState](_.otherKeyvals)
  val originalKeyvalsLens: Lens[JamState, Map[JamBytes, KeyValue]] = Focus[JamState](_.originalKeyvals)
  val rawServiceDataByStateKeyLens: Lens[JamState, Map[JamBytes, JamBytes]] =
    Focus[JamState](_.rawServiceDataByStateKey)

  // Convenience accessors using lens.get and lens.replace syntax

  /** Get current validators (kappa) */
  def getKappa(state: JamState): List[ValidatorKey] = kappaLens.get(state)

  /** Get previous validators (lambda) */
  def getLambda(state: JamState): List[ValidatorKey] = lambdaLens.get(state)

  /** Get entropy pool (eta) */
  def getEta(state: JamState): List[Hash] = etaLens.get(state)

  /** Get availability assignments (rho) */
  def getRho(state: JamState): List[Option[AvailabilityAssignment]] = rhoLens.get(state)

  /** Get judgements (psi) */
  def getPsi(state: JamState): Psi = psiLens.get(state)

  /** Get timeslot (tau) */
  def getTau(state: JamState): Long = tauLens.get(state)

  /** Update timeslot (tau) */
  def setTau(state: JamState, newTau: Long): JamState = tauLens.replace(newTau)(state)

  /** Update entropy pool (eta) */
  def setEta(state: JamState, newEta: List[Hash]): JamState = etaLens.replace(newEta)(state)

  /** Update current validators (kappa) */
  def setKappa(state: JamState, newKappa: List[ValidatorKey]): JamState = kappaLens.replace(newKappa)(state)

  /** Update availability assignments (rho) */
  def setRho(state: JamState, newRho: List[Option[AvailabilityAssignment]]): JamState = rhoLens.replace(newRho)(state)

  /** Update judgements (psi) */
  def setPsi(state: JamState, newPsi: Psi): JamState = psiLens.replace(newPsi)(state)

  /** Modify a field using a transformation function */
  def modifyTau(state: JamState, f: Long => Long): JamState = tauLens.modify(f)(state)

  /** Modify entropy pool using a transformation function */
  def modifyEta(state: JamState, f: List[Hash] => List[Hash]): JamState = etaLens.modify(f)(state)

  /** Modify availability assignments using a transformation function */
  def modifyRho(
    state: JamState,
    f: List[Option[AvailabilityAssignment]] => List[Option[AvailabilityAssignment]]
  ): JamState =
    rhoLens.modify(f)(state)

  /**
   * Create an empty JamState with default values.
   */
  def empty(
    validatorCount: Int,
    coreCount: Int,
    epochLength: Int,
    authQueueSize: Int = 80
  ): JamState = JamState(
    tau = 0L,
    validators = ValidatorState.empty,
    entropy = EntropyBuffer.empty,
    gamma = SafroleGamma(
      z = JamBytes.zeros(SafroleGamma.RingCommitmentSize),
      s = TicketsOrKeys.Keys(List.empty),
      a = List.empty
    ),
    authPools = List.fill(coreCount)(List.empty[Hash]),
    authQueues = List.fill(coreCount)(List.fill(authQueueSize)(Hash.zero)),
    cores = CoreState.empty(coreCount),
    accumulation = AccumulationSubState.empty(epochLength),
    statistics = StatisticsSubState.empty(validatorCount),
    psi = Psi.empty,
    beta = HistoricalBetaContainer(),
    serviceStatistics = List.empty,
    postOffenders = List.empty,
    lastAccumulationOutputs = List.empty,
    otherKeyvals = List.empty,
    originalKeyvals = Map.empty,
    rawServiceDataByStateKey = Map.empty
  )

  /**
   * Convert a FullJamState to JamState.
   * This is a temporary bridge during migration from the old state architecture.
   *
   * @param fjs The FullJamState to convert
   * @param config Chain configuration for sizing
   * @return A JamState with all fields populated from FullJamState
   */
  def fromFullJamState(fjs: FullJamState, config: ChainConfig): JamState =
    JamState(
      tau = fjs.timeslot,
      validators = ValidatorState(
        current = fjs.currentValidators,
        previous = fjs.previousValidators,
        queue = fjs.validatorQueue,
        nextEpoch = fjs.safroleGammaK
      ),
      entropy = EntropyBuffer(
        pool = if fjs.entropyPool.size == 4 then fjs.entropyPool
        else fjs.entropyPool.padTo(4, Hash.zero).take(4)
      ),
      gamma = SafroleGamma(
        z = fjs.safroleGammaZ,
        s = fjs.safroleGammaS,
        a = fjs.safroleGammaA
      ),
      authPools = fjs.authPools,
      authQueues = fjs.authQueues,
      cores = CoreState(
        reports = fjs.reports,
        statistics = fjs.coreStatistics
      ),
      accumulation = AccumulationSubState(
        readyQueue = if fjs.accumulationQueue.isEmpty then
          List.fill(config.epochLength)(List.empty)
        else fjs.accumulationQueue,
        accumulated = if fjs.accumulationHistory.isEmpty then
          List.fill(config.epochLength)(List.empty)
        else fjs.accumulationHistory,
        privileges = fjs.privilegedServices,
        serviceAccounts = fjs.serviceAccounts
      ),
      statistics = StatisticsSubState(
        current = fjs.activityStatsCurrent,
        last = fjs.activityStatsLast
      ),
      psi = fjs.judgements,
      beta = fjs.recentHistory,
      serviceStatistics = fjs.serviceStatistics,
      postOffenders = fjs.postOffenders,
      lastAccumulationOutputs = fjs.lastAccumulationOutputs,
      otherKeyvals = fjs.otherKeyvals,
      originalKeyvals = fjs.originalKeyvals,
      rawServiceDataByStateKey = fjs.rawServiceDataByStateKey
    )

  /**
   * Safrole lens bundle - extracts and applies Safrole-related state.
   * Fields: tau, eta, lambda, kappa, gammaK, iota, gammaA, gammaS, gammaZ, postOffenders
   */
  object SafroleLenses:
    import io.forge.jam.protocol.safrole.SafroleTypes.SafroleState

    /** Extract SafroleState from JamState */
    def extract(state: JamState): SafroleState = SafroleState(
      tau = tauLens.get(state),
      eta = etaLens.get(state),
      lambda = lambdaLens.get(state),
      kappa = kappaLens.get(state),
      gammaK = gammaKLens.get(state),
      iota = iotaLens.get(state),
      gammaA = gammaALens.get(state),
      gammaS = gammaSLens.get(state),
      gammaZ = gammaZLens.get(state),
      postOffenders = postOffendersLens.get(state)
    )

    /** Apply SafroleState changes back to JamState */
    def apply(state: JamState, post: SafroleState): JamState = state.copy(
      tau = post.tau,
      entropy = state.entropy.copy(pool = post.eta),
      validators = state.validators.copy(
        previous = post.lambda,
        current = post.kappa,
        nextEpoch = post.gammaK,
        queue = post.iota
      ),
      gamma = state.gamma.copy(
        a = post.gammaA,
        s = post.gammaS,
        z = post.gammaZ
      ),
      postOffenders = post.postOffenders
    )

  /**
   * Dispute lens bundle - extracts and applies Dispute-related state.
   * Fields: psi, rho, tau, kappa, lambda
   */
  object DisputeLenses:
    import io.forge.jam.protocol.dispute.DisputeTypes.DisputeState

    /** Extract DisputeState from JamState */
    def extract(state: JamState): DisputeState = DisputeState(
      psi = psiLens.get(state),
      rho = rhoLens.get(state),
      tau = tauLens.get(state),
      kappa = kappaLens.get(state),
      lambda = lambdaLens.get(state)
    )

    /** Apply DisputeState changes back to JamState (only psi and rho are modified) */
    def apply(state: JamState, post: DisputeState): JamState = state.copy(
      psi = post.psi,
      cores = state.cores.copy(reports = post.rho)
    )

  /**
   * Assurance lens bundle - extracts and applies Assurance-related state.
   * Fields: availAssignments (rho), currValidators (kappa)
   */
  object AssuranceLenses:
    import io.forge.jam.protocol.assurance.AssuranceTypes.AssuranceState

    /** Extract AssuranceState from JamState */
    def extract(state: JamState): AssuranceState = AssuranceState(
      availAssignments = rhoLens.get(state),
      currValidators = kappaLens.get(state)
    )

    /** Apply AssuranceState changes back to JamState */
    def apply(state: JamState, post: AssuranceState): JamState = state.copy(
      cores = state.cores.copy(reports = post.availAssignments)
    )

  /**
   * Authorization lens bundle - extracts and applies Authorization-related state.
   * Fields: authPools, authQueues
   */
  object AuthorizationLenses:
    import io.forge.jam.protocol.authorization.AuthorizationTypes.AuthState

    /** Extract AuthState from JamState */
    def extract(state: JamState): AuthState = AuthState(
      authPools = authPoolsLens.get(state),
      authQueues = authQueuesLens.get(state)
    )

    /** Apply AuthState changes back to JamState */
    def apply(state: JamState, post: AuthState): JamState = state.copy(
      authPools = post.authPools,
      authQueues = post.authQueues
    )

  /**
   * History lens bundle - extracts and applies History-related state.
   * Fields: beta (HistoricalBetaContainer)
   */
  object HistoryLenses:
    import io.forge.jam.protocol.history.HistoryTypes.HistoricalState

    /** Extract HistoricalState from JamState */
    def extract(state: JamState): HistoricalState = HistoricalState(
      beta = betaLens.get(state)
    )

    /** Apply HistoricalState changes back to JamState */
    def apply(state: JamState, post: HistoricalState): JamState = state.copy(
      beta = post.beta
    )

  /**
   * Statistics lens bundle - extracts and applies Statistics-related state.
   * Fields: valsCurrStats, valsLastStats, slot (tau), currValidators (kappa)
   */
  object StatisticsLenses:
    import io.forge.jam.protocol.statistics.StatisticsTypes.StatState

    /** Extract StatState from JamState */
    def extract(state: JamState): StatState = StatState(
      valsCurrStats = valsCurrStatsLens.get(state),
      valsLastStats = valsLastStatsLens.get(state),
      slot = tauLens.get(state),
      currValidators = kappaLens.get(state),
      prevValidators = lambdaLens.get(state)
    )

    /** Apply StatState changes back to JamState (only stats are modified) */
    def apply(state: JamState, post: StatState): JamState = state.copy(
      statistics = state.statistics.copy(
        current = post.valsCurrStats,
        last = post.valsLastStats
      )
    )

  /**
   * Report lens bundle - extracts and applies Report-related state.
   * Fields: availAssignments (rho), validators, entropy, offenders, recentBlocks, authPools, accounts, statistics
   */
  object ReportLenses:
    import io.forge.jam.protocol.report.ReportTypes.ReportState
    import io.forge.jam.core.types.service.{ServiceAccount, ServiceData}

    /** Extract ReportState from JamState (includes service account conversion) */
    def extract(state: JamState): ReportState =
      // Convert accumulation service accounts to core ServiceAccount type
      val accounts = state.accumulation.serviceAccounts.map { item =>
        ServiceAccount(
          id = item.id,
          data = ServiceData(service = item.data.service)
        )
      }

      ReportState(
        availAssignments = rhoLens.get(state),
        currValidators = kappaLens.get(state),
        prevValidators = lambdaLens.get(state),
        entropy = etaLens.get(state),
        offenders = psiLens.get(state).offenders.map(key => io.forge.jam.core.primitives.Hash(key.bytes)),
        recentBlocks = betaLens.get(state),
        authPools = authPoolsLens.get(state),
        accounts = accounts,
        coresStatistics = coreStatisticsLens.get(state),
        servicesStatistics = serviceStatisticsLens.get(state)
      )

    /** Apply ReportState changes back to JamState */
    def apply(state: JamState, post: ReportState): JamState = state.copy(
      cores = state.cores.copy(
        reports = post.availAssignments,
        statistics = post.coresStatistics
      ),
      serviceStatistics = post.servicesStatistics
    )

  /**
   * Convert a JamState back to FullJamState for encoding.
   * This is a temporary bridge during migration from the old state architecture.
   *
   * @param js The JamState to convert
   * @return A FullJamState with all fields populated from JamState
   */
  def toFullJamState(js: JamState): FullJamState =
    FullJamState(
      timeslot = js.tau,
      entropyPool = js.entropy.pool,
      currentValidators = js.validators.current,
      previousValidators = js.validators.previous,
      validatorQueue = js.validators.queue,
      safroleGammaK = js.validators.nextEpoch,
      safroleGammaZ = js.gamma.z,
      safroleGammaS = js.gamma.s,
      safroleGammaA = js.gamma.a,
      authPools = js.authPools,
      authQueues = js.authQueues,
      recentHistory = js.beta,
      reports = js.cores.reports,
      judgements = js.psi,
      privilegedServices = js.accumulation.privileges,
      accumulationQueue = js.accumulation.readyQueue,
      accumulationHistory = js.accumulation.accumulated,
      serviceAccounts = js.accumulation.serviceAccounts,
      serviceStatistics = js.serviceStatistics,
      coreStatistics = js.cores.statistics,
      activityStatsCurrent = js.statistics.current,
      activityStatsLast = js.statistics.last,
      postOffenders = js.postOffenders,
      lastAccumulationOutputs = js.lastAccumulationOutputs,
      otherKeyvals = js.otherKeyvals,
      originalKeyvals = js.originalKeyvals,
      rawServiceDataByStateKey = js.rawServiceDataByStateKey
    )
