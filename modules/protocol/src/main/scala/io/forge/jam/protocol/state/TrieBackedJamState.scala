package io.forge.jam.protocol.state

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{
  Hash,
  Ed25519PublicKey,
  BandersnatchPublicKey,
  BlsPublicKey
}
import io.forge.jam.core.scodec.{FullJamStateCodecs, JamCodecs}
import io.forge.jam.core.scodec.FullJamStateCodecs.{
  StatCountData,
  CoreStatisticsData,
  ServiceStatisticsData,
  TicketsOrKeysData
}
import io.forge.jam.core.trie.{StateTrie, StateTrieStore}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.types.workpackage.AvailabilityAssignment
import io.forge.jam.core.types.history.HistoricalBetaContainer
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.protocol.accumulation.{
  AccumulationReadyRecord,
  AccumulationServiceData,
  AccumulationServiceItem,
  Privileges
}
import io.forge.jam.protocol.dispute.DisputeTypes.Psi
import io.forge.jam.protocol.report.ReportTypes.{
  CoreStatisticsRecord,
  ServiceStatisticsEntry
}
import io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys
import io.forge.jam.protocol.statistics.StatisticsTypes.StatCount
import io.forge.jam.protocol.traces.{FullJamState, StateKeys}

import _root_.scodec.Codec
import _root_.scodec.bits.BitVector
import scala.collection.mutable

final class TrieBackedJamState(
    private val trie: StateTrie,
    val config: ChainConfig,
    val storage: ServiceStorageView,
    private val store: Option[StateTrieStore] = None
):



  private var _tau: Long = 0L
  private var _tauLoaded: Boolean = false
  private var _tauDirty: Boolean = false

  private var _entropyPool: List[Hash] = List.fill(4)(Hash.zero)
  private var _entropyPoolLoaded: Boolean = false
  private var _entropyPoolDirty: Boolean = false

  private var _kappa: List[ValidatorKey] =
    List.fill(config.validatorCount)(TrieBackedJamState.emptyValidator)
  private var _kappaLoaded: Boolean = false
  private var _kappaDirty: Boolean = false

  private var _lambda: List[ValidatorKey] =
    List.fill(config.validatorCount)(TrieBackedJamState.emptyValidator)
  private var _lambdaLoaded: Boolean = false
  private var _lambdaDirty: Boolean = false

  private var _iota: List[ValidatorKey] =
    List.fill(config.validatorCount)(TrieBackedJamState.emptyValidator)
  private var _iotaLoaded: Boolean = false
  private var _iotaDirty: Boolean = false

  private var _authPools: List[List[Hash]] = List.fill(config.coresCount)(List.empty)
  private var _authPoolsLoaded: Boolean = false
  private var _authPoolsDirty: Boolean = false

  private var _authQueues: List[List[Hash]] =
    List.fill(config.coresCount)(List.fill(config.authQueueSize)(Hash.zero))
  private var _authQueuesLoaded: Boolean = false
  private var _authQueuesDirty: Boolean = false

  private var _beta: HistoricalBetaContainer = HistoricalBetaContainer()
  private var _betaLoaded: Boolean = false
  private var _betaDirty: Boolean = false

  private var _reports: List[Option[AvailabilityAssignment]] =
    List.fill(config.coresCount)(None)
  private var _reportsLoaded: Boolean = false
  private var _reportsDirty: Boolean = false

  private var _readyQueue: List[List[AccumulationReadyRecord]] =
    List.fill(config.epochLength)(List.empty)
  private var _readyQueueLoaded: Boolean = false
  private var _readyQueueDirty: Boolean = false

  private var _accumulated: List[List[JamBytes]] =
    List.fill(config.epochLength)(List.empty)
  private var _accumulatedLoaded: Boolean = false
  private var _accumulatedDirty: Boolean = false

  private var _lastAccumulationOutputs: List[(Long, JamBytes)] = List.empty
  private var _lastAccumulationOutputsLoaded: Boolean = false
  private var _lastAccumulationOutputsDirty: Boolean = false

  private var _privileges: Privileges =
    Privileges(0, List.fill(config.coresCount)(0L), 0, 0, List.empty)
  private var _privilegesLoaded: Boolean = false
  private var _privilegesDirty: Boolean = false

  private var _psi: Psi = Psi.empty
  private var _psiLoaded: Boolean = false
  private var _psiDirty: Boolean = false


  private var _gammaK: List[ValidatorKey] =
    List.fill(config.validatorCount)(TrieBackedJamState.emptyValidator)
  private var _gammaZ: JamBytes =
    JamBytes.zeros(FullJamStateCodecs.BandersnatchRingCommitmentSize)
  private var _gammaS: TicketsOrKeys =
    TicketsOrKeys.Keys(List.fill(config.epochLength)(BandersnatchPublicKey.zero))
  private var _gammaA: List[TicketMark] = List.empty
  private var _safroleLoaded: Boolean = false
  private var _safroleDirty: Boolean = false


  private var _statsCurrent: List[StatCount] =
    List.fill(config.validatorCount)(StatCount.zero)
  private var _statsLast: List[StatCount] =
    List.fill(config.validatorCount)(StatCount.zero)
  private var _coreStatistics: List[CoreStatisticsRecord] =
    List.fill(config.coresCount)(CoreStatisticsRecord.zero)
  private var _serviceStatistics: List[ServiceStatisticsEntry] = List.empty
  private var _activityStatsLoaded: Boolean = false
  private var _activityStatsDirty: Boolean = false


  private var _serviceAccounts: List[AccumulationServiceItem] = List.empty
  private var _serviceAccountsLoaded: Boolean = false
  private var _serviceAccountsDirty: Boolean = false
  private var _serviceAccountsPreIds: Set[Long] = Set.empty
  private var _serviceAccountsPreInfo: Map[Long, ServiceInfo] = Map.empty


  private var _postOffenders: List[Ed25519PublicKey] = List.empty


  def timeslot: Long =
    if !_tauLoaded then
      _tau = TrieBackedJamState.readTimeslot(trie)
      _tauLoaded = true
    _tau

  def timeslot_=(v: Long): Unit =
    _tau = v
    _tauLoaded = true
    _tauDirty = true

  def authPools: List[List[Hash]] =
    if !_authPoolsLoaded then
      _authPools = TrieBackedJamState.readAuthPools(trie, config)
      _authPoolsLoaded = true
    _authPools

  def authPools_=(v: List[List[Hash]]): Unit =
    _authPools = v
    _authPoolsLoaded = true
    _authPoolsDirty = true

  def authQueues: List[List[Hash]] =
    if !_authQueuesLoaded then
      _authQueues = TrieBackedJamState.readAuthQueues(trie, config)
      _authQueuesLoaded = true
    _authQueues

  def authQueues_=(v: List[List[Hash]]): Unit =
    _authQueues = v
    _authQueuesLoaded = true
    _authQueuesDirty = true

  def psi: Psi =
    if !_psiLoaded then
      _psi = TrieBackedJamState.readPsi(trie)
      _psiLoaded = true
    _psi

  def psi_=(v: Psi): Unit =
    _psi = v
    _psiLoaded = true
    _psiDirty = true

  def beta: HistoricalBetaContainer =
    if !_betaLoaded then
      _beta = TrieBackedJamState.readRecentHistory(trie)
      _betaLoaded = true
    _beta

  def beta_=(v: HistoricalBetaContainer): Unit =
    _beta = v
    _betaLoaded = true
    _betaDirty = true

  def serviceStatistics: List[ServiceStatisticsEntry] =
    if !_activityStatsLoaded then loadActivityStats()
    _serviceStatistics

  def serviceStatistics_=(v: List[ServiceStatisticsEntry]): Unit =
    if !_activityStatsLoaded then loadActivityStats()
    _serviceStatistics = v
    _activityStatsDirty = true

  def postOffenders: List[Ed25519PublicKey] = _postOffenders

  def postOffenders_=(v: List[Ed25519PublicKey]): Unit = _postOffenders = v

  def lastAccumulationOutputs: List[(Long, JamBytes)] =
    if !_lastAccumulationOutputsLoaded then
      _lastAccumulationOutputs =
        TrieBackedJamState.readLastAccumulationOutputs(trie)
      _lastAccumulationOutputsLoaded = true
    _lastAccumulationOutputs

  def lastAccumulationOutputs_=(v: List[(Long, JamBytes)]): Unit =
    _lastAccumulationOutputs = v
    _lastAccumulationOutputsLoaded = true
    _lastAccumulationOutputsDirty = true


  val validators: ValidatorsRef = new ValidatorsRef(this)
  val entropy: EntropyRef = new EntropyRef(this)
  val gamma: GammaRef = new GammaRef(this)
  val cores: CoresRef = new CoresRef(this)
  val accumulation: AccumulationRef = new AccumulationRef(this)
  val statistics: StatisticsRef = new StatisticsRef(this)


  private[state] def kappa: List[ValidatorKey] =
    if !_kappaLoaded then
      _kappa = TrieBackedJamState.readValidators(
        trie,
        StateKeys.CURRENT_VALIDATORS,
        config.validatorCount
      )
      _kappaLoaded = true
    _kappa

  private[state] def kappa_=(v: List[ValidatorKey]): Unit =
    _kappa = v
    _kappaLoaded = true
    _kappaDirty = true

  private[state] def lambda: List[ValidatorKey] =
    if !_lambdaLoaded then
      _lambda = TrieBackedJamState.readValidators(
        trie,
        StateKeys.PREVIOUS_VALIDATORS,
        config.validatorCount
      )
      _lambdaLoaded = true
    _lambda

  private[state] def lambda_=(v: List[ValidatorKey]): Unit =
    _lambda = v
    _lambdaLoaded = true
    _lambdaDirty = true

  private[state] def iota: List[ValidatorKey] =
    if !_iotaLoaded then
      _iota = TrieBackedJamState.readValidators(
        trie,
        StateKeys.VALIDATOR_QUEUE,
        config.validatorCount
      )
      _iotaLoaded = true
    _iota

  private[state] def iota_=(v: List[ValidatorKey]): Unit =
    _iota = v
    _iotaLoaded = true
    _iotaDirty = true

  private[state] def gammaK: List[ValidatorKey] =
    if !_safroleLoaded then loadSafrole()
    _gammaK

  private[state] def gammaK_=(v: List[ValidatorKey]): Unit =
    if !_safroleLoaded then loadSafrole()
    _gammaK = v
    _safroleDirty = true

  private[state] def gammaZ: JamBytes =
    if !_safroleLoaded then loadSafrole()
    _gammaZ

  private[state] def gammaZ_=(v: JamBytes): Unit =
    if !_safroleLoaded then loadSafrole()
    _gammaZ = v
    _safroleDirty = true

  private[state] def gammaS: TicketsOrKeys =
    if !_safroleLoaded then loadSafrole()
    _gammaS

  private[state] def gammaS_=(v: TicketsOrKeys): Unit =
    if !_safroleLoaded then loadSafrole()
    _gammaS = v
    _safroleDirty = true

  private[state] def gammaA: List[TicketMark] =
    if !_safroleLoaded then loadSafrole()
    _gammaA

  private[state] def gammaA_=(v: List[TicketMark]): Unit =
    if !_safroleLoaded then loadSafrole()
    _gammaA = v
    _safroleDirty = true

  private[state] def entropyPool: List[Hash] =
    if !_entropyPoolLoaded then
      _entropyPool = TrieBackedJamState.readEntropyPool(trie)
      _entropyPoolLoaded = true
    _entropyPool

  private[state] def entropyPool_=(v: List[Hash]): Unit =
    require(v.size == 4, s"entropy pool must be 4 hashes, got ${v.size}")
    _entropyPool = v
    _entropyPoolLoaded = true
    _entropyPoolDirty = true

  private[state] def coreReports: List[Option[AvailabilityAssignment]] =
    if !_reportsLoaded then
      _reports = TrieBackedJamState.readReports(trie, config.coresCount)
      _reportsLoaded = true
    _reports

  private[state] def coreReports_=(v: List[Option[AvailabilityAssignment]]): Unit =
    _reports = v
    _reportsLoaded = true
    _reportsDirty = true

  private[state] def coreStatistics: List[CoreStatisticsRecord] =
    if !_activityStatsLoaded then loadActivityStats()
    _coreStatistics

  private[state] def coreStatistics_=(v: List[CoreStatisticsRecord]): Unit =
    if !_activityStatsLoaded then loadActivityStats()
    _coreStatistics = v
    _activityStatsDirty = true

  private[state] def readyQueue: List[List[AccumulationReadyRecord]] =
    if !_readyQueueLoaded then
      _readyQueue =
        TrieBackedJamState.readAccumulationQueue(trie, config.epochLength)
      _readyQueueLoaded = true
    _readyQueue

  private[state] def readyQueue_=(v: List[List[AccumulationReadyRecord]]): Unit =
    _readyQueue = v
    _readyQueueLoaded = true
    _readyQueueDirty = true

  private[state] def accumulated: List[List[JamBytes]] =
    if !_accumulatedLoaded then
      _accumulated =
        TrieBackedJamState.readAccumulationHistory(trie, config.epochLength)
      _accumulatedLoaded = true
    _accumulated

  private[state] def accumulated_=(v: List[List[JamBytes]]): Unit =
    _accumulated = v
    _accumulatedLoaded = true
    _accumulatedDirty = true

  private[state] def privileges: Privileges =
    if !_privilegesLoaded then
      _privileges = TrieBackedJamState.readPrivileges(trie, config.coresCount)
      _privilegesLoaded = true
    _privileges

  private[state] def privileges_=(v: Privileges): Unit =
    _privileges = v
    _privilegesLoaded = true
    _privilegesDirty = true

  private[state] def serviceAccounts: List[AccumulationServiceItem] =
    if !_serviceAccountsLoaded then loadServiceAccounts()
    _serviceAccounts

  private[state] def serviceAccounts_=(v: List[AccumulationServiceItem]): Unit =
    if !_serviceAccountsLoaded then loadServiceAccounts()
    _serviceAccounts = v
    _serviceAccountsDirty = true

  private[state] def statsCurrent: List[StatCount] =
    if !_activityStatsLoaded then loadActivityStats()
    _statsCurrent

  private[state] def statsCurrent_=(v: List[StatCount]): Unit =
    if !_activityStatsLoaded then loadActivityStats()
    _statsCurrent = v
    _activityStatsDirty = true

  private[state] def statsLast: List[StatCount] =
    if !_activityStatsLoaded then loadActivityStats()
    _statsLast

  private[state] def statsLast_=(v: List[StatCount]): Unit =
    if !_activityStatsLoaded then loadActivityStats()
    _statsLast = v
    _activityStatsDirty = true


  private def loadSafrole(): Unit =
    val (k, z, s, a) = TrieBackedJamState.readSafroleBlock(trie, config)
    _gammaK = k
    _gammaZ = z
    _gammaS = s
    _gammaA = a
    _safroleLoaded = true

  private def loadActivityStats(): Unit =
    val (cur, prev, core, svc) =
      TrieBackedJamState.readActivityStats(trie, config)
    _statsCurrent = cur
    _statsLast = prev
    _coreStatistics = core
    _serviceStatistics = svc
    _activityStatsLoaded = true

  private def loadServiceAccounts(): Unit =
    val items = TrieBackedJamState.readServiceAccounts(trie, store)
    _serviceAccounts = items
    _serviceAccountsPreIds = items.iterator.map(_.id).toSet
    _serviceAccountsPreInfo =
      items.iterator.map(i => i.id -> i.data.service).toMap
    _serviceAccountsLoaded = true


  private final class Snapshot(
      val tau: Long, val tauLoaded: Boolean, val tauDirty: Boolean,
      val entropyPool: List[Hash], val entropyPoolLoaded: Boolean, val entropyPoolDirty: Boolean,
      val kappa: List[ValidatorKey], val kappaLoaded: Boolean, val kappaDirty: Boolean,
      val lambda: List[ValidatorKey], val lambdaLoaded: Boolean, val lambdaDirty: Boolean,
      val iota: List[ValidatorKey], val iotaLoaded: Boolean, val iotaDirty: Boolean,
      val authPools: List[List[Hash]], val authPoolsLoaded: Boolean, val authPoolsDirty: Boolean,
      val authQueues: List[List[Hash]], val authQueuesLoaded: Boolean, val authQueuesDirty: Boolean,
      val beta: HistoricalBetaContainer, val betaLoaded: Boolean, val betaDirty: Boolean,
      val reports: List[Option[AvailabilityAssignment]], val reportsLoaded: Boolean, val reportsDirty: Boolean,
      val readyQueue: List[List[AccumulationReadyRecord]], val readyQueueLoaded: Boolean, val readyQueueDirty: Boolean,
      val accumulated: List[List[JamBytes]], val accumulatedLoaded: Boolean, val accumulatedDirty: Boolean,
      val lastAccOuts: List[(Long, JamBytes)], val lastAccOutsLoaded: Boolean, val lastAccOutsDirty: Boolean,
      val privileges: Privileges, val privilegesLoaded: Boolean, val privilegesDirty: Boolean,
      val psi: Psi, val psiLoaded: Boolean, val psiDirty: Boolean,
      val gammaK: List[ValidatorKey], val gammaZ: JamBytes, val gammaS: TicketsOrKeys, val gammaA: List[TicketMark],
      val safroleLoaded: Boolean, val safroleDirty: Boolean,
      val statsCur: List[StatCount], val statsLast: List[StatCount],
      val coreStats: List[CoreStatisticsRecord], val svcStats: List[ServiceStatisticsEntry],
      val actLoaded: Boolean, val actDirty: Boolean,
      val svcAccs: List[AccumulationServiceItem], val svcAccsLoaded: Boolean, val svcAccsDirty: Boolean,
      val svcAccsPreIds: Set[Long],
      val postOffenders: List[Ed25519PublicKey]
  )

  private val snapshots = mutable.ArrayBuffer.empty[Snapshot]

  def savepoint(): Unit =
    snapshots += new Snapshot(
      _tau, _tauLoaded, _tauDirty,
      _entropyPool, _entropyPoolLoaded, _entropyPoolDirty,
      _kappa, _kappaLoaded, _kappaDirty,
      _lambda, _lambdaLoaded, _lambdaDirty,
      _iota, _iotaLoaded, _iotaDirty,
      _authPools, _authPoolsLoaded, _authPoolsDirty,
      _authQueues, _authQueuesLoaded, _authQueuesDirty,
      _beta, _betaLoaded, _betaDirty,
      _reports, _reportsLoaded, _reportsDirty,
      _readyQueue, _readyQueueLoaded, _readyQueueDirty,
      _accumulated, _accumulatedLoaded, _accumulatedDirty,
      _lastAccumulationOutputs, _lastAccumulationOutputsLoaded, _lastAccumulationOutputsDirty,
      _privileges, _privilegesLoaded, _privilegesDirty,
      _psi, _psiLoaded, _psiDirty,
      _gammaK, _gammaZ, _gammaS, _gammaA, _safroleLoaded, _safroleDirty,
      _statsCurrent, _statsLast, _coreStatistics, _serviceStatistics,
      _activityStatsLoaded, _activityStatsDirty,
      _serviceAccounts, _serviceAccountsLoaded, _serviceAccountsDirty,
      _serviceAccountsPreIds,
      _postOffenders
    )
    storage.savepoint()

  def discardCheckpoint(): Unit =
    if snapshots.nonEmpty then snapshots.remove(snapshots.length - 1)
    storage.discardCheckpoint()

  def restore(): Unit =
    if snapshots.isEmpty then
      storage.restore()
      return
    val s = snapshots.remove(snapshots.length - 1)
    _tau = s.tau; _tauLoaded = s.tauLoaded; _tauDirty = s.tauDirty
    _entropyPool = s.entropyPool; _entropyPoolLoaded = s.entropyPoolLoaded; _entropyPoolDirty = s.entropyPoolDirty
    _kappa = s.kappa; _kappaLoaded = s.kappaLoaded; _kappaDirty = s.kappaDirty
    _lambda = s.lambda; _lambdaLoaded = s.lambdaLoaded; _lambdaDirty = s.lambdaDirty
    _iota = s.iota; _iotaLoaded = s.iotaLoaded; _iotaDirty = s.iotaDirty
    _authPools = s.authPools; _authPoolsLoaded = s.authPoolsLoaded; _authPoolsDirty = s.authPoolsDirty
    _authQueues = s.authQueues; _authQueuesLoaded = s.authQueuesLoaded; _authQueuesDirty = s.authQueuesDirty
    _beta = s.beta; _betaLoaded = s.betaLoaded; _betaDirty = s.betaDirty
    _reports = s.reports; _reportsLoaded = s.reportsLoaded; _reportsDirty = s.reportsDirty
    _readyQueue = s.readyQueue; _readyQueueLoaded = s.readyQueueLoaded; _readyQueueDirty = s.readyQueueDirty
    _accumulated = s.accumulated; _accumulatedLoaded = s.accumulatedLoaded; _accumulatedDirty = s.accumulatedDirty
    _lastAccumulationOutputs = s.lastAccOuts
    _lastAccumulationOutputsLoaded = s.lastAccOutsLoaded
    _lastAccumulationOutputsDirty = s.lastAccOutsDirty
    _privileges = s.privileges; _privilegesLoaded = s.privilegesLoaded; _privilegesDirty = s.privilegesDirty
    _psi = s.psi; _psiLoaded = s.psiLoaded; _psiDirty = s.psiDirty
    _gammaK = s.gammaK; _gammaZ = s.gammaZ; _gammaS = s.gammaS; _gammaA = s.gammaA
    _safroleLoaded = s.safroleLoaded; _safroleDirty = s.safroleDirty
    _statsCurrent = s.statsCur; _statsLast = s.statsLast
    _coreStatistics = s.coreStats; _serviceStatistics = s.svcStats
    _activityStatsLoaded = s.actLoaded; _activityStatsDirty = s.actDirty
    _serviceAccounts = s.svcAccs
    _serviceAccountsLoaded = s.svcAccsLoaded
    _serviceAccountsDirty = s.svcAccsDirty
    _serviceAccountsPreIds = s.svcAccsPreIds
    _postOffenders = s.postOffenders
    storage.restore()

  def savepointDepth: Int = snapshots.length


  def commit(target: StateTrie): Unit =
    val updates =
      mutable.ArrayBuffer.empty[(JamBytes, Option[JamBytes])]

    if _tauDirty then
      updates += ((StateKeys.simpleKey(StateKeys.TIMESLOT),
        Some(TrieBackedJamState.encodeTimeslot(_tau))))
    if _entropyPoolDirty then
      updates += ((StateKeys.simpleKey(StateKeys.ENTROPY_POOL),
        Some(TrieBackedJamState.encodeEntropyPool(_entropyPool))))
    if _kappaDirty then
      updates += ((StateKeys.simpleKey(StateKeys.CURRENT_VALIDATORS),
        Some(TrieBackedJamState.encodeValidatorList(_kappa))))
    if _lambdaDirty then
      updates += ((StateKeys.simpleKey(StateKeys.PREVIOUS_VALIDATORS),
        Some(TrieBackedJamState.encodeValidatorList(_lambda))))
    if _iotaDirty then
      updates += ((StateKeys.simpleKey(StateKeys.VALIDATOR_QUEUE),
        Some(TrieBackedJamState.encodeValidatorList(_iota))))
    if _authPoolsDirty then
      updates += ((StateKeys.simpleKey(StateKeys.CORE_AUTHORIZATION_POOL),
        Some(TrieBackedJamState.encodeAuthPools(_authPools))))
    if _authQueuesDirty then
      updates += ((StateKeys.simpleKey(StateKeys.AUTHORIZATION_QUEUE),
        Some(TrieBackedJamState.encodeAuthQueues(_authQueues, config))))
    if _betaDirty then
      updates += ((StateKeys.simpleKey(StateKeys.RECENT_HISTORY),
        Some(TrieBackedJamState.encodeRecentHistory(_beta))))
    if _reportsDirty then
      updates += ((StateKeys.simpleKey(StateKeys.REPORTS),
        Some(TrieBackedJamState.encodeReports(_reports, config.coresCount))))
    if _readyQueueDirty then
      updates += ((StateKeys.simpleKey(StateKeys.ACCUMULATION_QUEUE),
        Some(TrieBackedJamState.encodeAccumulationQueue(_readyQueue))))
    if _accumulatedDirty then
      updates += ((StateKeys.simpleKey(StateKeys.ACCUMULATION_HISTORY),
        Some(TrieBackedJamState.encodeAccumulationHistory(_accumulated))))
    if _lastAccumulationOutputsDirty then
      updates += ((StateKeys.simpleKey(StateKeys.LAST_ACCUMULATION_OUTPUTS),
        Some(TrieBackedJamState.encodeLastAccumulationOutputs(_lastAccumulationOutputs))))
    if _privilegesDirty then
      updates += ((StateKeys.simpleKey(StateKeys.PRIVILEGED_SERVICES),
        Some(TrieBackedJamState.encodePrivileges(_privileges, config.coresCount))))
    if _psiDirty then
      updates += ((StateKeys.simpleKey(StateKeys.JUDGEMENTS),
        Some(TrieBackedJamState.encodePsi(_psi))))
    if _safroleDirty then
      updates += ((StateKeys.simpleKey(StateKeys.SAFROLE_STATE),
        Some(TrieBackedJamState.encodeSafroleBlock(_gammaK, _gammaZ, _gammaS, _gammaA))))
    if _activityStatsDirty then
      updates += ((StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS),
        Some(TrieBackedJamState.encodeActivityStats(
          _statsCurrent, _statsLast, _coreStatistics, _serviceStatistics, config))))
    if _serviceAccountsDirty then
      val currIds = _serviceAccounts.iterator.map(_.id).toSet
      _serviceAccounts.foreach { item =>
        val prev = _serviceAccountsPreInfo.get(item.id)
        if !prev.contains(item.data.service) then
          val encoded = TrieBackedJamState.encodeServiceInfo(item.data.service)
          updates += ((TrieBackedJamState.encodeServiceAccountKey(item.id),
            Some(encoded)))
          store.foreach { s =>
            s.putCachedServiceInfo(item.id, encoded)
            if !_serviceAccountsPreIds.contains(item.id) then
              s.addKnownServiceId(item.id)
          }
      }
      _serviceAccountsPreIds.foreach { id =>
        if !currIds.contains(id) then
          updates += ((TrieBackedJamState.encodeServiceAccountKey(id), None))
          store.foreach { s =>
            s.evictCachedServiceInfo(id)
            s.removeKnownServiceId(id)
          }
      }

    if updates.nonEmpty then target.update(updates.toSeq)


  def toFullJamState(): FullJamState =
    FullJamState(
      timeslot = timeslot,
      entropyPool = entropyPool,
      currentValidators = kappa,
      previousValidators = lambda,
      validatorQueue = iota,
      safroleGammaK = gammaK,
      safroleGammaZ = gammaZ,
      safroleGammaS = gammaS,
      safroleGammaA = gammaA,
      authPools = authPools,
      authQueues = authQueues,
      recentHistory = beta,
      reports = coreReports,
      judgements = psi,
      privilegedServices = privileges,
      accumulationQueue = readyQueue,
      accumulationHistory = accumulated,
      serviceAccounts = serviceAccounts,
      serviceStatistics = serviceStatistics,
      coreStatistics = coreStatistics,
      activityStatsCurrent = statsCurrent,
      activityStatsLast = statsLast,
      postOffenders = postOffenders,
      lastAccumulationOutputs = lastAccumulationOutputs
    )

object TrieBackedJamState:

  def at(
      store: io.forge.jam.core.trie.StateTrieStore,
      root: io.forge.jam.core.primitives.Hash,
      config: ChainConfig
  ): TrieBackedJamState =
    val trie = store.at(root)
    new TrieBackedJamState(trie, config, new ServiceStorageView(trie), Some(store))

  private[state] val emptyValidator: ValidatorKey =
    ValidatorKey(
      BandersnatchPublicKey.zero,
      Ed25519PublicKey(new Array[Byte](Ed25519PublicKey.Size)),
      BlsPublicKey(new Array[Byte](BlsPublicKey.Size)),
      JamBytes.zeros(ValidatorKey.MetadataSize)
    )


  private def readTimeslot(trie: StateTrie): Long =
    trie.read(StateKeys.simpleKey(StateKeys.TIMESLOT)) match
      case None    => 0L
      case Some(v) =>
        FullJamStateCodecs.timeslotCodec
          .decodeValue(BitVector(v.toByteVector))
          .require

  private def readEntropyPool(trie: StateTrie): List[Hash] =
    trie.read(StateKeys.simpleKey(StateKeys.ENTROPY_POOL)) match
      case None    => List.fill(4)(Hash.zero)
      case Some(v) =>
        FullJamStateCodecs.entropyPoolCodec
          .decodeValue(BitVector(v.toByteVector))
          .require

  private def readValidators(
      trie: StateTrie,
      prefix: Byte,
      validatorCount: Int
  ): List[ValidatorKey] =
    trie.read(StateKeys.simpleKey(prefix)) match
      case None => List.fill(validatorCount)(emptyValidator)
      case Some(v) =>
        FullJamStateCodecs
          .validatorListCodec(validatorCount)
          .decodeValue(BitVector(v.toByteVector))
          .require

  private def readAuthPools(
      trie: StateTrie,
      config: ChainConfig
  ): List[List[Hash]] =
    trie.read(StateKeys.simpleKey(StateKeys.CORE_AUTHORIZATION_POOL)) match
      case None    => List.fill(config.coresCount)(List.empty)
      case Some(v) =>
        FullJamStateCodecs.decodeAuthPools(v.toArray, config.coresCount)

  private def readAuthQueues(
      trie: StateTrie,
      config: ChainConfig
  ): List[List[Hash]] =
    trie.read(StateKeys.simpleKey(StateKeys.AUTHORIZATION_QUEUE)) match
      case None =>
        List.fill(config.coresCount)(
          List.fill(config.authQueueSize)(Hash.zero)
        )
      case Some(v) =>
        FullJamStateCodecs.decodeAuthQueues(
          v.toArray,
          config.coresCount,
          config.authQueueSize
        )

  private def readPsi(trie: StateTrie): Psi =
    trie.read(StateKeys.simpleKey(StateKeys.JUDGEMENTS)) match
      case None    => Psi.empty
      case Some(v) =>
        summon[Codec[Psi]]
          .decodeValue(BitVector(v.toByteVector))
          .require

  private def readRecentHistory(trie: StateTrie): HistoricalBetaContainer =
    trie.read(StateKeys.simpleKey(StateKeys.RECENT_HISTORY)) match
      case None    => HistoricalBetaContainer()
      case Some(v) =>
        summon[Codec[HistoricalBetaContainer]]
          .decodeValue(BitVector(v.toByteVector))
          .require

  private def readReports(
      trie: StateTrie,
      coresCount: Int
  ): List[Option[AvailabilityAssignment]] =
    trie.read(StateKeys.simpleKey(StateKeys.REPORTS)) match
      case None    => List.fill(coresCount)(None)
      case Some(v) =>
        FullJamStateCodecs
          .reportsCodec[AvailabilityAssignment](coresCount)
          .decodeValue(BitVector(v.toByteVector))
          .require

  private def readAccumulationQueue(
      trie: StateTrie,
      epochLength: Int
  ): List[List[AccumulationReadyRecord]] =
    trie.read(StateKeys.simpleKey(StateKeys.ACCUMULATION_QUEUE)) match
      case None    => List.fill(epochLength)(List.empty)
      case Some(v) =>
        val codec = JamCodecs.fixedSizeList(
          JamCodecs.compactPrefixedList(summon[Codec[AccumulationReadyRecord]]),
          epochLength
        )
        codec.decodeValue(BitVector(v.toByteVector)).require

  private def readAccumulationHistory(
      trie: StateTrie,
      epochLength: Int
  ): List[List[JamBytes]] =
    trie.read(StateKeys.simpleKey(StateKeys.ACCUMULATION_HISTORY)) match
      case None    => List.fill(epochLength)(List.empty)
      case Some(v) =>
        FullJamStateCodecs
          .decodeAccumulationHistory(v.toArray, epochLength)
          .map(_.map(bv => JamBytes.fromByteVector(bv)))

  private def readLastAccumulationOutputs(
      trie: StateTrie
  ): List[(Long, JamBytes)] =
    trie.read(StateKeys.simpleKey(StateKeys.LAST_ACCUMULATION_OUTPUTS)) match
      case None    => List.empty
      case Some(v) =>
        FullJamStateCodecs
          .decodeLastAccumulationOutputs(v.toArray)
          .map { case (id, bv) => (id, JamBytes.fromByteVector(bv)) }

  private def readPrivileges(trie: StateTrie, coresCount: Int): Privileges =
    trie.read(StateKeys.simpleKey(StateKeys.PRIVILEGED_SERVICES)) match
      case None =>
        Privileges(0, List.fill(coresCount)(0L), 0, 0, List.empty)
      case Some(v) =>
        Privileges
          .codec(coresCount)
          .decodeValue(BitVector(v.toByteVector))
          .require

  private def readSafroleBlock(
      trie: StateTrie,
      config: ChainConfig
  ): (List[ValidatorKey], JamBytes, TicketsOrKeys, List[TicketMark]) =
    trie.read(StateKeys.simpleKey(StateKeys.SAFROLE_STATE)) match
      case None =>
        (
          List.fill(config.validatorCount)(emptyValidator),
          JamBytes.zeros(FullJamStateCodecs.BandersnatchRingCommitmentSize),
          TicketsOrKeys.Keys(
            List.fill(config.epochLength)(BandersnatchPublicKey.zero)
          ),
          List.empty
        )
      case Some(v) =>
        val (k, zBv, sData, a) =
          FullJamStateCodecs
            .safroleGammaStateCodec(config.validatorCount, config.epochLength)
            .decodeValue(BitVector(v.toByteVector))
            .require
        val s = sData match
          case TicketsOrKeysData.Tickets(t) => TicketsOrKeys.Tickets(t)
          case TicketsOrKeysData.Keys(ks)   => TicketsOrKeys.Keys(ks)
        (k, JamBytes.fromByteVector(zBv), s, a)

  private def readActivityStats(
      trie: StateTrie,
      config: ChainConfig
  ): (List[StatCount], List[StatCount], List[CoreStatisticsRecord], List[ServiceStatisticsEntry]) =
    trie.read(StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS)) match
      case None =>
        (
          List.fill(config.validatorCount)(StatCount.zero),
          List.fill(config.validatorCount)(StatCount.zero),
          List.fill(config.coresCount)(CoreStatisticsRecord.zero),
          List.empty
        )
      case Some(v) =>
        val stats = FullJamStateCodecs.decodeActivityStatistics(
          v.toArray,
          config.validatorCount,
          config.coresCount
        )
        val cur = stats.accumulator.map(toStatCount)
        val prev = stats.previous.map(toStatCount)
        val core = stats.core.map(toCoreStatistics)
        val svc = stats.service.map(toServiceStatistics)
        (cur, prev, core, svc)

  private def toStatCount(s: StatCountData): StatCount =
    StatCount(
      s.blocks,
      s.tickets,
      s.preImages,
      s.preImagesSize,
      s.guarantees,
      s.assurances
    )

  private def toCoreStatistics(c: CoreStatisticsData): CoreStatisticsRecord =
    CoreStatisticsRecord(
      c.daLoad,
      c.popularity,
      c.imports,
      c.extrinsicCount,
      c.extrinsicSize,
      c.exports,
      c.bundleSize,
      c.gasUsed
    )

  private def toServiceStatistics(
      s: ServiceStatisticsData
  ): ServiceStatisticsEntry =
    ServiceStatisticsEntry(
      id = s.serviceId,
      record = io.forge.jam.protocol.report.ReportTypes.ServiceActivityRecord(
        providedCount = s.preimagesCount.toInt,
        providedSize = s.preimagesSize,
        refinementCount = s.refinesCount,
        refinementGasUsed = s.refinesGas,
        extrinsicCount = s.extrinsicsCount,
        extrinsicSize = s.extrinsicsSize,
        imports = s.importsCount,
        exports = s.exportsCount,
        accumulateCount = s.accumulatesCount,
        accumulateGasUsed = s.accumulatesGas
      )
    )

  private def readServiceAccounts(
      trie: StateTrie,
      store: Option[StateTrieStore]
  ): List[AccumulationServiceItem] =
    val cachedIds = store.flatMap(_.cachedServiceIds)
    val out = mutable.ArrayBuffer.empty[AccumulationServiceItem]

    def materialize(id: Long, encoded: JamBytes): AccumulationServiceItem =
      val info = FullJamStateCodecs.decodeServiceInfo(encoded.toArray)
      AccumulationServiceItem(
        id = id,
        data = AccumulationServiceData(
          service = info,
          storage = List.empty,
          preimages = List.empty,
          preimageRequests = List.empty
        )
      )

    cachedIds match
      case Some(ids) =>
        ids.foreach { id =>
          val cached = store.flatMap(_.cachedServiceInfo(id))
          cached match
            case Some(encoded) =>
              out += materialize(id, encoded)
            case None =>
              val key = encodeServiceAccountKey(id)
              trie.read(key).foreach { encoded =>
                store.foreach(_.putCachedServiceInfo(id, encoded))
                out += materialize(id, encoded)
              }
        }
      case None =>
        val ffPrefix = JamBytes(Array(0xff.toByte))
        val ids = mutable.HashSet.empty[Long]
        trie.getKeyValues(ffPrefix, 8).foreach { case (k, v) =>
          if k(2) == 0 then
            val serviceId =
              ((k(1).toLong & 0xff)) |
                ((k(3).toLong & 0xff) << 8) |
                ((k(5).toLong & 0xff) << 16) |
                ((k(7).toLong & 0xff) << 24)
            ids += serviceId
            store.foreach(_.putCachedServiceInfo(serviceId, v))
            out += materialize(serviceId, v)
        }
        store.foreach(_.primeKnownServiceIds(ids.toSet))
    out.sortBy(_.id).toList


  private def encodeJB[A](codec: Codec[A], value: A): JamBytes =
    JamBytes.fromByteVector(codec.encode(value).require.bytes)

  private def encodeTimeslot(tau: Long): JamBytes =
    encodeJB(FullJamStateCodecs.timeslotCodec, tau)

  private def encodeEntropyPool(pool: List[Hash]): JamBytes =
    encodeJB(FullJamStateCodecs.entropyPoolCodec, pool)

  private def encodeValidatorList(vs: List[ValidatorKey]): JamBytes =
    encodeJB(FullJamStateCodecs.validatorListCodec(vs.length), vs)

  private def encodeAuthPools(pools: List[List[Hash]]): JamBytes =
    encodeJB(FullJamStateCodecs.authPoolsCodec(pools.length), pools)

  private def encodeAuthQueues(
      queues: List[List[Hash]],
      config: ChainConfig
  ): JamBytes =
    val queueSize =
      if queues.nonEmpty then queues.head.length else config.authQueueSize
    encodeJB(FullJamStateCodecs.authQueuesCodec(queues.length, queueSize), queues)

  private def encodeRecentHistory(beta: HistoricalBetaContainer): JamBytes =
    encodeJB(summon[Codec[HistoricalBetaContainer]], beta)

  private def encodeReports(
      reports: List[Option[AvailabilityAssignment]],
      coresCount: Int
  ): JamBytes =
    val padded = reports.padTo(coresCount, None)
    encodeJB(
      FullJamStateCodecs.reportsCodec[AvailabilityAssignment](padded.length),
      padded
    )

  private def encodePsi(psi: Psi): JamBytes =
    encodeJB(summon[Codec[Psi]], psi)

  private def encodeAccumulationQueue(
      queue: List[List[AccumulationReadyRecord]]
  ): JamBytes =
    val codec = JamCodecs.fixedSizeList(
      JamCodecs.compactPrefixedList(summon[Codec[AccumulationReadyRecord]]),
      queue.length
    )
    encodeJB(codec, queue)

  private def encodeAccumulationHistory(
      history: List[List[JamBytes]]
  ): JamBytes =
    val bvForm = history.map(_.map(_.toByteVector))
    encodeJB(FullJamStateCodecs.accumulationHistoryCodec(history.length), bvForm)

  private def encodeLastAccumulationOutputs(
      outputs: List[(Long, JamBytes)]
  ): JamBytes =
    val bvForm = outputs.map { case (id, jb) => (id, jb.toByteVector) }
    JamBytes.fromByteVector(
      FullJamStateCodecs.encodeLastAccumulationOutputs(bvForm)
    )

  private def encodePrivileges(p: Privileges, coresCount: Int): JamBytes =
    encodeJB(Privileges.codec(coresCount), p)

  private def encodeSafroleBlock(
      gammaK: List[ValidatorKey],
      gammaZ: JamBytes,
      gammaS: TicketsOrKeys,
      gammaA: List[TicketMark]
  ): JamBytes =
    val (sData, sLen) = gammaS match
      case TicketsOrKeys.Tickets(t) =>
        (TicketsOrKeysData.Tickets(t), t.length)
      case TicketsOrKeys.Keys(k) =>
        (TicketsOrKeysData.Keys(k), k.length)
    encodeJB(
      FullJamStateCodecs.safroleGammaStateCodec(gammaK.length, sLen),
      (gammaK, gammaZ.toByteVector, sData, gammaA)
    )

  private def encodeActivityStats(
      cur: List[StatCount],
      prev: List[StatCount],
      core: List[CoreStatisticsRecord],
      svc: List[ServiceStatisticsEntry],
      config: ChainConfig
  ): JamBytes =
    val curPadded = cur.padTo(config.validatorCount, StatCount.zero)
    val prevPadded = prev.padTo(config.validatorCount, StatCount.zero)
    val corePadded = core.padTo(config.coresCount, CoreStatisticsRecord.zero)
    val data = FullJamStateCodecs.ActivityStatisticsData(
      accumulator = curPadded.map(fromStatCount),
      previous = prevPadded.map(fromStatCount),
      core = corePadded.map(fromCoreStatistics),
      service = svc.map(fromServiceStatistics)
    )
    encodeJB(
      FullJamStateCodecs
        .activityStatisticsCodec(config.validatorCount, config.coresCount),
      data
    )

  private def fromStatCount(s: StatCount): StatCountData =
    StatCountData(
      s.blocks,
      s.tickets,
      s.preImages,
      s.preImagesSize,
      s.guarantees,
      s.assurances
    )

  private def fromCoreStatistics(c: CoreStatisticsRecord): CoreStatisticsData =
    CoreStatisticsData(
      c.daLoad,
      c.popularity,
      c.imports,
      c.extrinsicCount,
      c.extrinsicSize,
      c.exports,
      c.bundleSize,
      c.gasUsed
    )

  private def fromServiceStatistics(
      e: ServiceStatisticsEntry
  ): ServiceStatisticsData =
    ServiceStatisticsData(
      serviceId = e.id,
      preimagesCount = e.record.providedCount.toLong,
      preimagesSize = e.record.providedSize,
      refinesCount = e.record.refinementCount,
      refinesGas = e.record.refinementGasUsed,
      importsCount = e.record.imports,
      extrinsicsCount = e.record.extrinsicCount,
      extrinsicsSize = e.record.extrinsicSize,
      exportsCount = e.record.exports,
      accumulatesCount = e.record.accumulateCount,
      accumulatesGas = e.record.accumulateGasUsed
    )

  private def encodeServiceInfo(info: ServiceInfo): JamBytes =
    encodeJB(FullJamStateCodecs.serviceInfoCodec, info)

  private def encodeServiceAccountKey(serviceId: Long): JamBytes =
    val key = new Array[Byte](31)
    key(0) = StateKeys.SERVICE_ACCOUNT
    key(1) = (serviceId & 0xff).toByte
    key(3) = ((serviceId >> 8) & 0xff).toByte
    key(5) = ((serviceId >> 16) & 0xff).toByte
    key(7) = ((serviceId >> 24) & 0xff).toByte
    JamBytes(key)
