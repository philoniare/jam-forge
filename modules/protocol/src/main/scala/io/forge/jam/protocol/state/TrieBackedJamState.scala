package io.forge.jam.protocol.state

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{
  Hash,
  Ed25519PublicKey,
  BandersnatchPublicKey,
  BlsPublicKey
}
import io.forge.jam.core.codec.{FullJamStateCodecs, JamCodecs}
import io.forge.jam.core.codec.FullJamStateCodecs.TicketsOrKeysData
import io.forge.jam.core.trie.{StateTrie, StateTrieStore}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.types.workpackage.AvailabilityAssignment
import io.forge.jam.core.types.history.HistoricalBetaContainer
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.protocol.accumulation.{AccumulationReadyRecord, AccumulationServiceData, AccumulationServiceItem, Privileges}
import io.forge.jam.protocol.dispute.DisputeTypes.Psi
import io.forge.jam.protocol.report.ReportTypes.{
  CoreStatisticsRecord,
  ServiceStatisticsEntry
}
import io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys
import io.forge.jam.protocol.statistics.ActivityStatisticsCodec
import io.forge.jam.protocol.statistics.StatisticsTypes.StatCount

import io.forge.jam.core.codec.Codec
import scala.collection.mutable
import scala.compiletime.uninitialized

final class TrieBackedJamState(
    private val trie: StateTrie,
    val config: ChainConfig,
    val storage: ServiceStorageView,
    private val store: Option[StateTrieStore] = None
):

  private val tauCell = StateCell(StateKeys.TIMESLOT, TrieBackedJamState.readTimeslot(trie), TrieBackedJamState.encodeTimeslot)
  private val entropyPoolCell = StateCell(StateKeys.ENTROPY_POOL, TrieBackedJamState.readEntropyPool(trie), TrieBackedJamState.encodeEntropyPool)
  private val kappaCell = StateCell(
    StateKeys.CURRENT_VALIDATORS,
    TrieBackedJamState.readValidators(trie, StateKeys.CURRENT_VALIDATORS, config.validatorCount),
    TrieBackedJamState.encodeValidatorList
  )
  private val lambdaCell = StateCell(
    StateKeys.PREVIOUS_VALIDATORS,
    TrieBackedJamState.readValidators(trie, StateKeys.PREVIOUS_VALIDATORS, config.validatorCount),
    TrieBackedJamState.encodeValidatorList
  )
  private val iotaCell = StateCell(
    StateKeys.VALIDATOR_QUEUE,
    TrieBackedJamState.readValidators(trie, StateKeys.VALIDATOR_QUEUE, config.validatorCount),
    TrieBackedJamState.encodeValidatorList
  )
  private val authPoolsCell =
    StateCell(StateKeys.CORE_AUTHORIZATION_POOL, TrieBackedJamState.readAuthPools(trie, config), TrieBackedJamState.encodeAuthPools)
  private val authQueuesCell = StateCell(
    StateKeys.AUTHORIZATION_QUEUE,
    TrieBackedJamState.readAuthQueues(trie, config),
    TrieBackedJamState.encodeAuthQueues(_, config)
  )
  private val betaCell = StateCell(StateKeys.RECENT_HISTORY, TrieBackedJamState.readRecentHistory(trie), TrieBackedJamState.encodeRecentHistory)
  private val reportsCell = StateCell(
    StateKeys.REPORTS,
    TrieBackedJamState.readReports(trie, config.coresCount),
    TrieBackedJamState.encodeReports(_, config.coresCount)
  )
  private val readyQueueCell = StateCell(
    StateKeys.ACCUMULATION_QUEUE,
    TrieBackedJamState.readAccumulationQueue(trie, config.epochLength),
    TrieBackedJamState.encodeAccumulationQueue
  )
  private val accumulatedCell = StateCell(
    StateKeys.ACCUMULATION_HISTORY,
    TrieBackedJamState.readAccumulationHistory(trie, config.epochLength),
    TrieBackedJamState.encodeAccumulationHistory
  )
  private val lastAccumulationOutputsCell = StateCell(
    StateKeys.LAST_ACCUMULATION_OUTPUTS,
    TrieBackedJamState.readLastAccumulationOutputs(trie),
    TrieBackedJamState.encodeLastAccumulationOutputs
  )
  private val privilegesCell = StateCell(
    StateKeys.PRIVILEGED_SERVICES,
    TrieBackedJamState.readPrivileges(trie, config.coresCount),
    TrieBackedJamState.encodePrivileges(_, config.coresCount)
  )
  private val psiCell = StateCell(StateKeys.JUDGEMENTS, TrieBackedJamState.readPsi(trie), TrieBackedJamState.encodePsi)

  // In commit order.
  private val cells: Array[StateCell[?]] = Array(
    tauCell, entropyPoolCell, kappaCell, lambdaCell, iotaCell, authPoolsCell, authQueuesCell, betaCell, reportsCell,
    readyQueueCell, accumulatedCell, lastAccumulationOutputsCell, privilegesCell, psiCell
  )


  private var _gammaK: IndexedSeq[ValidatorKey] =
    IndexedSeq.fill(config.validatorCount)(TrieBackedJamState.emptyValidator)
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


  private val _accountChanges = mutable.LinkedHashMap.empty[Long, Option[ServiceInfo]]


  private var _postOffenders: List[Ed25519PublicKey] = List.empty


  def timeslot: Long = tauCell.get

  def timeslot_=(v: Long): Unit =
    tauCell.set(v)

  def authPools: List[List[Hash]] = authPoolsCell.get

  def authPools_=(v: List[List[Hash]]): Unit =
    authPoolsCell.set(v)

  def authQueues: List[List[Hash]] = authQueuesCell.get

  def authQueues_=(v: List[List[Hash]]): Unit =
    authQueuesCell.set(v)

  def psi: Psi = psiCell.get

  def psi_=(v: Psi): Unit =
    psiCell.set(v)

  def beta: HistoricalBetaContainer = betaCell.get

  def beta_=(v: HistoricalBetaContainer): Unit =
    betaCell.set(v)

  def serviceStatistics: List[ServiceStatisticsEntry] =
    if !_activityStatsLoaded then loadActivityStats()
    _serviceStatistics

  def serviceStatistics_=(v: List[ServiceStatisticsEntry]): Unit =
    if !_activityStatsLoaded then loadActivityStats()
    if _serviceStatistics != v then
      _serviceStatistics = v
      _activityStatsDirty = true

  def postOffenders: List[Ed25519PublicKey] = _postOffenders

  def postOffenders_=(v: List[Ed25519PublicKey]): Unit = _postOffenders = v

  def lastAccumulationOutputs: List[(Long, JamBytes)] = lastAccumulationOutputsCell.get

  def lastAccumulationOutputs_=(v: List[(Long, JamBytes)]): Unit =
    lastAccumulationOutputsCell.set(v)


  val validators: ValidatorsRef = new ValidatorsRef(this)
  val entropy: EntropyRef = new EntropyRef(this)
  val gamma: GammaRef = new GammaRef(this)
  val cores: CoresRef = new CoresRef(this)
  val accumulation: AccumulationRef = new AccumulationRef(this)
  val statistics: StatisticsRef = new StatisticsRef(this)


  private[state] def kappa: IndexedSeq[ValidatorKey] = kappaCell.get

  private[state] def kappa_=(v: IndexedSeq[ValidatorKey]): Unit =
    kappaCell.set(v)

  private[state] def lambda: IndexedSeq[ValidatorKey] = lambdaCell.get

  private[state] def lambda_=(v: IndexedSeq[ValidatorKey]): Unit =
    lambdaCell.set(v)

  private[state] def iota: IndexedSeq[ValidatorKey] = iotaCell.get

  private[state] def iota_=(v: IndexedSeq[ValidatorKey]): Unit =
    iotaCell.set(v)

  private[state] def gammaK: IndexedSeq[ValidatorKey] =
    if !_safroleLoaded then loadSafrole()
    _gammaK

  private[state] def gammaK_=(v: IndexedSeq[ValidatorKey]): Unit =
    if !_safroleLoaded then loadSafrole()
    if _gammaK != v then
      _gammaK = v
      _safroleDirty = true

  private[state] def gammaZ: JamBytes =
    if !_safroleLoaded then loadSafrole()
    _gammaZ

  private[state] def gammaZ_=(v: JamBytes): Unit =
    if !_safroleLoaded then loadSafrole()
    if _gammaZ != v then
      _gammaZ = v
      _safroleDirty = true

  private[state] def gammaS: TicketsOrKeys =
    if !_safroleLoaded then loadSafrole()
    _gammaS

  private[state] def gammaS_=(v: TicketsOrKeys): Unit =
    if !_safroleLoaded then loadSafrole()
    if _gammaS != v then
      _gammaS = v
      _safroleDirty = true

  private[state] def gammaA: List[TicketMark] =
    if !_safroleLoaded then loadSafrole()
    _gammaA

  private[state] def gammaA_=(v: List[TicketMark]): Unit =
    if !_safroleLoaded then loadSafrole()
    if _gammaA != v then
      _gammaA = v
      _safroleDirty = true

  private[state] def entropyPool: List[Hash] = entropyPoolCell.get

  private[state] def entropyPool_=(v: List[Hash]): Unit =
    require(v.size == 4, s"entropy pool must be 4 hashes, got ${v.size}")
    entropyPoolCell.set(v)

  private[state] def coreReports: List[Option[AvailabilityAssignment]] = reportsCell.get

  private[state] def coreReports_=(v: List[Option[AvailabilityAssignment]]): Unit =
    reportsCell.set(v)

  private[state] def coreStatistics: List[CoreStatisticsRecord] =
    if !_activityStatsLoaded then loadActivityStats()
    _coreStatistics

  private[state] def coreStatistics_=(v: List[CoreStatisticsRecord]): Unit =
    if !_activityStatsLoaded then loadActivityStats()
    if _coreStatistics != v then
      _coreStatistics = v
      _activityStatsDirty = true

  private[state] def readyQueue: List[List[AccumulationReadyRecord]] = readyQueueCell.get

  private[state] def readyQueue_=(v: List[List[AccumulationReadyRecord]]): Unit =
    readyQueueCell.set(v)

  private[state] def accumulated: List[List[JamBytes]] = accumulatedCell.get

  private[state] def accumulated_=(v: List[List[JamBytes]]): Unit =
    accumulatedCell.set(v)

  private[state] def privileges: Privileges = privilegesCell.get

  private[state] def privileges_=(v: Privileges): Unit =
    privilegesCell.set(v)

  private val createdAt: Hash = trie.rootHash

  private def cacheStore: Option[StateTrieStore] =
    if store.isDefined && trie.rootHash == createdAt then store else None

  private[state] def priorServiceInfoEncoded(id: Long): Option[JamBytes] =
    val s = cacheStore
    s.flatMap(_.cachedServiceInfo(createdAt, id)).orElse {
      val read = trie.read(StateKey.computeServiceAccountKey(id))
      for
        st <- s
        encoded <- read
      do st.putCachedServiceInfo(createdAt, id, encoded)
      read
    }

  private[state] def priorServiceInfo(id: Long): Option[ServiceInfo] =
    priorServiceInfoEncoded(id).map(e => FullJamStateCodecs.decodeServiceInfo(e.toArray))

  private[state] def serviceInfo(id: Long): Option[ServiceInfo] =
    _accountChanges.get(id) match
      case Some(change) => change
      case None         => priorServiceInfo(id)

  private[state] def accountChanges: Map[Long, Option[ServiceInfo]] = _accountChanges.toMap

  private[state] def setServiceInfo(id: Long, info: ServiceInfo): Unit = _accountChanges(id) = Some(info)

  private[state] def removeService(id: Long): Unit = _accountChanges(id) = None

  private[state] def serviceIds: Set[Long] =
    val prior = TrieBackedJamState.readServiceIds(trie, cacheStore, createdAt)
    _accountChanges.foldLeft(prior) {
      case (ids, (id, Some(_))) => ids + id
      case (ids, (id, None))    => ids - id
    }

  private[state] def serviceAccounts: List[AccumulationServiceItem] =
    serviceIds.toList.sorted.flatMap(id => serviceInfo(id).map(TrieBackedJamState.accountItem(id, _)))

  private[state] def serviceAccounts_=(v: List[AccumulationServiceItem]): Unit =
    val wanted = v.iterator.map(_.id).toSet
    v.foreach(item => if !serviceInfo(item.id).contains(item.data.service) then setServiceInfo(item.id, item.data.service))
    serviceIds.foreach(id => if !wanted.contains(id) then removeService(id))

  private def accountChanged(id: Long, change: Option[ServiceInfo]): Boolean =
    change match
      case Some(info) => !priorServiceInfo(id).contains(info)
      case None       => priorServiceInfoEncoded(id).isDefined

  private[state] def statsCurrent: List[StatCount] =
    if !_activityStatsLoaded then loadActivityStats()
    _statsCurrent

  private[state] def statsCurrent_=(v: List[StatCount]): Unit =
    if !_activityStatsLoaded then loadActivityStats()
    if _statsCurrent != v then
      _statsCurrent = v
      _activityStatsDirty = true

  private[state] def statsLast: List[StatCount] =
    if !_activityStatsLoaded then loadActivityStats()
    _statsLast

  private[state] def statsLast_=(v: List[StatCount]): Unit =
    if !_activityStatsLoaded then loadActivityStats()
    if _statsLast != v then
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
    val stats = TrieBackedJamState.readActivityStats(trie, config)
    _statsCurrent = stats.accumulator
    _statsLast = stats.previous
    _coreStatistics = stats.core
    _serviceStatistics = stats.service
    _activityStatsLoaded = true



  
  /** How many components [[commit]] would re-encode (all service accounts count as one). */
  private[protocol] def dirtyComponents: Int =
    cells.count(_.dirty) + Seq(_safroleDirty, _activityStatsDirty, _accountChanges.exists(accountChanged(_, _))).count(identity)

  def commit(target: StateTrie): Unit =
    val updates = commitUpdates()
    if updates.nonEmpty then target.update(updates.toIndexedSeq)

  private val committedAccountRecords = mutable.ArrayBuffer.empty[(Long, Option[JamBytes])]

  /**
   * The trie writes [[commit]] would apply, re-encoding only dirty components, so a caller can merge them with other
   * writes into one [[StateTrie.update]] and rebuild each touched node once per block. The store's caches are not
   * touched: the account records written are kept in [[accountRecordChanges]], for the importer to hand to
   * [[StateTrieStore.commitChild]] once the new state is saved and pinned.
   */
  def commitUpdates(): mutable.ArrayBuffer[(JamBytes, Option[JamBytes])] =
    val updates =
      mutable.ArrayBuffer.empty[(JamBytes, Option[JamBytes])]
    committedAccountRecords.clear()

    cells.foreach { c =>
      if c.dirty then updates += ((StateKeys.simpleKey(c.prefix), Some(c.encoded)))
    }
    if _safroleDirty then
      updates += ((StateKeys.simpleKey(StateKeys.SAFROLE_STATE),
        Some(TrieBackedJamState.encodeSafroleBlock(_gammaK, _gammaZ, _gammaS, _gammaA))))
    if _activityStatsDirty then
      updates += ((StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS),
        Some(TrieBackedJamState.encodeActivityStats(
          cur = _statsCurrent,
          prev = _statsLast,
          core = _coreStatistics,
          svc = _serviceStatistics,
          config = config))))
    _accountChanges.foreach {
      case (id, Some(info)) =>
        val prior = priorServiceInfoEncoded(id)
        val encoded = TrieBackedJamState.encodeServiceInfo(info)
        if !prior.contains(encoded) then
          updates += ((StateKey.computeServiceAccountKey(id), Some(encoded)))
          committedAccountRecords += ((id, Some(encoded)))
      case (id, None) =>
        if priorServiceInfoEncoded(id).isDefined then
          updates += ((StateKey.computeServiceAccountKey(id), None))
          committedAccountRecords += ((id, None))
    }

    updates

  def accountRecordChanges: Seq[(Long, Option[JamBytes])] = committedAccountRecords.toSeq

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
          .decodeUnsafe(v.toArrayUnsafe)

  private def readEntropyPool(trie: StateTrie): List[Hash] =
    trie.read(StateKeys.simpleKey(StateKeys.ENTROPY_POOL)) match
      case None    => List.fill(4)(Hash.zero)
      case Some(v) =>
        FullJamStateCodecs.entropyPoolCodec
          .decodeUnsafe(v.toArrayUnsafe)

  private def readValidators(
      trie: StateTrie,
      prefix: Byte,
      validatorCount: Int
  ): IndexedSeq[ValidatorKey] =
    decodedValidators.read(trie, prefix, ()) { v =>
      FullJamStateCodecs
        .validatorListCodec
        .decodeUnsafe(v.toArrayUnsafe)
    }.getOrElse(IndexedSeq.fill(validatorCount)(emptyValidator))

  private val decodedValidators = new DecodedOnce[IndexedSeq[ValidatorKey]](slots = 4)
  private val decodedReports = new DecodedOnce[List[Option[AvailabilityAssignment]]](slots = 2)
  private val decodedSafrole =
    new DecodedOnce[(IndexedSeq[ValidatorKey], JamBytes, TicketsOrKeys, List[TicketMark])](slots = 2)
  private val decodedAuthQueues = new DecodedOnce[List[List[Hash]]](slots = 2)
  private val decodedAuthPools = new DecodedOnce[List[List[Hash]]](slots = 2)

  private final class DecodedOnce[A](slots: Int):
    private final case class Entry(hash: Hash, shape: Any, value: A)
    private val entries = new java.util.concurrent.atomic.AtomicReferenceArray[Entry](slots)
    private val next = new java.util.concurrent.atomic.AtomicInteger(0)

    def read(trie: StateTrie, prefix: Byte, shape: Any)(decode: JamBytes => A): Option[A] =
      val key = StateKeys.simpleKey(prefix)
      trie.valueHash(key) match
        case None => trie.read(key).map(decode)
        case Some(hash) =>
          var i = 0
          var found: Entry = null
          while found == null && i < slots do
            val e = entries.get(i)
            if e != null && e.hash == hash && e.shape == shape then found = e
            i += 1
          if found != null then Some(found.value)
          else
            trie.read(key).map { v =>
              val value = decode(v)
              entries.set(math.floorMod(next.getAndIncrement(), slots), Entry(hash, shape, value))
              value
            }

  private def readAuthPools(
      trie: StateTrie,
      config: ChainConfig
  ): List[List[Hash]] =
    decodedAuthPools
      .read(trie, StateKeys.CORE_AUTHORIZATION_POOL, config.coresCount) { v =>
        FullJamStateCodecs.decodeAuthPools(v.toArrayUnsafe, config.coresCount)
      }
      .getOrElse(List.fill(config.coresCount)(List.empty))

  private def readAuthQueues(
      trie: StateTrie,
      config: ChainConfig
  ): List[List[Hash]] =
    decodedAuthQueues
      .read(trie, StateKeys.AUTHORIZATION_QUEUE, (config.coresCount, config.authQueueSize)) { v =>
        FullJamStateCodecs.decodeAuthQueues(v.toArrayUnsafe, config.coresCount, config.authQueueSize)
      }
      .getOrElse(List.fill(config.coresCount)(List.fill(config.authQueueSize)(Hash.zero)))

  private def readPsi(trie: StateTrie): Psi =
    trie.read(StateKeys.simpleKey(StateKeys.JUDGEMENTS)) match
      case None    => Psi.empty
      case Some(v) =>
        summon[Codec[Psi]]
          .decodeUnsafe(v.toArrayUnsafe)

  private def readRecentHistory(trie: StateTrie): HistoricalBetaContainer =
    trie.read(StateKeys.simpleKey(StateKeys.RECENT_HISTORY)) match
      case None    => HistoricalBetaContainer()
      case Some(v) =>
        summon[Codec[HistoricalBetaContainer]]
          .decodeUnsafe(v.toArrayUnsafe)

  private def readReports(
      trie: StateTrie,
      coresCount: Int
  ): List[Option[AvailabilityAssignment]] =
    decodedReports.read(trie, StateKeys.REPORTS, coresCount) { v =>
      FullJamStateCodecs
        .reportsCodec[AvailabilityAssignment](coresCount)
        .decodeUnsafe(v.toArrayUnsafe)
    }.getOrElse(List.fill(coresCount)(None))

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
        codec.decodeUnsafe(v.toArrayUnsafe)

  private def readAccumulationHistory(
      trie: StateTrie,
      epochLength: Int
  ): List[List[JamBytes]] =
    trie.read(StateKeys.simpleKey(StateKeys.ACCUMULATION_HISTORY)) match
      case None    => List.fill(epochLength)(List.empty)
      case Some(v) =>
        FullJamStateCodecs
          .decodeAccumulationHistory(v.toArrayUnsafe, epochLength)

  private def readLastAccumulationOutputs(
      trie: StateTrie
  ): List[(Long, JamBytes)] =
    trie.read(StateKeys.simpleKey(StateKeys.LAST_ACCUMULATION_OUTPUTS)) match
      case None    => List.empty
      case Some(v) =>
        FullJamStateCodecs
          .decodeLastAccumulationOutputs(v.toArrayUnsafe)

  private def readPrivileges(trie: StateTrie, coresCount: Int): Privileges =
    trie.read(StateKeys.simpleKey(StateKeys.PRIVILEGED_SERVICES)) match
      case None =>
        Privileges(0, List.fill(coresCount)(0L), 0, 0, List.empty)
      case Some(v) =>
        Privileges
          .codec(coresCount)
          .decodeUnsafe(v.toArrayUnsafe)

  private def readSafroleBlock(
      trie: StateTrie,
      config: ChainConfig
  ): (IndexedSeq[ValidatorKey], JamBytes, TicketsOrKeys, List[TicketMark]) =
    decodedSafrole.read(trie, StateKeys.SAFROLE_STATE, config.epochLength) { v =>
      val (k, z, sData, a) =
        FullJamStateCodecs
          .safroleGammaStateCodec(config.epochLength)
          .decodeUnsafe(v.toArrayUnsafe)
      val s = sData match
        case TicketsOrKeysData.Tickets(t) => TicketsOrKeys.Tickets(t)
        case TicketsOrKeysData.Keys(ks)   => TicketsOrKeys.Keys(ks)
      (k, z, s, a)
    }.getOrElse(
      (
        IndexedSeq.fill(config.validatorCount)(emptyValidator),
        JamBytes.zeros(FullJamStateCodecs.BandersnatchRingCommitmentSize),
        TicketsOrKeys.Keys(
          List.fill(config.epochLength)(BandersnatchPublicKey.zero)
        ),
        List.empty
      )
    )

  private def readActivityStats(
      trie: StateTrie,
      config: ChainConfig
  ): ActivityStatisticsCodec.ActivityStatistics =
    trie.read(StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS)) match
      case None =>
        ActivityStatisticsCodec.ActivityStatistics(
          accumulator = List.fill(config.validatorCount)(StatCount.zero),
          previous = List.fill(config.validatorCount)(StatCount.zero),
          core = List.fill(config.coresCount)(CoreStatisticsRecord.zero),
          service = List.empty
        )
      case Some(v) =>
        ActivityStatisticsCodec.decodeActivityStatistics(
          v.toArray,
          config.coresCount
        )

  private def accountItem(id: Long, info: ServiceInfo): AccumulationServiceItem =
    AccumulationServiceItem(
      id = id,
      data = AccumulationServiceData(
        service = info,
        storage = List.empty,
        preimages = List.empty,
        preimageRequests = List.empty
      )
    )

  private def readServiceIds(trie: StateTrie, store: Option[StateTrieStore], root: Hash): Set[Long] =
    store.flatMap(_.cachedServiceIds(root)) match
      case Some(ids) => ids
      case None =>
        val ffPrefix = JamBytes(Array(0xff.toByte))
        val ids = mutable.HashSet.empty[Long]
        val records = mutable.ArrayBuffer.empty[(Long, JamBytes)]
        def isAccountRecordKey(arr: Array[Byte]): Boolean =
          if (arr(0) & 0xff) != 0xff then false
          else if arr(2) != 0 || arr(4) != 0 || arr(6) != 0 then false
          else
            var i = 8
            while i < arr.length do
              if arr(i) != 0 then return false
              i += 1
            true
        trie.getKeyValues(ffPrefix, 8).foreach { case (k, v) =>
          if isAccountRecordKey(k.toArray) then
            val serviceId =
              ((k(1).toLong & 0xff)) |
                ((k(3).toLong & 0xff) << 8) |
                ((k(5).toLong & 0xff) << 16) |
                ((k(7).toLong & 0xff) << 24)
            ids += serviceId
            if store.isDefined then records += ((serviceId, v))
        }
        val idSet = ids.toSet
        store.foreach { s =>
          s.putCachedServiceInfos(root, records)
          s.primeKnownServiceIds(root, idSet)
        }
        idSet

  private def encodeJB[A](codec: Codec[A], value: A): JamBytes =
    codec.encodeJam(value)

  private def encodeTimeslot(tau: Long): JamBytes =
    encodeJB(FullJamStateCodecs.timeslotCodec, tau)

  private def encodeEntropyPool(pool: List[Hash]): JamBytes =
    encodeJB(FullJamStateCodecs.entropyPoolCodec, pool)

  private def encodeValidatorList(vs: IndexedSeq[ValidatorKey]): JamBytes =
    encodeJB(FullJamStateCodecs.validatorListCodec, vs)

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
    encodeJB(FullJamStateCodecs.accumulationHistoryCodec(history.length), history)

  private def encodeLastAccumulationOutputs(
      outputs: List[(Long, JamBytes)]
  ): JamBytes =
    FullJamStateCodecs.encodeLastAccumulationOutputs(outputs)

  private def encodePrivileges(p: Privileges, coresCount: Int): JamBytes =
    encodeJB(Privileges.codec(coresCount), p)

  private def encodeSafroleBlock(
      gammaK: IndexedSeq[ValidatorKey],
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
      FullJamStateCodecs.safroleGammaStateCodec(sLen),
      (gammaK, gammaZ, sData, gammaA)
    )

  private def encodeActivityStats(
      cur: List[StatCount],
      prev: List[StatCount],
      core: List[CoreStatisticsRecord],
      svc: List[ServiceStatisticsEntry],
      config: ChainConfig
  ): JamBytes =
    // |pi_V| = |kappa| and |pi_L| = |lambda| (GP 0.8.0 §statistics), which differ from
    // `config.validatorCount` once a designate has resized the set, so never pad them to it.
    val corePadded = core.padTo(config.coresCount, CoreStatisticsRecord.zero)
    encodeJB(
      ActivityStatisticsCodec
        .activityStatisticsCodec(config.coresCount),
      ActivityStatisticsCodec.ActivityStatistics(
        accumulator = cur,
        previous = prev,
        core = corePadded,
        service = svc
      )
    )

  private def encodeServiceInfo(info: ServiceInfo): JamBytes =
    encodeJB(FullJamStateCodecs.serviceInfoCodec, info)

/** A state component read from the trie on first use and re-encoded at commit only if set to a different value. */
private[state] final class StateCell[A](val prefix: Byte, read: => A, write: A => JamBytes):
  private var value: A = uninitialized
  private var loaded = false
  private var changed = false

  def get: A =
    if !loaded then
      value = read
      loaded = true
    value

  def set(v: A): Unit =
    if !loaded || value != v then
      value = v
      loaded = true
      changed = true

  def dirty: Boolean = changed

  def encoded: JamBytes = write(value)
