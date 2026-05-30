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
import io.forge.jam.core.types.workpackage.AvailabilityAssignment
import io.forge.jam.core.types.history.HistoricalBetaContainer
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.dispute.DisputeTypes.Psi
import io.forge.jam.protocol.accumulation.{
  AccumulationServiceItem,
  AccumulationServiceData,
  AccumulationReadyRecord,
  Privileges,
  StateKey
}
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.protocol.report.ReportTypes.{
  CoreStatisticsRecord,
  ServiceStatisticsEntry
}
import io.forge.jam.protocol.statistics.StatisticsTypes.StatCount
import org.slf4j.LoggerFactory
import _root_.scodec.bits.BitVector
import _root_.scodec.Codec
import io.forge.jam.core.scodec.FullJamStateCodecs
import io.forge.jam.core.trie.StateTrie

/** Unified JAM state container holding all state components.
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
    privilegedServices: Privileges =
      Privileges(0, List.empty, 0, 0, List.empty),

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

    // Last accumulation outputs (service commitments)
    lastAccumulationOutputs: List[(Long, JamBytes)] = List.empty,

    // Raw keyvals for all state components (for pass-through when unchanged)
    otherKeyvals: List[KeyValue] = List.empty,

    // Original keyvals by full key (for preserving exact bytes when unchanged)
    originalKeyvals: Map[JamBytes, KeyValue] = Map.empty,

    // Raw service data by state key (for preimages, storage, etc.)
    rawServiceDataByStateKey: Map[JamBytes, JamBytes] = Map.empty
):

  /** Convert back to raw keyvals for state root computation.
    */
  def toKeyvals(
      config: ChainConfig,
      preState: Option[FullJamState] = None
  ): List[KeyValue] =
    val builder = scala.collection.mutable.ListBuffer[KeyValue]()

    // Build a map of original keyvals from otherKeyvals by prefix for pass-through
    val otherByPrefix = otherKeyvals.groupBy(kv => kv.key(0).toInt & 0xff)

    inline def emitSimple[A <: AnyRef](
        prefix: Byte,
        current: A,
        pre: FullJamState => A,
        reencode: => JamBytes
    ): Unit =
      val key = StateKeys.simpleKey(prefix)
      val cached: Option[JamBytes] = preState.flatMap { ps =>
        if (current eq pre(ps)) then originalKeyvals.get(key).map(_.value)
        else None
      }
      builder += KeyValue(key, cached.getOrElse(reencode))

    emitSimple[List[List[Hash]]](
      StateKeys.CORE_AUTHORIZATION_POOL,
      authPools,
      _.authPools,
      encodeAuthPools(authPools)
    )
    emitSimple[List[List[Hash]]](
      StateKeys.AUTHORIZATION_QUEUE,
      authQueues,
      _.authQueues,
      encodeAuthQueues(authQueues)
    )
    emitSimple[HistoricalBetaContainer](
      StateKeys.RECENT_HISTORY,
      recentHistory,
      _.recentHistory,
      JamBytes.fromByteVector(
        summon[Codec[HistoricalBetaContainer]]
          .encode(recentHistory)
          .require
          .bytes
      )
    )

    // Safrole gamma state
    val safroleKey = StateKeys.simpleKey(StateKeys.SAFROLE_STATE)
    val safroleClean = preState.exists { ps =>
      // gammaZ is a JamBytes value class — use content equality (cheap, 144 bytes).
      // The list refs use eq to stay O(1).
      (safroleGammaK eq ps.safroleGammaK) &&
      (safroleGammaS eq ps.safroleGammaS) &&
      (safroleGammaA eq ps.safroleGammaA) &&
      (safroleGammaZ == ps.safroleGammaZ)
    }
    val safroleCached =
      if safroleClean then originalKeyvals.get(safroleKey).map(_.value)
      else None
    builder += KeyValue(
      safroleKey,
      safroleCached.getOrElse(
        encodeSafroleGammaState(
          safroleGammaK,
          safroleGammaZ,
          safroleGammaS,
          safroleGammaA
        )
      )
    )

    otherByPrefix
      .get(StateKeys.JUDGEMENTS.toInt & 0xff)
      .foreach(kvs => builder ++= kvs)

    emitSimple[List[Hash]](
      StateKeys.ENTROPY_POOL,
      entropyPool,
      _.entropyPool,
      encodeEntropyPool(entropyPool)
    )
    emitSimple[List[ValidatorKey]](
      StateKeys.VALIDATOR_QUEUE,
      validatorQueue,
      _.validatorQueue,
      encodeValidatorList(validatorQueue)
    )
    emitSimple[List[ValidatorKey]](
      StateKeys.CURRENT_VALIDATORS,
      currentValidators,
      _.currentValidators,
      encodeValidatorList(currentValidators)
    )
    emitSimple[List[ValidatorKey]](
      StateKeys.PREVIOUS_VALIDATORS,
      previousValidators,
      _.previousValidators,
      encodeValidatorList(previousValidators)
    )

    // Reports — pad lazily, only when re-encoding fires.
    val reportsKey = StateKeys.simpleKey(StateKeys.REPORTS)
    val reportsClean = preState.exists(ps => reports eq ps.reports)
    val reportsCached =
      if reportsClean then originalKeyvals.get(reportsKey).map(_.value)
      else None
    builder += KeyValue(
      reportsKey,
      reportsCached.getOrElse {
        val paddedReports = reports.padTo(config.coresCount, None)
        encodeReports(paddedReports)
      }
    )

    // Timeslot — always changes; cheap to encode (4 bytes).
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.TIMESLOT),
      encodeTimeslot(timeslot)
    )

    emitSimple[Privileges](
      StateKeys.PRIVILEGED_SERVICES,
      privilegedServices,
      _.privilegedServices,
      encodePrivilegedServices(config)
    )

    // Activity statistics depend on coreStatistics and serviceStatistics which
    // are recomputed every block, so always re-encode.
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS),
      encodeActivityStatistics(config)
    )

    emitSimple[List[List[AccumulationReadyRecord]]](
      StateKeys.ACCUMULATION_QUEUE,
      accumulationQueue,
      _.accumulationQueue,
      encodeAccumulationQueue(accumulationQueue)
    )
    emitSimple[List[List[JamBytes]]](
      StateKeys.ACCUMULATION_HISTORY,
      accumulationHistory,
      _.accumulationHistory,
      encodeAccumulationHistory(accumulationHistory)
    )
    emitSimple[List[(Long, JamBytes)]](
      StateKeys.LAST_ACCUMULATION_OUTPUTS,
      lastAccumulationOutputs,
      _.lastAccumulationOutputs,
      encodeLastAccumulationOutputs(lastAccumulationOutputs)
    )

    val preInfoById: scala.collection.Map[Long, ServiceInfo] =
      preState match
        case Some(ps) =>
          val m = scala.collection.mutable.LongMap.empty[ServiceInfo]
          var sa = ps.serviceAccounts
          while sa.nonEmpty do
            m(sa.head.id) = sa.head.data.service
            sa = sa.tail
          m
        case None => Map.empty

    val infoCodec = summon[Codec[ServiceInfo]]
    var sa = serviceAccounts
    while sa.nonEmpty do
      val item = sa.head
      sa = sa.tail
      val serviceKey = StateKey.computeServiceAccountKey(item.id)
      val infoUnchanged =
        preInfoById.get(item.id).exists(_ eq item.data.service)
      val encoded =
        if infoUnchanged then
          originalKeyvals
            .get(serviceKey)
            .map(_.value)
            .getOrElse(
              JamBytes.fromByteVector(
                infoCodec.encode(item.data.service).require.bytes
              )
            )
        else
          JamBytes.fromByteVector(
            infoCodec.encode(item.data.service).require.bytes
          )
      builder += KeyValue(serviceKey, encoded)

    // Service storage/preimage/request data — sorted by raw byte order.
    val storageDataByKey = rawServiceDataByStateKey.filter { case (key, _) =>
      StateKeys.isServiceDataKeyFull(key)
    }
    val sortedStorage = storageDataByKey.toArray
    java.util.Arrays.sort(
      sortedStorage,
      (a: (JamBytes, JamBytes), b: (JamBytes, JamBytes)) =>
        a._1.bytes.compare(b._1.bytes)
    )
    var i = 0
    while i < sortedStorage.length do
      val (key, value) = sortedStorage(i)
      builder += KeyValue(key, value)
      i += 1

    builder.toList

  // ============================================================================
  // Encoding helpers - all use FullJamStateCodecs for consistency
  // ============================================================================

  /** Helper to encode a value using a codec and convert to JamBytes. */
  private def encode[A](codec: Codec[A], value: A): JamBytes =
    JamBytes.fromByteVector(codec.encode(value).require.bytes)

  /** Encode authorization pools using FullJamStateCodecs. */
  private def encodeAuthPools(pools: List[List[Hash]]): JamBytes =
    encode(FullJamStateCodecs.authPoolsCodec(pools.length), pools)

  /** Encode timeslot using FullJamStateCodecs. */
  private def encodeTimeslot(tau: Long): JamBytes =
    encode(FullJamStateCodecs.timeslotCodec, tau)

  /** Encode entropy pool using FullJamStateCodecs. */
  private def encodeEntropyPool(eta: List[Hash]): JamBytes =
    encode(FullJamStateCodecs.entropyPoolCodec, eta)

  /** Encode validator list using FullJamStateCodecs. */
  private def encodeValidatorList(validators: List[ValidatorKey]): JamBytes =
    encode(FullJamStateCodecs.validatorListCodec(validators.length), validators)

  /** Encode Safrole gamma state using FullJamStateCodecs. */
  private def encodeSafroleGammaState(
      gammaK: List[ValidatorKey],
      gammaZ: JamBytes,
      gammaS: TicketsOrKeys,
      gammaA: List[TicketMark]
  ): JamBytes =
    val (gammaS_data, gammaS_length) = gammaS match
      case TicketsOrKeys.Tickets(tickets) =>
        (FullJamStateCodecs.TicketsOrKeysData.Tickets(tickets), tickets.length)
      case TicketsOrKeys.Keys(keys) =>
        (FullJamStateCodecs.TicketsOrKeysData.Keys(keys), keys.length)
    encode(
      FullJamStateCodecs.safroleGammaStateCodec(gammaK.length, gammaS_length),
      (gammaK, gammaZ.toByteVector, gammaS_data, gammaA)
    )

  /** Encode authorization queues using FullJamStateCodecs. */
  private def encodeAuthQueues(queues: List[List[Hash]]): JamBytes =
    val queueSize = if queues.nonEmpty then queues.head.length else 80
    encode(FullJamStateCodecs.authQueuesCodec(queues.length, queueSize), queues)

  /** Encode reports using FullJamStateCodecs. */
  private def encodeReports(
      reports: List[Option[AvailabilityAssignment]]
  ): JamBytes =
    encode(
      FullJamStateCodecs.reportsCodec(reports.length)(using
        summon[Codec[AvailabilityAssignment]]
      ),
      reports
    )

  /** Encode activity statistics using FullJamStateCodecs. */
  private def encodeActivityStatistics(config: ChainConfig): JamBytes =
    val paddedCurrent =
      activityStatsCurrent.padTo(config.validatorCount, StatCount.zero)
    val paddedLast =
      activityStatsLast.padTo(config.validatorCount, StatCount.zero)
    val paddedCoreStats =
      coreStatistics.padTo(config.coresCount, CoreStatisticsRecord.zero)

    val statsData = FullJamStateCodecs.ActivityStatisticsData(
      accumulator = paddedCurrent.map(s =>
        FullJamStateCodecs.StatCountData(
          s.blocks,
          s.tickets,
          s.preImages,
          s.preImagesSize,
          s.guarantees,
          s.assurances
        )
      ),
      previous = paddedLast.map(s =>
        FullJamStateCodecs.StatCountData(
          s.blocks,
          s.tickets,
          s.preImages,
          s.preImagesSize,
          s.guarantees,
          s.assurances
        )
      ),
      core = paddedCoreStats.map(c =>
        FullJamStateCodecs.CoreStatisticsData(
          c.daLoad,
          c.popularity,
          c.imports,
          c.extrinsicCount,
          c.extrinsicSize,
          c.exports,
          c.bundleSize,
          c.gasUsed
        )
      ),
      service = serviceStatistics.map(e =>
        FullJamStateCodecs.ServiceStatisticsData(
          e.id,
          e.record.providedCount.toLong,
          e.record.providedSize,
          e.record.refinementCount,
          e.record.refinementGasUsed,
          e.record.imports,
          e.record.extrinsicCount,
          e.record.extrinsicSize,
          e.record.exports,
          e.record.accumulateCount,
          e.record.accumulateGasUsed
        )
      )
    )
    encode(
      FullJamStateCodecs
        .activityStatisticsCodec(config.validatorCount, config.coresCount),
      statsData
    )

  /** Encode privileged services using Privileges codec. */
  private def encodePrivilegedServices(config: ChainConfig): JamBytes =
    encode(Privileges.codec(config.coresCount), privilegedServices)

  /** Encode accumulation queue (ready queue) using scodec. */
  private def encodeAccumulationQueue(
      queue: List[List[AccumulationReadyRecord]]
  ): JamBytes =
    import io.forge.jam.core.scodec.JamCodecs
    val queueCodec = JamCodecs.fixedSizeList(
      JamCodecs.compactPrefixedList(summon[Codec[AccumulationReadyRecord]]),
      queue.length
    )
    encode(queueCodec, queue)

  /** Encode accumulation history using FullJamStateCodecs. */
  private def encodeAccumulationHistory(
      history: List[List[JamBytes]]
  ): JamBytes =
    val historyBv = history.map(_.map(_.toByteVector))
    encode(
      FullJamStateCodecs.accumulationHistoryCodec(history.length),
      historyBv
    )

  /** Encode last accumulation outputs using FullJamStateCodecs. */
  private def encodeLastAccumulationOutputs(
      outputs: List[(Long, JamBytes)]
  ): JamBytes =
    val outputsBv = outputs.map { case (id, jb) => (id, jb.toByteVector) }
    JamBytes.fromByteVector(
      FullJamStateCodecs.encodeLastAccumulationOutputs(outputsBv)
    )

object FullJamState:
  private val logger = LoggerFactory.getLogger(getClass)

  // Bandersnatch ring commitment size (144 bytes)
  private val RING_COMMITMENT_SIZE: Int =
    TinyConfig.BANDERSNATCH_RING_COMMITMENT_SIZE

  /** Simple protocol keys
    */
  private final class KeyvalIndex(
      val simpleByPrefix: Array[KeyValue], // 256 slots; null if absent
      val serviceAccountKeyvals: scala.collection.mutable.ArrayBuffer[KeyValue],
      val rawServiceDataByStateKey: scala.collection.mutable.HashMap[
        JamBytes,
        JamBytes
      ],
      val originalByKey: scala.collection.mutable.HashMap[JamBytes, KeyValue],
      val otherKeyvals: scala.collection.mutable.ListBuffer[KeyValue]
  )

  private def buildIndex(keyvals: List[KeyValue]): KeyvalIndex =
    val simple = new Array[KeyValue](256)
    val serviceAccounts = scala.collection.mutable.ArrayBuffer.empty[KeyValue]
    val rawData = scala.collection.mutable.HashMap.empty[JamBytes, JamBytes]
    val original = scala.collection.mutable.HashMap.empty[JamBytes, KeyValue]
    val other = scala.collection.mutable.ListBuffer.empty[KeyValue]

    val safroleRelated: Set[Int] = Set(
      StateKeys.TIMESLOT.toInt & 0xff,
      StateKeys.ENTROPY_POOL.toInt & 0xff,
      StateKeys.CURRENT_VALIDATORS.toInt & 0xff,
      StateKeys.PREVIOUS_VALIDATORS.toInt & 0xff,
      StateKeys.VALIDATOR_QUEUE.toInt & 0xff,
      StateKeys.SAFROLE_STATE.toInt & 0xff,
      StateKeys.CORE_AUTHORIZATION_POOL.toInt & 0xff,
      StateKeys.SERVICE_ACCOUNT.toInt & 0xff
    )

    var rem = keyvals
    while rem.nonEmpty do
      val kv = rem.head
      rem = rem.tail
      val key = kv.key
      original.update(key, kv)

      val first = key(0).toInt & 0xff
      val isServiceData = StateKeys.isServiceDataKeyFullWithFirst(key, first)

      if isServiceData then
        rawData.update(key, kv.value)
        if first == 0xff && key(2) == 0 then serviceAccounts += kv
      else if first == 0xff then serviceAccounts += kv
      else simple(first) = kv

      if !safroleRelated.contains(first) && !isServiceData then other += kv

    new KeyvalIndex(simple, serviceAccounts, rawData, original, other)

  def fromTrie(
      trie: StateTrie,
      config: ChainConfig
  ): FullJamState =
    val kvs = scala.collection.mutable.ListBuffer.empty[KeyValue]

    StateKeys.KNOWN_PREFIXES.foreach { prefix =>
      if prefix != 0xff then
        val key = StateKeys.simpleKey(prefix.toByte)
        trie.read(key).foreach(v => kvs += KeyValue(key, v))
    }

    val ffPrefix = JamBytes(Array(0xff.toByte))
    trie.getKeyValues(ffPrefix, 8).foreach { case (k, v) =>
      if k(2) == 0 then kvs += KeyValue(k, v)
    }

    fromKeyvals(kvs.toList, config)

  /** Create from raw state keyvals.
    */
  def fromKeyvals(
      keyvals: List[KeyValue],
      config: ChainConfig
  ): FullJamState =
    val idx = buildIndex(keyvals)

    val safroleState = StateCodec.decodeSafroleStateFromIndex(
      idx.simpleByPrefix,
      config
    )

    val reports = decodeReportsAt(idx, config.coresCount)
    val authPools = decodeAuthPoolsAt(idx, config.coresCount)
    val authQueues =
      decodeAuthQueuesAt(idx, config.coresCount, config.authQueueSize)
    val serviceAccounts = decodeServiceAccountsAt(idx)
    val recentHistory = decodeRecentHistoryAt(idx)
    val (activityStatsCurrent, activityStatsLast, _) =
      decodeActivityStatisticsAt(idx, config.validatorCount, config.coresCount)
    val coreStatistics = List.fill(config.coresCount)(CoreStatisticsRecord())
    val accumulationQueue = decodeAccumulationQueueAt(idx, config.epochLength)
    val accumulationHistory =
      decodeAccumulationHistoryAt(idx, config.epochLength)
    val lastAccumulationOutputs = decodeLastAccumulationOutputsAt(idx)
    val privilegedServices = decodePrivilegedServicesAt(idx, config.coresCount)

    // Convert mutable index state into the immutable shapes the FullJamState
    // case class expects. The mutable buffers are local to this function.
    val originalByKey: Map[JamBytes, KeyValue] = idx.originalByKey.toMap
    val rawServiceDataByStateKey: Map[JamBytes, JamBytes] =
      idx.rawServiceDataByStateKey.toMap
    val otherKvs: List[KeyValue] = idx.otherKeyvals.toList

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
      judgements =
        Psi.empty, // Judgements initialized empty, updated by Disputes STF
      reports = reports,
      authPools = authPools,
      authQueues = authQueues,
      recentHistory = recentHistory,
      privilegedServices = privilegedServices,
      accumulationQueue = accumulationQueue,
      accumulationHistory = accumulationHistory,
      serviceAccounts = serviceAccounts,
      activityStatsCurrent = activityStatsCurrent,
      activityStatsLast = activityStatsLast,
      coreStatistics = coreStatistics,
      lastAccumulationOutputs = lastAccumulationOutputs,
      otherKeyvals = otherKvs,
      originalKeyvals = originalByKey,
      rawServiceDataByStateKey = rawServiceDataByStateKey
    )

  // ============================================================================
  // Decoding helpers - all use FullJamStateCodecs for consistency
  // ============================================================================

  /** Look up the simple-prefix slot in the prebuilt index. */
  private def lookupSimple(idx: KeyvalIndex, prefix: Byte): Option[KeyValue] =
    val slot = idx.simpleByPrefix(prefix.toInt & 0xff)
    if slot == null then None else Some(slot)

  private def decodeActivityStatisticsAt(
      idx: KeyvalIndex,
      validatorCount: Int,
      coresCount: Int
  ): (List[StatCount], List[StatCount], List[CoreStatisticsRecord]) =
    lookupSimple(idx, StateKeys.ACTIVITY_STATISTICS) match
      case None =>
        (
          List.fill(validatorCount)(StatCount.zero),
          List.fill(validatorCount)(StatCount.zero),
          List.fill(coresCount)(CoreStatisticsRecord())
        )
      case Some(kv) =>
        val stats = FullJamStateCodecs.decodeActivityStatistics(
          kv.value.toArray,
          validatorCount,
          coresCount
        )
        val current = stats.accumulator.map(s =>
          StatCount(
            s.blocks,
            s.tickets,
            s.preImages,
            s.preImagesSize,
            s.guarantees,
            s.assurances
          )
        )
        val last = stats.previous.map(s =>
          StatCount(
            s.blocks,
            s.tickets,
            s.preImages,
            s.preImagesSize,
            s.guarantees,
            s.assurances
          )
        )
        val core = stats.core.map(c =>
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
        )
        (current, last, core)

  private def decodeLastAccumulationOutputsAt(
      idx: KeyvalIndex
  ): List[(Long, JamBytes)] =
    lookupSimple(idx, StateKeys.LAST_ACCUMULATION_OUTPUTS) match
      case None     => List.empty
      case Some(kv) =>
        FullJamStateCodecs
          .decodeLastAccumulationOutputs(kv.value.toArray)
          .map { case (id, bv) => (id, JamBytes.fromByteVector(bv)) }

  private def decodePrivilegedServicesAt(
      idx: KeyvalIndex,
      coresCount: Int
  ): Privileges =
    lookupSimple(idx, StateKeys.PRIVILEGED_SERVICES) match
      case None => Privileges(0, List.fill(coresCount)(0L), 0, 0, List.empty)
      case Some(kv) =>
        Privileges
          .codec(coresCount)
          .decodeValue(BitVector(kv.value.toByteVector)) match
          case scodec.Attempt.Successful(p) => p
          case scodec.Attempt.Failure(err)  =>
            logger.warn(
              s"[decodePrivilegedServices] decode failed: $err, using default"
            )
            Privileges(0, List.fill(coresCount)(0L), 0, 0, List.empty)

  private def decodeRecentHistoryAt(idx: KeyvalIndex): HistoricalBetaContainer =
    lookupSimple(idx, StateKeys.RECENT_HISTORY) match
      case None     => HistoricalBetaContainer()
      case Some(kv) =>
        summon[Codec[HistoricalBetaContainer]]
          .decode(BitVector(kv.value.toByteVector))
          .require
          .value

  private def decodeAuthPoolsAt(
      idx: KeyvalIndex,
      coresCount: Int
  ): List[List[Hash]] =
    lookupSimple(idx, StateKeys.CORE_AUTHORIZATION_POOL) match
      case None     => List.fill(coresCount)(List.empty[Hash])
      case Some(kv) =>
        FullJamStateCodecs.decodeAuthPools(kv.value.toArray, coresCount)

  private def decodeAccumulationQueueAt(
      idx: KeyvalIndex,
      epochLength: Int
  ): List[List[AccumulationReadyRecord]] =
    import io.forge.jam.core.scodec.JamCodecs
    lookupSimple(idx, StateKeys.ACCUMULATION_QUEUE) match
      case None     => List.fill(epochLength)(List.empty)
      case Some(kv) =>
        val queueCodec = JamCodecs.fixedSizeList(
          JamCodecs.compactPrefixedList(summon[Codec[AccumulationReadyRecord]]),
          epochLength
        )
        queueCodec.decodeValue(BitVector(kv.value.toByteVector)) match
          case scodec.Attempt.Successful(queue) => queue
          case scodec.Attempt.Failure(err)      =>
            logger.warn(
              s"[decodeAccumulationQueue] decode failed: $err, using empty queue"
            )
            List.fill(epochLength)(List.empty)

  private def decodeAccumulationHistoryAt(
      idx: KeyvalIndex,
      epochLength: Int
  ): List[List[JamBytes]] =
    lookupSimple(idx, StateKeys.ACCUMULATION_HISTORY) match
      case None     => List.fill(epochLength)(List.empty)
      case Some(kv) =>
        FullJamStateCodecs
          .decodeAccumulationHistory(kv.value.toArray, epochLength)
          .map(_.map(bv => JamBytes.fromByteVector(bv)))

  private def decodeAuthQueuesAt(
      idx: KeyvalIndex,
      coresCount: Int,
      authQueueSize: Int
  ): List[List[Hash]] =
    lookupSimple(idx, StateKeys.AUTHORIZATION_QUEUE) match
      case None => List.fill(coresCount)(List.fill(authQueueSize)(Hash.zero))
      case Some(kv) =>
        FullJamStateCodecs.decodeAuthQueues(
          kv.value.toArray,
          coresCount,
          authQueueSize
        )

  /** Service-account info keyvals already collected during the index build. */
  private def decodeServiceAccountsAt(
      idx: KeyvalIndex
  ): List[AccumulationServiceItem] =
    val out =
      scala.collection.mutable.ArrayBuffer.empty[AccumulationServiceItem]
    var i = 0
    while i < idx.serviceAccountKeyvals.size do
      val kv = idx.serviceAccountKeyvals(i)
      val key = kv.key
      if key.length >= 3 && key(2) == 0 then
        val serviceId = ((key(1).toLong & 0xff)) |
          ((key(3).toLong & 0xff) << 8) |
          ((key(5).toLong & 0xff) << 16) |
          ((key(7).toLong & 0xff) << 24)
        val serviceInfo = FullJamStateCodecs.decodeServiceInfo(kv.value.toArray)
        out += AccumulationServiceItem(
          id = serviceId,
          data = AccumulationServiceData(
            service = serviceInfo,
            storage = List.empty,
            preimages = List.empty,
            preimageRequests = List.empty
          )
        )
      i += 1
    out.sortBy(_.id).toList

  private def decodeReportsAt(
      idx: KeyvalIndex,
      coresCount: Int
  ): List[Option[AvailabilityAssignment]] =
    lookupSimple(idx, StateKeys.REPORTS) match
      case None     => List.fill(coresCount)(None)
      case Some(kv) =>
        val codec =
          FullJamStateCodecs.reportsCodec[AvailabilityAssignment](coresCount)
        codec.decodeValue(BitVector(kv.value.toByteVector)).require

  /** Create an empty/default FullJamState.
    */
  def empty(config: ChainConfig): FullJamState =
    val emptyValidatorKey = ValidatorKey(
      BandersnatchPublicKey.zero,
      Ed25519PublicKey(new Array[Byte](Ed25519PublicKey.Size)),
      BlsPublicKey(new Array[Byte](BlsPublicKey.Size)),
      JamBytes.zeros(ValidatorKey.MetadataSize)
    )
    val emptyValidators = List.fill(config.validatorCount)(emptyValidatorKey)
    val emptyEntropy = List.fill(4)(Hash.zero)
    val emptyReports =
      List.fill(config.coresCount)(Option.empty[AvailabilityAssignment])
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
      safroleGammaS = TicketsOrKeys.Keys(
        List.fill(config.epochLength)(BandersnatchPublicKey.zero)
      ),
      safroleGammaA = List.empty,
      reports = emptyReports,
      authPools = emptyAuthPools,
      authQueues = emptyAuthQueues,
      activityStatsCurrent = emptyStatCount,
      activityStatsLast = emptyStatCount,
      coreStatistics = emptyCoreStats
    )
