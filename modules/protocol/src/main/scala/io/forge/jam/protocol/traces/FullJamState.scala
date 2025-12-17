package io.forge.jam.protocol.traces

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey}
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
  Privileges
}
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.protocol.report.ReportTypes.{CoreStatisticsRecord, ServiceStatisticsEntry}
import io.forge.jam.protocol.statistics.StatisticsTypes.StatCount
import org.slf4j.LoggerFactory
import _root_.scodec.bits.BitVector
import _root_.scodec.Codec
import io.forge.jam.core.scodec.FullJamStateCodecs

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

  // Last accumulation outputs (service commitments)
  lastAccumulationOutputs: List[(Long, JamBytes)] = List.empty,

  // Raw keyvals for all state components (for pass-through when unchanged)
  otherKeyvals: List[KeyValue] = List.empty,

  // Original keyvals by full key (for preserving exact bytes when unchanged)
  originalKeyvals: Map[JamBytes, KeyValue] = Map.empty,

  // Raw service data by state key (for preimages, storage, etc.)
  rawServiceDataByStateKey: Map[JamBytes, JamBytes] = Map.empty
):

  /**
   * Convert back to raw keyvals for state root computation.
   */
  def toKeyvals(config: ChainConfig = ChainConfig.TINY): List[KeyValue] =
    val builder = scala.collection.mutable.ListBuffer[KeyValue]()

    // Build a map of original keyvals from otherKeyvals by prefix for pass-through
    val otherByPrefix = otherKeyvals.groupBy(kv => kv.key.toArray(0).toInt & 0xff)

    // Core authorization pools (0x01) - always re-encode (modified by Authorization STF)
    val encodedAuthPools = encodeAuthPools(authPools)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.CORE_AUTHORIZATION_POOL),
      encodedAuthPools
    )

    // Authorization queues (0x02) - always re-encode (modified by Authorization STF)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.AUTHORIZATION_QUEUE),
      encodeAuthQueues(authQueues)
    )

    // Recent history (0x03) - always re-encode (modified by History STF)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.RECENT_HISTORY),
      JamBytes.fromByteVector(summon[Codec[HistoricalBetaContainer]].encode(recentHistory).require.bytes)
    )

    // Safrole gamma state (0x04) - always re-encode (modified by Safrole STF at epoch boundaries)
    val safroleKey = StateKeys.simpleKey(StateKeys.SAFROLE_STATE)
    builder += KeyValue(
      safroleKey,
      encodeSafroleGammaState(safroleGammaK, safroleGammaZ, safroleGammaS, safroleGammaA)
    )

    // Judgements (0x05) - pass through original if available
    otherByPrefix.get(StateKeys.JUDGEMENTS.toInt & 0xff).foreach(kvs => builder ++= kvs)

    // Entropy pool (0x06) - always re-encode (modified by Safrole STF)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.ENTROPY_POOL),
      encodeEntropyPool(entropyPool)
    )

    // Validator queue (0x07) - always re-encode (rotated by Safrole STF at epoch boundaries)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.VALIDATOR_QUEUE),
      encodeValidatorList(validatorQueue)
    )

    // Current validators (0x08) - always re-encode (rotated by Safrole STF at epoch boundaries)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.CURRENT_VALIDATORS),
      encodeValidatorList(currentValidators)
    )

    // Previous validators (0x09) - always re-encode (rotated by Safrole STF at epoch boundaries)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.PREVIOUS_VALIDATORS),
      encodeValidatorList(previousValidators)
    )

    // Reports (0x0A) - always re-encode (modified by Reports STF)
    val paddedReports = reports.padTo(config.coresCount, None)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.REPORTS),
      encodeReports(paddedReports)
    )

    // Timeslot (0x0B) - always re-encode (modified by Safrole STF)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.TIMESLOT),
      encodeTimeslot(timeslot)
    )

    // Privileged services (0x0C) - always re-encode (modified by Accumulation STF via bless host call)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.PRIVILEGED_SERVICES),
      encodePrivilegedServices(config)
    )

    // Activity statistics (0x0D) - always re-encode with new format
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS),
      encodeActivityStatistics(config)
    )

    // Accumulation queue (0x0E) - always re-encode (modified by Accumulation STF)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.ACCUMULATION_QUEUE),
      encodeAccumulationQueue(accumulationQueue)
    )

    // Accumulation history (0x0F) - always re-encode (modified by Accumulation STF)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.ACCUMULATION_HISTORY),
      encodeAccumulationHistory(accumulationHistory)
    )

    // Last accumulation outputs (0x10) - always re-encode from lastAccumulationOutputs field
    // Gray Paper C(16) ↦ encode{sq{(encode[4]{s}, encode{h}) | (s, h) ∈ lastaccout}}
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.LAST_ACCUMULATION_OUTPUTS),
      encodeLastAccumulationOutputs(lastAccumulationOutputs)
    )

    // Service accounts (0xFF) - always encode current state (not original)
    // because account info (bytesUsed, items) can change during accumulation
    for item <- serviceAccounts do
      val serviceKey = encodeServiceAccountKey(item.id)
      builder += KeyValue(
        serviceKey,
        JamBytes.fromByteVector(summon[Codec[ServiceInfo]].encode(item.data.service).require.bytes)
      )

    // Service storage/preimage/request data - use rawServiceDataByStateKey as source of truth
    // These keys use interleaved encoding: [n0, a0, n1, a1, n2, a2, n3, a3, a4, ...]
    // where n = encode(serviceId), a = blake2b(val_encoded + data)
    // The first byte is the low byte of the service ID, NOT a fixed prefix
    // Filter to only include service data keys (not known protocol keys)
    val storageDataByKey = rawServiceDataByStateKey.filter {
      case (key, _) =>
        StateKeys.isServiceDataKeyFull(key)
    }

    // Add storage keyvals from accumulation state
    for (key, value) <- storageDataByKey.toList.sortBy(_._1.toHex) do
      builder += KeyValue(key, value)

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

  /**
   * Encode service account key.
   * Format: prefix 255, service ID interleaved at positions 1, 3, 5, 7
   */
  private def encodeServiceAccountKey(serviceId: Long): JamBytes =
    val key = new Array[Byte](31)
    key(0) = StateKeys.SERVICE_ACCOUNT
    key(1) = (serviceId & 0xff).toByte
    key(3) = ((serviceId >> 8) & 0xff).toByte
    key(5) = ((serviceId >> 16) & 0xff).toByte
    key(7) = ((serviceId >> 24) & 0xff).toByte
    JamBytes(key)

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
      case TicketsOrKeys.Tickets(tickets) => (FullJamStateCodecs.TicketsOrKeysData.Tickets(tickets), tickets.length)
      case TicketsOrKeys.Keys(keys) => (FullJamStateCodecs.TicketsOrKeysData.Keys(keys), keys.length)
    encode(
      FullJamStateCodecs.safroleGammaStateCodec(gammaK.length, gammaS_length),
      (gammaK, gammaZ.toByteVector, gammaS_data, gammaA)
    )

  /** Encode authorization queues using FullJamStateCodecs. */
  private def encodeAuthQueues(queues: List[List[Hash]]): JamBytes =
    val queueSize = if queues.nonEmpty then queues.head.length else 80
    encode(FullJamStateCodecs.authQueuesCodec(queues.length, queueSize), queues)

  /** Encode reports using FullJamStateCodecs. */
  private def encodeReports(reports: List[Option[AvailabilityAssignment]]): JamBytes =
    encode(FullJamStateCodecs.reportsCodec(reports.length)(using summon[Codec[AvailabilityAssignment]]), reports)

  /** Encode activity statistics using FullJamStateCodecs. */
  private def encodeActivityStatistics(config: ChainConfig): JamBytes =
    val paddedCurrent = activityStatsCurrent.padTo(config.validatorCount, StatCount.zero)
    val paddedLast = activityStatsLast.padTo(config.validatorCount, StatCount.zero)
    val paddedCoreStats = coreStatistics.padTo(config.coresCount, CoreStatisticsRecord.zero)

    val statsData = FullJamStateCodecs.ActivityStatisticsData(
      accumulator = paddedCurrent.map(s => FullJamStateCodecs.StatCountData(s.blocks, s.tickets, s.preImages, s.preImagesSize, s.guarantees, s.assurances)),
      previous = paddedLast.map(s => FullJamStateCodecs.StatCountData(s.blocks, s.tickets, s.preImages, s.preImagesSize, s.guarantees, s.assurances)),
      core = paddedCoreStats.map(c => FullJamStateCodecs.CoreStatisticsData(c.daLoad, c.popularity, c.imports, c.extrinsicCount, c.extrinsicSize, c.exports, c.bundleSize, c.gasUsed)),
      service = serviceStatistics.map(e => FullJamStateCodecs.ServiceStatisticsData(
        e.id, e.record.providedCount.toLong, e.record.providedSize,
        e.record.refinementCount, e.record.refinementGasUsed,
        e.record.imports, e.record.extrinsicCount, e.record.extrinsicSize, e.record.exports,
        e.record.accumulateCount, e.record.accumulateGasUsed
      ))
    )
    encode(FullJamStateCodecs.activityStatisticsCodec(config.validatorCount, config.coresCount), statsData)

  /** Encode privileged services using Privileges codec. */
  private def encodePrivilegedServices(config: ChainConfig): JamBytes =
    encode(Privileges.codec(config.coresCount), privilegedServices)

  /** Encode accumulation queue (ready queue) using scodec. */
  private def encodeAccumulationQueue(queue: List[List[AccumulationReadyRecord]]): JamBytes =
    import io.forge.jam.core.scodec.JamCodecs
    val queueCodec = JamCodecs.fixedSizeList(
      JamCodecs.compactPrefixedList(summon[Codec[AccumulationReadyRecord]]),
      queue.length
    )
    encode(queueCodec, queue)

  /** Encode accumulation history using FullJamStateCodecs. */
  private def encodeAccumulationHistory(history: List[List[JamBytes]]): JamBytes =
    val historyBv = history.map(_.map(_.toByteVector))
    encode(FullJamStateCodecs.accumulationHistoryCodec(history.length), historyBv)

  /** Encode last accumulation outputs using FullJamStateCodecs. */
  private def encodeLastAccumulationOutputs(outputs: List[(Long, JamBytes)]): JamBytes =
    val outputsBv = outputs.map { case (id, jb) => (id, jb.toByteVector) }
    JamBytes.fromByteVector(FullJamStateCodecs.encodeLastAccumulationOutputs(outputsBv))

object FullJamState:
  private val logger = LoggerFactory.getLogger(getClass)

  // Bandersnatch ring commitment size (144 bytes)
  private val RING_COMMITMENT_SIZE: Int = TinyConfig.BANDERSNATCH_RING_COMMITMENT_SIZE

  /**
   * Create from raw state keyvals.
   */
  def fromKeyvals(keyvals: List[KeyValue], config: ChainConfig = ChainConfig.TINY): FullJamState =
    val safroleState = StateCodec.decodeSafroleState(keyvals, config)

    // Build a map of ALL original keyvals by full key for preserving exact bytes
    val originalByKey = keyvals.map(kv => kv.key -> kv).toMap

    // Separate Safrole-related keyvals from others
    val safroleRelatedPrefixes = Set(
      StateKeys.TIMESLOT.toInt & 0xff,
      StateKeys.ENTROPY_POOL.toInt & 0xff,
      StateKeys.CURRENT_VALIDATORS.toInt & 0xff,
      StateKeys.PREVIOUS_VALIDATORS.toInt & 0xff,
      StateKeys.VALIDATOR_QUEUE.toInt & 0xff,
      StateKeys.SAFROLE_STATE.toInt & 0xff,
      StateKeys.CORE_AUTHORIZATION_POOL.toInt & 0xff,
      StateKeys.SERVICE_ACCOUNT.toInt & 0xff
    )

    val otherKvs = keyvals.filterNot(kv => safroleRelatedPrefixes.contains(kv.key.toArray(0).toInt & 0xff))

    // Decode reports from keyvals (0x0A)
    val reports = decodeReports(keyvals, config.coresCount)

    // Decode auth pools from keyvals
    val authPools = decodeAuthPools(keyvals, config.coresCount)

    // Decode auth queues from keyvals (0x02)
    val authQueues = decodeAuthQueues(keyvals, config.coresCount, config.authQueueSize)

    // Decode service accounts from keyvals
    val serviceAccounts = decodeServiceAccounts(keyvals)

    // rawServiceDataByStateKey contains all service storage/preimage/request keys
    // These use interleaved encoding where the first byte is the low byte of the service ID
    // Filter using isServiceDataKeyFull to capture all non-protocol keys
    // Note: Service data keys can have the same first byte as protocol keys if the service ID's
    // low byte matches a protocol prefix (e.g., 0x07). We must check the full key.
    val rawServiceDataByStateKey: Map[JamBytes, JamBytes] = keyvals
      .filter(kv => StateKeys.isServiceDataKeyFull(kv.key))
      .map(kv => kv.key -> kv.value)
      .toMap

    val recentHistory = decodeRecentHistory(keyvals)

    val (activityStatsCurrent, activityStatsLast, _) =
      decodeActivityStatistics(keyvals, config.validatorCount, config.coresCount)
    val coreStatistics = List.fill(config.coresCount)(CoreStatisticsRecord())

    val accumulationQueue = decodeAccumulationQueue(keyvals, config.epochLength)
    val accumulationHistory = decodeAccumulationHistory(keyvals, config.epochLength)
    val lastAccumulationOutputs = decodeLastAccumulationOutputs(keyvals)

    // Decode privileged services from keyvals (0x0C)
    val privilegedServices = decodePrivilegedServices(keyvals, config.coresCount)

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

  /** Helper to find a keyval by prefix - only matches simple protocol state keys. */
  private def findKeyval(keyvals: List[KeyValue], prefix: Byte): Option[KeyValue] =
    keyvals.find { kv =>
      val keyBytes = kv.key.toArray
      if keyBytes.length != 31 then false
      else if keyBytes(0) != prefix then false
      else keyBytes.drop(1).forall(_ == 0) // Only match simple keys (all zeros after prefix)
    }

  /** Decode activity statistics using FullJamStateCodecs. */
  private def decodeActivityStatistics(
    keyvals: List[KeyValue],
    validatorCount: Int,
    coresCount: Int
  ): (List[StatCount], List[StatCount], List[CoreStatisticsRecord]) =
    findKeyval(keyvals, StateKeys.ACTIVITY_STATISTICS) match
      case None =>
        (
          List.fill(validatorCount)(StatCount.zero),
          List.fill(validatorCount)(StatCount.zero),
          List.fill(coresCount)(CoreStatisticsRecord())
        )
      case Some(kv) =>
        val stats = FullJamStateCodecs.decodeActivityStatistics(kv.value.toArray, validatorCount, coresCount)
        val current = stats.accumulator.map(s => StatCount(s.blocks, s.tickets, s.preImages, s.preImagesSize, s.guarantees, s.assurances))
        val last = stats.previous.map(s => StatCount(s.blocks, s.tickets, s.preImages, s.preImagesSize, s.guarantees, s.assurances))
        val core = stats.core.map(c => CoreStatisticsRecord(c.daLoad, c.popularity, c.imports, c.extrinsicCount, c.extrinsicSize, c.exports, c.bundleSize, c.gasUsed))
        (current, last, core)

  /** Decode last accumulation outputs using FullJamStateCodecs. */
  private def decodeLastAccumulationOutputs(keyvals: List[KeyValue]): List[(Long, JamBytes)] =
    findKeyval(keyvals, StateKeys.LAST_ACCUMULATION_OUTPUTS) match
      case None => List.empty
      case Some(kv) =>
        FullJamStateCodecs.decodeLastAccumulationOutputs(kv.value.toArray)
          .map { case (id, bv) => (id, JamBytes.fromByteVector(bv)) }

  /** Decode privileged services using Privileges codec. */
  private def decodePrivilegedServices(keyvals: List[KeyValue], coresCount: Int): Privileges =
    findKeyval(keyvals, StateKeys.PRIVILEGED_SERVICES) match
      case None =>
        // Default with correct coresCount
        Privileges(0, List.fill(coresCount)(0L), 0, 0, List.empty)
      case Some(kv) =>
        Privileges.codec(coresCount).decodeValue(BitVector(kv.value.toByteVector)) match
          case scodec.Attempt.Successful(p) => p
          case scodec.Attempt.Failure(err) =>
            logger.warn(s"[decodePrivilegedServices] decode failed: $err, using default")
            Privileges(0, List.fill(coresCount)(0L), 0, 0, List.empty)

  /** Decode recent history using scodec Codec. */
  private def decodeRecentHistory(keyvals: List[KeyValue]): HistoricalBetaContainer =
    findKeyval(keyvals, StateKeys.RECENT_HISTORY) match
      case None => HistoricalBetaContainer()
      case Some(kv) =>
        summon[Codec[HistoricalBetaContainer]].decode(BitVector(kv.value.toByteVector)).require.value

  /** Decode authorization pools using FullJamStateCodecs. */
  private def decodeAuthPools(keyvals: List[KeyValue], coresCount: Int): List[List[Hash]] =
    findKeyval(keyvals, StateKeys.CORE_AUTHORIZATION_POOL) match
      case None => List.fill(coresCount)(List.empty[Hash])
      case Some(kv) => FullJamStateCodecs.decodeAuthPools(kv.value.toArray, coresCount)

  /** Decode accumulation queue using scodec. */
  private def decodeAccumulationQueue(keyvals: List[KeyValue], epochLength: Int): List[List[AccumulationReadyRecord]] =
    import io.forge.jam.core.scodec.JamCodecs
    findKeyval(keyvals, StateKeys.ACCUMULATION_QUEUE) match
      case None => List.fill(epochLength)(List.empty)
      case Some(kv) =>
        val queueCodec = JamCodecs.fixedSizeList(
          JamCodecs.compactPrefixedList(summon[Codec[AccumulationReadyRecord]]),
          epochLength
        )
        queueCodec.decodeValue(BitVector(kv.value.toByteVector)) match
          case scodec.Attempt.Successful(queue) => queue
          case scodec.Attempt.Failure(err) =>
            logger.warn(s"[decodeAccumulationQueue] decode failed: $err, using empty queue")
            List.fill(epochLength)(List.empty)

  /** Decode accumulation history using FullJamStateCodecs. */
  private def decodeAccumulationHistory(keyvals: List[KeyValue], epochLength: Int): List[List[JamBytes]] =
    findKeyval(keyvals, StateKeys.ACCUMULATION_HISTORY) match
      case None => List.fill(epochLength)(List.empty)
      case Some(kv) =>
        FullJamStateCodecs.decodeAccumulationHistory(kv.value.toArray, epochLength)
          .map(_.map(bv => JamBytes.fromByteVector(bv)))

  /** Decode authorization queues using FullJamStateCodecs. */
  private def decodeAuthQueues(keyvals: List[KeyValue], coresCount: Int, authQueueSize: Int): List[List[Hash]] =
    findKeyval(keyvals, StateKeys.AUTHORIZATION_QUEUE) match
      case None => List.fill(coresCount)(List.fill(authQueueSize)(Hash.zero))
      case Some(kv) => FullJamStateCodecs.decodeAuthQueues(kv.value.toArray, coresCount, authQueueSize)

  /** Decode service accounts from keyvals. Key format: prefix 255, service ID interleaved. */
  private def decodeServiceAccounts(keyvals: List[KeyValue]): List[AccumulationServiceItem] =
    keyvals
      .filter(kv => (kv.key.toArray(0).toInt & 0xff) == (StateKeys.SERVICE_ACCOUNT.toInt & 0xff))
      .map { kv =>
        val keyBytes = kv.key.toArray
        val serviceId = ((keyBytes(1).toLong & 0xff)) |
          ((keyBytes(3).toLong & 0xff) << 8) |
          ((keyBytes(5).toLong & 0xff) << 16) |
          ((keyBytes(7).toLong & 0xff) << 24)
        val serviceInfo = FullJamStateCodecs.decodeServiceInfo(kv.value.toArray)
        AccumulationServiceItem(
          id = serviceId,
          data = AccumulationServiceData(
            service = serviceInfo,
            storage = List.empty,
            preimages = List.empty,
            preimageRequests = List.empty
          )
        )
      }.sortBy(_.id)

  /** Decode reports using FullJamStateCodecs. */
  private def decodeReports(keyvals: List[KeyValue], coresCount: Int): List[Option[AvailabilityAssignment]] =
    findKeyval(keyvals, StateKeys.REPORTS) match
      case None => List.fill(coresCount)(None)
      case Some(kv) =>
        val codec = FullJamStateCodecs.reportsCodec[AvailabilityAssignment](coresCount)
        codec.decodeValue(BitVector(kv.value.toByteVector)).require

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
