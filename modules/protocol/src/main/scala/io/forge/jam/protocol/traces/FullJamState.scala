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
import _root_.scodec.bits.ByteVector
import _root_.scodec.Codec
import io.forge.jam.core.scodec.{JamCodecs, FullJamStateCodecs}

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

  // Raw keyvals for all state components (for pass-through when unchanged)
  otherKeyvals: List[KeyValue] = List.empty,

  // Original keyvals by full key (for preserving exact bytes when unchanged)
  originalKeyvals: Map[JamBytes, KeyValue] = Map.empty,

  // Raw service data by state key (for preimages, storage, etc.)
  rawServiceDataByStateKey: Map[JamBytes, JamBytes] = Map.empty
):

  private val logger = LoggerFactory.getLogger(getClass)

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
      summon[Codec[HistoricalBetaContainer]].encode(recentHistory).require.bytes
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

    // Privileged services (0x0C) - pass through original if available
    otherByPrefix.get(StateKeys.PRIVILEGED_SERVICES.toInt & 0xff).foreach(kvs => builder ++= kvs)

    // Activity statistics (0x0D) - always re-encode with new format
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS),
      encodeActivityStatistics(config)
    )

    // Accumulation queue (0x0E) - pass through original if available
    otherByPrefix.get(StateKeys.ACCUMULATION_QUEUE.toInt & 0xff).foreach(kvs => builder ++= kvs)

    // Accumulation history (0x0F) - always re-encode (modified by Accumulation STF)
    builder += KeyValue(
      StateKeys.simpleKey(StateKeys.ACCUMULATION_HISTORY),
      encodeAccumulationHistory(accumulationHistory)
    )

    // Last accumulation outputs (0x10) - pass through original if available
    otherByPrefix.get(StateKeys.LAST_ACCUMULATION_OUTPUTS.toInt & 0xff).foreach(kvs => builder ++= kvs)

    // Service accounts (0xFF) - always encode current state (not original)
    // because account info (bytesUsed, items) can change during accumulation
    for item <- serviceAccounts do
      val serviceKey = encodeServiceAccountKey(item.id)
      builder += KeyValue(
        serviceKey,
        summon[Codec[ServiceInfo]].encode(item.data.service).require.bytes
      )

    // Service storage data (prefix 0x00) - use ONLY rawServiceDataByStateKey as source of truth
    // Keys deleted during accumulation (via WRITE with valueLen=0) are removed from this map
    // so we should NOT preserve original keys - they may have been intentionally deleted
    val storageDataByKey = rawServiceDataByStateKey.filter {
      case (key, _) =>
        (key.toArray(0).toInt & 0xff) == 0
    }

    // Add storage keyvals from accumulation state (only 0x00 prefix)
    for (key, value) <- storageDataByKey.toList.sortBy(_._1.toHex) do
      builder += KeyValue(key, value)

    builder.toList

  /**
   * Encode authorization pools.
   * Format: For each core, compact length prefix + N x 32-byte hashes
   */
  private def encodeAuthPools(pools: List[List[Hash]]): JamBytes =
    val poolCodec: Codec[List[Hash]] = JamCodecs.compactPrefixedList(JamCodecs.hashCodec)
    val codec = JamCodecs.fixedSizeList(poolCodec, pools.length)
    JamBytes.fromByteVector(codec.encode(pools).require.bytes)

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

  // Encoding helpers

  private def encodeTimeslot(tau: Long): JamBytes =
    JamBytes.fromByteVector(FullJamStateCodecs.timeslotCodec.encode(tau).require.bytes)

  private def encodeEntropyPool(eta: List[Hash]): JamBytes =
    JamBytes.fromByteVector(FullJamStateCodecs.entropyPoolCodec.encode(eta).require.bytes)

  private def encodeValidatorList(validators: List[ValidatorKey]): JamBytes =
    val codec = FullJamStateCodecs.validatorListCodec(validators.length)
    JamBytes.fromByteVector(codec.encode(validators).require.bytes)

  private def encodeSafroleGammaState(
    gammaK: List[ValidatorKey],
    gammaZ: JamBytes,
    gammaS: TicketsOrKeys,
    gammaA: List[TicketMark]
  ): JamBytes =
    // Convert TicketsOrKeys to FullJamStateCodecs.TicketsOrKeysData
    val gammaS_data = gammaS match
      case TicketsOrKeys.Tickets(tickets) => FullJamStateCodecs.TicketsOrKeysData.Tickets(tickets)
      case TicketsOrKeys.Keys(keys) => FullJamStateCodecs.TicketsOrKeysData.Keys(keys)

    val codec = FullJamStateCodecs.safroleGammaStateCodec(gammaK.length, gammaS.length)
    JamBytes.fromByteVector(codec.encode((gammaK, gammaZ.toByteVector, gammaS_data, gammaA)).require.bytes)

  /**
   * Encode authorization queues.
   * Format: For each core, 80 x 32-byte hashes (fixed size)
   */
  private def encodeAuthQueues(queues: List[List[Hash]]): JamBytes =
    val queueSize = if queues.nonEmpty then queues.head.length else 80
    val codec = FullJamStateCodecs.authQueuesCodec(queues.length, queueSize)
    JamBytes.fromByteVector(codec.encode(queues).require.bytes)

  /**
   * Encode reports (rho) - availability assignments.
   */
  private def encodeReports(reports: List[Option[AvailabilityAssignment]]): JamBytes =
    val codec = FullJamStateCodecs.reportsCodec(reports.length)(using summon[Codec[AvailabilityAssignment]])
    JamBytes.fromByteVector(codec.encode(reports).require.bytes)

  /**
   * Encode activity statistics (alpha) in v0.7.0+ format.
   * Format: accumulator (validator stats), previous (validator stats), core stats, service stats
   */
  private def encodeActivityStatistics(config: ChainConfig): JamBytes =
    // Pad to validator count
    val paddedCurrent = activityStatsCurrent.padTo(config.validatorCount, StatCount.zero)
    val paddedLast = activityStatsLast.padTo(config.validatorCount, StatCount.zero)
    val paddedCoreStats = coreStatistics.padTo(config.coresCount, CoreStatisticsRecord.zero)

    // Convert to FullJamStateCodecs types
    val statsData = FullJamStateCodecs.ActivityStatisticsData(
      accumulator = paddedCurrent.map(s => FullJamStateCodecs.StatCountData(s.blocks, s.tickets, s.preImages, s.preImagesSize, s.guarantees, s.assurances)),
      previous = paddedLast.map(s => FullJamStateCodecs.StatCountData(s.blocks, s.tickets, s.preImages, s.preImagesSize, s.guarantees, s.assurances)),
      core = paddedCoreStats.map(c => FullJamStateCodecs.CoreStatisticsData(c.daLoad, c.popularity, c.imports, c.extrinsicCount, c.extrinsicSize, c.exports, c.bundleSize, c.gasUsed)),
      service = serviceStatistics.map(e => FullJamStateCodecs.ServiceStatisticsData(
        e.id, 0L, 0L, // preimages count/size
        e.record.refinementCount, e.record.refinementGasUsed,
        e.record.imports, e.record.extrinsicCount, e.record.extrinsicSize, e.record.exports,
        e.record.accumulateCount, e.record.accumulateGasUsed
      ))
    )

    JamBytes.fromByteVector(FullJamStateCodecs.activityStatisticsCodec(config.validatorCount, config.coresCount).encode(statsData).require.bytes)

  /**
   * Encode accumulation history.
   */
  private def encodeAccumulationHistory(history: List[List[JamBytes]]): JamBytes =
    val historyBv = history.map(_.map(_.toByteVector))
    val codec = FullJamStateCodecs.accumulationHistoryCodec(history.length)
    JamBytes.fromByteVector(codec.encode(historyBv).require.bytes)

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

    // rawServiceDataByStateKey should only contain 0x00 prefix storage data
    // This is used by accumulation to track storage writes
    val rawServiceDataByStateKey: Map[JamBytes, JamBytes] = keyvals
      .filter(kv => (kv.key.toArray(0).toInt & 0xff) == 0)
      .map(kv => kv.key -> kv.value)
      .toMap
    logger.debug(s"[fromKeyvals] rawServiceDataByStateKey.size=${rawServiceDataByStateKey.size}")
    rawServiceDataByStateKey.keys.foreach(k => logger.debug(s"[fromKeyvals] storageKey=${k.toHex.take(32)}..."))

    // Decode recent history from keyvals
    val historyKv = keyvals.find(kv =>
      (kv.key.toArray(0).toInt & 0xff) == (StateKeys.RECENT_HISTORY.toInt & 0xff)
    )
    historyKv.foreach(kv => logger.debug(s"[fromKeyvals] 0x03 keyval len=${kv.value.length}"))
    val recentHistory = decodeRecentHistory(keyvals)
    logger.debug(s"[fromKeyvals] recentHistory.history.size=${recentHistory.history.size}")

    // Decode activity statistics from keyvals
    val (activityStatsCurrent, activityStatsLast, _) =
      decodeActivityStatistics(keyvals, config.validatorCount, config.coresCount)
    // Initialize empty core statistics (computed fresh each block)
    val coreStatistics = List.fill(config.coresCount)(CoreStatisticsRecord())

    // Decode accumulation history from keyvals (0x0F)
    val accumulationHistory = decodeAccumulationHistory(keyvals, config.epochLength)
    logger.debug(s"[fromKeyvals] accumulationHistory total hashes=${accumulationHistory.map(_.size).sum}")

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
      serviceAccounts = serviceAccounts,
      activityStatsCurrent = activityStatsCurrent,
      activityStatsLast = activityStatsLast,
      coreStatistics = coreStatistics,
      accumulationHistory = accumulationHistory,
      otherKeyvals = otherKvs,
      originalKeyvals = originalByKey,
      rawServiceDataByStateKey = rawServiceDataByStateKey
    )

  /**
   * Decode activity statistics from keyvals (0x0D).
   * Format: accumulator (validator stats), previous (validator stats), core stats, service stats
   * Returns (current validator stats, last validator stats, core stats)
   */
  private def decodeActivityStatistics(
    keyvals: List[KeyValue],
    validatorCount: Int,
    coresCount: Int
  ): (List[StatCount], List[StatCount], List[CoreStatisticsRecord]) =
    val statsKv = keyvals.find(kv =>
      (kv.key.toArray(0).toInt & 0xff) == (StateKeys.ACTIVITY_STATISTICS.toInt & 0xff)
    )

    statsKv match
      case None =>
        // No stats in state, return empty
        (
          List.fill(validatorCount)(StatCount.zero),
          List.fill(validatorCount)(StatCount.zero),
          List.fill(coresCount)(CoreStatisticsRecord())
        )

      case Some(kv) =>
        val bytes = kv.value.toArray
        var pos = 0

        // Decode accumulator (current validator stats) - fixed-size array
        val currentStats = scala.collection.mutable.ListBuffer[StatCount]()
        for _ <- 0 until validatorCount do
          val blocks = codec.decodeU32LE(bytes, pos).toLong; pos += 4
          val tickets = codec.decodeU32LE(bytes, pos).toLong; pos += 4
          val preImages = codec.decodeU32LE(bytes, pos).toLong; pos += 4
          val preImagesSize = codec.decodeU32LE(bytes, pos).toLong; pos += 4
          val guarantees = codec.decodeU32LE(bytes, pos).toLong; pos += 4
          val assurances = codec.decodeU32LE(bytes, pos).toLong; pos += 4
          currentStats += StatCount(blocks, tickets, preImages, preImagesSize, guarantees, assurances)

        // Decode previous (last validator stats) - fixed-size array
        val lastStats = scala.collection.mutable.ListBuffer[StatCount]()
        for _ <- 0 until validatorCount do
          val blocks = codec.decodeU32LE(bytes, pos).toLong; pos += 4
          val tickets = codec.decodeU32LE(bytes, pos).toLong; pos += 4
          val preImages = codec.decodeU32LE(bytes, pos).toLong; pos += 4
          val preImagesSize = codec.decodeU32LE(bytes, pos).toLong; pos += 4
          val guarantees = codec.decodeU32LE(bytes, pos).toLong; pos += 4
          val assurances = codec.decodeU32LE(bytes, pos).toLong; pos += 4
          lastStats += StatCount(blocks, tickets, preImages, preImagesSize, guarantees, assurances)

        // Decode core stats - fixed-size array, each field compact-encoded
        val coreStats = scala.collection.mutable.ListBuffer[CoreStatisticsRecord]()
        for _ <- 0 until coresCount do
          val (daLoad, daLoadLen) = codec.decodeCompactInteger(bytes, pos); pos += daLoadLen
          val (popularity, popularityLen) = codec.decodeCompactInteger(bytes, pos); pos += popularityLen
          val (imports, importsLen) = codec.decodeCompactInteger(bytes, pos); pos += importsLen
          val (extrinsicCount, extrinsicCountLen) = codec.decodeCompactInteger(bytes, pos); pos += extrinsicCountLen
          val (extrinsicSize, extrinsicSizeLen) = codec.decodeCompactInteger(bytes, pos); pos += extrinsicSizeLen
          val (exports, exportsLen) = codec.decodeCompactInteger(bytes, pos); pos += exportsLen
          val (bundleSize, bundleSizeLen) = codec.decodeCompactInteger(bytes, pos); pos += bundleSizeLen
          val (gasUsed, gasUsedLen) = codec.decodeCompactInteger(bytes, pos); pos += gasUsedLen
          coreStats += CoreStatisticsRecord(
            daLoad = daLoad,
            popularity = popularity,
            imports = imports,
            extrinsicCount = extrinsicCount,
            extrinsicSize = extrinsicSize,
            exports = exports,
            bundleSize = bundleSize,
            gasUsed = gasUsed
          )

        // Skip service stats for now (we'll compute fresh each block)
        (currentStats.toList, lastStats.toList, coreStats.toList)

  /**
   * Decode recent history from keyvals.
   */
  private def decodeRecentHistory(keyvals: List[KeyValue]): HistoricalBetaContainer =
    val historyKv = keyvals.find(kv =>
      (kv.key.toArray(0).toInt & 0xff) == (StateKeys.RECENT_HISTORY.toInt & 0xff)
    )

    historyKv match
      case None =>
        HistoricalBetaContainer()
      case Some(kv) =>
        val (container, _) = kv.value.decodeAs[HistoricalBetaContainer](0)
        container

  /**
   * Decode authorization pools from keyvals.
   * Format: For each core, compact length prefix + N x 32-byte hashes
   */
  private def decodeAuthPools(keyvals: List[KeyValue], coresCount: Int): List[List[Hash]] =
    // Find the auth pools keyval
    val authPoolsKv = keyvals.find(kv =>
      (kv.key.toArray(0).toInt & 0xff) == (StateKeys.CORE_AUTHORIZATION_POOL.toInt & 0xff)
    )

    authPoolsKv match
      case None =>
        // No auth pools in state, return empty
        List.fill(coresCount)(List.empty[Hash])

      case Some(kv) =>
        val bytes = kv.value.toArray
        var offset = 0
        val pools = scala.collection.mutable.ListBuffer[List[Hash]]()

        for _ <- 0 until coresCount do
          if offset >= bytes.length then
            pools += List.empty[Hash]
          else
            // Read compact length for this core's pool
            val (poolLen, lenBytes) = codec.decodeCompactInteger(bytes, offset)
            offset += lenBytes

            // Read poolLen hashes
            val hashes = scala.collection.mutable.ListBuffer[Hash]()
            for _ <- 0 until poolLen.toInt do
              if offset + 32 <= bytes.length then
                hashes += Hash(bytes.slice(offset, offset + 32))
                offset += 32

            pools += hashes.toList

        pools.toList

  /**
   * Decode accumulation history from keyvals (0x0F).
   * Format: For each slot: compact count + N x 32-byte hashes
   */
  private def decodeAccumulationHistory(keyvals: List[KeyValue], epochLength: Int): List[List[JamBytes]] =
    val historyKv = keyvals.find(kv =>
      (kv.key.toArray(0).toInt & 0xff) == (StateKeys.ACCUMULATION_HISTORY.toInt & 0xff)
    )
    historyKv match
      case None => List.fill(epochLength)(List.empty)
      case Some(kv) =>
        val bytes = kv.value.toArray
        var pos = 0
        val slots = scala.collection.mutable.ListBuffer[List[JamBytes]]()
        for _ <- 0 until epochLength do
          if pos < bytes.length then
            val (count, countLen) = codec.decodeCompactInteger(bytes, pos)
            pos += countLen
            val hashes = scala.collection.mutable.ListBuffer[JamBytes]()
            for _ <- 0 until count.toInt do
              val hash = JamBytes(bytes.slice(pos, pos + Hash.Size))
              hashes += hash
              pos += Hash.Size
            slots += hashes.toList
          else
            slots += List.empty
        slots.toList

  /**
   * Decode authorization queues from keyvals (0x02).
   * Format: Fixed-size array: coresCount * authQueueSize * 32 bytes
   */
  private def decodeAuthQueues(keyvals: List[KeyValue], coresCount: Int, authQueueSize: Int): List[List[Hash]] =
    // Find the auth queues keyval
    val authQueuesKv = keyvals.find(kv =>
      (kv.key.toArray(0).toInt & 0xff) == (StateKeys.AUTHORIZATION_QUEUE.toInt & 0xff)
    )

    authQueuesKv match
      case None =>
        // No auth queues in state, return empty (all zeros)
        List.fill(coresCount)(List.fill(authQueueSize)(Hash.zero))

      case Some(kv) =>
        val bytes = kv.value.toArray
        val queues = scala.collection.mutable.ListBuffer[List[Hash]]()

        for coreIndex <- 0 until coresCount do
          val hashes = scala.collection.mutable.ListBuffer[Hash]()
          for queueIndex <- 0 until authQueueSize do
            val offset = (coreIndex * authQueueSize + queueIndex) * 32
            if offset + 32 <= bytes.length then
              hashes += Hash(bytes.slice(offset, offset + 32))
            else
              hashes += Hash.zero

          queues += hashes.toList

        queues.toList

  /**
   * Decode service accounts from keyvals.
   * Key format: prefix 255, service ID at positions 1, 3, 5, 7
   */
  private def decodeServiceAccounts(keyvals: List[KeyValue]): List[AccumulationServiceItem] =
    // Find all service account keyvals (prefix 255)
    val serviceKvs = keyvals.filter(kv =>
      (kv.key.toArray(0).toInt & 0xff) == (StateKeys.SERVICE_ACCOUNT.toInt & 0xff)
    )

    serviceKvs.map { kv =>
      val keyBytes = kv.key.toArray
      // Extract service index from positions 1, 3, 5, 7
      val serviceId = (keyBytes(1).toInt & 0xff) |
        ((keyBytes(3).toInt & 0xff) << 8) |
        ((keyBytes(5).toInt & 0xff) << 16) |
        ((keyBytes(7).toInt & 0xff) << 24)

      val valueBytes = kv.value.toArray
      val (serviceInfo, _) = JamBytes(valueBytes).decodeAs[ServiceInfo](0)

      AccumulationServiceItem(
        id = serviceId.toLong,
        data = AccumulationServiceData(
          service = serviceInfo,
          storage = List.empty,
          preimages = List.empty,
          preimagesStatus = List.empty
        )
      )
    }.sortBy(_.id)

  /**
   * Decode reports (rho) from keyvals (0x0A).
   * Format: coresCount * Option[AvailabilityAssignment]
   */
  private def decodeReports(keyvals: List[KeyValue], coresCount: Int): List[Option[AvailabilityAssignment]] =
    val reportsKv = keyvals.find(kv =>
      (kv.key.toArray(0).toInt & 0xff) == (StateKeys.REPORTS.toInt & 0xff)
    )

    reportsKv match
      case None =>
        // No reports in state, return all None
        List.fill(coresCount)(None)

      case Some(kv) =>
        val bytes = kv.value
        var pos = 0
        val reports = scala.collection.mutable.ListBuffer[Option[AvailabilityAssignment]]()

        for _ <- 0 until coresCount do
          if pos >= bytes.length then
            reports += None
          else
            val marker = bytes.toArray(pos) & 0xff
            pos += 1
            if marker == 0 then
              reports += None
            else
              // marker == 1, decode AvailabilityAssignment
              val (assignment, consumed) = bytes.decodeAs[AvailabilityAssignment](pos)
              pos += consumed
              reports += Some(assignment)

        reports.toList

  /**
   * Create an empty/default FullJamState.
   */
  def empty(config: ChainConfig = ChainConfig.TINY): FullJamState =
    val emptyValidatorKey = ValidatorKey(
      BandersnatchPublicKey.zero,
      Ed25519PublicKey(new Array[Byte](Ed25519PublicKey.Size)),
      BlsPublicKey(new Array[Byte](BlsPublicKey.Size)),
      ByteVector.fill(ValidatorKey.MetadataSize.toLong)(0)
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
