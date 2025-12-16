package io.forge.jam.core.scodec

import scodec.*
import scodec.bits.*
import scodec.codecs.*
import spire.math.UByte
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.*
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.service.ServiceInfo

/**
 * scodec codecs for FullJamState encoding/decoding.
 *
 * Provides bidirectional codecs for JAM state components with binary compatibility.
 */
object FullJamStateCodecs:

  val BandersnatchRingCommitmentSize: Int = 144
  val ValidatorKeySize: Int = 336
  val MetadataSize: Int = 128
  val TicketMarkSize: Int = Hash.Size + 1
  val StatCountSize: Int = 24

  /** Codec for timeslot (tau) - 4 bytes little-endian uint32. */
  val timeslotCodec: Codec[Long] = uint32L.xmap(_.toLong, _.toInt & 0xFFFFFFFFL)

  /** Codec for entropy pool - exactly 4 hashes (128 bytes total). */
  val entropyPoolCodec: Codec[List[Hash]] =
    JamCodecs.fixedSizeList(JamCodecs.hashCodec, 4)

  /** Codec for a fixed-size list of validators. */
  def validatorListCodec(count: Int): Codec[List[ValidatorKey]] =
    JamCodecs.fixedSizeList(summon[Codec[ValidatorKey]], count)

  /** Codec for authorization pools: per-core compact length prefix + N hashes. */
  def authPoolsCodec(coresCount: Int): Codec[List[List[Hash]]] =
    val poolCodec: Codec[List[Hash]] = JamCodecs.compactPrefixedList(JamCodecs.hashCodec)
    JamCodecs.fixedSizeList(poolCodec, coresCount)

  /** Codec for authorization queues: fixed-size coresCount * queueSize * 32 bytes. */
  def authQueuesCodec(coresCount: Int, queueSize: Int): Codec[List[List[Hash]]] =
    val queueCodec: Codec[List[Hash]] = JamCodecs.fixedSizeList(JamCodecs.hashCodec, queueSize)
    JamCodecs.fixedSizeList(queueCodec, coresCount)

  /** Codec for accumulation history: per-slot compact count + N x 32-byte hashes. */
  def accumulationHistoryCodec(epochLength: Int): Codec[List[List[ByteVector]]] =
    val byteVectorCodec: Codec[ByteVector] = fixedSizeBytes(Hash.Size.toLong, bytes)
    val slotCodec: Codec[List[ByteVector]] = JamCodecs.compactPrefixedList(byteVectorCodec)
    JamCodecs.fixedSizeList(slotCodec, epochLength)

  sealed trait TicketsOrKeysData

  object TicketsOrKeysData:
    final case class Tickets(tickets: List[TicketMark]) extends TicketsOrKeysData
    final case class Keys(keys: List[BandersnatchPublicKey]) extends TicketsOrKeysData

  private def ticketsOrKeysCodec(epochLength: Int): Codec[TicketsOrKeysData] =
    val ticketsListCodec: Codec[List[TicketMark]] = JamCodecs.fixedSizeList(JamCodecs.ticketMarkCodec, epochLength)
    val keysListCodec: Codec[List[BandersnatchPublicKey]] =
      JamCodecs.fixedSizeList(JamCodecs.bandersnatchPublicKeyCodec, epochLength)

    discriminated[TicketsOrKeysData]
      .by(byte)
      .subcaseP(0) { case t: TicketsOrKeysData.Tickets => t }(
        ticketsListCodec.xmap(TicketsOrKeysData.Tickets.apply, _.tickets)
      )
      .subcaseP(1) { case k: TicketsOrKeysData.Keys => k }(
        keysListCodec.xmap(TicketsOrKeysData.Keys.apply, _.keys)
      )

  /** Codec for safrole gamma state (gammaK + gammaZ + gammaS + gammaA). */
  def safroleGammaStateCodec(
    validatorCount: Int,
    epochLength: Int
  ): Codec[(List[ValidatorKey], ByteVector, TicketsOrKeysData, List[TicketMark])] =
    val gammaKCodec = validatorListCodec(validatorCount)
    val gammaZCodec: Codec[ByteVector] = fixedSizeBytes(BandersnatchRingCommitmentSize.toLong, bytes)
    val gammaSCodec = ticketsOrKeysCodec(epochLength)
    val gammaACodec: Codec[List[TicketMark]] = JamCodecs.compactPrefixedList(JamCodecs.ticketMarkCodec)

    (gammaKCodec :: gammaZCodec :: gammaSCodec :: gammaACodec).xmap(
      { case (k, z, s, a) => (k, z, s, a) },
      t => (t._1, t._2, t._3, t._4)
    )

  /** Codec for reports with 0/1 discriminator for Option. */
  def reportsCodec[A](coresCount: Int)(using assignmentCodec: Codec[A]): Codec[List[Option[A]]] =
    val optionalAssignmentCodec = JamCodecs.optionCodec(assignmentCodec)
    JamCodecs.fixedSizeList(optionalAssignmentCodec, coresCount)

  final case class StatCountData(
    blocks: Long,
    tickets: Long,
    preImages: Long,
    preImagesSize: Long,
    guarantees: Long,
    assurances: Long
  )

  object StatCountData:
    def zero: StatCountData = StatCountData(0, 0, 0, 0, 0, 0)

  final case class CoreStatisticsData(
    daLoad: Long,
    popularity: Long,
    imports: Long,
    extrinsicCount: Long,
    extrinsicSize: Long,
    exports: Long,
    bundleSize: Long,
    gasUsed: Long
  )

  object CoreStatisticsData:
    def zero: CoreStatisticsData = CoreStatisticsData(0, 0, 0, 0, 0, 0, 0, 0)

  final case class ServiceStatisticsData(
    serviceId: Long,
    preimagesCount: Long,
    preimagesSize: Long,
    refinesCount: Long,
    refinesGas: Long,
    importsCount: Long,
    extrinsicsCount: Long,
    extrinsicsSize: Long,
    exportsCount: Long,
    accumulatesCount: Long,
    accumulatesGas: Long
  )

  final case class ActivityStatisticsData(
    accumulator: List[StatCountData],
    previous: List[StatCountData],
    core: List[CoreStatisticsData],
    service: List[ServiceStatisticsData]
  )

  private val statCountCodec: Codec[StatCountData] =
    (uint32L :: uint32L :: uint32L :: uint32L :: uint32L :: uint32L).xmap(
      { case (b, t, p, ps, g, a) =>
        StatCountData(b.toLong, t.toLong, p.toLong, ps.toLong, g.toLong, a.toLong)
      },
      s => (
        (s.blocks & 0xFFFFFFFFL).toInt,
        (s.tickets & 0xFFFFFFFFL).toInt,
        (s.preImages & 0xFFFFFFFFL).toInt,
        (s.preImagesSize & 0xFFFFFFFFL).toInt,
        (s.guarantees & 0xFFFFFFFFL).toInt,
        (s.assurances & 0xFFFFFFFFL).toInt
      )
    )

  private val coreStatisticsCodec: Codec[CoreStatisticsData] =
    (JamCodecs.compactInteger :: JamCodecs.compactInteger :: JamCodecs.compactInteger ::
     JamCodecs.compactInteger :: JamCodecs.compactInteger :: JamCodecs.compactInteger ::
     JamCodecs.compactInteger :: JamCodecs.compactInteger).xmap(
      { case (d, p, i, xc, xs, e, b, g) =>
        CoreStatisticsData(d, p, i, xc, xs, e, b, g)
      },
      c => (c.daLoad, c.popularity, c.imports, c.extrinsicCount,
            c.extrinsicSize, c.exports, c.bundleSize, c.gasUsed)
    )

  private val serviceStatisticsCodec: Codec[ServiceStatisticsData] =
    (uint32L ::
     JamCodecs.compactInteger :: JamCodecs.compactInteger ::
     JamCodecs.compactInteger :: JamCodecs.compactInteger ::
     JamCodecs.compactInteger :: JamCodecs.compactInteger ::
     JamCodecs.compactInteger :: JamCodecs.compactInteger ::
     JamCodecs.compactInteger :: JamCodecs.compactInteger).xmap(
      { case (id, pc, ps, rc, rg, ic, xc, xs, ec, ac, ag) =>
        ServiceStatisticsData(id.toLong, pc, ps, rc, rg, ic, xc, xs, ec, ac, ag)
      },
      s => ((s.serviceId & 0xFFFFFFFFL).toInt,
            s.preimagesCount, s.preimagesSize, s.refinesCount, s.refinesGas,
            s.importsCount, s.extrinsicsCount, s.extrinsicsSize, s.exportsCount,
            s.accumulatesCount, s.accumulatesGas)
    )

  /** Codec for activity statistics. */
  def activityStatisticsCodec(validatorCount: Int, coresCount: Int): Codec[ActivityStatisticsData] =
    val accumulatorCodec = JamCodecs.fixedSizeList(statCountCodec, validatorCount)
    val previousCodec = JamCodecs.fixedSizeList(statCountCodec, validatorCount)
    val coreCodec = JamCodecs.fixedSizeList(coreStatisticsCodec, coresCount)
    val serviceCodec = JamCodecs.compactPrefixedList(serviceStatisticsCodec)

    (accumulatorCodec :: previousCodec :: coreCodec :: serviceCodec).xmap(
      { case (acc, prev, core, svc) =>
        ActivityStatisticsData(acc, prev, core, svc)
      },
      s => (s.accumulator, s.previous, s.core, s.service)
    )

  /** Use ServiceInfo codec from its companion object. */
  val serviceInfoCodec: Codec[ServiceInfo] = summon[Codec[ServiceInfo]]

  def decodeAuthPools(bytes: Array[Byte], coresCount: Int): List[List[Hash]] =
    authPoolsCodec(coresCount).decodeValue(BitVector(bytes)) match
      case Attempt.Successful(pools) => pools
      case Attempt.Failure(_) =>
        // Return empty pools on error (matching existing behavior)
        List.fill(coresCount)(List.empty[Hash])

  def decodeAuthQueues(bytes: Array[Byte], coresCount: Int, queueSize: Int): List[List[Hash]] =
    authQueuesCodec(coresCount, queueSize).decodeValue(BitVector(bytes)) match
      case Attempt.Successful(queues) => queues
      case Attempt.Failure(_) =>
        // Return empty queues on error (matching existing behavior)
        List.fill(coresCount)(List.fill(queueSize)(Hash.zero))

  def decodeActivityStatistics(
    bytes: Array[Byte],
    validatorCount: Int,
    coresCount: Int
  ): ActivityStatisticsData =
    activityStatisticsCodec(validatorCount, coresCount).decodeValue(BitVector(bytes)) match
      case Attempt.Successful(stats) => stats
      case Attempt.Failure(_) =>
        // Return empty stats on error (matching existing behavior)
        ActivityStatisticsData(
          accumulator = List.fill(validatorCount)(StatCountData.zero),
          previous = List.fill(validatorCount)(StatCountData.zero),
          core = List.fill(coresCount)(CoreStatisticsData.zero),
          service = List.empty
        )

  def decodeAccumulationHistory(bytes: Array[Byte], epochLength: Int): List[List[ByteVector]] =
    accumulationHistoryCodec(epochLength).decodeValue(BitVector(bytes)) match
      case Attempt.Successful(history) => history
      case Attempt.Failure(_) =>
        // Return empty history on error (matching existing behavior)
        List.fill(epochLength)(List.empty)

  def decodeServiceInfo(bytes: Array[Byte]): ServiceInfo =
    serviceInfoCodec.decodeValue(BitVector(bytes)).require

  final case class FullJamStateCodecSet(
    timeslot: Codec[Long],
    entropyPool: Codec[List[Hash]],
    validatorList: Codec[List[ValidatorKey]],
    authPools: Codec[List[List[Hash]]],
    authQueues: Codec[List[List[Hash]]],
    accumulationHistory: Codec[List[List[ByteVector]]],
    safroleGammaState: Codec[(List[ValidatorKey], ByteVector, TicketsOrKeysData, List[TicketMark])],
    activityStatistics: Codec[ActivityStatisticsData]
  )

  def fromConfig(config: ChainConfig): FullJamStateCodecSet =
    FullJamStateCodecSet(
      timeslot = timeslotCodec,
      entropyPool = entropyPoolCodec,
      validatorList = validatorListCodec(config.validatorCount),
      authPools = authPoolsCodec(config.coresCount),
      authQueues = authQueuesCodec(config.coresCount, config.authQueueSize),
      accumulationHistory = accumulationHistoryCodec(config.epochLength),
      safroleGammaState = safroleGammaStateCodec(config.validatorCount, config.epochLength),
      activityStatistics = activityStatisticsCodec(config.validatorCount, config.coresCount)
    )
