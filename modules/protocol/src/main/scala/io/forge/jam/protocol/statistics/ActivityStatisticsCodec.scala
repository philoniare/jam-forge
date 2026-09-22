package io.forge.jam.protocol.statistics

import io.forge.jam.core.scodec.{FullJamStateCodecs, JamCodecs}
import io.forge.jam.protocol.report.ReportTypes.{
  CoreStatisticsRecord,
  ServiceActivityRecord,
  ServiceStatisticsEntry
}
import io.forge.jam.protocol.statistics.StatisticsTypes.StatCount
import _root_.scodec.Codec
import _root_.scodec.codecs.*

object ActivityStatisticsCodec:

  final case class ActivityStatistics(
      accumulator: List[StatCount],
      previous: List[StatCount],
      core: List[CoreStatisticsRecord],
      service: List[ServiceStatisticsEntry]
  )

  /** Per-validator counters: 6 x u32 LE (24 bytes). */
  val statCountCodec: Codec[StatCount] = summon[Codec[StatCount]]

  /** Per-core record: 8 compact integers (d, p, i, x, z, e, b, u). */
  val coreStatisticsCodec: Codec[CoreStatisticsRecord] = summon[Codec[CoreStatisticsRecord]]

  /**
   * Per-service entry: u32 LE service id followed by 11 compact integers.
   */
  val serviceStatisticsCodec: Codec[ServiceStatisticsEntry] =
    (uint32L ::
      JamCodecs.compactInteger :: JamCodecs.compactInteger ::
      JamCodecs.compactInteger :: JamCodecs.compactInteger ::
      JamCodecs.compactInteger :: JamCodecs.compactInteger ::
      JamCodecs.compactInteger :: JamCodecs.compactInteger ::
      JamCodecs.compactInteger :: JamCodecs.compactInteger :: JamCodecs.compactInteger).xmap(
      { case (id, pc, ps, rc, rg, imp, xc, xs, exp, ac, atc, ag) =>
        ServiceStatisticsEntry(
          id & 0xffffffffL,
          ServiceActivityRecord(
            providedCount = pc.toInt,
            providedSize = ps,
            refinementCount = rc,
            refinementGasUsed = rg,
            imports = imp,
            extrinsicCount = xc,
            extrinsicSize = xs,
            exports = exp,
            accumulateCount = ac,
            accumulateTransferCount = atc,
            accumulateGasUsed = ag
          )
        )
      },
      e =>
        (
          e.id & 0xffffffffL,
          e.record.providedCount.toLong,
          e.record.providedSize,
          e.record.refinementCount,
          e.record.refinementGasUsed,
          e.record.imports,
          e.record.extrinsicCount,
          e.record.extrinsicSize,
          e.record.exports,
          e.record.accumulateCount,
          e.record.accumulateTransferCount,
          e.record.accumulateGasUsed
        )
    )

  /** Codec for the full activity-statistics state item. */
  def activityStatisticsCodec(validatorCount: Int, coresCount: Int): Codec[ActivityStatistics] =
    (JamCodecs.compactPrefixedList(statCountCodec) ::
      JamCodecs.compactPrefixedList(statCountCodec) ::
      JamCodecs.fixedSizeList(coreStatisticsCodec, coresCount) ::
      JamCodecs.compactPrefixedList(serviceStatisticsCodec)).xmap(
      { case (acc, prev, core, svc) =>
        ActivityStatistics(accumulator = acc, previous = prev, core = core, service = svc)
      },
      s => (s.accumulator, s.previous, s.core, s.service)
    )

  /**
   * Decode the activity-statistics state item, rejecting trailing bytes.
   */
  def decodeActivityStatistics(
      bytes: Array[Byte],
      validatorCount: Int,
      coresCount: Int
  ): ActivityStatistics =
    FullJamStateCodecs.decodeExact(
      activityStatisticsCodec(validatorCount, coresCount),
      bytes,
      "decodeActivityStatistics"
    )
