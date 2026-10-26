package io.forge.jam.protocol.statistics

import io.forge.jam.core.codec.{FullJamStateCodecs, JamCodecs}
import io.forge.jam.protocol.report.ReportTypes.{
  CoreStatisticsRecord,
  ServiceActivityRecord,
  ServiceStatisticsEntry
}
import io.forge.jam.protocol.statistics.StatisticsTypes.StatCount
import io.forge.jam.core.codec.Codec

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
  val serviceStatisticsCodec: Codec[ServiceStatisticsEntry] = Codec.of(
    (w, e) =>
      w.u32(e.id)
      w.compact(e.record.providedCount.toLong)
      w.compact(e.record.providedSize)
      w.compact(e.record.refinementCount)
      w.compact(e.record.refinementGasUsed)
      w.compact(e.record.imports)
      w.compact(e.record.extrinsicCount)
      w.compact(e.record.extrinsicSize)
      w.compact(e.record.exports)
      w.compact(e.record.accumulateCount)
      w.compact(e.record.accumulateTransferCount)
      w.compact(e.record.accumulateGasUsed),
    r =>
      val id = r.u32()
      val providedCount = r.compact().toInt
      val providedSize = r.compact()
      val refinementCount = r.compact()
      val refinementGasUsed = r.compact()
      val imports = r.compact()
      val extrinsicCount = r.compact()
      val extrinsicSize = r.compact()
      val exports = r.compact()
      val accumulateCount = r.compact()
      val accumulateTransferCount = r.compact()
      val accumulateGasUsed = r.compact()
      ServiceStatisticsEntry(
        id,
        ServiceActivityRecord(
          providedCount = providedCount,
          providedSize = providedSize,
          refinementCount = refinementCount,
          refinementGasUsed = refinementGasUsed,
          imports = imports,
          extrinsicCount = extrinsicCount,
          extrinsicSize = extrinsicSize,
          exports = exports,
          accumulateCount = accumulateCount,
          accumulateTransferCount = accumulateTransferCount,
          accumulateGasUsed = accumulateGasUsed
        )
      )
  )

  /** Codec for the full activity-statistics state item. */
  def activityStatisticsCodec(coresCount: Int): Codec[ActivityStatistics] =
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
      coresCount: Int
  ): ActivityStatistics =
    FullJamStateCodecs.decodeExact(
      activityStatisticsCodec(coresCount),
      bytes,
      "decodeActivityStatistics"
    )
