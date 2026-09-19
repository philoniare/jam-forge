package io.forge.jam.protocol.statistics

import io.forge.jam.core.scodec.{CodecDecodingException, JamCodecs}
import io.forge.jam.protocol.report.ReportTypes.{
  CoreStatisticsRecord,
  ServiceActivityRecord,
  ServiceStatisticsEntry
}
import io.forge.jam.protocol.statistics.StatisticsTypes.StatCount
import _root_.scodec.{Attempt, Codec}
import _root_.scodec.bits.BitVector
import _root_.scodec.codecs.*

object ActivityStatisticsCodec:

  /** (accumulator, previous, per-core, per-service) — the four components of pi. */
  type ActivityStatistics = (
    List[StatCount],
    List[StatCount],
    List[CoreStatisticsRecord],
    List[ServiceStatisticsEntry]
  )

  /** Per-validator counters: 6 x u32 LE (24 bytes). */
  val statCountCodec: Codec[StatCount] = summon[Codec[StatCount]]

  /** Per-core record: 8 compact integers (d, p, i, x, z, e, b, u). */
  val coreStatisticsCodec: Codec[CoreStatisticsRecord] = summon[Codec[CoreStatisticsRecord]]

  /**
   * Per-service entry: u32 LE service id followed by 10 compact integers.
   */
  val serviceStatisticsCodec: Codec[ServiceStatisticsEntry] =
    (uint32L ::
      JamCodecs.compactInteger :: JamCodecs.compactInteger ::
      JamCodecs.compactInteger :: JamCodecs.compactInteger ::
      JamCodecs.compactInteger :: JamCodecs.compactInteger ::
      JamCodecs.compactInteger :: JamCodecs.compactInteger ::
      JamCodecs.compactInteger :: JamCodecs.compactInteger).xmap(
      { case (id, pc, ps, rc, rg, imp, xc, xs, exp, ac, ag) =>
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
          e.record.accumulateGasUsed
        )
    )

  /** Codec for the full activity-statistics state item. */
  def activityStatisticsCodec(validatorCount: Int, coresCount: Int): Codec[ActivityStatistics] =
    (JamCodecs.fixedSizeList(statCountCodec, validatorCount) ::
      JamCodecs.fixedSizeList(statCountCodec, validatorCount) ::
      JamCodecs.fixedSizeList(coreStatisticsCodec, coresCount) ::
      JamCodecs.compactPrefixedList(serviceStatisticsCodec)).xmap(
      { case (acc, prev, core, svc) => (acc, prev, core, svc) },
      s => (s._1, s._2, s._3, s._4)
    )

  /** Decode the activity-statistics state item, rejecting trailing bytes. */
  def decodeActivityStatistics(
      bytes: Array[Byte],
      validatorCount: Int,
      coresCount: Int
  ): ActivityStatistics =
    activityStatisticsCodec(validatorCount, coresCount).decode(BitVector(bytes)) match
      case Attempt.Successful(result) =>
        if result.remainder.nonEmpty then
          throw new CodecDecodingException(
            s"decodeActivityStatistics: ${result.remainder.bytes.size} trailing byte(s) after a valid value"
          )
        result.value
      case Attempt.Failure(err) =>
        throw new CodecDecodingException(s"decodeActivityStatistics: ${err.messageWithContext}")
