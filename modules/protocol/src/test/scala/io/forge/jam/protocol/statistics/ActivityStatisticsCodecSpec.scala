package io.forge.jam.protocol.statistics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.scodec.CodecDecodingException
import io.forge.jam.protocol.report.ReportTypes.{
  CoreStatisticsRecord,
  ServiceActivityRecord,
  ServiceStatisticsEntry
}
import io.forge.jam.protocol.statistics.StatisticsTypes.StatCount

class ActivityStatisticsCodecSpec extends AnyFlatSpec with Matchers:

  private val PinnedHex: String =
    "0100000002000000030000000400000005000000060000000700000008000000090000000a0000000b0000000c000000" +
      "0d0000000e0000000f0000001000000011000000120000001300000014000000150000001600000017000000180000" +
      "006465666768696a6b6c6d6e6f70717273022a0000000102030405060708090aefbeadde0b0c0d0e0f1011121314"

  private val fixture: ActivityStatisticsCodec.ActivityStatistics =
    ActivityStatisticsCodec.ActivityStatistics(
      accumulator = List(
        StatCount(1, 2, 3, 4, 5, 6),
        StatCount(7, 8, 9, 10, 11, 12)
      ),
      previous = List(
        StatCount(13, 14, 15, 16, 17, 18),
        StatCount(19, 20, 21, 22, 23, 24)
      ),
      core = List(
        CoreStatisticsRecord(100, 101, 102, 103, 104, 105, 106, 107),
        CoreStatisticsRecord(108, 109, 110, 111, 112, 113, 114, 115)
      ),
      service = List(
        ServiceStatisticsEntry(
          42L,
          ServiceActivityRecord(
            providedCount = 1,
            providedSize = 2,
            refinementCount = 3,
            refinementGasUsed = 4,
            imports = 5,
            extrinsicCount = 6,
            extrinsicSize = 7,
            exports = 8,
            accumulateCount = 9,
            accumulateGasUsed = 10
          )
        ),
        ServiceStatisticsEntry(
          0xdeadbeefL,
          ServiceActivityRecord(
            providedCount = 11,
            providedSize = 12,
            refinementCount = 13,
            refinementGasUsed = 14,
            imports = 15,
            extrinsicCount = 16,
            extrinsicSize = 17,
            exports = 18,
            accumulateCount = 19,
            accumulateGasUsed = 20
          )
        )
      )
    )

  "ActivityStatisticsCodec" should "encode to the pinned wire bytes" in {
    val codec = ActivityStatisticsCodec.activityStatisticsCodec(2, 2)
    codec.encode(fixture).require.toHex shouldBe PinnedHex
  }

  it should "round-trip the pinned bytes back to the domain values" in {
    val bytes = ActivityStatisticsCodec
      .activityStatisticsCodec(2, 2)
      .encode(fixture)
      .require
      .toByteArray
    ActivityStatisticsCodec.decodeActivityStatistics(bytes, 2, 2) shouldBe fixture
  }

  it should "reject trailing bytes after a valid value" in {
    val bytes = ActivityStatisticsCodec
      .activityStatisticsCodec(2, 2)
      .encode(fixture)
      .require
      .toByteArray
    a[CodecDecodingException] should be thrownBy
      ActivityStatisticsCodec.decodeActivityStatistics(bytes :+ 0xab.toByte, 2, 2)
  }
