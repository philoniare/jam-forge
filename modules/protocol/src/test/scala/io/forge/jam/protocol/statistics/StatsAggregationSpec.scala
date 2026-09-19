package io.forge.jam.protocol.statistics

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.{CoreIndex, Timeslot}
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.protocol.generators.StfGenerators
import io.forge.jam.protocol.report.ReportTypes.CoreStatisticsRecord

class StatsAggregationSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private val cores = config.coresCount

  /** Deterministic draw from an StfGenerators generator. */
  private def sample[A](gen: Gen[A], seed: Long): A =
    gen.pureApply(Gen.Parameters.default, Seed(seed))

  private def guaranteeFromSeed(seed: Long): GuaranteeExtrinsic =
    GuaranteeExtrinsic(
      report = sample(StfGenerators.genWorkReport(config), seed),
      slot = Timeslot(5),
      signatures = List.empty
    )

  private def guaranteeOnCore(seed: Long, coreIndex: Int): GuaranteeExtrinsic =
    val g = guaranteeFromSeed(seed)
    g.copy(report = g.report.copy(coreIndex = CoreIndex(coreIndex)))

  private val guarantees: List[GuaranteeExtrinsic] =
    (1L to 5L).map(guaranteeFromSeed).toList

  test("coreStatsByCore returns exactly one record per core") {
    StatsAggregation.coreStatsByCore(guarantees, cores).length shouldBe cores
  }

  test("coreStatsByCore with no guarantees is all-zero records") {
    StatsAggregation.coreStatsByCore(Nil, cores) shouldBe List.fill(cores)(CoreStatisticsRecord())
  }

  test("serviceStatsFromGuarantees with no guarantees is empty") {
    StatsAggregation.serviceStatsFromGuarantees(Nil) shouldBe Nil
  }

  test("a single guarantee's record lands at its core index") {
    val guarantee = guaranteeFromSeed(42L)
    val coreIdx = guarantee.report.coreIndex.toInt
    val byCore = StatsAggregation.coreStatsByCore(List(guarantee), cores)

    byCore(coreIdx) shouldBe StatsAggregation.coreStatsFromGuarantee(guarantee)
    // Every other core is untouched.
    byCore.zipWithIndex.filter(_._2 != coreIdx).map(_._1).distinct shouldBe List(CoreStatisticsRecord())
  }

  test("two guarantees on the same core merge field-wise") {
    val a = guaranteeOnCore(7L, 0)
    val b = guaranteeOnCore(8L, 0)
    val merged = StatsAggregation.coreStatsByCore(List(a, b), cores)(0)

    merged shouldBe StatsAggregation.mergeCoreStats(
      StatsAggregation.coreStatsFromGuarantee(a),
      StatsAggregation.coreStatsFromGuarantee(b)
    )
  }

  test("service stats are sorted by service id and total the refine loads") {
    val entries = StatsAggregation.serviceStatsFromGuarantees(guarantees)

    entries.map(_.id) shouldBe entries.map(_.id).sorted
    entries.map(_.id).distinct.length shouldBe entries.length

    val allResults = guarantees.flatMap(_.report.results)
    entries.map(_.record.refinementCount).sum shouldBe allResults.length.toLong
    entries.map(_.record.refinementGasUsed).sum shouldBe allResults.map(_.refineLoad.gasUsed.toLong).sum
  }
