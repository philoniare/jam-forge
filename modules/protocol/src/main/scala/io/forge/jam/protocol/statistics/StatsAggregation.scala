package io.forge.jam.protocol.statistics

import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.protocol.report.ReportTypes.{CoreStatisticsRecord, ServiceActivityRecord, ServiceStatisticsEntry}

object StatsAggregation:

  def coreStatsByCore(guarantees: List[GuaranteeExtrinsic], coresCount: Int): List[CoreStatisticsRecord] =
    val statsByCore = guarantees
      .groupMapReduce(_.report.coreIndex.toInt)(coreStatsFromGuarantee)(mergeCoreStats)

    (0 until coresCount).map(i => statsByCore.getOrElse(i, CoreStatisticsRecord())).toList

  /** Core statistics contributed by a single guarantee. */
  def coreStatsFromGuarantee(guarantee: GuaranteeExtrinsic): CoreStatisticsRecord =
    val report = guarantee.report
    val totals = report.results.foldLeft((0L, 0L, 0L, 0L, 0L)) {
      case ((imports, extCount, extSize, exports, gas), result) =>
        val load = result.refineLoad
        (
          imports + load.imports.toLong,
          extCount + load.extrinsicCount.toLong,
          extSize + load.extrinsicSize.toLong,
          exports + load.exports.toLong,
          gas + load.gasUsed.toLong
        )
    }
    CoreStatisticsRecord(
      imports = totals._1,
      extrinsicCount = totals._2,
      extrinsicSize = totals._3,
      exports = totals._4,
      bundleSize = report.packageSpec.length.toLong,
      gasUsed = totals._5
    )

  /** Field-wise sum of two core statistics records. */
  def mergeCoreStats(a: CoreStatisticsRecord, b: CoreStatisticsRecord): CoreStatisticsRecord =
    CoreStatisticsRecord(
      imports = a.imports + b.imports,
      extrinsicCount = a.extrinsicCount + b.extrinsicCount,
      extrinsicSize = a.extrinsicSize + b.extrinsicSize,
      exports = a.exports + b.exports,
      bundleSize = a.bundleSize + b.bundleSize,
      gasUsed = a.gasUsed + b.gasUsed
    )

  def serviceStatsFromGuarantees(guarantees: List[GuaranteeExtrinsic]): List[ServiceStatisticsEntry] =
    if guarantees.isEmpty then return List.empty

    val allResults =
      for
        guarantee <- guarantees
        result <- guarantee.report.results
      yield result

    allResults
      .groupMapReduce(r => r.serviceId.toInt.toLong & 0xffffffffL)(computeServiceStats)(mergeServiceStats)
      .map { case (id, record) => ServiceStatisticsEntry(id, record) }
      .toList
      .sortBy(_.id)

  private def computeServiceStats(result: io.forge.jam.core.types.workresult.WorkResult): ServiceActivityRecord =
    val load = result.refineLoad
    ServiceActivityRecord(
      refinementCount = 1,
      refinementGasUsed = load.gasUsed.toLong,
      extrinsicCount = load.extrinsicCount.toLong,
      extrinsicSize = load.extrinsicSize.toLong,
      imports = load.imports.toLong,
      exports = load.exports.toLong
    )

  private def mergeServiceStats(a: ServiceActivityRecord, b: ServiceActivityRecord): ServiceActivityRecord =
    ServiceActivityRecord(
      refinementCount = a.refinementCount + b.refinementCount,
      refinementGasUsed = a.refinementGasUsed + b.refinementGasUsed,
      extrinsicCount = a.extrinsicCount + b.extrinsicCount,
      extrinsicSize = a.extrinsicSize + b.extrinsicSize,
      imports = a.imports + b.imports,
      exports = a.exports + b.exports
    )
