package io.forge.jam.protocol.refine

import io.forge.jam.core.{ChainConfig, constants}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.history.HistoricalBeta
import io.forge.jam.core.types.workpackage.WorkPackage

/** The checks a guarantor makes on a work-package before refining it: the work-package limits, and the parts of the
  * guarantee extrinsic's validity that do not depend on the refine output, judged against the chain
  * head. A package that fails one would produce a report no block author can include. Pure: no state access.
  */
object WorkPackageValidator:

  /** What the checks read from the chain at the head the report is expected to be included over. */
  final case class ChainContext(
      /** The authorizer pool of the core. */
      authPool: List[Hash],
      /** Recent blocks. */
      recentHistory: List[HistoricalBeta],
      /** The slot the report would be included at. */
      slot: Long
  )

  /** Top bit of an import index: the root is a work-package hash, not a segments-root. */
  private val BoxplusFlag = 0x8000

  def validate(
      wp: WorkPackage,
      coreIndex: Int,
      chain: ChainContext,
      config: ChainConfig
  ): Either[String, Unit] =
    val items = wp.items
    def sum(f: io.forge.jam.core.types.workitem.WorkItem => BigInt): BigInt = items.map(f).sum
    val imports = sum(i => i.importSegments.length)
    val exports = sum(i => i.exportCount.toInt)
    val extrinsics = sum(i => i.extrinsic.length)
    // Encoded size: config blob, authorization token, imported segments and extrinsics; with the authorizer code and
    // payloads at most 13,791,360 bytes (Cmaxinputsize here).
    val inputSize = BigInt(wp.authorization.length) + wp.authorizerConfig.length + sum(i =>
      BigInt(i.payload.length) + BigInt(i.importSegments.length) * constants.Csegmentfootprint +
        i.extrinsic.map(e => BigInt(e.len.toLong)).sum
    )
    val boxplus = items.flatMap(_.importSegments).filter(s => (s.index.toInt & BoxplusFlag) != 0).map(_.treeRoot).distinct
    val ctx = wp.context
    lazy val anchor = chain.recentHistory.find(_.headerHash == ctx.anchor)
    val lookupSlot = ctx.lookupAnchorSlot.toLong
    for
      _ <- check(coreIndex >= 0 && coreIndex < config.coresCount, s"core $coreIndex out of range")
      _ <- check(items.nonEmpty && items.size <= config.maxWorkItems, s"${items.size} work items")
      _ <- check(imports <= constants.Cmaxpackageimports, s"$imports imports")
      _ <- check(exports <= constants.Cmaxpackageexports, s"$exports exports")
      _ <- check(extrinsics <= constants.Cmaxpackagexts, s"$extrinsics extrinsics")
      _ <- check(inputSize <= constants.Cmaxinputsize, s"input size $inputSize")
      // The work-package rule is strict (<); the reports STF accepts equality, which is what inclusion needs.
      _ <- check(sum(i => unsigned(i.accumulateGasLimit.toLong)) <= config.reportAccGas, "accumulate gas over the report limit")
      _ <- check(sum(i => unsigned(i.refineGasLimit.toLong)) <= config.maxRefineGas, "refine gas over the package limit")
      _ <- check(boxplus.size + ctx.prerequisites.size <= config.maxDependencies, "too many dependencies")
      _ <- check(chain.authPool.contains(IsAuthorizedExecutor.authorizerHashOf(wp)), "authorizer not in the core's pool")
      _ <- check(anchor.isDefined, "anchor not in recent history")
      _ <- check(anchor.forall(_.stateRoot == ctx.stateRoot), "anchor state root mismatch")
      _ <- check(anchor.forall(_.beefyRoot == ctx.beefyRoot), "anchor beefy root mismatch")
      _ <- check(anchor.forall(_.slot == ctx.anchorSlot.toLong), "anchor slot mismatch")
      _ <- check(
        lookupSlot <= chain.slot && chain.slot - lookupSlot <= config.maxLookupAnchorAge,
        s"lookup anchor slot $lookupSlot not within ${config.maxLookupAnchorAge} of ${chain.slot}"
      )
    yield ()

  private def unsigned(x: Long): BigInt = BigInt(java.lang.Long.toUnsignedString(x))

  private def check(ok: Boolean, why: => String): Either[String, Unit] = if ok then Right(()) else Left(why)
