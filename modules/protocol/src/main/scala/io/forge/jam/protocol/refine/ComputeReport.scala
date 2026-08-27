package io.forge.jam.protocol.refine

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.primitives.{CoreIndex, Gas, Hash}
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.core.types.workpackage.{SegmentRootLookup, WorkPackage, WorkReport}
import io.forge.jam.core.types.workresult.{RefineLoad, WorkResult}
import spire.math.{UInt, UShort}

/** Errors that abort work-report computation entirely (the whole package is
  * unreportable, as opposed to per-item errors which are recorded in the
  * digests).
  */
enum ComputeReportError:
  /** Is-Authorized failed (BAD/BIG code, panic or out of gas). */
  case AuthorizationFailed(result: ExecutionResult)
  /** The authorizer trace exceeds Cmaxreportvarsize. */
  case AuthTraceOversize(length: Int)
  /** Availability-specifier construction failed. */
  case AvailabilityFailed(message: String)

/** The computed report plus everything a guarantor must distribute. */
final case class ComputedReport(
    report: WorkReport,
    bundle: WorkPackageBundle,
    bundleBytes: Array[Byte],
    exportedSegments: IndexedSeq[Array[Byte]],
    pagedProofs: IndexedSeq[Array[Byte]]
)

/** The work-report computation function Xi: runs Is-Authorized,
  * refines every work item with a running export-segment offset, assembles
  * the digests (with graceful oversize/bad-exports handling), the audit
  * bundle, and the availability specifier.
  */
class ComputeReport(val config: ChainConfig):

  /** Cmaxreportvarsize: bound on the authorizer
    * trace and the cumulative work-item output sizes.
    */
  private val MaxReportVarSize: Int = 48 * 1024

  private val isAuthorizedExecutor = new IsAuthorizedExecutor(config)
  private val refineExecutor = new RefineExecutor(config)

  /** Compute the work report for `workPackage` on `coreIndex`.
    *
    * @param segmentRootLookup the guarantor-composed dictionary mapping
    *   work-package hashes (for boxplus import references) to segments-roots
    * @param importSegments per work item, the reconstructed imported segments
    * @param extrinsicData per work item, the extrinsic blobs
    * @param justifications per work item, per import, the Merkle path
    *   justifying the segment (carried in the audit bundle)
    * @param accounts recent service-account state for historical lookups
    */
  def compute(
      workPackage: WorkPackage,
      coreIndex: Int,
      segmentRootLookup: Map[Hash, Hash],
      importSegments: IndexedSeq[IndexedSeq[Array[Byte]]],
      extrinsicData: IndexedSeq[IndexedSeq[Array[Byte]]],
      justifications: IndexedSeq[IndexedSeq[List[Array[Byte]]]],
      accounts: HistoricalLookupService
  ): Either[ComputeReportError, ComputedReport] =
    val auth = isAuthorizedExecutor.execute(workPackage, coreIndex, accounts)
    val authTrace = auth.result match
      case ExecutionResult.Ok(trace) => trace.toArray
      case other => return Left(ComputeReportError.AuthorizationFailed(other))

    if authTrace.length > MaxReportVarSize then
      return Left(ComputeReportError.AuthTraceOversize(authTrace.length))
    val digests = Vector.newBuilder[WorkResult]
    val allExports = Vector.newBuilder[Array[Byte]]
    var cumulativeOutputSize: Long = authTrace.length.toLong
    var exportOffset: Long = 0L

    workPackage.items.zipWithIndex.foreach { case (item, j) =>
      val refineResult = refineExecutor.executeRefine(
        coreIndex = coreIndex,
        workItemIndex = j,
        workPackage = workPackage,
        authorizerTrace = authTrace,
        importSegments = importSegments,
        extrinsicData = extrinsicData,
        exportSegmentOffset = exportOffset,
        accounts = accounts
      )

      val declaredExports = item.exportCount.toInt
      val zeroSegments =
        IndexedSeq.fill(declaredExports)(new Array[Byte](Csegmentsize.toInt))

      val (finalResult, exports) = refineResult.result match
        case ExecutionResult.Ok(output)
            if cumulativeOutputSize + output.length > MaxReportVarSize =>
          (ExecutionResult.Oversize, zeroSegments)
        case ExecutionResult.Ok(output)
            if refineResult.exports.length != declaredExports =>
          (ExecutionResult.BadExports, zeroSegments)
        case ok @ ExecutionResult.Ok(output) =>
          cumulativeOutputSize += output.length
          (ok, refineResult.exports.toIndexedSeq)
        case err =>
          (err, zeroSegments)

      digests += WorkResult(
        serviceId = item.service,
        codeHash = item.codeHash,
        payloadHash = Hashing.blake2b256(item.payload.toArray),
        accumulateGas = item.accumulateGasLimit,
        result = finalResult,
        refineLoad = RefineLoad(
          gasUsed = Gas(refineResult.gasUsed),
          imports = UShort(item.importSegments.length),
          extrinsicCount = UShort(item.extrinsic.length),
          extrinsicSize = UInt(item.extrinsic.map(_.len.toLong).sum.toInt),
          exports = item.exportCount
        )
      )
      allExports ++= exports
      exportOffset += declaredExports
    }

    // 3. Audit bundle and availability specifier.
    val bundle = WorkPackageBundle(
      workPackage,
      extrinsicData,
      importSegments,
      justifications
    )
    val bundleBytes = bundle.encode
    val exported = allExports.result()
    val packageHash = RefineFetch.workPackageHash(workPackage)

    AvailabilitySpecifier.build(packageHash, bundleBytes, exported, config) match
      case Left(err) => Left(ComputeReportError.AvailabilityFailed(err))
      case Right(spec) =>
        val report = WorkReport(
          packageSpec = spec,
          context = workPackage.context,
          coreIndex = CoreIndex(coreIndex),
          authorizerHash = isAuthorizedExecutor.authorizerHash(workPackage),
          authGasUsed = Gas(auth.gasUsed),
          authOutput = JamBytes(authTrace),
          segmentRootLookup = segmentRootLookup.toList
            .map { case (h, root) => SegmentRootLookup(h, root) }
            .sortBy(_.workPackageHash),
          results = digests.result().toList
        )
        Right(
          ComputedReport(
            report = report,
            bundle = bundle,
            bundleBytes = bundleBytes,
            exportedSegments = exported,
            pagedProofs = AvailabilitySpecifier.pagedProofs(exported)
          )
        )
