package io.forge.jam.protocol.pipeline

import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.protocol.report.ReportTypes.AncestorHeader
import io.forge.jam.protocol.state.{ServiceStorageView, TrieBackedJamState}
import io.forge.jam.protocol.safrole.SafroleTypes.SafroleOutputData
import io.forge.jam.protocol.accumulation.AccumulationExecutor
import io.forge.jam.protocol.pipeline.PipelineTypes.*
import io.forge.jam.protocol.pipeline.LiftedStfs.*
import io.forge.jam.protocol.pipeline.IntermediateSteps.*
import io.forge.jam.protocol.pipeline.StfLifters.inspect

import scala.util.control.NonFatal

final case class BlockPipelineResult(
    safroleOutput: Option[SafroleOutputData],
    availableReports: List[WorkReport],
    accumulateRoot: Option[Hash],
    accumulationStats: Map[Long, (Long, Int)],
    accumulationTransferCounts: Map[Long, Int],
    headerHash: Hash
)

object BlockPipeline:

  /**
   * The header checks of [[execute]] that run before any extrinsic is processed, on their own: the extrinsic hash,
   * Safrole's epoch step (slot after the state's, then the validator keys, entropy and sealing sequence for the
   * block's slot), then the seal and the entropy VRF signature under the author key that state implies. These are the
   * same steps `execute` runs, so a block refused here is refused by `execute` on the same state. `view` is advanced
   * by the epoch step and must be a throwaway.
   *
   * With `parentState` the view is the block's parent's posterior state and the epoch and winning-tickets markers
   * are checked too. Without it the view is an earlier ancestor's state that fixes the same author key, seal entropy
   * and sealing sequence for the block's slot (the caller's claim); the markers depend on the parent itself and are
   * left out.
   */
  def verifyHeader(block: Block, view: TrieBackedJamState, parentState: Boolean): Either[PipelineError, Unit] =
    val initialContext = PipelineContext.from(block, view.config, view)
    val markers: StfStep =
      if parentState then validateEpochMark.flatMap(_ => validateTicketsMark)
      else inspect((_, _) => ())
    val steps: StfStep = for {
      _ <- validateExtrinsicHash
      _ <- setPosteriorOffenders
      safroleOut <- safroleEpoch
      _ <- storeSafroleOutput(safroleOut)
      _ <- markers
      _ <- validateBlockSeal
      _ <- validateEntropyVrf
    } yield ()
    val pending = initialContext.pendingChecks
    steps.run((view, initialContext)) match
      case Left(err) => Left(pending.firstFailure().getOrElse(err))
      case Right(_)  => pending.firstFailure().toLeft(())

  def execute(
      block: Block,
      view: TrieBackedJamState,
      skipAncestryValidation: Boolean = false,
      sharedExecutor: Option[AccumulationExecutor] = None,
      ancestry: List[AncestorHeader] = List.empty,
      whileChecksRun: BlockPipelineResult => Unit = _ => ()
  ): Either[PipelineError, BlockPipelineResult] =
    val initialContext = StageTimers.time("x/context")(PipelineContext.from(block, view.config, view))
    val storageView: Option[ServiceStorageView] = Some(view.storage)

    val pipeline: StfStepWith[BlockPipelineResult] = for {
      // Step 0: Validate extrinsic hash
      _ <- StageTimers.step("extrinsic-hash")(validateExtrinsicHash)
      _ <- StageTimers.step("x/setPosteriorOffenders")(setPosteriorOffenders)

      // Step 1: Safrole
      safroleOut <- StageTimers.step("safrole")(safroleEpoch)
      _ <- StageTimers.step("x/storeSafroleOutput")(storeSafroleOutput(safroleOut))

      // Step 2: Block seal validation (uses post-Safrole state)
      _ <- StageTimers.step("x/validateEpochMark")(validateEpochMark)
      _ <- StageTimers.step("x/validateTicketsMark")(validateTicketsMark)
      _ <- StageTimers.step("seal")(validateBlockSeal)
      _ <- StageTimers.step("entropy-vrf")(validateEntropyVrf)

      _ <- StageTimers.step("tickets")(safroleTickets)

      // Capture post-Safrole tau for later restoration
      postSafroleTau <- inspect((v, _) => v.timeslot)

      // Step 3: Disputes
      disputeOut <- StageTimers.step("disputes")(disputes)
      _ <- StageTimers.step("x/storeDisputeOutput")(storeDisputeOutput(disputeOut))
      _ <- StageTimers.step("x/validateOffendersMark")(validateOffendersMark)

      // Step 4: Assurances
      assuranceOut <- StageTimers.step("assurances")(assurances)
      _ <- StageTimers.step("x/storeAvailableReports")(storeAvailableReports(assuranceOut.reported))

      // Step 5: Update beta before Reports
      _ <- StageTimers.step("x/updateRecentHistoryPartial")(updateRecentHistoryPartial)

      // Step 6: Reports
      _ <- StageTimers.step("reports")(reports(skipAncestryValidation, ancestry))

      _ <- StageTimers.step("x/savepointStorageView")(savepointStorageView(storageView))

      // Step 7: Accumulation
      accOut <- StageTimers.step("accumulation")(accumulation(sharedExecutor))
      accRoot = Hash(accOut.ok.toArray)
      _ <- StageTimers.step("x/storeAccumulateRoot")(storeAccumulateRoot(accRoot))
      _ <- StageTimers.step("x/storeAccumulationStats")(storeAccumulationStats(accOut.accumulationStats))
      _ <- StageTimers.step("x/storeAccumulationTransferCounts")(storeAccumulationTransferCounts(accOut.accumulationTransferCounts))
      _ <- StageTimers.step("x/storeLastAccumulationOutputs")(storeLastAccumulationOutputs(accOut.commitments))

      // Step 8: History (uses accumulateRoot)
      _ <- StageTimers.step("history")(history(accRoot))

      // Step 9: Authorization
      _ <- StageTimers.step("authorization")(authorization)

      // Step 10: Preimages
      _ <- StageTimers.step("preimages")(preimages)

      // Step 11: Statistics (needs pre-transition tau)
      _ <- StageTimers.step("x/setPreTransitionTau")(setPreTransitionTau)
      _ <- StageTimers.step("statistics")(statistics)
      _ <- StageTimers.step("x/restorePostTransitionTau")(restorePostTransitionTau(postSafroleTau))

      _ <- StageTimers.step("x/discardStorageViewCheckpoint")(discardStorageViewCheckpoint(storageView))

      finalResult <- inspect((_, ctx) =>
        BlockPipelineResult(
          safroleOutput = ctx.safroleOutput,
          availableReports = ctx.availableReports,
          accumulateRoot = ctx.accumulateRoot,
          accumulationStats = ctx.accumulationStats,
          accumulationTransferCounts = ctx.accumulationTransferCounts,
          headerHash = ctx.header.hash
        )
      )
    } yield finalResult

    val pending = initialContext.pendingChecks
    // `whileChecksRun` is the caller's work on a block whose steps all passed, done here so that it overlaps the
    // signature checks still running on other threads. The caller must not let it take effect until this returns
    // Right: a check may yet fail.
    val result =
      try
        val ran = pipeline.run((view, initialContext))
        ran.foreach((_, output) => whileChecksRun(output))
        ran
      catch
        case NonFatal(e) =>
          pending.firstFailure() match
            case Some(err) => Left(err)
            case None      => throw e
    val failed = StageTimers.time("await-checks")(pending.firstFailure())
    result match
      case Left(err)          => Left(failed.getOrElse(err))
      case Right((_, output)) => failed.toLeft(output)
