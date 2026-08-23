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

final case class BlockPipelineResult(
    safroleOutput: Option[SafroleOutputData],
    availableReports: List[WorkReport],
    accumulateRoot: Option[Hash],
    accumulationStats: Map[Long, (Long, Int)]
)

object BlockPipeline:

  def execute(
      block: Block,
      view: TrieBackedJamState,
      skipAncestryValidation: Boolean = false,
      sharedExecutor: Option[AccumulationExecutor] = None,
      ancestry: List[AncestorHeader] = List.empty
  ): Either[PipelineError, BlockPipelineResult] =
    val initialContext = PipelineContext.from(block, view.config, view)
    val storageView: Option[ServiceStorageView] = Some(view.storage)

    val pipeline: StfStepWith[BlockPipelineResult] = for {
      // Step 0: Validate extrinsic hash
      _ <- validateExtrinsicHash
      _ <- setPosteriorOffenders

      // Step 1: Safrole
      safroleOut <- safrole
      _ <- storeSafroleOutput(safroleOut)

      // Step 2: Block seal validation (uses post-Safrole state)
      _ <- validateBlockSeal
      _ <- validateEpochMark
      _ <- validateTicketsMark
      _ <- validateEntropyVrf

      // Capture post-Safrole tau for later restoration
      postSafroleTau <- inspect((v, _) => v.timeslot)

      // Step 3: Disputes
      disputeOut <- disputes
      _ <- storeDisputeOutput(disputeOut)
      _ <- validateOffendersMark

      // Step 4: Assurances
      assuranceOut <- assurances
      _ <- storeAvailableReports(assuranceOut.reported)

      // Step 5: Update beta before Reports
      _ <- updateRecentHistoryPartial

      // Step 6: Reports
      _ <- reports(skipAncestryValidation, ancestry)

      _ <- savepointStorageView(storageView)

      // Step 7: Accumulation
      accOut <- accumulation(sharedExecutor)
      accRoot = Hash(accOut.ok.toArray)
      _ <- storeAccumulateRoot(accRoot)
      _ <- storeAccumulationStats(accOut.accumulationStats)
      _ <- storeLastAccumulationOutputs(accOut.commitments)

      // Step 8: History (uses accumulateRoot)
      _ <- history(accRoot)

      // Step 9: Authorization
      _ <- authorization

      // Step 10: Preimages
      _ <- preimages

      // Step 11: Statistics (needs pre-transition tau)
      _ <- setPreTransitionTau
      _ <- statistics
      _ <- restorePostTransitionTau(postSafroleTau)

      _ <- discardStorageViewCheckpoint(storageView)

      finalResult <- inspect((_, ctx) =>
        BlockPipelineResult(
          safroleOutput = ctx.safroleOutput,
          availableReports = ctx.availableReports,
          accumulateRoot = ctx.accumulateRoot,
          accumulationStats = ctx.accumulationStats
        )
      )
    } yield finalResult

    pipeline.run((view, initialContext)).map(_._2)
