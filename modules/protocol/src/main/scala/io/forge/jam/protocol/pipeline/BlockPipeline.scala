package io.forge.jam.protocol.pipeline

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.protocol.state.JamState
import io.forge.jam.protocol.safrole.SafroleTypes.SafroleOutputData
import io.forge.jam.protocol.pipeline.PipelineTypes.*
import io.forge.jam.protocol.pipeline.LiftedStfs.*
import io.forge.jam.protocol.pipeline.IntermediateSteps.*
import io.forge.jam.protocol.pipeline.StfLifters.inspect

/**
 * Result of a successful pipeline execution.
 */
final case class BlockPipelineResult(
  state: JamState,
  safroleOutput: Option[SafroleOutputData],
  availableReports: List[WorkReport],
  accumulateRoot: Option[Hash],
  accumulationStats: Map[Long, (Long, Int)]
)

/**
 * Complete block import pipeline using Kleisli composition.
 */
object BlockPipeline:

  /**
   * Execute the full block import pipeline.
   *
   * @param block The block to import
   * @param initialState The pre-state
   * @param config Chain configuration
   * @param skipAncestryValidation Whether to skip anchor recency validation in Reports
   * @return Either an error or the final state with computed values
   */
  def execute(
    block: Block,
    initialState: JamState,
    config: ChainConfig,
    skipAncestryValidation: Boolean = false
  ): Either[PipelineError, BlockPipelineResult] =
    val initialContext = PipelineContext.from(block, config, initialState)

    val pipeline: StfStepWith[BlockPipelineResult] = for {
      // Step 1: Safrole
      safroleOut <- safrole
      _ <- storeSafroleOutput(safroleOut)

      // Step 2: Block seal validation (uses post-Safrole state)
      _ <- validateBlockSeal
      _ <- validateEpochMark
      _ <- validateTicketsMark

      // Capture post-Safrole tau for later restoration
      postSafroleTau <- inspect((s, _) => s.tau)

      // Step 3: Disputes
      _ <- disputes

      // Step 4: Assurances
      assuranceOut <- assurances
      _ <- storeAvailableReports(assuranceOut.reported)

      // Step 5: Update beta before Reports
      _ <- updateRecentHistoryPartial

      // Step 6: Reports
      _ <- reports(skipAncestryValidation)

      // Step 7: Accumulation
      accOut <- accumulation
      accRoot = Hash(accOut.ok.toArray)
      _ <- storeAccumulateRoot(accRoot)
      _ <- storeAccumulationStats(accOut.accumulationStats)

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

      // Extract final state and context
      finalResult <- inspect((s, ctx) => BlockPipelineResult(
        state = s,
        safroleOutput = ctx.safroleOutput,
        availableReports = ctx.availableReports,
        accumulateRoot = ctx.accumulateRoot,
        accumulationStats = ctx.accumulationStats
      ))
    } yield finalResult

    pipeline.run((initialState, initialContext)).map(_._2)
