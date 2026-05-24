package io.forge.jam.protocol.pipeline

import cats.data.StateT
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.{Hash, Ed25519PublicKey}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.safrole.SafroleTypes.SafroleOutputData

/**
 * Context passed through the pipeline.
 * Contains configuration and accumulated intermediate results.
 */
final case class PipelineContext(
  config: ChainConfig,
  block: Block,
  preTransitionTau: Long,
  preSafroleValidators: List[ValidatorKey] = List.empty,
  // Intermediate results passed between STFs
  safroleOutput: Option[SafroleOutputData] = None,
  disputeOffendersMark: List[Ed25519PublicKey] = List.empty,
  availableReports: List[WorkReport] = List.empty,
  accumulateRoot: Option[Hash] = None,
  accumulationStats: Map[Long, (Long, Int)] = Map.empty
)

object PipelineContext:
  def from(block: Block, config: ChainConfig, view: TrieBackedJamState): PipelineContext =
    PipelineContext(
      config = config,
      block = block,
      preTransitionTau = view.timeslot,
      preSafroleValidators = view.validators.current
    )

/**
 * Type aliases for pipeline composition.
 */
object PipelineTypes:
  // The base effect: Either with PipelineError
  type PipelineResult[A] = Either[PipelineError, A]

  type StfKleisli[A] = StateT[PipelineResult, (TrieBackedJamState, PipelineContext), A]

  // For STFs that don't produce output
  type StfStep = StfKleisli[Unit]

  // For STFs that produce typed output
  type StfStepWith[A] = StfKleisli[A]
