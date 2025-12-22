package io.forge.jam.protocol.pipeline

import cats.data.StateT
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{Hash, Ed25519PublicKey}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.protocol.state.JamState
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
  accumulationStats: Map[Long, (Long, Int)] = Map.empty,
  // Pre-accumulation state for preimages validation (per GP §12.1)
  preAccumulationRawServiceData: Option[Map[JamBytes, JamBytes]] = None
)

object PipelineContext:
  def from(block: Block, config: ChainConfig, preState: JamState): PipelineContext =
    PipelineContext(
      config = config,
      block = block,
      preTransitionTau = preState.tau,
      preSafroleValidators = preState.validators.current
    )

/**
 * Type aliases for pipeline composition.
 */
object PipelineTypes:
  // The base effect: Either with PipelineError
  type PipelineResult[A] = Either[PipelineError, A]

  // StateT over Either for state threading with error handling
  // StateT[F, S, A] where F = PipelineResult, S = (JamState, PipelineContext), A = output
  type StfKleisli[A] = StateT[PipelineResult, (JamState, PipelineContext), A]

  // For STFs that don't produce output
  type StfStep = StfKleisli[Unit]

  // For STFs that produce typed output
  type StfStepWith[A] = StfKleisli[A]
