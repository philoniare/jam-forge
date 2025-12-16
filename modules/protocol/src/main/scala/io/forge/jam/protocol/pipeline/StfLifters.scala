package io.forge.jam.protocol.pipeline

import cats.data.StateT
import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.state.JamState
import io.forge.jam.protocol.pipeline.PipelineTypes.*

/**
 * Functions to lift individual STF implementations into the pipeline.
 */
object StfLifters:

  /**
   * Lift a standard STF: (Input, JamState, ChainConfig) => (JamState, Either[Error, Output])
   */
  def liftStandard[I, O, E](
    stf: (I, JamState, ChainConfig) => (JamState, Either[E, O]),
    extractInput: PipelineContext => I,
    wrapError: E => PipelineError
  ): StfStepWith[O] = StateT { case (state, ctx) =>
    val input = extractInput(ctx)
    val (newState, result) = stf(input, state, ctx.config)
    result match
      case Right(output) => Right(((newState, ctx), output))
      case Left(err) => Left(wrapError(err))
  }

  /**
   * Lift a state-only STF: (Input, JamState, ChainConfig) => JamState
   */
  def liftStateOnly[I](
    stf: (I, JamState, ChainConfig) => JamState,
    extractInput: PipelineContext => I
  ): StfStep = StateT { case (state, ctx) =>
    val input = extractInput(ctx)
    val newState = stf(input, state, ctx.config)
    Right(((newState, ctx), ()))
  }

  /**
   * Lift a no-config STF: (Input, JamState) => (JamState, Either[Error, Output])
   */
  def liftNoConfig[I, O, E](
    stf: (I, JamState) => (JamState, Either[E, O]),
    extractInput: PipelineContext => I,
    wrapError: E => PipelineError
  ): StfStepWith[O] = StateT { case (state, ctx) =>
    val input = extractInput(ctx)
    val (newState, result) = stf(input, state)
    result match
      case Right(output) => Right(((newState, ctx), output))
      case Left(err) => Left(wrapError(err))
  }

  /**
   * Lift a Report STF (has extra skipAncestryValidation parameter).
   */
  def liftReport[I, O, E](
    stf: (I, JamState, ChainConfig, Boolean) => (JamState, Either[E, O]),
    extractInput: PipelineContext => I,
    wrapError: E => PipelineError,
    skipAncestryValidation: Boolean
  ): StfStepWith[O] = StateT { case (state, ctx) =>
    val input = extractInput(ctx)
    val (newState, result) = stf(input, state, ctx.config, skipAncestryValidation)
    result match
      case Right(output) => Right(((newState, ctx), output))
      case Left(err) => Left(wrapError(err))
  }

  /**
   * Pure state modification step (for intermediate logic).
   */
  def modifyState(f: (JamState, PipelineContext) => JamState): StfStep =
    StateT { case (state, ctx) =>
      Right(((f(state, ctx), ctx), ()))
    }

  /**
   * Pure context modification step (for passing data between STFs).
   */
  def modifyContext(f: PipelineContext => PipelineContext): StfStep =
    StateT { case (state, ctx) =>
      Right(((state, f(ctx)), ()))
    }

  /**
   * Validation step that may fail.
   */
  def validate(check: (JamState, PipelineContext) => Either[PipelineError, Unit]): StfStep =
    StateT { case (state, ctx) =>
      check(state, ctx).map(_ => ((state, ctx), ()))
    }

  /**
   * Inspection step for extracting values from state.
   */
  def inspect[A](f: (JamState, PipelineContext) => A): StfStepWith[A] =
    StateT { case (state, ctx) =>
      Right(((state, ctx), f(state, ctx)))
    }
