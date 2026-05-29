package io.forge.jam.protocol.pipeline

import cats.data.StateT
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.pipeline.PipelineTypes.*

object StfLifters:

  def liftStandard[I, O, E](
      stf: (I, TrieBackedJamState) => Either[E, O],
      extractInput: PipelineContext => I,
      wrapError: E => PipelineError
  ): StfStepWith[O] = StateT { case (view, ctx) =>
    val input = extractInput(ctx)
    stf(input, view) match
      case Right(output) => Right(((view, ctx), output))
      case Left(err)     => Left(wrapError(err))
  }

  def liftStandardCtx[I, O, E](
      stf: (I, TrieBackedJamState, PipelineContext) => Either[E, O],
      extractInput: PipelineContext => I,
      wrapError: E => PipelineError
  ): StfStepWith[O] = StateT { case (view, ctx) =>
    val input = extractInput(ctx)
    stf(input, view, ctx) match
      case Right(output) => Right(((view, ctx), output))
      case Left(err)     => Left(wrapError(err))
  }

  def liftStateOnly[I](
      stf: (I, TrieBackedJamState) => Unit,
      extractInput: PipelineContext => I
  ): StfStep = StateT { case (view, ctx) =>
    val input = extractInput(ctx)
    stf(input, view)
    Right(((view, ctx), ()))
  }

  def modifyState(f: (TrieBackedJamState, PipelineContext) => Unit): StfStep =
    StateT { case (view, ctx) =>
      f(view, ctx)
      Right(((view, ctx), ()))
    }

  def modifyContext(f: PipelineContext => PipelineContext): StfStep =
    StateT { case (view, ctx) =>
      Right(((view, f(ctx)), ()))
    }

  def validate(
      check: (TrieBackedJamState, PipelineContext) => Either[PipelineError, Unit]
  ): StfStep =
    StateT { case (view, ctx) =>
      check(view, ctx).map(_ => ((view, ctx), ()))
    }

  def inspect[A](
      f: (TrieBackedJamState, PipelineContext) => A
  ): StfStepWith[A] =
    StateT { case (view, ctx) =>
      Right(((view, ctx), f(view, ctx)))
    }
