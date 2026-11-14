package io.forge.jam.protocol.pipeline

import cats.data.StateT
import io.forge.jam.core.{ChainConfig, Hashing, Parallel}
import io.forge.jam.core.primitives.{Hash, Ed25519PublicKey}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.header.Header
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.protocol.SignatureBatch
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.safrole.SafroleTypes.SafroleOutputData

import java.util.concurrent.CompletableFuture
import scala.collection.mutable

/**
 * Signature checks that decide only whether the block is valid, started on the shared pool so the state transition
 * runs while they verify. No posterior state component depends on whether the seal or entropy signature verifies
 * nor on whether a ticket's ring proof does: the ticket identifier is the proof's output. Nor on whether the
 * assurance and guarantee signatures do: their batch is started in the Reports step, once every key is resolved.
 *
 * A block import awaits them all before it succeeds. Checks are reported in the order they were started, and a failed
 * check wins over any error raised by a later pipeline step, so the error is the one a sequential check would give.
 */
final class PendingChecks:
  private val checks = mutable.ListBuffer.empty[CompletableFuture[Option[PipelineError]]]

  /** Starts `check`, which returns the error to report if the block is invalid. */
  def start(check: => Option[PipelineError]): Unit =
    if !StageTimers.enabled then checks += Parallel.async(check)
    else
      // How long a check waits for a pool thread, and how long it then runs.
      val queued = System.nanoTime()
      checks += Parallel.async {
        StageTimers.addNanos("check/queued", System.nanoTime() - queued)
        StageTimers.time("check/run")(check)
      }

  /** Adds a check that is already running. */
  def add(check: CompletableFuture[Option[PipelineError]]): Unit =
    checks += check

  /** Waits for the checks in start order and returns the first failure. */
  def firstFailure(): Option[PipelineError] =
    checks.iterator.map(Parallel.await).collectFirst { case Some(err) => err }

/** The block header's encoding and hash, each computed at most once per block. */
final class EncodedHeader(header: Header, config: ChainConfig):
  lazy val bytes: Array[Byte] = Header.headerCodec(config.epochLength).encode(header)

  /** E_U(H), the encoding without the 96-byte seal. */
  def unsealed: Array[Byte] = java.util.Arrays.copyOf(bytes, bytes.length - 96)

  lazy val hash: Hash = Hashing.blake2b256(bytes)

  /** Safrole's input entropy for this header, worked out once however many steps ask for it. */
  lazy val entropyOutput: Hash = io.forge.jam.protocol.traces.InputExtractor.entropyOutput(header)

/**
 * Context passed through the pipeline.
 * Contains configuration and accumulated intermediate results.
 */
final case class PipelineContext(
  config: ChainConfig,
  block: Block,
  header: EncodedHeader,
  preTransitionTau: Long,
  preSafroleValidators: IndexedSeq[ValidatorKey] = IndexedSeq.empty,
  preSafroleKappa: IndexedSeq[ValidatorKey] = IndexedSeq.empty,
  preSafroleLambda: IndexedSeq[ValidatorKey] = IndexedSeq.empty,
  // Intermediate results passed between STFs
  safroleOutput: Option[SafroleOutputData] = None,
  disputeOffendersMark: List[Ed25519PublicKey] = List.empty,
  availableReports: List[WorkReport] = List.empty,
  accumulateRoot: Option[Hash] = None,
  accumulationStats: Map[Long, (Long, Int)] = Map.empty,
  accumulationTransferCounts: Map[Long, Int] = Map.empty,
  posteriorOffenders: List[Ed25519PublicKey] = List.empty,
  pendingChecks: PendingChecks = new PendingChecks,
  signatures: SignatureBatch = new SignatureBatch
)

object PipelineContext:
  def from(block: Block, config: ChainConfig, view: TrieBackedJamState): PipelineContext =
    PipelineContext(
      config = config,
      block = block,
      header = new EncodedHeader(block.header, config),
      preTransitionTau = view.timeslot,
      preSafroleValidators = view.validators.current,
      preSafroleKappa = view.validators.current,
      preSafroleLambda = view.validators.previous,
      posteriorOffenders =
        (view.psi.offenders
          ++ block.extrinsic.disputes.culprits.map(_.key)
          ++ block.extrinsic.disputes.faults.map(_.key)).distinct
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
