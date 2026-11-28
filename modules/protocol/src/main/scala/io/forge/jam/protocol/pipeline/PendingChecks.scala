package io.forge.jam.protocol.pipeline

import io.forge.jam.core.{ChainConfig, Hashing, Parallel}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.header.Header

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

  def adopt(other: PendingChecks): Unit =
    checks ++= other.checks

  def whenAllPass(action: () => Unit): Unit =
    val started = checks.toArray
    CompletableFuture.allOf(started*).thenRun(() => if started.forall(_.getNow(None).isEmpty) then action()): Unit

  /** Waits for the checks in start order and returns the first failure. */
  def firstFailure(): Option[PipelineError] =
    checks.iterator.map(Parallel.await).collectFirst { case Some(err) => err }

/** The block header's encoding and hash, each computed at most once per block. */
final class EncodedHeader(header: Header, config: ChainConfig):
  lazy val bytes: Array[Byte] = Header.headerCodec(config.epochLength).encode(header)

  /** E_U(H), the encoding without the 96-byte seal (GP 0.8.0 §5). */
  def unsealed: Array[Byte] = java.util.Arrays.copyOf(bytes, bytes.length - 96)

  lazy val hash: Hash = Hashing.blake2b256(bytes)

  /** Safrole's input entropy for this header, worked out once however many steps ask for it. */
  lazy val entropyOutput: Hash = InputExtractor.entropyOutput(header)
