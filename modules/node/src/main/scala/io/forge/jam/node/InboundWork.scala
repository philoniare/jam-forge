package io.forge.jam.node

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import io.forge.jam.network.JamnpConnection

/** Where inbound messages that cost real work (signature and ring-proof checks, state reads) are handled, so the
  * network event loops only decode and hand over: a few such messages cannot hold up every stream on their
  * loop. The work waiting is bounded in number, in bytes and per peer; a message over a limit is refused.
  */
final class InboundWork private (exec: ExecutorService, gate: WorkGate):

  /** Run `task` for a message of `bytes` from `conn` off the caller's thread; false when it was refused. */
  def submit(conn: JamnpConnection, bytes: Long)(task: => Unit): Boolean =
    if exec == null then
      task
      true
    else gate.tryExecute(exec, bytes, conn)(task)

  /** Messages waiting for a thread. */
  def queued: Int = if gate == null then 0 else gate.queued

  def shutdown(): Unit =
    if exec != null then
      exec.shutdown()
      exec.awaitTermination(10, TimeUnit.SECONDS)
      ()

object InboundWork:

  val Threads: Int = 4
  val MaxQueuedMessages: Int = 4096
  val MaxQueuedBytes: Long = 32L * 1024 * 1024
  val MaxQueuedPerPeer: Int = 256

  /** Handles each message on the calling thread: for services built without a node around them. */
  val direct: InboundWork = new InboundWork(null, null)

  def pooled(): InboundWork =
    val n = new AtomicInteger()
    val exec = Executors.newFixedThreadPool(
      Threads,
      r =>
        val t = new Thread(r, s"jam-inbound-${n.incrementAndGet()}"); t.setDaemon(true); t
    )
    new InboundWork(exec, new WorkGate("inbound", MaxQueuedMessages, MaxQueuedBytes, MaxQueuedPerPeer))
