package io.forge.jam.node

import java.util.concurrent.{ExecutorService, TimeUnit}

import io.forge.jam.network.PeerConnection

/** Where inbound messages that cost real work (signature and ring-proof checks, state reads) are handled, so the
  * network event loops only decode and hand over: a few such messages cannot hold up every stream on their
  * loop. The work waiting is bounded in number, in bytes and per peer; a message over a limit is refused.
  */
final class InboundWork private (exec: ExecutorService, gate: WorkGate):

  /** Run `task` for a message of `bytes` from `conn` off the caller's thread; false when it was refused. */
  def submit(conn: PeerConnection, bytes: Long)(task: => Unit): Boolean =
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

  def pooled(threads: NodeThreads = new NodeThreads): InboundWork =
    pooled("inbound", Threads, MaxQueuedMessages, MaxQueuedBytes, MaxQueuedPerPeer, threads)

  /** Where requests a peer has us answer from the store (CE 128 blocks, CE 129 state ranges) are served: each
    * reads megabytes, so only a few wait, and few per peer. A request over a limit is reset; the peer asks another.
    */
  def serving(threads: NodeThreads = new NodeThreads): InboundWork =
    pooled("serve", ServingThreads, MaxQueuedRequests, MaxQueuedBytes, MaxQueuedRequestsPerPeer, threads)

  def ordered(threads: NodeThreads = new NodeThreads): InboundWork =
    pooled("announce", 1, MaxQueuedMessages, MaxQueuedBytes, MaxQueuedAnnouncementsPerPeer, threads)

  val MaxQueuedAnnouncementsPerPeer: Int = 16

  val ServingThreads: Int = 2
  val MaxQueuedRequests: Int = 64
  val MaxQueuedRequestsPerPeer: Int = 4

  private def pooled(
      name: String,
      threads: Int,
      maxQueued: Int,
      maxBytes: Long,
      perPeer: Int,
      owner: NodeThreads
  ): InboundWork =
    val exec = owner.fixed(s"jam-$name", threads, numbered = true)
    new InboundWork(exec, new WorkGate(name, maxQueued, maxBytes, perPeer))
