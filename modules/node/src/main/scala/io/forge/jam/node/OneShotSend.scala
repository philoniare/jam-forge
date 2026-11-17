package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, ExecutorService, RejectedExecutionException, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import io.forge.jam.network.MessageStream

/** A one-shot send: open a stream, write the messages, finish. The egress thread only starts the open; the
  * messages are written when the peer grants the stream, so a peer that is slow to grant one holds up nobody
  * else's sends.
  */
object OneShotSend:

  /** Sends that may wait on one peer for a stream. A peer that grants none has this many held for it at most, each
    * for at most the timeout; further sends to it are dropped, as what they carry goes stale first.
    */
  val MaxWaitingPerPeer: Int = 64

  val OpenTimeoutSeconds: Long = 10

  private val waiting = new ConcurrentHashMap[AnyRef, Integer]()

  /** Sends waiting on `peer` for a stream. */
  private[node] def waitingOn(peer: AnyRef): Int =
    val n = waiting.get(peer)
    if n == null then 0 else n.intValue

  private def enter(peer: AnyRef): Boolean =
    if peer == null then true
    else
      var admitted = false
      waiting.compute(
        peer,
        (_, n) =>
          val held = if n == null then 0 else n.intValue
          admitted = held < MaxWaitingPerPeer
          Integer.valueOf(if admitted then held + 1 else held)
      )
      admitted

  private def leave(peer: AnyRef): Unit =
    if peer != null then
      waiting.computeIfPresent(peer, (_, n) => if n.intValue <= 1 then null else Integer.valueOf(n.intValue - 1))
      ()

  /** Queue the send on `egress`. `peer` identifies who the stream is opened to (null when unknown: no per-peer
    * limit then); `failed` is told why a send did not happen. Returns at once; throws nothing.
    */
  def submit(
      egress: ExecutorService,
      peer: AnyRef,
      open: () => CompletableFuture[? <: MessageStream],
      messages: Seq[Array[Byte]],
      failed: String => Unit,
      rejected: () => Unit = () => (),
      openTimeoutMillis: Long = OpenTimeoutSeconds * 1000
  ): Unit =
    queue(egress, rejected)(() => send(peer, open, messages, failed, openTimeoutMillis, _ => ()))

  def submitEach(
      egress: ExecutorService,
      peer: AnyRef,
      open: () => CompletableFuture[? <: MessageStream],
      messages: Seq[Array[Byte]],
      failed: String => Unit,
      rejected: () => Unit = () => (),
      openTimeoutMillis: Long = OpenTimeoutSeconds * 1000
  ): Unit =
    if messages.nonEmpty then
      queue(egress, rejected)(() => new Sequence(peer, open, messages.toIndexedSeq, failed, openTimeoutMillis).advance())

  private def queue(egress: ExecutorService, rejected: () => Unit)(task: () => Unit): Unit =
    try
      egress.submit(new Runnable { def run(): Unit = task() })
      ()
    catch case _: RejectedExecutionException => rejected()

  private final class Sequence(
      peer: AnyRef,
      open: () => CompletableFuture[? <: MessageStream],
      messages: IndexedSeq[Array[Byte]],
      failed: String => Unit,
      openTimeoutMillis: Long
  ):
    private val pending = new AtomicInteger()
    private var next = 0 // touched by the one caller that took `pending` from zero
    @volatile private var stopped = false

    def advance(): Unit =
      if pending.getAndIncrement() == 0 then
        var more = true
        while more do
          if next < messages.size then
            if stopped then
              failed(s"${messages.size - next} further send(s) to this peer dropped")
              next = messages.size
            else
              val message = messages(next)
              next += 1
              send(peer, open, List(message), failed, openTimeoutMillis, sent => {
                if !sent then stopped = true
                advance()
              })
          more = pending.decrementAndGet() != 0

  private def send(
      peer: AnyRef,
      open: () => CompletableFuture[? <: MessageStream],
      messages: Seq[Array[Byte]],
      failed: String => Unit,
      openTimeoutMillis: Long,
      settled: Boolean => Unit
  ): Unit =
    if !enter(peer) then
      failed(s"more than $MaxWaitingPerPeer sends already wait on this peer; dropped")
      settled(false)
    else
      val done = new AtomicBoolean(false)
      def settle(): Boolean =
        val first = done.compareAndSet(false, true)
        if first then leave(peer)
        first
      try
        open().whenComplete { (stream, err) =>
          if settle() then
            val sent =
              try
                if err != null then
                  failed(String.valueOf(err.getMessage))
                  false
                else
                  messages.foreach(stream.send)
                  stream.finish()
                  true
              catch
                case e: Exception =>
                  failed(String.valueOf(e.getMessage))
                  false
            settled(sent)
          else if stream != null then stream.reset() // granted after we gave up
        }
        CompletableFuture
          .delayedExecutor(openTimeoutMillis, TimeUnit.MILLISECONDS)
          .execute(() =>
            if settle() then
              failed(s"no stream granted within $openTimeoutMillis ms")
              settled(false)
          )
      catch
        case e: Exception =>
          if settle() then
            failed(String.valueOf(e.getMessage))
            settled(false)
