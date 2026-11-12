package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, ExecutorService, RejectedExecutionException, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

import io.forge.jam.network.JamnpStream

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
      open: () => CompletableFuture[JamnpStream],
      messages: Seq[Array[Byte]],
      failed: String => Unit,
      rejected: () => Unit = () => (),
      openTimeoutMillis: Long = OpenTimeoutSeconds * 1000
  ): Unit =
    try
      egress.submit(new Runnable {
        def run(): Unit =
          if !enter(peer) then failed(s"more than $MaxWaitingPerPeer sends already wait on this peer; dropped")
          else
            val settled = new AtomicBoolean(false)
            def settle(): Boolean =
              val first = settled.compareAndSet(false, true)
              if first then leave(peer)
              first
            try
              open().whenComplete { (stream, err) =>
                if settle() then
                  try
                    if err != null then failed(String.valueOf(err.getMessage))
                    else
                      messages.foreach(stream.send)
                      stream.finish()
                  catch case e: Exception => failed(String.valueOf(e.getMessage))
                else if stream != null then stream.reset() // granted after we gave up
              }
              CompletableFuture
                .delayedExecutor(openTimeoutMillis, TimeUnit.MILLISECONDS)
                .execute(() => if settle() then failed(s"no stream granted within $openTimeoutMillis ms"))
            catch
              case e: Exception =>
                if settle() then failed(String.valueOf(e.getMessage))
      })
      ()
    catch case _: RejectedExecutionException => rejected()
