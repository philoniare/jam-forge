package io.forge.jam.node

import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
import com.typesafe.scalalogging.LazyLogging

final class PostImportBus[H, B](onEvent: (H, B) => Unit, capacity: Int = 1024, maxWaitMillis: Long = 30_000L)
    extends LazyLogging:
  private val queue = new ArrayBlockingQueue[(H, B)](capacity)
  @volatile private var running = true
  private val published = new java.util.concurrent.atomic.AtomicLong
  private val handled = new java.util.concurrent.atomic.AtomicLong

  def idle: Boolean = handled.get() == published.get()
  private val thread = new Thread(() => drain(), "jam-post-import")
  thread.setDaemon(true)
  thread.start()

  def publish(head: H, block: B, mayWait: Boolean = true): Unit =
    if running then
      val event = (head, block)
      published.incrementAndGet()
      var placed = queue.offer(event)
      if !placed && mayWait then
        val deadline = System.nanoTime() + maxWaitMillis * 1_000_000L
        try
          while !placed && running && System.nanoTime() < deadline do
            placed = queue.offer(event, 100, TimeUnit.MILLISECONDS)
        catch case _: InterruptedException => Thread.currentThread().interrupt()
      if !placed then
        handled.incrementAndGet()
        if running then logger.warn("post-import bus queue full; dropping event for {}", head)

  private def drain(): Unit =
    while running || !queue.isEmpty do
      val ev = queue.poll(100, TimeUnit.MILLISECONDS)
      if ev != null then
        try onEvent(ev._1, ev._2)
        catch case e: Exception => logger.error("post-import listener failed", e)
        finally handled.incrementAndGet()

  def close(): Unit =
    running = false
    thread.join(TimeUnit.SECONDS.toMillis(30))
