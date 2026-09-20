package io.forge.jam.node

import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
import com.typesafe.scalalogging.LazyLogging

final class PostImportBus[H, B](onEvent: (H, B) => Unit) extends LazyLogging:
  private val queue = new ArrayBlockingQueue[(H, B)](1024)
  @volatile private var running = true
  private val thread = new Thread(() => drain(), "jam-post-import")
  thread.setDaemon(true)
  thread.start()

  def publish(head: H, block: B): Unit =
    if running && !queue.offer((head, block)) then
      logger.warn("post-import bus queue full; dropping event for {}", head)

  private def drain(): Unit =
    while running || !queue.isEmpty do
      val ev = queue.poll(100, TimeUnit.MILLISECONDS)
      if ev != null then
        try onEvent(ev._1, ev._2)
        catch case e: Exception => logger.error("post-import listener failed", e)

  def close(): Unit =
    running = false
    thread.join(TimeUnit.SECONDS.toMillis(30))
