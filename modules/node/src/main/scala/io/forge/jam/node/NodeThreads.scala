package io.forge.jam.node

import java.util.concurrent.{
  ConcurrentLinkedDeque,
  ExecutorService,
  Executors,
  ScheduledExecutorService,
  ThreadFactory,
  TimeUnit
}
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.LazyLogging

import scala.jdk.CollectionConverters.*

/** Owner of a node's thread pools. Every pool is made here under its name, and [[close]] stops them newest first and
  * waits for each: a service made later may use one made earlier, and once `close` returns nothing of theirs still
  * runs, so the stores can be closed (a read on a closed RocksDB crashes in native code).
  *
  * A service made on its own, as in a test, gets its own owner by default and stops its pools as before.
  */
final class NodeThreads extends LazyLogging:

  /** Newest first. */
  private val pools = new ConcurrentLinkedDeque[(String, ExecutorService)]()
  @volatile private var closed = false

  /** Take a pool made elsewhere into the closing order. */
  def own[E <: ExecutorService](name: String)(make: ThreadFactory => E): E = own(name, numbered = false)(make)

  def own[E <: ExecutorService](name: String, numbered: Boolean)(make: ThreadFactory => E): E =
    if closed then throw new IllegalStateException(s"thread pool $name made after the node's threads were closed")
    val pool = make(if numbered then NodeThreads.numbered(name) else NodeThreads.daemon(name))
    pools.addFirst(name -> pool)
    pool

  def single(name: String): ExecutorService = own(name)(Executors.newSingleThreadExecutor)
  def scheduled(name: String): ScheduledExecutorService = own(name)(Executors.newSingleThreadScheduledExecutor)
  def cached(name: String): ExecutorService = own(name)(Executors.newCachedThreadPool)
  def fixed(name: String, threads: Int, numbered: Boolean = false): ExecutorService =
    own(name, numbered)(Executors.newFixedThreadPool(threads, _))

  /** Stop every pool not stopped yet (interrupting its tasks), newest first, waiting up to `awaitMillis` for each.
    * Returns the names of the pools still running after their wait.
    */
  def close(awaitMillis: Long = NodeThreads.CloseAwaitMillis): List[String] =
    closed = true
    val running = pools.asScala.toList.flatMap { (name, pool) =>
      if !pool.isShutdown then pool.shutdownNow()
      val stopped =
        try pool.awaitTermination(awaitMillis, TimeUnit.MILLISECONDS)
        catch
          case _: InterruptedException =>
            Thread.currentThread.interrupt()
            pool.isTerminated
      Option.when(!stopped)(name)
    }
    if running.nonEmpty then logger.warn(s"thread pools still running after close: ${running.mkString(", ")}")
    running

object NodeThreads:

  /** How long [[NodeThreads.close]] waits for one pool. */
  val CloseAwaitMillis: Long = 10_000L

  /** Daemon threads all named `name`. */
  def daemon(name: String): ThreadFactory = r =>
    val t = new Thread(r, name)
    t.setDaemon(true)
    t

  /** Daemon threads named `name-1`, `name-2`, ... */
  def numbered(name: String): ThreadFactory =
    val n = new AtomicInteger()
    r =>
      val t = new Thread(r, s"$name-${n.incrementAndGet()}")
      t.setDaemon(true)
      t
