package io.forge.jam.node

import java.util.concurrent.{PriorityBlockingQueue, ThreadPoolExecutor, TimeUnit}
import java.util.concurrent.atomic.AtomicLong

import com.typesafe.scalalogging.LazyLogging

/** Runs the node's PVM-heavy work (re-executing a report to audit it, re-executing a co-guarantor's bundle to co-sign
  * it, refining a freshly submitted package) on one bounded set of threads, highest priority first and in arrival
  * order within a priority. Audits have deadlines (tranches of eight seconds), a co-signer is waited on by another
  * guarantor, and a new submission can wait the longest. Tasks here only compute: fetching a bundle or waiting for
  * co-signatures happens on other threads, so a slow peer never holds a refine thread.
  */
final class RefinePool(name: String, threads: Int = RefinePool.defaultThreads) extends LazyLogging:
  import RefinePool.*

  private val seq = new AtomicLong()

  private final class Task(val priority: Priority, val order: Long, body: () => Unit)
      extends Runnable
      with Comparable[Task]:
    def run(): Unit =
      try body()
      catch case e: Exception => logger.error(s"$name ${priority.toString.toLowerCase} task failed", e)
    def compareTo(o: Task): Int =
      val p = Integer.compare(priority.ordinal, o.priority.ordinal)
      if p != 0 then p else java.lang.Long.compare(order, o.order)

  private val exec = new ThreadPoolExecutor(
    threads,
    threads,
    0L,
    TimeUnit.MILLISECONDS,
    new PriorityBlockingQueue[Runnable](),
    { r =>
      val t = new Thread(r, s"$name-${seq.incrementAndGet()}"); t.setDaemon(true); t
    }
  )

  /** An executor that queues its tasks at `priority`, for [[WorkGate.tryExecute]]. */
  def at(priority: Priority): java.util.concurrent.Executor =
    r => exec.execute(new Task(priority, seq.incrementAndGet(), () => r.run()))

  def submit(priority: Priority)(body: => Unit): Unit =
    exec.execute(new Task(priority, seq.incrementAndGet(), () => body))

  /** Tasks waiting for a thread. */
  def queued: Int = exec.getQueue.size

  def shutdown(awaitSeconds: Long = 15): Unit =
    exec.shutdown()
    if !exec.awaitTermination(awaitSeconds, TimeUnit.SECONDS) then exec.shutdownNow()
    ()

  def shutdownNow(): Unit =
    exec.shutdownNow()
    ()

object RefinePool:
  enum Priority:
    case Audit, CoSign, Submission

  /** Leave two cores for import, networking and the voter; at least two, so an audit never waits behind a refine. */
  def defaultThreads: Int = math.max(2, Runtime.getRuntime.availableProcessors() - 2)
