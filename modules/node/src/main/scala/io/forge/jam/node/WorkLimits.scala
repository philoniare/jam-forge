package io.forge.jam.node

import java.util.concurrent.{
  ArrayBlockingQueue,
  Executor,
  RejectedExecutionException,
  RejectedExecutionHandler,
  ThreadFactory,
  ThreadPoolExecutor,
  TimeUnit
}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import com.typesafe.scalalogging.LazyLogging

/** Admission limit for work a peer can put on an executor's queue.
  *
  * The executors behind the node's services also run the service's own timers and import callbacks, which must not be
  * lost, so their queues stay as they are and only the peer-driven submissions go through a gate: at most `maxQueued`
  * of them, holding at most `maxBytes` of message data, wait at any time, and at most `maxPerSource` of them from
  * one source (a peer's connection), so one peer cannot fill the queue and crowd the others out. A submission over
  * a limit is refused (drop-newest) and the caller decides how to tell the peer, usually by resetting the stream.
  * One submission is always admitted when none is waiting, whatever its size.
  */
final class WorkGate(name: String, maxQueued: Int, maxBytes: Long = Long.MaxValue, perSource: Int = 0)
    extends LazyLogging:
  private val waiting = new AtomicInteger()
  private val waitingBytes = new AtomicLong()
  private val refused = new AtomicLong()
  private val bySource = new java.util.concurrent.ConcurrentHashMap[AnyRef, Integer]()

  /** A quarter of the queue for any one source unless given. */
  val maxPerSource: Int = if perSource > 0 then perSource else math.max(1, maxQueued / 4)

  /** Tasks admitted and not yet started. */
  def queued: Int = waiting.get()
  def queuedBytes: Long = waitingBytes.get()
  def dropped: Long = refused.get()

  /** Run `task` on `exec` unless a limit is reached; false when it was refused (or `exec` is shut down). `bytes` is
    * the size of the message data the task keeps alive while it waits, `source` whom it is from (null: not counted
    * per source).
    */
  def tryExecute(exec: Executor, bytes: Long = 0L, source: AnyRef = null)(task: => Unit): Boolean =
    val n = waiting.incrementAndGet()
    val b = waitingBytes.addAndGet(bytes)
    val fromSource = if source == null then 0 else bySource.merge(source, 1, (a, c) => a + c).intValue
    if fromSource > maxPerSource || (n > 1 && (n > maxQueued || b > maxBytes)) then
      leave(bytes, source)
      refuse()
      false
    else
      try
        exec.execute { () =>
          leave(bytes, source)
          task
        }
        true
      catch
        case _: RejectedExecutionException =>
          leave(bytes, source)
          false

  private def leave(bytes: Long, source: AnyRef): Unit =
    waiting.decrementAndGet()
    waitingBytes.addAndGet(-bytes)
    if source != null then bySource.computeIfPresent(source, (_, c) => if c <= 1 then null else c - 1)
    ()

  private def refuse(): Unit =
    val total = refused.incrementAndGet()
    // Powers of two: a flood logs a handful of lines, not one per message.
    if (total & (total - 1)) == 0 then
      logger.warn(s"$name: queue full ($maxQueued tasks / $maxBytes bytes); $total submission(s) refused so far")

object WorkLimits extends LazyLogging:

  /** A fixed pool whose queue holds at most `capacity` tasks. When it is full the task that has waited longest is
    * dropped to make room (drop-oldest): for sends that go stale, such as gossip, the newest is the one worth keeping.
    * Submitting never blocks, so it is safe from a network event loop. After shutdown a submission is rejected as
    * usual.
    */
  def dropOldestExecutor(name: String, threads: Int, capacity: Int, factory: ThreadFactory): DropOldestExecutor =
    new DropOldestExecutor(name, threads, capacity, factory)

  final class DropOldestExecutor private[WorkLimits] (name: String, threads: Int, capacity: Int, factory: ThreadFactory)
      extends ThreadPoolExecutor(
        threads,
        threads,
        0L,
        TimeUnit.MILLISECONDS,
        new ArrayBlockingQueue[Runnable](capacity),
        factory
      ):
    private val droppedCount = new AtomicLong()

    /** Tasks dropped because the queue was full. */
    def dropped: Long = droppedCount.get()

    /** Tasks waiting to run. */
    def queued: Int = getQueue.size()

    setRejectedExecutionHandler(new RejectedExecutionHandler {
      override def rejectedExecution(r: Runnable, e: ThreadPoolExecutor): Unit =
        if e.isShutdown then throw new RejectedExecutionException(s"$name is shut down")
        // Make room and retry (as ThreadPoolExecutor.DiscardOldestPolicy does), counting what was dropped.
        if e.getQueue.poll() != null then
          val total = droppedCount.incrementAndGet()
          if (total & (total - 1)) == 0 then
            logger.warn(s"$name: queue full ($capacity tasks); $total oldest task(s) dropped so far")
        e.execute(r)
    })

  /** Queue bound of the egress executor. One assurance or ticket is sent to every validator (1023 in the full
    * configuration), each send a task, so the bound is several such rounds.
    */
  val EgressQueueCapacity: Int = 16384
