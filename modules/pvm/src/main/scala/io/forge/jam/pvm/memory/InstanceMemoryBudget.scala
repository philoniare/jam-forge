package io.forge.jam.pvm.memory

import java.util.concurrent.locks.ReentrantLock

/** A byte budget for the memory of PVM instances that are running at the same time.
  *
  * An instance reserves its declared size (read-write data, heap pages, stack, arguments) before that memory is
  * allocated and gives it back when the run ends; a heap that grows during the run reserves the growth. When the
  * budget is used up, the next instance waits for a running one to finish, so parallel runs degrade towards one at a
  * time instead of the JVM heap growing with the number of services in a block. Waiting changes only when a run
  * starts, never what it computes.
  *
  * It cannot deadlock:
  *   - a run that holds nothing is admitted whenever nothing is reserved, however large it is, so an instance larger
  *     than the whole budget runs alone;
  *   - a run that already holds a reservation and needs more (heap growth) waits only while some other run can still
  *     finish. When every run is waiting to grow, the one that has waited longest goes ahead over budget and stays the
  *     only one allowed to until it ends, so the overshoot is at most one instance;
  *   - a reservation made on a thread that already holds one (a nested run) never waits.
  */
final class InstanceMemoryBudget(val limitBytes: Long):
  import InstanceMemoryBudget.Reservation

  private val lock = new ReentrantLock()
  private val changed = lock.newCondition()
  private var inUse = 0L
  private var peak = 0L

  /** Top-level reservations whose run is not waiting to grow: the runs that can still finish and release. */
  private var running = 0

  /** The run let through while every run was waiting to grow; it alone grows without waiting until it ends. */
  private var overBudget: Reservation = null
  private val admission = new java.util.ArrayDeque[Reservation]()
  private val growers = new java.util.ArrayDeque[Reservation]()
  private val current = new ThreadLocal[Reservation]()

  /** Bytes reserved now. */
  def inUseBytes: Long = locked(inUse)

  /** The most bytes reserved at once since the last [[resetPeak]]. */
  def peakBytes: Long = locked(peak)

  def resetPeak(): Unit = locked { peak = inUse }

  /** Runs waiting now: to start, or to grow. */
  def waitingCount: Int = locked(admission.size + growers.size)

  /** Reserve `bytes` for a run on the calling thread, waiting for room if needed. Close the reservation on the same
    * thread when the run ends.
    */
  def reserve(bytes: Long): Reservation =
    val outer = current.get()
    val r = new Reservation(this, outer)
    val n = math.max(bytes, 0L)
    lock.lock()
    try
      if outer == null then
        if !(admission.isEmpty && fits(n)) then
          admission.addLast(r)
          try
            while !((admission.peekFirst() eq r) && fits(n)) do changed.awaitUninterruptibly()
          finally
            admission.remove(r)
            changed.signalAll() // the next in line may fit too
        running += 1
      take(r, n)
    finally lock.unlock()
    current.set(r)
    r

  private def fits(n: Long): Boolean = inUse == 0 || inUse + n <= limitBytes

  private def take(r: Reservation, n: Long): Unit =
    r.held += n
    inUse += n
    if inUse > peak then peak = inUse

  private[memory] def grow(r: Reservation, bytes: Long): Unit =
    val n = math.max(bytes, 0L)
    lock.lock()
    try
      if r.outer == null && (r ne overBudget) && inUse + n > limitBytes then
        running -= 1
        growers.addLast(r)
        changed.signalAll() // the head of the line may now be the only run that can go on
        try
          // With no run left that could release, waiting would never end: the longest waiter goes ahead, and
          // stays the one run allowed over the budget until it ends.
          while !(inUse + n <= limitBytes || (overBudget == null && running == 0 && (growers.peekFirst() eq r))) do
            changed.awaitUninterruptibly()
          if inUse + n > limitBytes then overBudget = r
        finally
          growers.remove(r)
          running += 1
      take(r, n)
    finally lock.unlock()

  private[memory] def shrink(r: Reservation, bytes: Long): Unit =
    lock.lock()
    try
      val n = math.min(math.max(bytes, 0L), r.held)
      r.held -= n
      inUse -= n
      changed.signalAll()
    finally lock.unlock()

  private[memory] def close(r: Reservation): Unit =
    lock.lock()
    try
      if !r.closed then
        r.closed = true
        inUse -= r.held
        r.held = 0
        if r.outer == null then running -= 1
        if overBudget eq r then overBudget = null
        changed.signalAll()
    finally lock.unlock()
    if current.get() eq r then
      if r.outer == null then current.remove() else current.set(r.outer)

  private inline def locked[A](inline f: A): A =
    lock.lock()
    try f
    finally lock.unlock()

object InstanceMemoryBudget:

  /** What one run holds of a budget. Used by the thread that made it. */
  final class Reservation private[memory] (budget: InstanceMemoryBudget, private[memory] val outer: Reservation)
      extends AutoCloseable:
    private[memory] var held = 0L
    private[memory] var closed = false

    /** Reserve `bytes` more (the instance's memory is about to grow); may wait for other runs to finish. */
    def grow(bytes: Long): Unit = budget.grow(this, bytes)

    /** Give back `bytes` (a buffer the instance replaced). */
    def shrink(bytes: Long): Unit = budget.shrink(this, bytes)

    /** Give everything back. */
    override def close(): Unit = budget.close(this)

  /** `-Djam.pvm.instanceMemoryBytes`: the process-wide budget in bytes. */
  val LimitProperty = "jam.pvm.instanceMemoryBytes"

  /** A quarter of the JVM's maximum heap unless set: room for a few instances of the largest declarable size
    * (about 256 MiB each) next to everything else the heap holds.
    */
  private def defaultLimit: Long =
    Option(System.getProperty(LimitProperty)).flatMap(_.toLongOption).filter(_ > 0)
      .getOrElse(math.max(64L << 20, Runtime.getRuntime.maxMemory() / 4))

  /** The budget every accumulate, refine and is-authorized run in this process shares. */
  val global: InstanceMemoryBudget = new InstanceMemoryBudget(defaultLimit)
