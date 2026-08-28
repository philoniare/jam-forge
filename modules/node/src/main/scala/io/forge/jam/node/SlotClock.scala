package io.forge.jam.node

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

/** Wall-clock slot timing. Slots are `slotSeconds` long, numbered from the
  * JAM common era start (default: 2025-01-01T12:00:00Z per the graypaper).
  *
  * @param timeSource millisecond clock, injectable for tests
  */
final class SlotClock(
    val eraStartSeconds: Long = SlotClock.JamCommonEraSeconds,
    val slotSeconds: Int = 6,
    timeSource: () => Long = () => System.currentTimeMillis()
):
  private val slotMillis = slotSeconds * 1000L

  def currentSlot: Long =
    val now = timeSource()
    val elapsed = now - eraStartSeconds * 1000L
    if elapsed < 0 then 0L else elapsed / slotMillis

  /** Milliseconds from now until the start of `slot` (0 when in the past). */
  def millisUntilSlot(slot: Long): Long =
    val at = eraStartSeconds * 1000L + slot * slotMillis
    math.max(0L, at - timeSource())

  def slotStartMillis(slot: Long): Long =
    eraStartSeconds * 1000L + slot * slotMillis

  /** Fire `onSlot(slot)` at every slot boundary from the next slot onward.
    * Returns a handle that stops the ticking when closed.
    */
  def scheduleSlotTicks(onSlot: Long => Unit): AutoCloseable =
    val executor: ScheduledExecutorService =
      Executors.newSingleThreadScheduledExecutor { r =>
        val t = new Thread(r, "jam-slot-clock")
        t.setDaemon(true)
        t
      }

    def scheduleNext(): Unit =
      val next = currentSlot + 1
      executor.schedule(
        new Runnable {
          override def run(): Unit =
            try onSlot(next)
            finally scheduleNext()
        },
        millisUntilSlot(next),
        TimeUnit.MILLISECONDS
      )

    scheduleNext()
    () => executor.shutdownNow()

object SlotClock:
  /** 2025-01-01T12:00:00Z. */
  val JamCommonEraSeconds: Long = 1735732800L
