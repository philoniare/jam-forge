package io.forge.jam.protocol.pipeline

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.LongAdder

import cats.data.StateT
import io.forge.jam.protocol.pipeline.PipelineTypes.StfKleisli

/** Where block import spends its time, stage by stage. Off unless `-Djam.stageTimers=true` (or [[enable]]): the
  * pipeline is then run through [[step]] wrappers that add each stage's wall time to a counter. Off, a stage is run
  * as it is, with no clock read.
  *
  * Stages are timed on the importing thread, so work a stage hands to other threads (deferred signature batches) is
  * charged to the stage that waits for it.
  */
object StageTimers:

  @volatile private var on: Boolean = java.lang.Boolean.getBoolean("jam.stageTimers")
  private final class Cell:
    val nanos = new LongAdder
    val calls = new LongAdder
  private val cells = new ConcurrentHashMap[String, Cell]()
  // First-use order, for a stable report.
  private val order = new java.util.concurrent.ConcurrentLinkedQueue[String]()

  def enabled: Boolean = on
  def enable(): Unit = on = true
  def reset(): Unit =
    cells.clear()
    order.clear()

  private def cell(name: String): Cell =
    cells.computeIfAbsent(name, n => { order.add(n); new Cell })

  /** Time `body` under `name`. */
  inline def time[A](name: String)(inline body: A): A =
    if !on then body
    else
      val t0 = System.nanoTime()
      try body
      finally
        val c = cell(name)
        c.nanos.add(System.nanoTime() - t0)
        c.calls.increment()

  /** Add a duration measured elsewhere (across threads, say) to `name`. */
  def addNanos(name: String, nanos: Long): Unit =
    if on then
      val c = cell(name)
      c.nanos.add(nanos)
      c.calls.increment()

  /** Add `amount` to the counter `name` (reported as its total in the `total_ms` column, e.g. gas used). */
  def count(name: String, amount: Long): Unit =
    if on then
      val c = cell(name)
      c.nanos.add(amount * 1_000_000L)
      c.calls.increment()

  /** A pipeline step timed under `name`. */
  def step[A](name: String)(s: StfKleisli[A]): StfKleisli[A] =
    if !on then s else StateT(state => time(name)(s.run(state)))

  /** (stage, calls, total ms, mean µs per call), in first-use order. */
  def snapshot: List[(String, Long, Double, Double)] =
    import scala.jdk.CollectionConverters.*
    order.asScala.toList.map { n =>
      val c = cells.get(n)
      val calls = c.calls.sum
      val ms = c.nanos.sum / 1e6
      (n, calls, ms, if calls == 0 then 0.0 else ms * 1000 / calls)
    }

  /** The report as `RESULT stage <name> calls= total_ms= mean_us= share=` lines; `share` is of the stage named
    * `total` when there is one.
    */
  def report: List[String] =
    val rows = snapshot
    val total = rows.find(_._1 == "total").map(_._3).filter(_ > 0)
    rows.map { (n, calls, ms, us) =>
      val share = total.map(t => f" share=${100 * ms / t}%.1f%%").getOrElse("")
      f"RESULT stage $n%-22s calls=$calls%d total_ms=$ms%.1f mean_us=$us%.1f$share"
    }
