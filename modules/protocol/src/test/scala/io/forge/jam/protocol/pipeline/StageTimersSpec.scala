package io.forge.jam.protocol.pipeline

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StageTimersSpec extends AnyFunSuite with Matchers:

  test("timings lists the timed stages with calls and nanoseconds, and leaves out the counters") {
    val was = StageTimers.enabled
    StageTimers.enable()
    try
      StageTimers.time("spec/stage")(Thread.sleep(2))
      StageTimers.addNanos("spec/added", 1_500L)
      StageTimers.count("spec/gas", 7L)
      val byName = StageTimers.timings.map(t => t._1 -> t).toMap
      byName("spec/stage")._2 shouldBe 1L
      byName("spec/stage")._3 should be >= 2_000_000L
      byName("spec/added") shouldBe (("spec/added", 1L, 1_500L))
      byName.contains("spec/gas") shouldBe false
      StageTimers.snapshot.map(_._1) should contain("spec/gas")
    finally if !was then StageTimers.disable()
  }

  test("timings is empty while the timers are off") {
    assume(!StageTimers.enabled)
    StageTimers.timings shouldBe Nil
  }
