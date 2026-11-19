package io.forge.jam.node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DurationHistogramSpec extends AnyFunSuite with Matchers:

  test("observations are counted in every bucket at or above them, with their sum and count") {
    val h = new DurationHistogram(Vector(0.001, 0.01, 0.1))
    h.observeNanos(500_000L) // 0.5 ms
    h.observeNanos(1_000_000L) // 1 ms: `le` is inclusive
    h.observeNanos(50_000_000L) // 50 ms
    h.observeNanos(2_000_000_000L) // 2 s: above every bound
    val s = h.snapshot
    s.bounds shouldBe Vector(0.001, 0.01, 0.1)
    s.cumulative shouldBe Vector(2L, 2L, 3L)
    s.count shouldBe 4L
    s.sumSeconds shouldBe 2.0515 +- 1e-9
  }

  test("an empty histogram reports zeros") {
    val s = new DurationHistogram(Vector(0.5)).snapshot
    s.cumulative shouldBe Vector(0L)
    s.count shouldBe 0L
    s.sumSeconds shouldBe 0.0
  }

  test("bounds must ascend") {
    an[IllegalArgumentException] should be thrownBy new DurationHistogram(Vector(0.1, 0.01))
  }

  test("the Prometheus text has cumulative buckets ending in +Inf, then the sum and the count") {
    val h = new DurationHistogram(Vector(0.001, 0.01))
    h.observeNanos(2_000_000L)
    h.observeNanos(3_000_000_000L)
    val out = new StringBuilder
    NodeMetrics.histogram(out, "jam_x_seconds", "Time of x.", h.snapshot)
    out.toString shouldBe
      """# HELP jam_x_seconds Time of x.
        |# TYPE jam_x_seconds histogram
        |jam_x_seconds_bucket{le="0.001"} 0
        |jam_x_seconds_bucket{le="0.01"} 1
        |jam_x_seconds_bucket{le="+Inf"} 2
        |jam_x_seconds_sum 3.002
        |jam_x_seconds_count 2
        |""".stripMargin
  }
