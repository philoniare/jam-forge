package io.forge.jam.node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LowConnectivityWatchSpec extends AnyFunSuite with Matchers:
  import LowConnectivityWatch.{Change, below}

  test("below means fewer than 2/3 of the others: exactly 2/3 is enough, and no others is never below") {
    below(connected = 4, others = 6) shouldBe false
    below(connected = 3, others = 6) shouldBe true
    below(connected = 3, others = 5) shouldBe true
    below(connected = 4, others = 5) shouldBe false
    below(connected = 0, others = 0) shouldBe false
    below(connected = 1, others = 1) shouldBe false
    below(connected = 0, others = 1) shouldBe true
    LowConnectivityWatch.needed(5) shouldBe 4
    LowConnectivityWatch.needed(6) shouldBe 4
    LowConnectivityWatch.needed(1022) shouldBe 682
  }

  test("low after the grace period, once per episode; restored when enough are connected again") {
    val watch = new LowConnectivityWatch(graceMillis = 10_000L)
    watch.current shouldBe None
    watch.observe(connected = 3, others = 5, now = 0L) shouldBe None
    watch.current shouldBe Some(ValidatorConnectivity(3, 5, low = false))
    watch.observe(3, 5, 9_999L) shouldBe None
    watch.observe(2, 5, 10_000L) shouldBe Some(Change.Low(2, 5, 10_000L))
    watch.current shouldBe Some(ValidatorConnectivity(2, 5, low = true))
    watch.observe(1, 5, 50_000L) shouldBe None
    watch.current shouldBe Some(ValidatorConnectivity(1, 5, low = true))
    watch.observe(4, 5, 51_000L) shouldBe Some(Change.Restored(4, 5))
    watch.current shouldBe Some(ValidatorConnectivity(4, 5, low = false))
    watch.observe(1, 5, 52_000L) shouldBe None
    watch.observe(1, 5, 61_999L) shouldBe None
    watch.observe(1, 5, 62_000L) shouldBe Some(Change.Low(1, 5, 10_000L))
    watch.observe(5, 5, 63_000L) shouldBe Some(Change.Restored(5, 5))
    watch.observe(0, 5, 64_000L) shouldBe None
    watch.observe(5, 5, 65_000L) shouldBe None
  }

  test("a node that stops being a validator has no connectivity to report, and starts afresh") {
    val watch = new LowConnectivityWatch(graceMillis = 0L)
    watch.observe(0, 5, 0L) shouldBe Some(Change.Low(0, 5, 0L))
    watch.clear()
    watch.current shouldBe None
    watch.observe(0, 5, 1L) shouldBe Some(Change.Low(0, 5, 0L))
  }
