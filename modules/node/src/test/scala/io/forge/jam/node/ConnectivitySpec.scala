package io.forge.jam.node

import io.forge.jam.core.JamBytes
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ConnectivitySpec extends AnyFunSuite with Matchers:

  private def edKey(first: Int, last: Int = 0): Array[Byte] =
    val k = new Array[Byte](32)
    k(0) = first.toByte
    k(31) = last.toByte
    k

  test("JAMNP-S: exactly one side of a pair is the preferred initiator") {
    val a = edKey(1)
    val b = edKey(2)
    // Both last bytes <= 127 and a < b: P(a, b) = a.
    Connectivity.isPreferredInitiator(a, b) shouldBe true
    Connectivity.isPreferredInitiator(b, a) shouldBe false
    // One last byte > 127 flips the choice.
    val bHigh = edKey(2, 200)
    Connectivity.isPreferredInitiator(a, bHigh) shouldBe false
    Connectivity.isPreferredInitiator(bHigh, a) shouldBe true
  }

  test("JAMNP-S: validator endpoint is a 16-byte IPv6 address then a little-endian port") {
    val meta = new Array[Byte](128)
    meta(15) = 1 // ::1
    meta(16) = 0x40.toByte // 0x9c40 = 40000
    meta(17) = 0x9c.toByte
    val ep = Connectivity.endpoint(meta).get
    ep.getPort shouldBe 40000
    ep.getAddress.isLoopbackAddress shouldBe true

    // IPv4-mapped addresses (::ffff:a.b.c.d) are usable as IPv4.
    val mapped = new Array[Byte](18)
    mapped(10) = 0xff.toByte; mapped(11) = 0xff.toByte
    mapped(12) = 10; mapped(15) = 7
    mapped(16) = 1
    Connectivity.endpoint(mapped).get.getAddress.getHostAddress shouldBe "10.0.0.7"

    Connectivity.endpoint(new Array[Byte](128)) shouldBe None // unspecified address
    Connectivity.endpoint(meta.take(17)) shouldBe None
  }

  test("JAMNP-S: connectivity changes wait for the first finalized block and max(E/30, 1) slots") {
    val e = 600 // full config: max(600/30, 1) = 20 slots
    Connectivity.transitionDelay(e) shouldBe 20
    Connectivity.transitionDelay(12) shouldBe 1
    // Slot 1210 is 10 slots into epoch 2: too early.
    Connectivity.appliedEpoch(1210, e, finalizedSlot = 1205, finalityRunning = true) shouldBe 1
    // 20 slots in, but the epoch's first block is not yet finalized.
    Connectivity.appliedEpoch(1220, e, finalizedSlot = 1199, finalityRunning = true) shouldBe 1
    Connectivity.appliedEpoch(1220, e, finalizedSlot = 1200, finalityRunning = true) shouldBe 2
    // Without a finality gadget only the slot delay applies.
    Connectivity.appliedEpoch(1220, e, finalizedSlot = 0, finalityRunning = false) shouldBe 2
    Connectivity.appliedEpoch(5, e, finalizedSlot = 0, finalityRunning = true) shouldBe 0
  }

  test("JAMNP-S: grid neighbours share a row or column in a set, or an index across sets") {
    // V = 9, W = 3: index 4 sits at row 1, column 1.
    val current = (0 until 9).map(i => edKey(i + 1))
    val next = (0 until 9).map(i => edKey(i + 101))
    val n = Connectivity.gridNeighbours(Seq(current, current, next), current(4))
    val expectedInSet = Set(3, 5, 1, 7).map(i => JamBytes(current(i)))
    n shouldBe expectedInSet + JamBytes(next(4))
  }

  test("JAMNP-S: UP 0 between validators only when they are grid neighbours") {
    val self = edKey(1)
    val neighbour = edKey(2)
    val far = edKey(3)
    val outsider = edKey(4)
    val validators = Set(self, neighbour, far).map(JamBytes(_))
    val neighbours = Set(JamBytes(neighbour))
    Connectivity.wantsBlockAnnouncements(validators, neighbours, self, neighbour) shouldBe true
    Connectivity.wantsBlockAnnouncements(validators, neighbours, self, far) shouldBe false
    Connectivity.wantsBlockAnnouncements(validators, neighbours, self, outsider) shouldBe true
    Connectivity.wantsBlockAnnouncements(validators, Set.empty, outsider, far) shouldBe true
  }
