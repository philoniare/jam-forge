package io.forge.jam.node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** A peer's block announcements wait for the thread that handles them. When more wait than may, the oldest give way:
  * a later announcement says what the earlier ones did and more.
  */
class AnnouncementMailboxSpec extends AnyFunSuite with Matchers:

  private def drained(box: AnnouncementMailbox): List[(Boolean, Int)] =
    val out = List.newBuilder[(Boolean, Int)]
    box.drain((first, msg) => out += (first -> msg(0).toInt))
    out.result()

  test("when more announcements wait than may, the oldest are dropped and the handshake is kept") {
    val box = new AnnouncementMailbox(capacity = 3)
    box.offer(first = true, Array[Byte](0)) shouldBe true
    (1 to 6).foreach(i => box.offer(first = false, Array(i.toByte)) shouldBe false)
    drained(box) shouldBe List(true -> 0, false -> 5, false -> 6)
  }

  test("one drain runs at a time, and the next message after it starts another") {
    val box = new AnnouncementMailbox(capacity = 3)
    box.offer(first = false, Array[Byte](1)) shouldBe true
    box.offer(first = false, Array[Byte](2)) shouldBe false
    drained(box) shouldBe List(false -> 1, false -> 2)
    box.offer(first = false, Array[Byte](3)) shouldBe true
    // The handling thread refused the drain: the message stays, and the next one asks again.
    box.idle()
    box.offer(first = false, Array[Byte](4)) shouldBe true
    drained(box) shouldBe List(false -> 3, false -> 4)
  }
