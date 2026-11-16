package io.forge.jam.node

import io.forge.jam.core.Hashing
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.node.SyncCodec.HashSlot
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The UP 0 decoders refuse a short, oversized or padded message with a `Left` and never throw, so a malformed
  * message resets its stream on purpose rather than through an exception.
  */
class SyncCodecSpec extends AnyFunSuite with Matchers:

  private def hs(i: Int, slot: Long): HashSlot = HashSlot(Hashing.blake2b256(s"block $i".getBytes), slot)

  private val finalHead = hs(0, 7L)
  private val leaves = List(hs(1, 8L), hs(2, 9L), hs(3, 0xffffffffL))
  private val handshake = SyncCodec.encodeHandshake(finalHead, leaves)

  private def decodesWithoutThrowing[A](decode: => A): A =
    noException should be thrownBy decode
    decode

  test("a handshake round-trips") {
    val Right((f, ls)) = SyncCodec.decodeHandshake(handshake): @unchecked
    f shouldBe finalHead
    ls shouldBe leaves
  }

  test("a handshake with no leaves round-trips") {
    SyncCodec.decodeHandshake(SyncCodec.encodeHandshake(finalHead, Nil)) shouldBe Right((finalHead, Nil))
  }

  test("a handshake cut short at any byte is refused") {
    for len <- 0 until handshake.length do
      val cut = java.util.Arrays.copyOf(handshake, len)
      withClue(s"length $len: ") {
        decodesWithoutThrowing(SyncCodec.decodeHandshake(cut)).isLeft shouldBe true
      }
  }

  test("a handshake with bytes after its last leaf is refused") {
    for extra <- 1 to 40 do
      withClue(s"$extra trailing byte(s): ") {
        decodesWithoutThrowing(SyncCodec.decodeHandshake(handshake ++ Array.fill[Byte](extra)(0x5a))).isLeft shouldBe true
      }
  }

  test("a handshake naming more than the leaf cap is refused, whatever follows the count") {
    val head = SyncCodec.encodeHashSlot(finalHead)
    for count <- List(SyncService.MaxHandshakeLeaves.toLong + 1, 1L << 20, 1L << 40, Long.MaxValue) do
      val countBytes = JamCodecs.encodeCompactInteger(count)
      withClue(s"count $count, no leaves: ") {
        decodesWithoutThrowing(SyncCodec.decodeHandshake(head ++ countBytes)).isLeft shouldBe true
      }
    val over = (1 to SyncService.MaxHandshakeLeaves + 1).toList.map(hs(_, 3L))
    decodesWithoutThrowing(SyncCodec.decodeHandshake(SyncCodec.encodeHandshake(finalHead, over))).isLeft shouldBe true
  }

  test("a handshake at the leaf cap is accepted") {
    val atCap = (1 to SyncService.MaxHandshakeLeaves).toList.map(hs(_, 3L))
    SyncCodec.decodeHandshake(SyncCodec.encodeHandshake(finalHead, atCap)).map(_._2.size) shouldBe
      Right(SyncService.MaxHandshakeLeaves)
  }

  test("a handshake whose length prefix is itself truncated is refused") {
    val head = SyncCodec.encodeHashSlot(finalHead)
    // 0xff opens an 8-byte compact integer; give it fewer bytes than it needs.
    for n <- 0 to 7 do
      withClue(s"$n of 8 count bytes: ") {
        decodesWithoutThrowing(SyncCodec.decodeHandshake(head ++ Array(0xff.toByte) ++ new Array[Byte](n))).isLeft shouldBe true
      }
  }

  private val header = Array.tabulate[Byte](120)(i => i.toByte)
  private val announcement = SyncCodec.encodeAnnouncement(header, finalHead)

  test("an announcement round-trips") {
    val Right((h, f)) = SyncCodec.decodeAnnouncement(announcement): @unchecked
    h.toSeq shouldBe header.toSeq
    f shouldBe finalHead
  }

  test("an announcement too short to hold a header's fixed fields and a final is refused at every length") {
    val minimum = SyncCodec.MinAnnouncedHeaderBytes + 36
    for len <- 0 until minimum do
      withClue(s"length $len: ") {
        decodesWithoutThrowing(SyncCodec.decodeAnnouncement(new Array[Byte](len))).isLeft shouldBe true
      }
    SyncCodec.decodeAnnouncement(new Array[Byte](minimum)).isRight shouldBe true
  }

  test("an announcement cut short at any byte is refused or never throws") {
    for len <- 0 until announcement.length do
      val cut = java.util.Arrays.copyOf(announcement, len)
      withClue(s"length $len: ") {
        val r = decodesWithoutThrowing(SyncCodec.decodeAnnouncement(cut))
        if len < SyncCodec.MinAnnouncedHeaderBytes + 36 then r.isLeft shouldBe true
      }
  }
