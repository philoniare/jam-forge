package io.forge.jam.network

import java.net.{InetAddress, InetSocketAddress}

import io.netty.buffer.Unpooled
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SignedQuicTokenHandlerSpec extends AnyFunSuite with Matchers:

  private def address(last: Int, port: Int = 4000) =
    new InetSocketAddress(InetAddress.getByAddress(Array[Byte](10, 0, 0, last.toByte)), port)

  private val dcid = Array.tabulate[Byte](18)(i => (i * 7).toByte)

  private def issue(h: SignedQuicTokenHandler, to: InetSocketAddress) =
    val out = Unpooled.buffer()
    h.writeToken(out, Unpooled.wrappedBuffer(dcid), to) shouldBe true
    out.readableBytes() should be <= h.maxTokenLength()
    out

  test("a token is accepted from the address it was issued to, and gives back the connection id") {
    var now = 1_000_000L
    val h = new SignedQuicTokenHandler(() => now)
    val token = issue(h, address(1))
    val at = h.validateToken(token, address(1))
    at should be >= 0
    val id = new Array[Byte](token.readableBytes() - at)
    token.getBytes(token.readerIndex() + at, id)
    id shouldBe dcid
    now += SignedQuicTokenHandler.ValidMillis
    h.validateToken(token, address(1)) should be >= 0
  }

  test("a token from another address or port, an old one, a changed one and another node's are refused") {
    var now = 1_000_000L
    val h = new SignedQuicTokenHandler(() => now)
    val token = issue(h, address(1))
    h.validateToken(token, address(2)) shouldBe -1
    h.validateToken(token, address(1, port = 4001)) shouldBe -1
    (0 until token.readableBytes()).foreach { i =>
      val changed = token.copy()
      changed.setByte(i, changed.getByte(i) ^ 1)
      withClue(s"byte $i: ") { h.validateToken(changed, address(1)) shouldBe -1 }
    }
    h.validateToken(token.copy(0, token.readableBytes() - 1), address(1)) shouldBe -1
    h.validateToken(Unpooled.wrappedBuffer(new Array[Byte](10)), address(1)) shouldBe -1
    h.validateToken(Unpooled.wrappedBuffer(new Array[Byte](100)), address(1)) shouldBe -1
    val plain = Unpooled.buffer().writeBytes("netty".getBytes("US-ASCII")).writeBytes(address(1).getAddress.getAddress).writeBytes(dcid)
    h.validateToken(plain, address(1)) shouldBe -1
    new SignedQuicTokenHandler(() => now).validateToken(token, address(1)) shouldBe -1
    now += SignedQuicTokenHandler.ValidMillis + 1
    h.validateToken(token, address(1)) shouldBe -1
    now -= 2 * SignedQuicTokenHandler.ValidMillis
    h.validateToken(token, address(1)) shouldBe -1 // dated in the future
  }
