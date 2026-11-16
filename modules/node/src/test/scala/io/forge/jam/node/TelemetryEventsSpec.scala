package io.forge.jam.node

import java.io.DataInputStream
import java.net.{InetAddress, InetSocketAddress, ServerSocket}

import io.forge.jam.core.LittleEndian
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TelemetryEventsSpec extends AnyFunSuite with Matchers:

  private def capture(count: Int)(send: Telemetry => Unit): List[(Int, Array[Byte])] =
    val server = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
    val t = new Telemetry(new InetSocketAddress(InetAddress.getLoopbackAddress, server.getLocalPort), () => Array[Byte](1))
    try
      t.start()
      val in = new DataInputStream(server.accept().getInputStream)
      def message(): Array[Byte] =
        val size = new Array[Byte](4)
        in.readFully(size)
        val content = new Array[Byte](LittleEndian.get(size, 0, 4).toInt)
        in.readFully(content)
        content
      message() shouldBe Array[Byte](1) // node information
      send(t)
      List.fill(count)(message()).map(m => (m(8) & 0xff, m.drop(9)))
    finally
      t.shutdown()
      server.close()

  private val peer = Array.tabulate[Byte](32)(_.toByte)

  test("a peer address is an IPv6 address, an IPv4 one in mapped form, then the port") {
    val v4 = Telemetry.peerAddress(Some(new InetSocketAddress(InetAddress.getByAddress(Array[Byte](10, 1, 2, 3)), 40000)))
    v4.take(16) shouldBe (Array.fill[Byte](10)(0) ++ Array[Byte](-1, -1, 10, 1, 2, 3))
    LittleEndian.get(v4, 16, 2) shouldBe 40000L
    Telemetry.peerAddress(None) shouldBe new Array[Byte](18)
  }

  test("connected in, peer misbehaved and the two block failures have the JIP-3 layout and link their events") {
    val address = Some(new InetSocketAddress(InetAddress.getByAddress(Array[Byte](10, 1, 2, 3)), 40000))
    val events = capture(6) { t =>
      t.connectedIn(address, peer) // events 0 and 1
      t.peerMisbehaved(peer, "InvalidBlock") // 2
      val importing = t.importing(7L, Array[Byte](9, 9)) // 3
      t.blockVerificationFailed(importing, "bad seal") // 4
      t.blockExecutionFailed(importing, "collision") // 5
    }
    events.map(_._1) shouldBe List(21, 23, 28, 43, 44, 46)
    events(0)._2 shouldBe Telemetry.peerAddress(address)
    events(1)._2 shouldBe (Telemetry.u64(0) ++ peer)
    events(2)._2 shouldBe (peer ++ Array[Byte](12) ++ "InvalidBlock".getBytes("UTF-8"))
    events(3)._2 shouldBe (Telemetry.u32(7) ++ Array[Byte](9, 9))
    events(4)._2 shouldBe (Telemetry.u64(3) ++ Array[Byte](8) ++ "bad seal".getBytes("UTF-8"))
    events(5)._2 shouldBe (Telemetry.u64(3) ++ Array[Byte](9) ++ "collision".getBytes("UTF-8"))
  }
