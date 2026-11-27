package io.forge.jam.node

import java.io.DataInputStream
import java.net.{InetAddress, InetSocketAddress, ServerSocket}
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, Executors, TimeUnit}

import scala.jdk.CollectionConverters.*

import io.forge.jam.core.LittleEndian
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TicketGenerationDiagnosticsSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private final class Tap extends AutoCloseable:
    private val server = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
    val telemetry =
      new Telemetry(new InetSocketAddress(InetAddress.getLoopbackAddress, server.getLocalPort), () => Array[Byte](1))
    val events = new ConcurrentLinkedQueue[(Int, Array[Byte])]()
    private val reader = new Thread(() =>
      try
        val in = new DataInputStream(server.accept().getInputStream)
        def message(): Array[Byte] =
          val size = new Array[Byte](4)
          in.readFully(size)
          val content = new Array[Byte](LittleEndian.get(size, 0, 4).toInt)
          in.readFully(content)
          content
        message(): Unit // node information
        while true do
          val m = message()
          events.add((m(8) & 0xff, m.drop(9)))
      catch case _: Exception => ()
    )
    reader.setDaemon(true)
    reader.start()
    telemetry.start()

    def of(discriminator: Int): List[Array[Byte]] = events.asScala.collect { case (`discriminator`, b) => b }.toList

    def reason(body: Array[Byte]): String =
      val text = body.drop(8)
      new String(text.drop(if (text(0) & 0x80) == 0 then 1 else 2), "UTF-8")

    def close(): Unit =
      telemetry.shutdown()
      server.close()

  test("a validator whose Bandersnatch key is not its on-chain one makes no tickets and reports why") {
    withNodes(1, "tickets-wrong-bandersnatch") { nodes =>
      val a = nodes.head
      val tap = new Tap
      val egress = Executors.newSingleThreadExecutor()
      try
        val tickets = new TicketService(a.chain, a.pools, a.distribution, egress, telemetry = Some(tap.telemetry))
        val wrong = ValidatorKeySet.withBandersnatchSeed(devKeys(0), Array.fill[Byte](32)(7)).copy(bandersnatchHeld = "0badc0de v0.seed")
        tickets.setValidatorKeys(Seq(wrong))
        tickets.maybeGenerate(1L)
        a.pools.ticketCount shouldBe 0
        eventually("TicketGenerationFailed")(tap.of(81).nonEmpty)
        tap.of(80).size shouldBe 1
        val why = tap.reason(tap.of(81).head)
        why should include("Bandersnatch")
        why should include("validator 0")
        why should include(devKeys(0).bandersnatchPublic.take(4).map(b => f"$b%02x").mkString)
        why should include("0badc0de v0.seed")
        why.getBytes("UTF-8").length should be <= 128
      finally
        egress.shutdownNow(): Unit
        tap.close()
    }
  }

  test("a ticket generation that does not finish is reported, with where it is stuck, and only once") {
    withNodes(1, "tickets-stuck") { nodes =>
      val a = nodes.head
      val tap = new Tap
      val egress = Executors.newSingleThreadExecutor()
      val release = new CountDownLatch(1)
      val threads = new NodeThreads
      try
        val tickets = new TicketService(
          a.chain,
          a.pools,
          a.distribution,
          egress,
          threads = threads,
          telemetry = Some(tap.telemetry),
          ringProver = (_, _, _, _, _) =>
            release.await(30, TimeUnit.SECONDS): Unit
            None
          ,
          stuckAfterMillis = 200L
        )
        tickets.setValidatorKeys(Seq(devKeys(0)))
        tickets.generateInBackground(1L)
        eventually("generation to start")(tap.of(80).nonEmpty)
        Thread.sleep(300)
        tickets.generateInBackground(2L) // the next slot finds the generator still busy
        tickets.generateInBackground(3L)
        eventually("TicketGenerationFailed")(tap.of(81).nonEmpty)
        Thread.sleep(200)
        tap.of(81).size shouldBe 1
        val why = tap.reason(tap.of(81).head)
        why should include("still running")
        why should include("TicketGenerationDiagnosticsSpec") // the frame it waits in
      finally
        release.countDown()
        threads.close(0L): Unit
        egress.shutdownNow(): Unit
        tap.close()
    }
  }
