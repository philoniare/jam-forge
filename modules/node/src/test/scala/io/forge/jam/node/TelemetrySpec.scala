package io.forge.jam.node

import java.io.DataInputStream
import java.net.{InetSocketAddress, ServerSocket, Socket}

import io.forge.jam.core.LittleEndian
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TelemetrySpec extends AnyFunSuite with Matchers:

  private val nodeInfo = "node-info".getBytes("UTF-8")

  private def telemetry(port: Int) =
    new Telemetry(
      new InetSocketAddress("127.0.0.1", port),
      () => nodeInfo,
      writeTimeoutMillis = 500L,
      retryMillis = 50L,
      maxRetryMillis = 200L
    )

  private def readMessage(in: DataInputStream): Array[Byte] =
    val size = new Array[Byte](4)
    in.readFully(size)
    val content = new Array[Byte](LittleEndian.get(size, 0, 4).toInt)
    in.readFully(content)
    content

  private def accept(server: ServerSocket): (Socket, DataInputStream) =
    val socket = server.accept()
    socket.setSoTimeout(10000)
    (socket, new DataInputStream(socket.getInputStream))

  test("a server that stops reading never holds up an emitter, and is dropped and dialled again after the write timeout") {
    val server = new ServerSocket(0)
    server.setSoTimeout(10000)
    val t = telemetry(server.getLocalPort)
    try
      t.start()
      val (stalled, in) = accept(server)
      readMessage(in) shouldBe nodeInfo
      val body = new Array[Byte](64 * 1024)
      val started = System.nanoTime()
      var slowest = 0L
      (1 to 4 * 4096).foreach { _ =>
        val before = System.nanoTime()
        t.emit(99, body)
        slowest = math.max(slowest, System.nanoTime() - before)
      }
      slowest should be < 100_000_000L // no emit waits on the writer
      (System.nanoTime() - started) should be < 5_000_000_000L
      t.droppedEvents should be > 0L
      val (again, in2) = accept(server)
      readMessage(in2) shouldBe nodeInfo
      stalled.close()
      again.close()
    finally
      t.shutdown()
      server.close()
  }

  test("a server that closes the connection gets the node information again first on the next one") {
    val server = new ServerSocket(0)
    server.setSoTimeout(10000)
    val t = telemetry(server.getLocalPort)
    try
      t.start()
      val (first, in) = accept(server)
      readMessage(in) shouldBe nodeInfo
      first.close()
      val deadline = System.currentTimeMillis() + 10000
      server.setSoTimeout(100)
      var next: Option[(Socket, DataInputStream)] = None
      while next.isEmpty && System.currentTimeMillis() < deadline do
        t.bestBlockChanged(1, io.forge.jam.core.primitives.Hash.zero)
        try next = Some(accept(server))
        catch case _: java.net.SocketTimeoutException => ()
      val (second, in2) = next.getOrElse(fail("no second connection"))
      readMessage(in2) shouldBe nodeInfo
      t.bestBlockChanged(2, io.forge.jam.core.primitives.Hash.zero)
      val events = Iterator.continually(readMessage(in2)).take(50).find(m => m(8) == 11.toByte && LittleEndian.get(m, 9, 4) == 2L)
      events.isDefined shouldBe true
      second.close()
    finally
      t.shutdown()
      server.close()
  }

  test("a server that goes away and comes back on its port is dialled again, node information first") {
    val first = new ServerSocket(0)
    val port = first.getLocalPort
    first.setSoTimeout(10000)
    val t = telemetry(port)
    try
      t.start()
      val (s1, in1) = accept(first)
      readMessage(in1) shouldBe nodeInfo
      s1.close()
      first.close()
      (1 to 20).foreach { _ =>
        t.bestBlockChanged(1, io.forge.jam.core.primitives.Hash.zero)
        Thread.sleep(20)
      }
      val back = new ServerSocket()
      back.setReuseAddress(true)
      back.bind(new InetSocketAddress("127.0.0.1", port))
      back.setSoTimeout(10000)
      try
        val (s2, in2) = accept(back)
        readMessage(in2) shouldBe nodeInfo
        s2.close()
      finally back.close()
    finally t.shutdown()
  }
