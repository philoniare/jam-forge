package io.forge.jam.node

import java.io.DataInputStream
import java.net.{InetAddress, InetSocketAddress, ServerSocket}
import java.util.concurrent.CopyOnWriteArrayList

import io.forge.jam.core.LittleEndian

import scala.jdk.CollectionConverters.*

final class TelemetryCollector extends AutoCloseable:
  private val server = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)

  val address: InetSocketAddress = new InetSocketAddress(InetAddress.getLoopbackAddress, server.getLocalPort)

  @volatile var nodeInformation: Option[Array[Byte]] = None
  private val received = new CopyOnWriteArrayList[TelemetryCollector.Event]()

  private val reader = new Thread(
    () =>
      try
        val socket = server.accept()
        val in = new DataInputStream(socket.getInputStream)
        def message(): Array[Byte] =
          val size = new Array[Byte](4)
          in.readFully(size)
          val content = new Array[Byte](LittleEndian.get(size, 0, 4).toInt)
          in.readFully(content)
          content
        nodeInformation = Some(message())
        var id = 0L
        while true do
          val m = message()
          val discriminator = m(8) & 0xff
          received.add(TelemetryCollector.Event(id, discriminator, m.drop(9)))
          id += (if discriminator == 0 then LittleEndian.get(m, 17, 8) else 1L)
      catch case _: Exception => ()
    ,
    "telemetry-collector"
  )
  reader.setDaemon(true)
  reader.start()

  def events: List[TelemetryCollector.Event] = received.asScala.toList

  def all(d: Int)(p: Array[Byte] => Boolean = _ => true): List[TelemetryCollector.Event] =
    events.filter(e => e.discriminator == d && p(e.body))

  def referring(d: Int, ref: Long): List[TelemetryCollector.Event] = all(d)(b => b.length >= 8 && LittleEndian.get(b, 0, 8) == ref)

  def await(what: String, seconds: Int = 30)(find: => Option[TelemetryCollector.Event]): TelemetryCollector.Event =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    var found = find
    while found.isEmpty && System.currentTimeMillis() < deadline do
      Thread.sleep(20)
      found = find
    found.getOrElse(throw new AssertionError(s"no telemetry event: $what; got ${events.map(_.discriminator)}"))

  def close(): Unit = server.close()

object TelemetryCollector:
  final case class Event(id: Long, discriminator: Int, body: Array[Byte]):
    def u16(at: Int): Long = LittleEndian.get(body, at, 2)
    def u32(at: Int): Long = LittleEndian.get(body, at, 4)
    def u64(at: Int): Long = LittleEndian.get(body, at, 8)
    def bytes(at: Int, n: Int): Array[Byte] = java.util.Arrays.copyOfRange(body, at, at + n)
