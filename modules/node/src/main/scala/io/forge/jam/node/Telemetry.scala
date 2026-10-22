package io.forge.jam.node

import java.io.{ByteArrayOutputStream, OutputStream}
import java.net.{InetSocketAddress, Socket}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.LittleEndian
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.primitives.Hash

/** Telemetry client: when enabled, the node opens
  * a TCP connection to `--telemetry HOST:PORT` carrying length-prefixed
  * messages (u32 LE size ++ content), the node information message first,
  * then events. Emitted: 11 best block changed, 12 finalized block changed,
  * 13 sync status changed, 27 disconnected, 40 authoring, 41 authoring
  * failed, 42 authored.
  *
  * Events queue on a bounded buffer and a single sender thread writes them;
  * when the server is unreachable they are dropped and the connection is
  * retried, starting again with the node information message (event IDs are
  * per connection, so they restart at 0).
  */
final class Telemetry(
    address: InetSocketAddress,
    nodeInfo: () => Array[Byte]
) extends LazyLogging:
  import Telemetry.*

  private val queue = new ArrayBlockingQueue[(Long, Array[Byte])](QueueCapacity)
  @volatile private var running = true

  /** Connection generation and the ID of the next event on it. */
  private var generation = 0L
  private var nextEventId = 0L
  private val idLock = new Object

  private val sender = new Thread(() => run(), "jam-telemetry")
  sender.setDaemon(true)

  /** Starts sending; call once the node information is final (the listen
    * port is bound). Events emitted before are queued.
    */
  def start(): Unit = sender.start()

  /** Queue an event (discriminator ++ body; the timestamp is added here).
    * Returns its (generation, event ID) so later events can reference it,
    * or None when it was dropped.
    */
  def emit(discriminator: Int, body: Array[Byte]): Option[EventRef] =
    val content = LittleEndian.encode(timestampMicros(), 8) ++ Array(discriminator.toByte) ++ body
    idLock.synchronized {
      val ref = EventRef(generation, nextEventId)
      if queue.offer((generation, content)) then
        nextEventId += 1
        Some(ref)
      else None
    }

  /** Event ID of `ref` if it was sent on the current connection. */
  def idOnCurrentConnection(ref: Option[EventRef]): Option[Long] =
    idLock.synchronized(ref.filter(_.generation == generation).map(_.id))

  def bestBlockChanged(slot: Long, hash: Hash): Unit =
    emit(11, u32(slot) ++ hash.bytes.toArray)
    ()

  def finalizedBlockChanged(slot: Long, hash: Hash): Unit =
    emit(12, u32(slot) ++ hash.bytes.toArray)
    ()

  def syncStatusChanged(inSync: Boolean): Unit =
    emit(13, Array((if inSync then 1 else 0).toByte))
    ()

  /** 27: the connection to `peer` broke; the terminator is unknown (a
    * timeout or reset is the usual cause).
    */
  def disconnected(peer: Array[Byte], reason: String): Unit =
    emit(27, peer ++ Array(0.toByte) ++ string(reason, 128))
    ()

  def authoring(slot: Long, parent: Hash): Option[EventRef] =
    emit(40, u32(slot) ++ parent.bytes.toArray)

  def authoringFailed(authoring: Option[EventRef], reason: String): Unit =
    idOnCurrentConnection(authoring).foreach(id => emit(41, u64(id) ++ string(reason, 128)))

  def authored(authoring: Option[EventRef], outline: Array[Byte]): Unit =
    idOnCurrentConnection(authoring).foreach(id => emit(42, u64(id) ++ outline))

  def shutdown(): Unit =
    running = false
    sender.interrupt()

  private def run(): Unit =
    while running do
      try
        val socket = new Socket()
        try
          socket.connect(address, 5000)
          socket.setTcpNoDelay(true)
          val out = socket.getOutputStream
          val gen = idLock.synchronized {
            // Events queued while disconnected belong to no connection.
            queue.clear()
            generation += 1
            nextEventId = 0
            generation
          }
          logger.info(s"telemetry connected to $address")
          writeMessage(out, nodeInfo())
          while running do
            val next = queue.poll(1, TimeUnit.SECONDS)
            if next != null && next._1 == gen then writeMessage(out, next._2)
        finally socket.close()
      catch
        case _: InterruptedException => running = false
        case e: Exception =>
          logger.debug(s"telemetry to $address unavailable: ${e.getMessage}")
          idLock.synchronized {
            queue.clear()
            generation += 1
            nextEventId = 0
          }
          try Thread.sleep(RetryMillis)
          catch case _: InterruptedException => running = false

  private def writeMessage(out: OutputStream, content: Array[Byte]): Unit =
    out.write(LittleEndian.encode(content.length.toLong, 4))
    out.write(content)
    out.flush()

object Telemetry:
  final case class EventRef(generation: Long, id: Long)

  private val QueueCapacity = 4096
  private val RetryMillis = 2000L

  /** JIP-3 Timestamp: microseconds since the start of the JAM Common Era. */
  def timestampMicros(): Long =
    System.currentTimeMillis() * 1000L - SlotClock.JamCommonEraSeconds * 1000000L

  def u16(v: Long): Array[Byte] = LittleEndian.encode(v, 2)
  def u32(v: Long): Array[Byte] = LittleEndian.encode(v, 4)
  def u64(v: Long): Array[Byte] = LittleEndian.encode(v, 8)

  /** JIP-3 String<N>: len++ UTF-8 bytes, truncated to `max` bytes on a
    * character boundary.
    */
  def string(s: String, max: Int): Array[Byte] =
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    var n = math.min(bytes.length, max)
    // Back off to a UTF-8 character boundary (continuation bytes are 10xxxxxx).
    while n > 0 && n < bytes.length && (bytes(n) & 0xc0) == 0x80 do n -= 1
    JamCodecs.encodeCompactInteger(n.toLong) ++ bytes.take(n)

  /** Node information message (protocol version 1). */
  def nodeInformation(
      jamParameters: Array[Byte],
      genesisHash: Hash,
      peerId: Array[Byte],
      listenPort: Int,
      recompiler: Boolean,
      implementationVersion: String,
      grayPaperVersion: String,
      note: String
  ): Array[Byte] =
    val out = new ByteArrayOutputStream()
    out.write(1)
    out.write(jamParameters)
    out.write(genesisHash.bytes.toArray)
    out.write(peerId)
    // Peer Address: IPv6 (unspecified: the node listens on all interfaces) ++ u16 port
    out.write(new Array[Byte](16))
    out.write(u16(listenPort.toLong))
    out.write(u32(if recompiler then 1L else 0L))
    out.write(string("jam-forge", 32))
    out.write(string(implementationVersion, 32))
    out.write(string(grayPaperVersion, 16))
    out.write(string(note, 512))
    out.toByteArray

  /** JIP-3 Block Outline. */
  def blockOutline(
      sizeBytes: Int,
      headerHash: Hash,
      tickets: Int,
      preimages: Int,
      preimagesSize: Long,
      guarantees: Int,
      assurances: Int,
      disputeVerdicts: Int
  ): Array[Byte] =
    u32(sizeBytes.toLong) ++ headerHash.bytes.toArray ++ u32(tickets.toLong) ++ u32(preimages.toLong) ++
      u32(preimagesSize) ++ u32(guarantees.toLong) ++ u32(assurances.toLong) ++ u32(disputeVerdicts.toLong)
