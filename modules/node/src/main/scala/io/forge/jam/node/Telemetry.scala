package io.forge.jam.node

import java.io.ByteArrayOutputStream
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, SocketChannel}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.LittleEndian
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.primitives.Hash

/** Telemetry client: when enabled, the node opens
  * a TCP connection to `--telemetry HOST:PORT` carrying length-prefixed
  * messages (u32 LE size ++ content), the node information message first,
  * then events. Emitted: 10 status, 11 best block changed, 12 finalized block changed,
  * 13 sync status changed, 21/23 connected in, 24/26 connected out, 27 disconnected, 28 peer misbehaved,
  * 40 authoring, 41 authoring failed, 42 authored, 43 importing, 44 block verification failed, 45 block
  * verified, 46 block execution failed. Not emitted: 47 block executed (the accumulate costs it carries are not
  * collected) and the events of block distribution, tickets, guaranteeing, availability, recovery and preimages.
  *
  * Events queue on a bounded buffer and a single writer thread sends them; no other thread ever waits on
  * telemetry. An event that finds the buffer full is dropped and counted: the newest, since JIP-3 event IDs are
  * implicit (the order of events on the connection) and earlier events may already be referenced by ID. JIP-3's own
  * report of dropped events is not sent (its layout is not in this repository): drops are counted
  * ([[droppedEvents]]) and logged once per episode. A write that does not complete within `writeTimeoutMillis` (a
  * server that stopped reading) closes the connection. A lost or refused connection is retried with a doubling
  * backoff from `retryMillis` up to `maxRetryMillis`; loss and recovery are logged at warn once per change. Every
  * new connection starts with the node information message (event IDs are per connection, so they restart at 0).
  */
final class Telemetry(
    address: InetSocketAddress,
    nodeInfo: () => Array[Byte],
    writeTimeoutMillis: Long = Telemetry.WriteTimeoutMillis,
    retryMillis: Long = Telemetry.RetryMillis,
    maxRetryMillis: Long = Telemetry.MaxRetryMillis
) extends LazyLogging:
  import Telemetry.*

  private val queue = new ArrayBlockingQueue[(Long, Array[Byte])](QueueCapacity)
  @volatile private var running = true

  /** Connection generation and the ID of the next event on it. */
  private var generation = 0L
  private var nextEventId = 0L
  private val idLock = new Object

  private val dropped = new java.util.concurrent.atomic.AtomicLong()
  private val droppedUnlogged = new java.util.concurrent.atomic.AtomicLong()

  def droppedEvents: Long = dropped.get

  private def drop(n: Long): Unit =
    if n > 0 then
      dropped.addAndGet(n)
      droppedUnlogged.addAndGet(n): Unit

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
      else
        drop(1)
        None
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

  def status(
      peers: Int,
      validatorPeers: Int,
      announcingPeers: Int,
      guaranteesByCore: Array[Byte],
      shards: Long,
      shardBytes: Long,
      preimages: Int,
      preimageBytes: Long
  ): Unit =
    emit(
      10,
      u32(peers.toLong) ++ u32(validatorPeers.toLong) ++ u32(announcingPeers.toLong) ++ guaranteesByCore ++
        u32(math.min(shards, 0xffffffffL)) ++ u64(shardBytes) ++ u32(preimages.toLong) ++
        u32(math.min(preimageBytes, 0xffffffffL))
    )
    ()

  def connectedIn(address: Option[java.net.SocketAddress], peer: Array[Byte]): Unit =
    idOnCurrentConnection(emit(21, peerAddress(address))).foreach(id => emit(23, u64(id) ++ peer))

  def connectedOut(peer: Array[Byte], address: Option[java.net.SocketAddress]): Unit =
    idOnCurrentConnection(emit(24, peer ++ peerAddress(address))).foreach(id => emit(26, u64(id)))

  def peerMisbehaved(peer: Array[Byte], reason: String): Unit =
    emit(28, peer ++ string(reason, 128))
    ()

  def importing(slot: Long, outline: Array[Byte]): Option[EventRef] = emit(43, u32(slot) ++ outline)

  def blockVerificationFailed(importing: Option[EventRef], reason: String): Unit =
    idOnCurrentConnection(importing).foreach(id => emit(44, u64(id) ++ string(reason, 128)))

  def blockVerified(importing: Option[EventRef]): Unit =
    idOnCurrentConnection(importing).foreach(id => emit(45, u64(id)))

  def blockExecutionFailed(importing: Option[EventRef], reason: String): Unit =
    idOnCurrentConnection(importing).foreach(id => emit(46, u64(id) ++ string(reason, 128)))

  def authoring(slot: Long, parent: Hash): Option[EventRef] =
    emit(40, u32(slot) ++ parent.bytes.toArray)

  def authoringFailed(authoring: Option[EventRef], reason: String): Unit =
    idOnCurrentConnection(authoring).foreach(id => emit(41, u64(id) ++ string(reason, 128)))

  def authored(authoring: Option[EventRef], outline: Array[Byte]): Unit =
    idOnCurrentConnection(authoring).foreach(id => emit(42, u64(id) ++ outline))

  def shutdown(): Unit =
    running = false
    sender.interrupt()

  private var wasConnected: Option[Boolean] = None
  private var lastDropLog = System.nanoTime() - DropLogIntervalMillis * 1_000_000L

  private def newGeneration(): Long =
    idLock.synchronized {
      drop(queue.size.toLong)
      queue.clear()
      generation += 1
      nextEventId = 0
      generation
    }

  private def run(): Unit =
    var backoff = retryMillis
    while running do
      var channel: SocketChannel = null
      var selector: Selector = null
      try
        channel = SocketChannel.open()
        channel.configureBlocking(false)
        selector = Selector.open()
        connect(channel, selector)
        channel.socket().setTcpNoDelay(true)
        val gen = newGeneration()
        if wasConnected.contains(false) then logger.warn(s"telemetry reconnected to $address")
        else logger.info(s"telemetry connected to $address")
        wasConnected = Some(true)
        backoff = retryMillis
        write(channel, selector, nodeInfo())
        while running do
          val next = queue.poll(1, TimeUnit.SECONDS)
          if next != null && next._1 == gen then write(channel, selector, next._2)
          if droppedUnlogged.get > 0 && System.nanoTime() - lastDropLog >= DropLogIntervalMillis * 1_000_000L then
            lastDropLog = System.nanoTime()
            logger.warn(s"telemetry dropped ${droppedUnlogged.getAndSet(0)} event(s): the queue was full")
      catch
        case _: InterruptedException => running = false
        case _: java.nio.channels.ClosedByInterruptException => running = false
        case e: Exception if running =>
          newGeneration()
          val unlogged = droppedUnlogged.getAndSet(0)
          val drops = if unlogged > 0 then s" ($unlogged event(s) dropped)" else ""
          wasConnected match
            case Some(true) =>
              logger.warn(s"telemetry connection to $address lost: ${e.getMessage}$drops; reconnecting")
            case None => logger.warn(s"telemetry server $address unavailable: ${e.getMessage}$drops; retrying")
            case Some(false) => logger.debug(s"telemetry to $address still unavailable: ${e.getMessage}$drops")
          wasConnected = Some(false)
          try Thread.sleep(backoff)
          catch case _: InterruptedException => running = false
          backoff = math.min(backoff * 2, maxRetryMillis)
        case _: Exception => ()
      finally
        if channel != null then
          try channel.close()
          catch case _: java.io.IOException => ()
        if selector != null then
          try selector.close()
          catch case _: java.io.IOException => ()

  private def connect(channel: SocketChannel, selector: Selector): Unit =
    if !channel.connect(address) then
      val key = channel.register(selector, SelectionKey.OP_CONNECT)
      val deadline = System.nanoTime() + ConnectTimeoutMillis * 1_000_000L
      while !channel.finishConnect() do
        val left = (deadline - System.nanoTime()) / 1_000_000L
        if left <= 0 then throw new java.net.SocketTimeoutException("connect timed out")
        selector.select(left)
        selector.selectedKeys().clear()
      key.cancel()
      selector.selectNow(): Unit

  private def write(channel: SocketChannel, selector: Selector, content: Array[Byte]): Unit =
    val buffer = ByteBuffer.wrap(LittleEndian.encode(content.length.toLong, 4) ++ content)
    val deadline = System.nanoTime() + writeTimeoutMillis * 1_000_000L
    channel.write(buffer)
    if buffer.hasRemaining then
      val key = channel.register(selector, SelectionKey.OP_WRITE)
      try
        while buffer.hasRemaining do
          val left = (deadline - System.nanoTime()) / 1_000_000L
          if left <= 0 then throw new java.net.SocketTimeoutException(s"a write did not complete in $writeTimeoutMillis ms")
          if !running then throw new InterruptedException
          selector.select(math.min(left, 1000L))
          selector.selectedKeys().clear()
          channel.write(buffer)
      finally
        key.cancel()
        selector.selectNow(): Unit

object Telemetry:
  final case class EventRef(generation: Long, id: Long)

  private val QueueCapacity = 4096
  private val RetryMillis = 1000L
  private val MaxRetryMillis = 30000L
  private val WriteTimeoutMillis = 10000L
  private val ConnectTimeoutMillis = 5000L
  private val DropLogIntervalMillis = 10000L

  /** JIP-3 Timestamp: microseconds since the start of the JAM Common Era. */
  def timestampMicros(): Long =
    System.currentTimeMillis() * 1000L - SlotClock.JamCommonEraSeconds * 1000000L

  def peerAddress(address: Option[java.net.SocketAddress]): Array[Byte] =
    address match
      case Some(a: InetSocketAddress) if a.getAddress != null =>
        val ip = a.getAddress.getAddress
        val v6 =
          if ip.length == 16 then ip
          else Array.fill[Byte](10)(0) ++ Array[Byte](0xff.toByte, 0xff.toByte) ++ ip
        v6 ++ u16(a.getPort.toLong)
      case _ => new Array[Byte](18)

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
