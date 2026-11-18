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
import io.forge.jam.protocol.pvmhost.{AccumulateCost, ExecCost, IsAuthorizedCost, RefineCost}

/** Telemetry client (JIP-3): when enabled, the node opens a TCP connection to `--telemetry HOST:PORT` carrying
  * length-prefixed messages (u32 LE size ++ content), the node information message first, then events.
  *
  * Emitted: 0 dropped; 10 status, 11 best block changed, 12 finalized block changed, 13 sync status changed;
  * 21/23 connected in, 24/26 connected out, 27 disconnected, 28 peer misbehaved; 40 to 47, block authoring, importing
  * and execution (47 with each accumulated service's Accumulate Cost); 60 to 68, block announcement (UP 0) and CE 128
  * block requests; 80 to 84, Safrole ticket generation and CE 131/132; 90 to 113, the guarantor's pipeline (CE 133,
  * 134, 146), CE 135 guarantees and the guarantee pool; 120 to 131, CE 137 shard requests and CE 141 assurances; 140 to
  * 153, CE 138 bundle shards, bundle reconstruction and CE 147; 160 to 178, segment recovery (CE 139/140, CE 148);
  * 190 to 199, CE 142/143 preimages and the preimage pool.
  *
  * Not emitted: 20 connection refused, 22 connect in failed and 25 connect out failed (the transport reports a
  * connection only once its handshake is done, and drops refused or failed ones without telling the node), and 21 and
  * 24 are sent at that point too. The guarantee discard reason "replaced by better guarantee" and the preimage reasons
  * "not requested on-chain" and "too many preimages" do not arise: the pools never replace a guarantee, nor cap or
  * re-check preimages. Where JIP-3 asks events 140, 146, 147 and 148 to refer to an auditing event, which it has not
  * defined yet, they carry [[Telemetry.NoEvent]].
  *
  * Events queue on a bounded buffer and a single writer thread sends them; no other thread ever waits on telemetry.
  * An event that finds the buffer full is dropped and counted ([[droppedEvents]]): the newest, since earlier events may
  * already be referenced by ID. A dropped event keeps its ID, and each run of them is replaced on the wire by one
  * JIP-3 "dropped" event (0) where they would have been: just before the next event that was queued, or on its own
  * once the buffer has drained. Drops are logged once per episode. A write that does not complete within
  * `writeTimeoutMillis` (a server that stopped reading) closes the connection. A lost or refused connection is retried
  * with a doubling backoff from `retryMillis` up to `maxRetryMillis`; loss and recovery are logged at warn once per
  * change. Every new connection starts with the node information message (event IDs are per connection, so they
  * restart at 0).
  */
final class Telemetry(
    address: InetSocketAddress,
    nodeInfo: () => Array[Byte],
    writeTimeoutMillis: Long = Telemetry.WriteTimeoutMillis,
    retryMillis: Long = Telemetry.RetryMillis,
    maxRetryMillis: Long = Telemetry.MaxRetryMillis,
    queueCapacity: Int = Telemetry.QueueCapacity
) extends LazyLogging:
  import Telemetry.*

  private val queue = new ArrayBlockingQueue[Queued](queueCapacity)
  @volatile private var running = true

  /** Connection generation and the ID of the next event on it. */
  private var generation = 0L
  private var nextEventId = 0L
  private var pendingDrops: Option[DropRun] = None
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
    val timestamp = timestampMicros()
    val content = LittleEndian.encode(timestamp, 8) ++ Array(discriminator.toByte) ++ body
    idLock.synchronized {
      val ref = EventRef(generation, nextEventId)
      nextEventId += 1
      if queue.offer(Queued(generation, content, pendingDrops)) then
        pendingDrops = None
        Some(ref)
      else
        drop(1)
        pendingDrops = Some(pendingDrops.fold(DropRun(timestamp, timestamp, 1))(r => r.copy(last = timestamp, count = r.count + 1)))
        None
    }

  /** Event ID of `ref` if it was sent on the current connection. */
  def idOnCurrentConnection(ref: Option[EventRef]): Option[Long] =
    idLock.synchronized(ref.filter(_.generation == generation).map(_.id))

  private def after(ref: Option[EventRef], discriminator: Int)(body: => Array[Byte]): Option[EventRef] =
    idOnCurrentConnection(ref).flatMap(id => emit(discriminator, u64(id) ++ body))

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

  def blockExecuted(ref: Option[EventRef], costs: Map[Long, AccumulateCost]): Unit =
    after(ref, 47)(accumulatedServices(costs)): Unit


  def blockAnnouncementStreamOpened(peer: Array[Byte], remote: Boolean): Option[EventRef] = emit(60, peer ++ side(remote))

  def blockAnnouncementStreamClosed(opened: Option[EventRef], remote: Boolean, why: String): Unit =
    after(opened, 61)(side(remote) ++ reason(why)): Unit

  def blockAnnounced(peer: Array[Byte], remote: Boolean, slot: Long, hash: Hash): Unit =
    emit(62, peer ++ side(remote) ++ u32(slot) ++ hash.bytes.toArray): Unit

  def sendingBlockRequest(peer: Array[Byte], from: Hash, ascending: Boolean, maxBlocks: Long): Option[EventRef] =
    emit(63, peer ++ from.bytes.toArray ++ direction(ascending) ++ u32(maxBlocks))

  def receivingBlockRequest(peer: Array[Byte]): Option[EventRef] = emit(64, peer)

  def blockRequestFailed(request: Option[EventRef], why: String): Unit = after(request, 65)(reason(why)): Unit

  def blockRequestSent(request: Option[EventRef]): Unit = after(request, 66)(Array.emptyByteArray): Unit

  def blockRequestReceived(request: Option[EventRef], from: Hash, ascending: Boolean, maxBlocks: Long): Unit =
    after(request, 67)(from.bytes.toArray ++ direction(ascending) ++ u32(maxBlocks)): Unit

  def blockTransferred(request: Option[EventRef], slot: Long, outline: Array[Byte], last: Boolean): Unit =
    after(request, 68)(u32(slot) ++ outline ++ bool(last)): Unit


  def generatingTickets(epoch: Long): Option[EventRef] = emit(80, u32(epoch))

  def ticketGenerationFailed(generating: Option[EventRef], why: String): Unit = after(generating, 81)(reason(why)): Unit

  def ticketsGenerated(generating: Option[EventRef], outputs: Seq[Array[Byte]]): Unit =
    after(generating, 82)(sequence(outputs)(identity)): Unit

  def ticketTransferFailed(peer: Array[Byte], remote: Boolean, ce132: Boolean, why: String): Unit =
    emit(83, peer ++ side(remote) ++ bool(ce132) ++ reason(why)): Unit

  def ticketTransferred(peer: Array[Byte], remote: Boolean, ce132: Boolean, epoch: Long, attempt: Int, vrfOutput: Hash): Unit =
    emit(84, peer ++ side(remote) ++ bool(ce132) ++ u32(epoch) ++ Array(attempt.toByte) ++ vrfOutput.bytes.toArray): Unit


  def workPackageSubmission(builder: Array[Byte], ce146: Boolean): Option[EventRef] = emit(90, builder ++ bool(ce146))

  def workPackageBeingShared(primary: Array[Byte]): Option[EventRef] = emit(91, primary)

  def workPackageFailed(submission: Option[EventRef], why: String): Unit = after(submission, 92)(reason(why)): Unit

  def duplicateWorkPackage(submission: Option[EventRef], core: Int, packageHash: Hash): Unit =
    after(submission, 93)(u16(core.toLong) ++ packageHash.bytes.toArray): Unit

  def workPackageReceived(submission: Option[EventRef], core: Int, outline: Array[Byte]): Unit =
    after(submission, 94)(u16(core.toLong) ++ outline): Unit

  def authorized(submission: Option[EventRef], cost: IsAuthorizedCost): Unit =
    after(submission, 95)(isAuthorizedCost(cost)): Unit

  def extrinsicDataReceived(submission: Option[EventRef]): Unit = after(submission, 96)(Array.emptyByteArray): Unit

  def importsReceived(submission: Option[EventRef]): Unit = after(submission, 97)(Array.emptyByteArray): Unit

  def sharingWorkPackage(submission: Option[EventRef], secondary: Array[Byte]): Unit = after(submission, 98)(secondary): Unit

  def workPackageSharingFailed(submission: Option[EventRef], secondary: Array[Byte], why: String): Unit =
    after(submission, 99)(secondary ++ reason(why)): Unit

  def bundleSent(submission: Option[EventRef], secondary: Array[Byte]): Unit = after(submission, 100)(secondary): Unit

  def refined(submission: Option[EventRef], costs: Seq[RefineCost]): Unit =
    after(submission, 101)(sequence(costs)(refineCost)): Unit

  def workReportBuilt(submission: Option[EventRef], outline: Array[Byte]): Unit = after(submission, 102)(outline): Unit

  def workReportSignatureSent(shared: Option[EventRef]): Unit = after(shared, 103)(Array.emptyByteArray): Unit

  def workReportSignatureReceived(submission: Option[EventRef], secondary: Array[Byte]): Unit =
    after(submission, 104)(secondary): Unit

  def guaranteeBuilt(submission: Option[EventRef], outline: Array[Byte]): Option[EventRef] = after(submission, 105)(outline)

  def sendingGuarantee(built: Option[EventRef], recipient: Array[Byte]): Option[EventRef] = after(built, 106)(recipient)

  def guaranteeSendFailed(sending: Option[EventRef], why: String): Unit = after(sending, 107)(reason(why)): Unit

  def guaranteeSent(sending: Option[EventRef]): Unit = after(sending, 108)(Array.emptyByteArray): Unit

  def guaranteesDistributed(submission: Option[EventRef]): Unit = after(submission, 109)(Array.emptyByteArray): Unit

  def receivingGuarantee(sender: Array[Byte]): Option[EventRef] = emit(110, sender)

  def guaranteeReceiveFailed(receiving: Option[EventRef], why: String): Unit = after(receiving, 111)(reason(why)): Unit

  def guaranteeReceived(receiving: Option[EventRef], outline: Array[Byte]): Unit = after(receiving, 112)(outline): Unit

  def guaranteeDiscarded(outline: Array[Byte], discardReason: Int): Unit =
    emit(113, outline ++ Array(discardReason.toByte)): Unit


  def sendingShardRequest(guarantor: Array[Byte], erasureRoot: Hash, shardIndex: Int): Option[EventRef] =
    emit(120, guarantor ++ erasureRoot.bytes.toArray ++ u16(shardIndex.toLong))

  def receivingShardRequest(assurer: Array[Byte]): Option[EventRef] = emit(121, assurer)

  def shardRequestFailed(request: Option[EventRef], why: String): Unit = after(request, 122)(reason(why)): Unit

  def shardRequestSent(request: Option[EventRef]): Unit = after(request, 123)(Array.emptyByteArray): Unit

  def shardRequestReceived(request: Option[EventRef], erasureRoot: Hash, shardIndex: Int): Unit =
    after(request, 124)(erasureRoot.bytes.toArray ++ u16(shardIndex.toLong)): Unit

  def shardsTransferred(request: Option[EventRef]): Unit = after(request, 125)(Array.emptyByteArray): Unit

  def distributingAssurance(anchor: Hash, bitfield: Array[Byte]): Option[EventRef] =
    emit(126, anchor.bytes.toArray ++ bitfield)

  def assuranceSendFailed(distributing: Option[EventRef], recipient: Array[Byte], why: String): Unit =
    after(distributing, 127)(recipient ++ reason(why)): Unit

  def assuranceSent(distributing: Option[EventRef], recipient: Array[Byte]): Unit =
    after(distributing, 128)(recipient): Unit

  def assuranceDistributed(distributing: Option[EventRef]): Unit = after(distributing, 129)(Array.emptyByteArray): Unit

  def assuranceReceiveFailed(sender: Array[Byte], why: String): Unit = emit(130, sender ++ reason(why)): Unit

  def assuranceReceived(sender: Array[Byte], anchor: Hash): Unit = emit(131, sender ++ anchor.bytes.toArray): Unit


  def sendingBundleShardRequest(assurer: Array[Byte], shardIndex: Int): Option[EventRef] =
    emit(140, u64(NoEvent) ++ assurer ++ u16(shardIndex.toLong))

  def receivingBundleShardRequest(auditor: Array[Byte]): Option[EventRef] = emit(141, auditor)

  def bundleShardRequestFailed(request: Option[EventRef], why: String): Unit = after(request, 142)(reason(why)): Unit

  def bundleShardRequestSent(request: Option[EventRef]): Unit = after(request, 143)(Array.emptyByteArray): Unit

  def bundleShardRequestReceived(request: Option[EventRef], erasureRoot: Hash, shardIndex: Int): Unit =
    after(request, 144)(erasureRoot.bytes.toArray ++ u16(shardIndex.toLong)): Unit

  def bundleShardTransferred(request: Option[EventRef]): Unit = after(request, 145)(Array.emptyByteArray): Unit

  def reconstructingBundle(trivial: Boolean): Unit = emit(146, u64(NoEvent) ++ bool(trivial)): Unit

  def bundleReconstructed(): Unit = emit(147, u64(NoEvent)): Unit

  def sendingBundleRequest(guarantor: Array[Byte]): Option[EventRef] = emit(148, u64(NoEvent) ++ guarantor)

  def receivingBundleRequest(auditor: Array[Byte]): Option[EventRef] = emit(149, auditor)

  def bundleRequestFailed(request: Option[EventRef], why: String): Unit = after(request, 150)(reason(why)): Unit

  def bundleRequestSent(request: Option[EventRef]): Unit = after(request, 151)(Array.emptyByteArray): Unit

  def bundleRequestReceived(request: Option[EventRef], erasureRoot: Hash): Unit =
    after(request, 152)(erasureRoot.bytes.toArray): Unit

  def bundleTransferred(request: Option[EventRef]): Unit = after(request, 153)(Array.emptyByteArray): Unit


  def workPackageHashMapped(submission: Option[EventRef], packageHash: Hash, segmentsRoot: Hash): Unit =
    after(submission, 160)(packageHash.bytes.toArray ++ segmentsRoot.bytes.toArray): Unit

  def segmentsRootMapped(submission: Option[EventRef], segmentsRoot: Hash, erasureRoot: Hash): Unit =
    after(submission, 161)(segmentsRoot.bytes.toArray ++ erasureRoot.bytes.toArray): Unit

  def sendingSegmentShardRequest(
      submission: Option[EventRef],
      assurer: Array[Byte],
      ce140: Boolean,
      shards: Seq[(Int, Int)]
  ): Option[EventRef] =
    after(submission, 162)(assurer ++ bool(ce140) ++ sequence(shards)((s, i) => u16(s.toLong) ++ u16(i.toLong)))

  def receivingSegmentShardRequest(sender: Array[Byte], ce140: Boolean): Option[EventRef] = emit(163, sender ++ bool(ce140))

  def segmentShardRequestFailed(request: Option[EventRef], why: String): Unit = after(request, 164)(reason(why)): Unit

  def segmentShardRequestSent(request: Option[EventRef]): Unit = after(request, 165)(Array.emptyByteArray): Unit

  def segmentShardRequestReceived(request: Option[EventRef], shards: Int): Unit =
    after(request, 166)(u16(math.min(shards, 0xffff).toLong)): Unit

  def segmentShardsTransferred(request: Option[EventRef]): Unit = after(request, 167)(Array.emptyByteArray): Unit

  def reconstructingSegments(submission: Option[EventRef], segments: Seq[Int], trivial: Boolean): Option[EventRef] =
    after(submission, 168)(sequence(segments)(s => u16(s.toLong)) ++ bool(trivial))

  def segmentReconstructionFailed(reconstructing: Option[EventRef], why: String): Unit =
    after(reconstructing, 169)(reason(why)): Unit

  def segmentsReconstructed(reconstructing: Option[EventRef]): Unit =
    after(reconstructing, 170)(Array.emptyByteArray): Unit

  def segmentVerificationFailed(submission: Option[EventRef], segments: Seq[Int], why: String): Unit =
    after(submission, 171)(sequence(segments)(s => u16(s.toLong)) ++ reason(why)): Unit

  def segmentsVerified(submission: Option[EventRef], segments: Seq[Int]): Unit =
    after(submission, 172)(sequence(segments)(s => u16(s.toLong))): Unit

  def sendingSegmentRequest(submission: Option[EventRef], guarantor: Array[Byte], segments: Seq[Int]): Option[EventRef] =
    after(submission, 173)(guarantor ++ sequence(segments)(s => u16(s.toLong)))

  def receivingSegmentRequest(guarantor: Array[Byte]): Option[EventRef] = emit(174, guarantor)

  def segmentRequestFailed(request: Option[EventRef], why: String): Unit = after(request, 175)(reason(why)): Unit

  def segmentRequestSent(request: Option[EventRef]): Unit = after(request, 176)(Array.emptyByteArray): Unit

  def segmentRequestReceived(request: Option[EventRef], segments: Int): Unit =
    after(request, 177)(u16(math.min(segments, 0xffff).toLong)): Unit

  def segmentsTransferred(request: Option[EventRef]): Unit = after(request, 178)(Array.emptyByteArray): Unit


  def preimageAnnouncementFailed(peer: Array[Byte], remote: Boolean, why: String): Unit =
    emit(190, peer ++ side(remote) ++ reason(why)): Unit

  def preimageAnnounced(peer: Array[Byte], remote: Boolean, service: Long, hash: Hash, length: Long): Unit =
    emit(191, peer ++ side(remote) ++ u32(service) ++ hash.bytes.toArray ++ u32(length)): Unit

  def announcedPreimageForgotten(service: Long, hash: Hash, length: Long, forgetReason: Int): Unit =
    emit(192, u32(service) ++ hash.bytes.toArray ++ u32(length) ++ Array(forgetReason.toByte)): Unit

  def sendingPreimageRequest(recipient: Array[Byte], hash: Hash): Option[EventRef] =
    emit(193, recipient ++ hash.bytes.toArray)

  def receivingPreimageRequest(sender: Array[Byte]): Option[EventRef] = emit(194, sender)

  def preimageRequestFailed(request: Option[EventRef], why: String): Unit = after(request, 195)(reason(why)): Unit

  def preimageRequestSent(request: Option[EventRef]): Unit = after(request, 196)(Array.emptyByteArray): Unit

  def preimageRequestReceived(request: Option[EventRef], hash: Hash): Unit = after(request, 197)(hash.bytes.toArray): Unit

  def preimageTransferred(request: Option[EventRef], length: Long): Unit = after(request, 198)(u32(length)): Unit

  def preimageDiscarded(hash: Hash, length: Long, discardReason: Int): Unit =
    emit(199, hash.bytes.toArray ++ u32(length) ++ Array(discardReason.toByte)): Unit

  def shutdown(): Unit =
    running = false
    sender.interrupt()

  private var wasConnected: Option[Boolean] = None
  private var lastDropLog = System.nanoTime() - DropLogIntervalMillis * 1_000_000L

  private def newGeneration(): Long =
    idLock.synchronized {
      drop(queue.size.toLong)
      queue.clear()
      pendingDrops = None
      generation += 1
      nextEventId = 0
      generation
    }

  private def takeTrailingDrops(gen: Long): Option[DropRun] =
    idLock.synchronized {
      if generation != gen || !queue.isEmpty then None
      else
        val run = pendingDrops
        pendingDrops = None
        run
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
          if next != null && next.generation == gen then
            next.dropsBefore.foreach(drops => write(channel, selector, droppedEvent(drops)))
            write(channel, selector, next.content)
          else if next == null then takeTrailingDrops(gen).foreach(drops => write(channel, selector, droppedEvent(drops)))
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

  private final case class Queued(generation: Long, content: Array[Byte], dropsBefore: Option[DropRun])

  private final case class DropRun(first: Long, last: Long, count: Long)

  private def droppedEvent(run: DropRun): Array[Byte] =
    u64(run.first) ++ Array(0.toByte) ++ u64(run.last) ++ u64(run.count)

  val NoEvent: Long = -1L

  object GuaranteeDiscard:
    val ReportedOnChain = 0
    val Replaced = 1
    val CannotBeReported = 2
    val TooMany = 3
    val Other = 4

  object AnnouncedPreimageForget:
    val ProvidedOnChain = 0
    val NotRequested = 1
    val FailedToAcquire = 2
    val TooMany = 3
    val BadLength = 4
    val Other = 5

  object PreimageDiscard:
    val ProvidedOnChain = 0
    val NotRequested = 1
    val TooMany = 2
    val Other = 3

  private[node] val QueueCapacity = 4096
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

  private def side(remote: Boolean): Array[Byte] = Array((if remote then 1 else 0).toByte)
  private def bool(b: Boolean): Array[Byte] = Array((if b then 1 else 0).toByte)
  private def reason(r: String): Array[Byte] = string(r, 128)

  private def direction(ascending: Boolean): Array[Byte] = Array((if ascending then 0 else 1).toByte)

  /** JIP-3 String<N>: len++ UTF-8 bytes, truncated to `max` bytes on a
    * character boundary.
    */
  def string(s: String, max: Int): Array[Byte] =
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    var n = math.min(bytes.length, max)
    // Back off to a UTF-8 character boundary (continuation bytes are 10xxxxxx).
    while n > 0 && n < bytes.length && (bytes(n) & 0xc0) == 0x80 do n -= 1
    JamCodecs.encodeCompactInteger(n.toLong) ++ bytes.take(n)

  def sequence[A](items: Seq[A])(encode: A => Array[Byte]): Array[Byte] =
    val out = new ByteArrayOutputStream()
    out.write(JamCodecs.encodeCompactInteger(items.size.toLong))
    items.foreach(i => out.write(encode(i)))
    out.toByteArray

  /** Node information message (protocol version 1). */
  def nodeInformation(
      jamParameters: Array[Byte],
      genesisHash: Hash,
      peerId: Array[Byte],
      listenPort: Int,
      recompiler: Boolean,
      implementationVersion: String,
      grayPaperVersion: String,
      note: String,
      advertisedAddress: Option[java.net.InetAddress] = None
  ): Array[Byte] =
    val out = new ByteArrayOutputStream()
    out.write(1)
    out.write(jamParameters)
    out.write(genesisHash.bytes.toArray)
    out.write(peerId)
    out.write(advertisedAddress.fold(new Array[Byte](16))(Connectivity.addressBytes))
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

  def blockOutline(block: io.forge.jam.core.types.block.Block, sizeBytes: Int, headerHash: Hash): Array[Byte] =
    val x = block.extrinsic
    blockOutline(
      sizeBytes,
      headerHash,
      x.tickets.size,
      x.preimages.size,
      x.preimages.map(_.blob.length.toLong).sum,
      x.guarantees.size,
      x.assurances.size,
      x.disputes.verdicts.size
    )

  def workReportOutline(reportHash: Hash, bundleSize: Long, erasureRoot: Hash, segmentsRoot: Hash): Array[Byte] =
    reportHash.bytes.toArray ++ u32(bundleSize) ++ erasureRoot.bytes.toArray ++ segmentsRoot.bytes.toArray

  def guaranteeOutline(reportHash: Hash, slot: Long, guarantors: Seq[Int]): Array[Byte] =
    reportHash.bytes.toArray ++ u32(slot) ++ sequence(guarantors)(v => u16(v.toLong))

  def guaranteeOutline(g: io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic): Array[Byte] =
    import io.forge.jam.core.codec.JamCodecs.encode
    guaranteeOutline(
      io.forge.jam.core.Hashing.blake2b256(g.report.encode.toArray),
      g.slot.toLong,
      g.signatures.map(_.validatorIndex.toInt)
    )

  def workPackageOutline(wp: io.forge.jam.core.types.workpackage.WorkPackage, size: Int, hash: Hash): Array[Byte] =
    u32(size.toLong) ++ hash.bytes.toArray ++ wp.context.anchor.bytes.toArray ++ u32(wp.context.lookupAnchorSlot.toLong) ++
      sequence(wp.context.prerequisites)(_.bytes.toArray) ++
      sequence(wp.items) { item =>
        u32(item.service.toLong) ++ u32(item.payload.length.toLong) ++ u64(item.refineGasLimit.toLong) ++
          u64(item.accumulateGasLimit.toLong) ++ u32(item.extrinsic.map(_.len.toLong).sum) ++
          sequence(item.importSegments)(s => s.treeRoot.bytes.toArray ++ u16(s.index.toLong)) ++
          u16(item.exportCount.toLong)
      }

  def execCost(c: ExecCost): Array[Byte] = u64(c.gas) ++ u64(c.nanos)

  def isAuthorizedCost(c: IsAuthorizedCost): Array[Byte] = execCost(c.total) ++ u64(c.compileNanos) ++ execCost(c.hostCalls)

  def refineCost(c: RefineCost): Array[Byte] =
    execCost(c.total) ++ u64(c.compileNanos) ++ execCost(c.historicalLookup) ++ execCost(c.machineExpunge) ++
      execCost(c.peekPokePages) ++ execCost(c.invoke) ++ execCost(c.other)

  def accumulateCost(c: AccumulateCost): Array[Byte] =
    u32(c.calls.toLong) ++ u32(c.transfers.toLong) ++ u32(c.items.toLong) ++ execCost(c.total) ++ u64(c.compileNanos) ++
      execCost(c.readWrite) ++ execCost(c.lookup) ++ execCost(c.preimages) ++ execCost(c.service) ++
      execCost(c.transfer) ++ u64(c.transferGas) ++ execCost(c.other)

  val MaxAccumulatedServices: Int = 500
  val CombinedServices: Long = 0xffffffffL

  def accumulatedServices(costs: Map[Long, AccumulateCost]): Array[Byte] =
    val listed =
      if costs.size <= MaxAccumulatedServices then costs.toSeq.sortBy(_._1)
      else
        val ranked = costs.toSeq.sortBy((id, c) => (-c.total.gas, id))
        val (kept, combined) = ranked.splitAt(MaxAccumulatedServices - 1)
        kept.sortBy(_._1) :+ (CombinedServices -> combined.map(_._2).reduce(_ + _))
    sequence(listed)((id, c) => u32(id) ++ accumulateCost(c))
