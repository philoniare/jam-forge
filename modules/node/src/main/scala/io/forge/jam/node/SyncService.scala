package io.forge.jam.node

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.trie.TrieNode
import io.forge.jam.network.{PeerConnection, MessageStream, StreamKind}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Wire encodings for the UP 0 / CE 128 protocol messages (jamnp-s spec).
  * `Final` and `Leaf` are hash(32) ++ slot(4 LE); the handshake leaf list is
  * compact-length prefixed.
  */
object SyncCodec:

  final case class HashSlot(hash: Hash, slot: Long)

  def encodeHashSlot(hs: HashSlot): Array[Byte] =
    hs.hash.bytes.toArray ++ encodeU32(hs.slot)

  def decodeHashSlot(bytes: Array[Byte], offset: Int): HashSlot =
    HashSlot(
      Hash(java.util.Arrays.copyOfRange(bytes, offset, offset + 32)),
      decodeU32(bytes, offset + 32)
    )

  /** Handshake: Final ++ len++[Leaf]. */
  def encodeHandshake(finalHead: HashSlot, leaves: List[HashSlot]): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream()
    out.write(encodeHashSlot(finalHead))
    out.write(JamCodecs.encodeCompactInteger(leaves.length.toLong))
    leaves.foreach(l => out.write(encodeHashSlot(l)))
    out.toByteArray

  /** Size of a `HashSlot` on the wire. */
  private val HashSlotBytes = 36

  /** Bytes a compact integer occupies, read from its first byte: one more than its leading one bits, or 9 for 0xff. */
  private def compactSize(prefix: Byte): Int =
    val ones = Integer.numberOfLeadingZeros(~prefix & 0xff) - 24
    if ones == 8 then 9 else ones + 1

  /** Refuses a message that is short, names more than `SyncService.MaxHandshakeLeaves` leaves, or has bytes after
    * its last leaf. Never throws.
    */
  def decodeHandshake(bytes: Array[Byte]): Either[String, (HashSlot, List[HashSlot])] =
    if bytes.length < HashSlotBytes + 1 then Left(s"handshake of ${bytes.length} bytes is too short")
    else
      val countSize = compactSize(bytes(HashSlotBytes))
      if bytes.length < HashSlotBytes + countSize then Left("handshake leaf count is truncated")
      else
        val (count, consumed) = JamCodecs.decodeCompactInteger(bytes, HashSlotBytes)
        if count < 0 || count > SyncService.MaxHandshakeLeaves then Left(s"handshake names $count leaves")
        else
          val start = HashSlotBytes + consumed
          val expected = start + count.toInt * HashSlotBytes
          if bytes.length != expected then
            Left(s"handshake naming $count leaves should be $expected bytes, got ${bytes.length}")
          else
            val leaves = List.tabulate(count.toInt)(i => decodeHashSlot(bytes, start + i * HashSlotBytes))
            Right((decodeHashSlot(bytes, 0), leaves))

  /** Announcement: Header ++ Final. The header is carried as its encoded
    * bytes; the trailing 36 bytes are the announcer's latest finalized block.
    */
  def encodeAnnouncement(headerBytes: Array[Byte], finalHead: HashSlot): Array[Byte] =
    headerBytes ++ encodeHashSlot(finalHead)

  /** A header starts with parent hash, state root and extrinsic hash (32 bytes each) and the slot (4 LE): anything
    * shorter cannot be one.
    */
  val MinAnnouncedHeaderBytes: Int = 100

  /** Refuses a message too short to hold a header's fixed fields and a final. The header is opaque here, so bytes
    * appended to an announcement are not caught: they shift into the header and change its hash, so they name a
    * block no peer holds. Never throws.
    */
  def decodeAnnouncement(bytes: Array[Byte]): Either[String, (Array[Byte], HashSlot)] =
    if bytes.length < MinAnnouncedHeaderBytes + HashSlotBytes then
      Left(s"announcement of ${bytes.length} bytes is too short")
    else
      val split = bytes.length - HashSlotBytes
      Right((java.util.Arrays.copyOfRange(bytes, 0, split), decodeHashSlot(bytes, split)))

  /** CE 128 request: Header Hash ++ Direction(1) ++ Maximum Blocks(4 LE). */
  def encodeBlockRequest(from: Hash, ascending: Boolean, maxBlocks: Long): Array[Byte] =
    from.bytes.toArray ++ Array[Byte](if ascending then 0 else 1) ++ encodeU32(maxBlocks)

  def decodeBlockRequest(bytes: Array[Byte]): Either[String, (Hash, Boolean, Long)] =
    if bytes.length != 37 then Left(s"expected 37 bytes, got ${bytes.length}")
    else if bytes(32) != 0 && bytes(32) != 1 then Left(s"bad direction ${bytes(32)}")
    else
      Right(
        (
          Hash(java.util.Arrays.copyOfRange(bytes, 0, 32)),
          bytes(32) == 0,
          decodeU32(bytes, 33)
        )
      )

  /** CE 129 request: Header Hash(32) ++ Start Key(31) ++ End Key(31) ++
    * Maximum Size(4 LE) = 98 bytes.
    */
  def encodeStateRequest(headerHash: Hash, start: JamBytes, end: JamBytes, maxSize: Long): Array[Byte] =
    require(start.length == 31, s"start key must be 31 bytes, got ${start.length}")
    require(end.length == 31, s"end key must be 31 bytes, got ${end.length}")
    headerHash.bytes ++ start.toArray ++ end.toArray ++ encodeU32(maxSize)

  def decodeStateRequest(bytes: Array[Byte]): Either[String, (Hash, JamBytes, JamBytes, Long)] =
    if bytes.length != 98 then
      Left(s"CE129: expected 98 bytes, got ${bytes.length}")
    else
      val headerHash = Hash(java.util.Arrays.copyOfRange(bytes, 0, 32))
      val start = JamBytes(java.util.Arrays.copyOfRange(bytes, 32, 63))
      val end = JamBytes(java.util.Arrays.copyOfRange(bytes, 63, 94))
      val maxSize = decodeU32(bytes, 94)
      Right((headerHash, start, end, maxSize))

  /** CE 129 response message 1: concatenated 64-byte boundary nodes, each
    * "as returned by B/L" in GP 0.8.0 appendix D (may be empty).
    */
  def encodeBoundaryNodes(nodes: List[TrieNode]): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream(nodes.length * TrieNode.GpEncodingBytes)
    nodes.foreach(n => out.write(n.gpEncoding))
    out.toByteArray

  /** The 64-byte boundary nodes of a CE 129 response; a node's hash is the
    * blake2b-256 of its bytes.
    */
  def decodeBoundaryNodes(bytes: Array[Byte]): Either[String, List[Array[Byte]]] =
    if bytes.length % TrieNode.GpEncodingBytes != 0 then
      Left(s"boundary nodes: ${bytes.length} bytes is not a multiple of ${TrieNode.GpEncodingBytes}")
    else Right(bytes.grouped(TrieNode.GpEncodingBytes).toList)

  /** CE 129 response message 2: concatenated `key(31) ++ compactLen(value)
    * ++ value` pairs.
    */
  def encodeStatePairs(pairs: List[(JamBytes, JamBytes)]): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream()
    pairs.foreach { case (k, v) =>
      out.write(k.toArray)
      out.write(JamCodecs.encodeCompactInteger(v.length.toLong))
      out.write(v.toArray)
    }
    out.toByteArray

  def decodeStatePairs(bytes: Array[Byte]): List[(JamBytes, JamBytes)] =
    val out = mutable.ArrayBuffer[(JamBytes, JamBytes)]()
    var offset = 0
    while offset < bytes.length do
      if bytes.length - offset < 32 then throw new IllegalArgumentException("state pairs: truncated key")
      val key = JamBytes(java.util.Arrays.copyOfRange(bytes, offset, offset + 31))
      offset += 31
      val (len, consumed) = JamCodecs.decodeCompactInteger(bytes, offset)
      offset += consumed
      if len < 0 || len > bytes.length - offset then throw new IllegalArgumentException("state pairs: truncated value")
      val value = JamBytes(java.util.Arrays.copyOfRange(bytes, offset, offset + len.toInt))
      offset += len.toInt
      out += ((key, value))
    out.toList

  private def encodeU32(v: Long): Array[Byte] =
    Array(
      (v & 0xff).toByte,
      ((v >> 8) & 0xff).toByte,
      ((v >> 16) & 0xff).toByte,
      ((v >> 24) & 0xff).toByte
    )

  private def decodeU32(b: Array[Byte], off: Int): Long =
    (b(off) & 0xffL) | ((b(off + 1) & 0xffL) << 8) |
      ((b(off + 2) & 0xffL) << 16) | ((b(off + 3) & 0xffL) << 24)

/** Block synchronization: serves UP 0 (handshake + announcements) and CE 128
  * (block requests), announces our own new blocks, and catches up on
  * announced blocks we don't have.
  */
final class SyncService(
    chain: ChainManager,
    /** Where CE 128 and CE 129 requests are answered: off the network event loops, which only hand them over. */
    serving: InboundWork = InboundWork.direct,
    announcements: InboundWork = InboundWork.direct,
    /** Owner of this service's threads: the node's, or its own when made alone. */
    threads: NodeThreads = new NodeThreads,
    misbehaved: (PeerConnection, Offence) => Unit = (_, _) => (),
    observer: SyncService.ImportObserver = SyncService.ImportObserver.none,
    scores: PeerScores = new PeerScores(),
    telemetry: Option[Telemetry] = None
) extends LazyLogging:
  import SyncCodec.*

  /** UP 0 streams by peer connection, for broadcasting announcements. */
  private val announceStreams = ConcurrentHashMap.newKeySet[MessageStream]()

  private val peerFinals = new ConcurrentHashMap[PeerConnection, HashSlot]()

  @volatile private var held = false
  def hold(): Unit = held = true

  def release(): Unit =
    held = false
    peerFinals.keySet.forEach(conn => catchUp(conn))

  private[node] def fetchBlocks(conn: PeerConnection, from: Hash, maxBlocks: Int, ascending: Boolean = false)
      : Option[List[Array[Byte]]] =
    exchange(conn, from, maxBlocks, ascending, strict = true).flatMap { blocks =>
      if blocks.size <= maxBlocks then Some(blocks)
      else
        logger.warn(s"CE128: ${blocks.size} block(s) in answer to a request for $maxBlocks")
        misbehaved(conn, Offence.Malformed)
        None
    }

  /** The latest `Final` announced by the peer on `conn`, if any handshake or
    * announcement has been received from it yet.
    */
  def peerFinal(conn: PeerConnection): Option[HashSlot] = Option(peerFinals.get(conn))

  /** Each connected peer's best head as last seen: the newest leaf of its
    * UP 0 handshake, then every block it announces.
    */
  private val peerBestHeads = new ConcurrentHashMap[PeerConnection, HashSlot]()

  /** Best heads of the peers with a live UP 0 stream, keyed by peer key so a
    * peer reached over two connections counts once.
    */
  def peerBests: Iterable[Hash] = peerBests(_ => true)

  /** The best head of each peer that `counts`, one per peer key. */
  def peerBests(counts: PeerConnection => Boolean): Iterable[Hash] =
    peerBestHeads.asScala.toList
      .filter(e => e._1.isOpen && counts(e._1))
      .groupBy(e => JamBytes(e._1.peerKey))
      .values
      .map(_.maxBy(_._2.slot)._2.hash)

  def peerBestSlots: List[Long] =
    peerBestHeads.asScala.toList
      .filter(_._1.isOpen)
      .groupBy(e => JamBytes(e._1.peerKey))
      .values
      .map(_.map(_._2.slot).max)
      .toList

  /** Finalized blocks announced by peers with a live UP 0 stream, one per peer key (the newest). */
  def peerFinalized: List[(PeerConnection, HashSlot)] =
    peerFinals.asScala.toList
      .filter(_._1.isOpen)
      .groupBy(e => JamBytes(e._1.peerKey))
      .values
      .map(_.maxBy(_._2.slot))
      .toList

  /** Keep to the chain `needed` peers announce as finalized, fetching the agreed block when we lack it. */
  def followPeerFinality(needed: Int, counts: PeerConnection => Boolean = _ => true): Unit =
    val finals = peerFinalized.filter((conn, _) => counts(conn))
    chain.followPeerFinality(finals.map((_, f) => (f.hash, f.slot)), needed) match
      case Left(missing) =>
        if fetching.add(missing) then
          finals.find(_._2.hash == missing).foreach((conn, _) => requestAndImport(conn, missing, onDone = () => fetching.remove(missing)))
      case Right(_) => ()

  private val fetching = ConcurrentHashMap.newKeySet[Hash]()

  /** Fetch block `hash` and its missing ancestors from `conn`, once at a time per block. */
  def fetchBlock(conn: PeerConnection, hash: Hash): Unit =
    if fetching.add(hash) then requestAndImport(conn, hash, onDone = () => fetching.remove(hash))

  private[node] def fetchingBlock(hash: Hash): Boolean = fetching.contains(hash)

  /** Drop what is held for a connection that closed. Its UP 0 stream's close listener normally does this; a message
    * handled while the stream closes can put an entry back, which nothing else would remove.
    */
  def forgetConnection(conn: PeerConnection): Unit =
    upStreams.remove(conn)
    peerFinals.remove(conn)
    peerBestHeads.remove(conn)
    ()

  private def notePeerBest(conn: PeerConnection, head: HashSlot): Unit =
    peerBestHeads.merge(conn, head, (old, nu) => if nu.slot >= old.slot then nu else old)
    ()

  private val StateRequestMaxSizeCap: Long = 16L * 1024 * 1024

  /** Cap on blocks per CE 128 response, whatever the requested maximum. */
  private val MaxBlocksPerResponse: Long = 256L
  private val MaxBlockResponseBytes: Long = 16L * 1024 * 1024

  /** Single-threaded import pipeline so block application stays sequential. */
  private val importExecutor = threads.single("jam-sync-import")

  private val fetchExecutor = threads.fixed("jam-sync-fetch", SyncService.MaxConcurrentFetches)

  /** Stop the import pipeline and wait for an in-flight import to finish —
    * must run before the chain's storage closes (a late import reading a
    * closed RocksDB segfaults in JNI).
    */
  def shutdown(): Unit =
    fetchExecutor.shutdownNow() // interrupts response waits
    importExecutor.shutdownNow()
    fetchExecutor.awaitTermination(5, TimeUnit.SECONDS)
    importExecutor.awaitTermination(15, TimeUnit.SECONDS)
    ()

  private def ourFinal: HashSlot =
    val f = chain.finalized
    HashSlot(f.hash, f.slot)

  private def ourHandshake: Array[Byte] =
    encodeHandshake(ourFinal, chain.leaves.map(h => HashSlot(h.hash, h.slot)))

  // =========================================================================
  // UP 0 — block announcement
  // =========================================================================

  /** The live UP 0 stream on each connection. */
  private val upStreams = new ConcurrentHashMap[PeerConnection, MessageStream]()

  /** Handler for peer-opened UP 0 streams (we are the acceptor). */
  def blockAnnouncementHandler: io.forge.jam.network.PeerStreamHandler =
    (conn: PeerConnection, stream: MessageStream) => attachAnnouncementStream(conn, stream, "accepted")

  /** Open UP 0 to a peer we connected to (we are the initiator). */
  def openAnnouncementStream(conn: PeerConnection): MessageStream =
    val stream = conn
      .openStream(StreamKind.BlockAnnouncement)
      .get(10, TimeUnit.SECONDS)
    attachAnnouncementStream(conn, stream, "dialed")
    stream

  /** True when `conn` has an open UP 0 stream. */
  def hasAnnouncementStream(conn: PeerConnection): Boolean =
    Option(upStreams.get(conn)).exists(_.isOpen)

  /** Close the UP 0 stream on `conn`, if any (e.g. the peer stopped being a
    * grid neighbour at an epoch change).
    */
  def closeAnnouncementStream(conn: PeerConnection): Unit =
    Option(upStreams.get(conn)).foreach { s =>
      closedHere.put(s, "the peer is no longer a grid neighbour")
      s.close()
    }

  /** Both sides send their handshake as soon as the stream is open (JAMNP-S
    * UP 0). A UP kind has one stream per connection: when a second one
    * shows up, the one with the greatest QUIC stream ID is kept and the
    * others reset (JAMNP-S "Protocols and streams").
    */
  private def attachAnnouncementStream(conn: PeerConnection, stream: MessageStream, side: String): Unit =
    var handshaken = false
    val previous = upStreams.merge(conn, stream, (old, nu) => if old.isOpen && old.id > nu.id then old else nu)
    if previous ne stream then
      logger.debug(s"UP0: duplicate stream ${stream.id} on one connection; keeping ${previous.id}")
      stream.reset()
      return
    announceStreams.forEach(s => if s.ne(stream) && streamConnection.get(s) == conn then dropDuplicate(s))
    streamConnection.put(stream, conn)
    announceStreams.add(stream)
    val opened = telemetry.flatMap(_.blockAnnouncementStreamOpened(conn.peerKey, remote = side == "accepted"))
    stream.onClosed { () =>
      announceStreams.remove(stream)
      streamConnection.remove(stream)
      if upStreams.remove(conn, stream) then
        peerFinals.remove(conn)
        peerBestHeads.remove(conn)
      val ours = Option(closedHere.remove(stream))
      if conn.isOpen then
        telemetry.foreach(_.blockAnnouncementStreamClosed(opened, remote = ours.isEmpty, ours.getOrElse("closed by the peer")))
    }
    val waiting = new AnnouncementMailbox(SyncService.MaxWaitingAnnouncements)
    def malformed(reason: String): Unit =
      logger.debug(s"UP0: bad message from $side peer: $reason")
      misbehaved(conn, Offence.Malformed)
      closedHere.put(stream, s"malformed message: $reason")
      stream.reset()
    def decoded[A](decode: => Either[String, A]): Either[String, A] =
      try decode
      catch case e: Exception => Left(String.valueOf(e.getMessage))
    def ours(what: String)(f: => Unit): Unit =
      try f
      catch case e: Exception => logger.warn(s"UP0: handling a $what from $side peer failed here, not charged: $e")
    def handle(first: Boolean, msg: Array[Byte]): Unit =
      if first then
        decoded(decodeHandshake(msg)) match
          case Left(reason) => malformed(reason)
          case Right((peerFinal, peerLeaves)) =>
            ours("handshake") {
              peerFinals.put(conn, peerFinal)
              logger.info(
                s"UP0 handshake decoded from $side peer: final=${peerFinal.hash.toHex.take(18)} " +
                  s"slot=${peerFinal.slot} leaves=${peerLeaves.size}"
              )
              onPeerLeaves(conn, peerLeaves, peerFinal)
            }
      else
        decoded(decodeAnnouncement(msg)) match
          case Left(reason)                   => malformed(reason)
          case Right((headerBytes, peerFinal)) => ours("announcement")(onAnnouncement(conn, headerBytes, peerFinal))
    stream.onMessage { msg =>
      val first = !handshaken
      handshaken = true
      if waiting.offer(first, msg) && !announcements.submit(conn, 0L)(waiting.drain(handle)) then waiting.idle()
    }
    stream.send(ourHandshake)

  /** The connection each UP 0 stream belongs to. */
  private val streamConnection = new ConcurrentHashMap[MessageStream, PeerConnection]()

  private val closedHere = new ConcurrentHashMap[MessageStream, String]()

  private def dropDuplicate(old: MessageStream): Unit =
    logger.debug(s"UP0: stream ${old.id} superseded by a newer stream on the same connection; resetting")
    announceStreams.remove(old)
    streamConnection.remove(old)
    closedHere.put(old, "superseded by a newer stream")
    old.reset()

  /** Announce a newly imported/authored block on all UP 0 streams. */
  def announce(headerBytes: Array[Byte]): Unit =
    val msg = encodeAnnouncement(headerBytes, ourFinal)
    lazy val announced =
      (io.forge.jam.core.Hashing.blake2b256(headerBytes), io.forge.jam.core.LittleEndian.get(headerBytes, 96, 4))
    announceStreams.forEach { s =>
      s.send(msg)
      telemetry.foreach { t =>
        val conn = streamConnection.get(s)
        if conn != null then t.blockAnnounced(conn.peerKey, remote = false, announced._2, announced._1)
      }
    }

  /** `headerBytes` holds at least `MinAnnouncedHeaderBytes`, so its parent hash and slot can be read. */
  private def onAnnouncement(conn: PeerConnection, headerBytes: Array[Byte], peerFinal: HashSlot): Unit =
    peerFinals.put(conn, peerFinal)
    val headerHash = io.forge.jam.core.Hashing.blake2b256(headerBytes)
    val slot = io.forge.jam.core.LittleEndian.get(headerBytes, 96, 4)
    telemetry.foreach(_.blockAnnounced(conn.peerKey, remote = true, slot, headerHash))
    notePeerBest(conn, HashSlot(headerHash, slot))
    if !chain.hasBlock(headerHash) then
      // The header starts with its parent's hash: when we hold the parent, the announced block is all we lack.
      val parentKnown = chain.hasBlock(Hash(java.util.Arrays.copyOfRange(headerBytes, 0, 32)))
      val wanted = if parentKnown then 1 else blocksWanted(Some(slot))
      if wanted > 0 then
        logger.debug(s"announced block ${headerHash.toHex.take(18)} unknown; requesting $wanted block(s)")
        requestAndImport(conn, headerHash, wanted, announced = true)

  private def onPeerLeaves(conn: PeerConnection, leaves: List[HashSlot], peerFinal: HashSlot): Unit =
    leaves.maxByOption(_.slot).foreach(notePeerBest(conn, _))
    leaves.foreach { leaf =>
      if leaf.slot < peerFinal.slot then
        logger.debug(
          s"peer leaf ${leaf.hash.toHex.take(18)} (slot ${leaf.slot}) below peer's own " +
            s"final (slot ${peerFinal.slot}); ignoring"
        )
      else if !chain.hasBlock(leaf.hash) then
        val wanted = blocksWanted(Some(leaf.slot))
        if wanted > 0 then
          logger.debug(s"peer leaf ${leaf.hash.toHex.take(18)} unknown; requesting $wanted block(s)")
          requestAndImport(conn, leaf.hash, wanted)
    }

  // =========================================================================
  // CE 128 — block request
  // =========================================================================

  /** Handler serving peer block requests. The response is one message
    * holding the block sequence (JAMNP-S CE 128 `<-- [Block]`). A request
    * for an unknown block, or one that can no longer be finalized, is reset.
    */
  def blockRequestHandler: io.forge.jam.network.PeerStreamHandler =
    (conn: PeerConnection, stream: MessageStream) =>
      var asked = false
      val receiving = telemetry.flatMap(_.receivingBlockRequest(conn.peerKey))
      def refuse(why: String): Unit =
        telemetry.foreach(_.blockRequestFailed(receiving, why))
        stream.reset()
      stream.onMessage { msg =>
        // One request per stream; reading up to 256 blocks from the store is not event-loop work.
        if asked then stream.reset()
        else
          asked = true
          decodeBlockRequest(msg) match
            case Left(err) =>
              logger.warn(s"CE128: $err")
              refuse(s"malformed request: $err")
            case Right((from, ascending, maxBlocks)) =>
              telemetry.foreach(_.blockRequestReceived(receiving, from, ascending, maxBlocks))
              val taken = serving.submit(conn, msg.length.toLong) {
                try
                  if !chain.hasBlock(from) || !chain.canBeFinalized(from) then refuse("block not served: unknown or not on a chain that can be finalized")
                  else
                    val max = math.min(maxBlocks, MaxBlocksPerResponse).toInt
                    val blocks =
                      if ascending then chain.blocksAscending(from, max)
                      else chain.blocksDescending(from, max)
                    if blocks.isEmpty && !ascending then refuse("block body not held")
                    else
                      val out = new java.io.ByteArrayOutputStream()
                      val sent = blocks.iterator
                        .takeWhile(b => out.size == 0 || out.size + b.length <= MaxBlockResponseBytes)
                        .map { b => out.write(b); b }
                        .toList
                      stream.send(out.toByteArray)
                      stream.finish()
                      telemetry.foreach(t => reportTransferred(t, receiving, sent))
                catch
                  case e: Exception =>
                    logger.warn(s"CE128 handler failed: ${e.getMessage}")
                    refuse(s"failed here: ${e.getMessage}")
              }
              // Too many requests waiting (from this peer, or in all): refused.
              if !taken then refuse("too many requests waiting")
      }

  private def reportTransferred(t: Telemetry, request: Option[Telemetry.EventRef], blocks: List[Array[Byte]]): Unit =
    val last = blocks.size - 1
    blocks.zipWithIndex.foreach { (bytes, i) =>
      chain.decodeBlock(bytes).foreach { b =>
        t.blockTransferred(request, b.header.slot.toLong, Telemetry.blockOutline(b, bytes.length, chain.headerHashOf(b)), i == last)
      }
    }

  /** How many blocks to ask for, descending from a block at `slot` that we lack: every block we could still use
    * has a slot above our finalized one, and slots are distinct along a chain, so the gap to our finalized slot is
    * the most a response can usefully hold. 0 when the block is at or below our finalized slot (it is on our chain
    * and we have it, or it never can be).
    */
  private def blocksWanted(slot: Option[Long]): Int =
    slot match
      case Some(s) => math.max(0L, math.min(s - chain.finalized.slot, SyncService.MaxBlocksPerRequest.toLong)).toInt
      case None    => SyncService.MaxBlocksPerRequest

  /** Fetch up to `maxBlocks` blocks descending (inclusive) from `from` on a fresh CE 128 stream
    * and import them oldest-first. The request waits on a fetch thread; the import runs on the import executor.
    */
  private def requestAndImport(
      conn: PeerConnection,
      from: Hash,
      maxBlocks: Int = SyncService.MaxBlocksPerRequest,
      onDone: () => Unit = () => (),
      ascending: Boolean = false,
      announced: Boolean = false
  ): Unit =
    if held then
      onDone()
      return
    val request = new FetchQueue.Request(conn, from, maxBlocks, onDone, ascending, announced)
    val dropped = fetches.offer(request)
    dropped.foreach(_.onDone())
    if dropped.forall(_ ne request) then schedulePump()

  /** Fetches waiting to be sent, and those out with a peer or being imported. */
  private val fetches = new FetchQueue[PeerConnection](
    maxInFlight = SyncService.MaxConcurrentFetches,
    maxPerPeer = SyncService.MaxPendingFetchesPerPeer,
    maxPending = SyncService.MaxPendingFetches,
    maxAlternates = SyncService.MaxAlternatePeers,
    isOpen = _.isOpen,
    samePeer = (a, b) => (a eq b) || java.util.Arrays.equals(a.peerKey, b.peerKey),
    order = requests => scores.leastFaultedFirst(requests)(_.peer.peerKey)
  )
  private val pumpScheduled = new java.util.concurrent.atomic.AtomicBoolean(false)

  /** Fetches waiting to be sent. */
  private[node] def pendingFetchCount: Int = fetches.size

  private val answers = new java.util.concurrent.atomic.LongAdder
  private val syncedBlocks = new java.util.concurrent.atomic.LongAdder
  private val fetchNanos = new java.util.concurrent.atomic.LongAdder
  private val importNanos = new java.util.concurrent.atomic.LongAdder

  def stats: SyncService.Stats = SyncService.Stats(answers.sum, syncedBlocks.sum, fetchNanos.sum, importNanos.sum)

  private def schedulePump(): Unit =
    if pumpScheduled.compareAndSet(false, true) then
      try
        importExecutor.execute { () =>
          pumpScheduled.set(false)
          pump()
        }
      catch
        case _: java.util.concurrent.RejectedExecutionException =>
          pumpScheduled.set(false) // shutting down

  private def pump(): Unit =
    var next = fetches.next()
    while next.isDefined do
      val request = next.get
      // Nothing a fetch throws may end the loop: no later fetch would ever run.
      try
        // An earlier fetch walking back from a descendant may already have brought this block.
        if request.ascending || !chain.hasBlock(request.from) then fetchExecutor.execute(() => fetchThenImport(request))
        else finish(request, answered = true)
      catch
        case e: Throwable =>
          if !importExecutor.isShutdown then logger.error("block sync failed", e)
          finish(request, answered = true)
      next = fetches.next()

  private def fetch(conn: PeerConnection, from: Hash, maxBlocks: Int, ascending: Boolean, announced: Boolean)
      : Option[List[Array[Byte]]] =
    val asked = System.nanoTime()
    val answer =
      try exchange(conn, from, maxBlocks, ascending, announced)
      catch
        case e: Exception =>
          logger.warn(s"block sync failed: ${e.getMessage}")
          None
    fetchNanos.add(System.nanoTime() - asked)
    if answer.isDefined then answers.increment()
    answer

  private def fetchThenImport(request: FetchQueue.Request[PeerConnection]): Unit =
    val peer = request.peer
    val room = new java.util.concurrent.Semaphore(SyncService.MaxAnswersAhead)
    val continues = new java.util.concurrent.atomic.AtomicBoolean(true)
    def timed(body: => Unit): Unit =
      val importing = System.nanoTime()
      try body
      catch case e: Throwable => logger.error("block sync failed", e)
      finally importNanos.add(System.nanoTime() - importing)
    try
      var from = request.from
      var answer = fetch(peer, from, request.maxBlocks, request.ascending, request.announced)
      var more = true
      while more do
        val blocks = answer
        val askedFrom = from
        val next =
          if !request.ascending || held || !peer.isOpen || !continues.get then None
          else blocks.flatMap(_.lastOption).flatMap(b => chain.decodeBlock(b).toOption).map(chain.headerHashOf)
        next match
          case Some(last) =>
            room.acquire()
            importExecutor.execute { () =>
              try
                if continues.get then
                  var reached = false
                  timed { reached = importFetched(peer, askedFrom, ascending = true, blocks.get, nextFetched = true) }
                  if !reached then continues.set(false)
              finally room.release()
            }
            from = last
            answer = fetch(peer, last, SyncService.MaxBlocksPerRequest, ascending = true, announced = false)
          case None =>
            more = false
            importExecutor.execute { () =>
              try
                if continues.get then
                  timed(blocks.foreach(importFetched(peer, askedFrom, request.ascending, _, nextFetched = false)): Unit)
              finally
                finish(request, answered = blocks.isDefined)
                pump()
            }
    catch
      case _: java.util.concurrent.RejectedExecutionException | _: InterruptedException =>
        fetches.finished(request, have = false): Unit // shutting down

  private def finish(request: FetchQueue.Request[PeerConnection], answered: Boolean): Unit =
    val released = fetches.finished(request, have = chain.hasBlock(request.from), answered = answered)
    released.foreach(_.onDone())
    request.onDone()

  /** Ask `conn` for the blocks after our best head, when its chain is ahead by more than a descending request
    * from its tip can bridge. Repeats (from [[importFetched]]) while blocks keep arriving.
    */
  private def catchUp(conn: PeerConnection): Unit = catchUpFrom(conn, chain.best.hash)

  /** Ask `conn` for the blocks after `from`: our best head, the last block of the previous answer, or our finalized
    * head when the peer has nothing after our best head (see [[importFetched]]).
    */
  private def catchUpFrom(conn: PeerConnection, from: Hash): Unit =
    if conn.isOpen then requestAndImport(conn, from, SyncService.MaxBlocksPerRequest, ascending = true)

  private def exchange(
      conn: PeerConnection,
      from: Hash,
      maxBlocks: Int,
      ascending: Boolean,
      announced: Boolean = false,
      strict: Boolean = false
  ): Option[List[Array[Byte]]] =
    val sending = telemetry.flatMap(_.sendingBlockRequest(conn.peerKey, from, ascending, maxBlocks.toLong))
    // The response is one message; a peer that keeps sending is not buffered without limit.
    StreamRequest.exchange(
      conn,
      StreamKind.BlockRequest,
      Seq(encodeBlockRequest(from, ascending = ascending, maxBlocks = maxBlocks.toLong)),
      10_000,
      () => 20_000,
      maxMessages = if strict then 1 else SyncService.MaxResponseMessages,
      excessFails = strict,
      onSent = () => telemetry.foreach(_.blockRequestSent(sending))
    ) match
      case Left(failure) =>
        logger.warn(s"block request failed: ${failure.message}")
        telemetry.foreach(_.blockRequestFailed(sending, failure.message))
        failure match
          case StreamRequest.Failure.TimedOut  => misbehaved(conn, Offence.Unanswered)
          case StreamRequest.Failure.Excess(_) => misbehaved(conn, Offence.Malformed)
          case _                               => ()
        None
      case Right(messages) =>
        var malformed = false
        val blocks = messages.flatMap { m =>
          chain.splitBlocks(m) match
            case Right(bs) => bs
            case Left(err) =>
              logger.warn(s"CE128 response undecodable: $err")
              misbehaved(conn, Offence.Malformed)
              malformed = true
              Nil
        }
        if announced && blocks.isEmpty && !malformed then misbehaved(conn, Offence.Unanswered)
        telemetry.foreach { t =>
          if malformed then t.blockRequestFailed(sending, "undecodable response")
          else if blocks.isEmpty then t.blockRequestFailed(sending, "no block in the response")
          else reportTransferred(t, sending, blocks)
        }
        Some(blocks)

  private def importFetched(
      conn: PeerConnection,
      from: Hash,
      ascending: Boolean,
      blocks: List[Array[Byte]],
      nextFetched: Boolean
  ): Boolean =
    // Oldest first; skip blocks until one extends our best head.
    var imported = 0
    var stored = 0 // new to us, kept on a side branch
    var unknownParent = false
    val ordered = if ascending then blocks else blocks.reverse
    val watch = (blockBytes: Array[Byte], run: () => Either[ChainError, ChainHead]) =>
      val watched = observer.importing(blockBytes)
      val outcome = run()
      observer.imported(watched, outcome)
      outcome
    var remaining = ordered
    while remaining.nonEmpty do
      val outcomes = chain.importBatch(remaining, Some(conn.peerPublicKey), SyncService.BatchBudgetNanos, watch)
      val (done, rest) = remaining.splitAt(outcomes.size)
      remaining = rest
      done.lazyZip(outcomes).foreach { (blockBytes, outcome) =>
        outcome match
          case Right(_) =>
            imported += 1
            chain.decodeBlock(blockBytes).foreach { b =>
              announce(chain.headerCodec.encode(b.header))
            }
          case Left(err) =>
            if err.isInstanceOf[ChainError.UnknownParent] then unknownParent = true
            err match
              case _: ChainError.InvalidBlock | ChainError.PreviouslyInvalid | ChainError.BodyMismatch =>
                misbehaved(conn, Offence.InvalidBlock)
              case _ => ()
            if err.isInstanceOf[ChainError.StoredOnSideBranch] then stored += 1
            logger.debug(s"sync import skipped: $err")
      }
    if imported > 0 then
      syncedBlocks.add(imported.toLong)
      logger.info(s"synced $imported block(s); best=${chain.best.slot}")
    // The peer's blocks do not reach back to our head (a descending request), or more may follow (an
    // ascending one that brought some): go on from our head.
    val best = chain.best.hash
    val finalized = chain.finalized.hash
    val advanced = ascending && imported + stored > 0
    val last =
      if advanced then ordered.lastOption.flatMap(b => chain.decodeBlock(b).toOption).map(chain.headerHashOf).filter(chain.hasBlock)
      else None
    if !ascending && imported == 0 && unknownParent then catchUp(conn)
    else if advanced then
      // More may follow. Go on from the last block of this answer: after our head when it extended our
      // chain, and along the peer's branch when the blocks are a fork we hold only as a side branch so far.
      if !(nextFetched && last.isDefined) then catchUpFrom(conn, last.getOrElse(best))
    else if ascending && from == best && best != finalized then
      // Nothing after our best head: the peer may not have it at all (we are on a fork it never saw, or
      // one it cannot finalize, which CE 128 must not serve). Every chain shares the finalized block.
      logger.info("catch-up: the peer has nothing after our head; asking from our finalized block")
      catchUpFrom(conn, finalized)
    else if ascending && blocks.isEmpty then logger.info("catch-up: the peer returned no block after our head")
    last.isDefined

  // =========================================================================
  // CE 129 — state request (trie range)
  // =========================================================================
  def stateRequestHandler: io.forge.jam.network.PeerStreamHandler =
    (conn: PeerConnection, stream: MessageStream) =>
      var asked = false
      stream.onMessage { msg =>
        // One request per stream; walking up to 16 MiB of the trie is not event-loop work.
        if asked then stream.reset()
        else
          asked = true
          decodeStateRequest(msg) match
            case Left(err) =>
              logger.warn(s"CE129 request decode failed: $err")
              stream.reset()
            case Right((headerHash, start, end, maxSize)) =>
              val taken = serving.submit(conn, msg.length.toLong) {
                try
                  chain.stateAt(headerHash) match
                    case None =>
                      logger.debug(s"CE129: state request for unknown header ${headerHash.toHex.take(18)}")
                      stream.reset()
                    case Some(trie) =>
                      val cap = math.min(maxSize, StateRequestMaxSizeCap).toInt
                      val (boundary, pairs) = trie.range(start, end, cap)
                      stream.send(encodeBoundaryNodes(boundary))
                      stream.send(encodeStatePairs(pairs))
                      stream.finish()
                catch
                  case e: io.forge.jam.core.trie.MissingTrieNodeException =>
                    logger.debug(s"CE129: state of ${headerHash.toHex.take(18)} is no longer held whole (${e.getMessage})")
                    stream.reset()
                  case e: Exception =>
                    logger.warn(s"CE129 handler failed: ${e.getMessage}")
                    stream.reset()
              }
              if !taken then stream.reset()
      }

object SyncService:
  final case class Stats(answers: Long, blocks: Long, fetchNanos: Long, importNanos: Long)
  object Stats:
    val zero: Stats = Stats(0L, 0L, 0L, 0L)


  trait ImportObserver:
    def importing(blockBytes: Array[Byte]): AnyRef | Null
    def imported(handle: AnyRef | Null, outcome: Either[ChainError, ChainHead]): Unit

  object ImportObserver:
    val none: ImportObserver = new ImportObserver:
      def importing(blockBytes: Array[Byte]): AnyRef | Null = null
      def imported(handle: AnyRef | Null, outcome: Either[ChainError, ChainHead]): Unit = ()
  /** Block fetches that may wait for the import thread: per peer (the newest are kept) and in total. */
  val MaxPendingFetchesPerPeer: Int = 8
  val MaxPendingFetches: Int = 2048

  val MaxHandshakeLeaves: Int = 4096

  val MaxWaitingAnnouncements: Int = 16

  /** The most blocks one CE 128 request asks for. */
  val MaxBlocksPerRequest: Int = 64

  val BatchBudgetNanos: Long = 1_000_000_000L

  val MaxAnswersAhead: Int = 2

  val MaxConcurrentFetches: Int = 4

  /** Other peers remembered as fallbacks for a block already being fetched. */
  val MaxAlternatePeers: Int = 3

  /** Messages kept from one CE 128 response (the protocol sends one). */
  val MaxResponseMessages: Int = 4
