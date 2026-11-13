package io.forge.jam.node

import java.util.concurrent.{ConcurrentHashMap, Executors, LinkedBlockingQueue, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.trie.TrieNode
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamKind}

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

  def decodeHandshake(bytes: Array[Byte]): (HashSlot, List[HashSlot]) =
    val finalHead = decodeHashSlot(bytes, 0)
    val (count, consumed) = JamCodecs.decodeCompactInteger(bytes, 36)
    var offset = 36 + consumed
    val leaves = (0L until count).map { _ =>
      val l = decodeHashSlot(bytes, offset)
      offset += 36
      l
    }.toList
    (finalHead, leaves)

  /** Announcement: Header ++ Final. The header is carried as its encoded
    * bytes; the trailing 36 bytes are the announcer's latest finalized block.
    */
  def encodeAnnouncement(headerBytes: Array[Byte], finalHead: HashSlot): Array[Byte] =
    headerBytes ++ encodeHashSlot(finalHead)

  def decodeAnnouncement(bytes: Array[Byte]): (Array[Byte], HashSlot) =
    val headerBytes = java.util.Arrays.copyOfRange(bytes, 0, bytes.length - 36)
    (headerBytes, decodeHashSlot(bytes, bytes.length - 36))

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
      val key = JamBytes(java.util.Arrays.copyOfRange(bytes, offset, offset + 31))
      offset += 31
      val (len, consumed) = JamCodecs.decodeCompactInteger(bytes, offset)
      offset += consumed
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
    serving: InboundWork = InboundWork.direct
) extends LazyLogging:
  import SyncCodec.*

  /** UP 0 streams by peer connection, for broadcasting announcements. */
  private val announceStreams = ConcurrentHashMap.newKeySet[JamnpStream]()

  private val peerFinals = new ConcurrentHashMap[JamnpConnection, HashSlot]()

  /** The latest `Final` announced by the peer on `conn`, if any handshake or
    * announcement has been received from it yet.
    */
  def peerFinal(conn: JamnpConnection): Option[HashSlot] = Option(peerFinals.get(conn))

  /** Each connected peer's best head as last seen: the newest leaf of its
    * UP 0 handshake, then every block it announces.
    */
  private val peerBestHeads = new ConcurrentHashMap[JamnpConnection, HashSlot]()

  /** Best heads of the peers with a live UP 0 stream, keyed by peer key so a
    * peer reached over two connections counts once.
    */
  def peerBests: Iterable[Hash] =
    peerBestHeads.asScala.toList
      .filter(_._1.isOpen)
      .groupBy(e => JamBytes(e._1.peerKey))
      .values
      .map(_.maxBy(_._2.slot)._2.hash)

  /** Finalized blocks announced by peers with a live UP 0 stream, one per peer key (the newest). */
  def peerFinalized: List[(JamnpConnection, HashSlot)] =
    peerFinals.asScala.toList
      .filter(_._1.isOpen)
      .groupBy(e => JamBytes(e._1.peerKey))
      .values
      .map(_.maxBy(_._2.slot))
      .toList

  /** Keep to the chain `needed` peers announce as finalized, fetching the agreed block when we lack it. */
  def followPeerFinality(needed: Int): Unit =
    val finals = peerFinalized
    chain.followPeerFinality(finals.map((_, f) => (f.hash, f.slot)), needed) match
      case Left(missing) =>
        if fetching.add(missing) then
          finals.find(_._2.hash == missing).foreach((conn, _) => requestAndImport(conn, missing, onDone = () => fetching.remove(missing)))
      case Right(_) => ()

  private val fetching = ConcurrentHashMap.newKeySet[Hash]()

  /** Fetch block `hash` and its missing ancestors from `conn`, once at a time per block. */
  def fetchBlock(conn: JamnpConnection, hash: Hash): Unit =
    if fetching.add(hash) then requestAndImport(conn, hash, onDone = () => fetching.remove(hash))

  /** Drop what is held for a connection that closed. Its UP 0 stream's close listener normally does this; a message
    * handled while the stream closes can put an entry back, which nothing else would remove.
    */
  def forgetConnection(conn: JamnpConnection): Unit =
    upStreams.remove(conn)
    peerFinals.remove(conn)
    peerBestHeads.remove(conn)
    ()

  private def notePeerBest(conn: JamnpConnection, head: HashSlot): Unit =
    peerBestHeads.merge(conn, head, (old, nu) => if nu.slot >= old.slot then nu else old)
    ()

  private val StateRequestMaxSizeCap: Long = 16L * 1024 * 1024

  /** Cap on blocks per CE 128 response, whatever the requested maximum. */
  private val MaxBlocksPerResponse: Long = 256L
  private val MaxBlockResponseBytes: Long = 16L * 1024 * 1024

  /** Single-threaded import pipeline so block application stays sequential. */
  private val importExecutor = Executors.newSingleThreadExecutor { r =>
    val t = new Thread(r, "jam-sync-import")
    t.setDaemon(true)
    t
  }

  /** Stop the import pipeline and wait for an in-flight import to finish —
    * must run before the chain's storage closes (a late import reading a
    * closed RocksDB segfaults in JNI).
    */
  def shutdown(): Unit =
    importExecutor.shutdownNow() // interrupts response waits
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
  private val upStreams = new ConcurrentHashMap[JamnpConnection, JamnpStream]()

  /** Handler for peer-opened UP 0 streams (we are the acceptor). */
  def blockAnnouncementHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) => attachAnnouncementStream(conn, stream, "accepted")

  /** Open UP 0 to a peer we connected to (we are the initiator). */
  def openAnnouncementStream(conn: JamnpConnection): JamnpStream =
    val stream = conn
      .openStream(StreamKind.BlockAnnouncement)
      .get(10, TimeUnit.SECONDS)
    attachAnnouncementStream(conn, stream, "dialed")
    stream

  /** True when `conn` has an open UP 0 stream. */
  def hasAnnouncementStream(conn: JamnpConnection): Boolean =
    Option(upStreams.get(conn)).exists(_.isOpen)

  /** Close the UP 0 stream on `conn`, if any (e.g. the peer stopped being a
    * grid neighbour at an epoch change).
    */
  def closeAnnouncementStream(conn: JamnpConnection): Unit =
    Option(upStreams.get(conn)).foreach(_.close())

  /** Both sides send their handshake as soon as the stream is open (JAMNP-S
    * UP 0). A UP kind has one stream per connection: when a second one
    * shows up, the one with the greatest QUIC stream ID is kept and the
    * others reset (JAMNP-S "Protocols and streams").
    */
  private def attachAnnouncementStream(conn: JamnpConnection, stream: JamnpStream, side: String): Unit =
    var handshaken = false
    val previous = upStreams.merge(conn, stream, (old, nu) => if old.isOpen && old.id > nu.id then old else nu)
    if previous ne stream then
      logger.debug(s"UP0: duplicate stream ${stream.id} on one connection; keeping ${previous.id}")
      stream.reset()
      return
    announceStreams.forEach(s => if s.ne(stream) && streamConnection.get(s) == conn then dropDuplicate(s))
    streamConnection.put(stream, conn)
    announceStreams.add(stream)
    stream.onClosed { () =>
      announceStreams.remove(stream)
      streamConnection.remove(stream)
      if upStreams.remove(conn, stream) then
        peerFinals.remove(conn)
        peerBestHeads.remove(conn)
    }
    stream.onMessage { msg =>
      if !handshaken then
        handshaken = true
        val (peerFinal, peerLeaves) = decodeHandshake(msg)
        peerFinals.put(conn, peerFinal)
        logger.info(
          s"UP0 handshake decoded from $side peer: final=${peerFinal.hash.toHex.take(18)} " +
            s"slot=${peerFinal.slot} leaves=${peerLeaves.size}"
        )
        onPeerLeaves(conn, peerLeaves, peerFinal)
      else onAnnouncement(conn, msg)
    }
    stream.send(ourHandshake)

  /** The connection each UP 0 stream belongs to. */
  private val streamConnection = new ConcurrentHashMap[JamnpStream, JamnpConnection]()

  private def dropDuplicate(old: JamnpStream): Unit =
    logger.debug(s"UP0: stream ${old.id} superseded by a newer stream on the same connection; resetting")
    announceStreams.remove(old)
    streamConnection.remove(old)
    old.reset()

  /** Announce a newly imported/authored block on all UP 0 streams. */
  def announce(headerBytes: Array[Byte]): Unit =
    val msg = encodeAnnouncement(headerBytes, ourFinal)
    announceStreams.forEach(_.send(msg))

  private def onAnnouncement(conn: JamnpConnection, msg: Array[Byte]): Unit =
    val (headerBytes, peerFinal) = decodeAnnouncement(msg)
    peerFinals.put(conn, peerFinal)
    val headerHash = io.forge.jam.core.Hashing.blake2b256(headerBytes)
    val slot =
      if headerBytes.length >= 100 then Some(io.forge.jam.core.LittleEndian.get(headerBytes, 96, 4)) else None
    slot.foreach(s => notePeerBest(conn, HashSlot(headerHash, s)))
    if !chain.hasBlock(headerHash) then
      // The header starts with its parent's hash: when we hold the parent, the announced block is all we lack.
      val parentKnown = headerBytes.length >= 32 && chain.hasBlock(Hash(java.util.Arrays.copyOfRange(headerBytes, 0, 32)))
      val wanted = if parentKnown then 1 else blocksWanted(slot)
      if wanted > 0 then
        logger.debug(s"announced block ${headerHash.toHex.take(18)} unknown; requesting $wanted block(s)")
        requestAndImport(conn, headerHash, wanted)

  private def onPeerLeaves(conn: JamnpConnection, leaves: List[HashSlot], peerFinal: HashSlot): Unit =
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
  def blockRequestHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      var asked = false
      stream.onMessage { msg =>
        // One request per stream; reading up to 256 blocks from the store is not event-loop work.
        if asked then stream.reset()
        else
          asked = true
          decodeBlockRequest(msg) match
            case Left(err) =>
              logger.warn(s"CE128: $err")
              stream.reset()
            case Right((from, ascending, maxBlocks)) =>
              val taken = serving.submit(conn, msg.length.toLong) {
                try
                  if !chain.hasBlock(from) || !chain.canBeFinalized(from) then stream.reset()
                  else
                    val max = math.min(maxBlocks, MaxBlocksPerResponse).toInt
                    val blocks =
                      if ascending then chain.blocksAscending(from, max)
                      else chain.blocksDescending(from, max)
                    // Keep the one message under the peer's frame cap.
                    val out = new java.io.ByteArrayOutputStream()
                    blocks.iterator
                      .takeWhile(b => out.size == 0 || out.size + b.length <= MaxBlockResponseBytes)
                      .foreach(b => out.write(b))
                    stream.send(out.toByteArray)
                    stream.finish()
                catch
                  case e: Exception =>
                    logger.warn(s"CE128 handler failed: ${e.getMessage}")
                    stream.reset()
              }
              // Too many requests waiting (from this peer, or in all): refused.
              if !taken then stream.reset()
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
    * and import them oldest-first. Runs on the import executor.
    */
  private def requestAndImport(
      conn: JamnpConnection,
      from: Hash,
      maxBlocks: Int = SyncService.MaxBlocksPerRequest,
      onDone: () => Unit = () => (),
      ascending: Boolean = false
  ): Unit =
    val request = new FetchRequest(conn, from, maxBlocks, onDone, ascending)
    // A peer can announce faster than we fetch (each fetch waits on it for up to 30 s), so the waiting fetches
    // are bounded: one per (peer, block), a few per peer, and when over a limit the oldest gives way. A fetch
    // walks back from its block, so the newest one covers what an older one for the same chain would have brought.
    val dropped = pendingFetches.synchronized {
      val it = pendingFetches.iterator()
      var samePeer = 0
      var duplicate = false
      // The same block wanted from another peer: one fetch brings it, so this peer is only kept as a fallback.
      var elsewhere = inFlight == from
      while it.hasNext do
        val p = it.next()
        if p.from == from then
          if p.conn eq conn then duplicate = true else elsewhere = true
        if p.conn eq conn then samePeer += 1
      if duplicate then List(request)
      else if elsewhere then
        val others = alternates.computeIfAbsent(from, _ => new java.util.ArrayDeque[FetchRequest]())
        if others.size < SyncService.MaxAlternatePeers && !others.stream().anyMatch(_.conn eq conn) then
          others.addLast(request)
          Nil
        else List(request)
      else
        pendingFetches.addLast(request)
        val out = List.newBuilder[FetchRequest]
        if samePeer + 1 > SyncService.MaxPendingFetchesPerPeer then
          val oldest = pendingFetches.iterator()
          var removed = false
          while !removed && oldest.hasNext do
            val p = oldest.next()
            if p.conn eq conn then
              oldest.remove()
              out += p
              removed = true
        if pendingFetches.size > SyncService.MaxPendingFetches then out += pendingFetches.pollFirst()
        val gone = out.result()
        // A fetch that gives way takes its fallbacks with it, unless another fetch for the block still waits.
        gone.flatMap { p =>
          val stillWanted = pendingFetches.stream().anyMatch(_.from == p.from) || inFlight == p.from
          val others = if stillWanted then null else alternates.remove(p.from)
          if others == null then List(p) else p :: others.asScala.toList
        }
    }
    dropped.foreach(_.onDone())
    if dropped.forall(_ ne request) && fetchLoopScheduled.compareAndSet(false, true) then
      try importExecutor.execute(() => fetchLoop())
      catch
        case _: java.util.concurrent.RejectedExecutionException =>
          fetchLoopScheduled.set(false) // shutting down

  private final class FetchRequest(
      val conn: JamnpConnection,
      val from: Hash,
      val maxBlocks: Int,
      val onDone: () => Unit,
      val ascending: Boolean = false
  )

  /** Fetches waiting for the import thread, oldest first. Guarded by itself. */
  private val pendingFetches = new java.util.ArrayDeque[FetchRequest]()

  /** The block the import thread is fetching now, if any. Guarded by `pendingFetches`. */
  private var inFlight: Hash = null

  /** Other peers that announced a block already waiting or being fetched, tried in turn if that fetch does not
    * bring the block. Entries exist only while a fetch for the block waits or runs. Guarded by `pendingFetches`.
    */
  private val alternates = new java.util.HashMap[Hash, java.util.ArrayDeque[FetchRequest]]()
  private val fetchLoopScheduled = new java.util.concurrent.atomic.AtomicBoolean(false)

  /** Fetches waiting for the import thread. */
  private[node] def pendingFetchCount: Int = pendingFetches.synchronized(pendingFetches.size)

  /** Runs the waiting fetches in order on the import thread until none is left. */
  private def fetchLoop(): Unit =
    var continue = true
    while continue do
      val next = pendingFetches.synchronized {
        val n = pendingFetches.pollFirst()
        inFlight = if n == null then null else n.from
        n
      }
      if next != null && !importExecutor.isShutdown then
        // Nothing a fetch throws may end the loop: no later fetch would ever run.
        try
          // An earlier fetch walking back from a descendant may already have brought this block.
          try if next.ascending || !chain.hasBlock(next.from) then fetch(next.conn, next.from, next.maxBlocks, next.ascending)
          finally
            val released = pendingFetches.synchronized {
              inFlight = null
              val others = alternates.get(next.from)
              val have = chain.hasBlock(next.from)
              val waiting = pendingFetches.stream().anyMatch(_.from == next.from)
              if others == null then Nil
              else if have then
                alternates.remove(next.from)
                others.asScala.toList
              else if waiting then Nil
              else
                // The fetch did not bring the block: ask the next peer that announced it, ahead of other fetches.
                var fallback = others.pollFirst()
                val skipped = List.newBuilder[FetchRequest]
                while fallback != null && !fallback.conn.isOpen do
                  skipped += fallback
                  fallback = others.pollFirst()
                if fallback == null || others.isEmpty then alternates.remove(next.from)
                if fallback != null then pendingFetches.addFirst(fallback)
                skipped.result()
            }
            released.foreach(_.onDone())
            next.onDone()
        catch case e: Throwable => logger.error("block sync failed", e)
      else
        fetchLoopScheduled.set(false)
        // A request added after the poll saw the loop as still scheduled: pick it up.
        continue = !importExecutor.isShutdown && pendingFetches.synchronized(!pendingFetches.isEmpty) &&
          fetchLoopScheduled.compareAndSet(false, true)

  /** Ask `conn` for the blocks after our best head, when its chain is ahead by more than a descending request
    * from its tip can bridge. Repeats (from [[fetch]]) while blocks keep arriving.
    */
  private def catchUp(conn: JamnpConnection): Unit = catchUpFrom(conn, chain.best.hash)

  /** Ask `conn` for the blocks after `from`: our best head, the last block of the previous answer, or our finalized
    * head when the peer has nothing after our best head (see [[fetch]]).
    */
  private def catchUpFrom(conn: JamnpConnection, from: Hash): Unit =
    if conn.isOpen then requestAndImport(conn, from, SyncService.MaxBlocksPerRequest, ascending = true)

  private def fetch(conn: JamnpConnection, from: Hash, maxBlocks: Int, ascending: Boolean): Unit =
        try
          val stream = conn
            .openStream(StreamKind.BlockRequest)
            .get(10, TimeUnit.SECONDS)
          // The response is one message; a peer that keeps sending is not buffered without limit.
          val received = new LinkedBlockingQueue[Array[Byte]](SyncService.MaxResponseMessages)
          val done = new LinkedBlockingQueue[java.lang.Boolean]()
          stream.onMessage(received.offer(_))
          stream.onClosed(() => done.offer(true))
          stream.send(encodeBlockRequest(from, ascending = ascending, maxBlocks = maxBlocks.toLong))
          stream.finish()

          if done.poll(20, TimeUnit.SECONDS) == null then
            logger.warn("block request timed out")
            stream.reset() // give the stream up on both sides
          else
            val blocks = mutable.ListBuffer.empty[Array[Byte]]
            var next = received.poll()
            while next != null do
              chain.splitBlocks(next) match
                case Right(bs) => blocks ++= bs
                case Left(err) => logger.warn(s"CE128 response undecodable: $err")
              next = received.poll()
            // Oldest first; skip blocks until one extends our best head.
            var imported = 0
            var stored = 0 // new to us, kept on a side branch
            var unknownParent = false
            val ordered = if ascending then blocks.toList else blocks.reverse.toList
            ordered.foreach { blockBytes =>
              chain.importBlock(blockBytes, Some(conn.peerPublicKey)) match
                case Right(_) =>
                  imported += 1
                  chain.decodeBlock(blockBytes).foreach { b =>
                    announce(chain.headerCodec.encode(b.header))
                  }
                case Left(err) =>
                  if err.isInstanceOf[ChainError.UnknownParent] then unknownParent = true
                  if err.isInstanceOf[ChainError.StoredOnSideBranch] then stored += 1
                  logger.debug(s"sync import skipped: $err")
            }
            if imported > 0 then
              logger.info(s"synced $imported block(s); best=${chain.best.slot}")
            // The peer's blocks do not reach back to our head (a descending request), or more may follow (an
            // ascending one that brought some): go on from our head.
            val best = chain.best.hash
            val finalized = chain.finalized.hash
            if !ascending && imported == 0 && unknownParent then catchUp(conn)
            else if ascending && imported + stored > 0 then
              // More may follow. Go on from the last block of this answer: after our head when it extended our
              // chain, and along the peer's branch when the blocks are a fork we hold only as a side branch so far.
              val last = ordered.lastOption.flatMap(b => chain.decodeBlock(b).toOption).map(chain.headerHashOf)
              catchUpFrom(conn, last.filter(chain.hasBlock).getOrElse(best))
            else if ascending && from == best && best != finalized then
              // Nothing after our best head: the peer may not have it at all (we are on a fork it never saw, or
              // one it cannot finalize, which CE 128 must not serve). Every chain shares the finalized block.
              logger.info("catch-up: the peer has nothing after our head; asking from our finalized block")
              catchUpFrom(conn, finalized)
            else if ascending && blocks.isEmpty then logger.info("catch-up: the peer returned no block after our head")
        catch
          case e: Exception =>
            logger.warn(s"block sync failed: ${e.getMessage}")

  // =========================================================================
  // CE 129 — state request (trie range)
  // =========================================================================
  def stateRequestHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
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
                  case e: Exception =>
                    logger.warn(s"CE129 handler failed: ${e.getMessage}")
                    stream.reset()
              }
              if !taken then stream.reset()
      }

object SyncService:
  /** Block fetches that may wait for the import thread: per peer (the newest are kept) and in total. */
  val MaxPendingFetchesPerPeer: Int = 8
  val MaxPendingFetches: Int = 2048

  /** The most blocks one CE 128 request asks for. */
  val MaxBlocksPerRequest: Int = 64

  /** Other peers remembered as fallbacks for a block already being fetched. */
  val MaxAlternatePeers: Int = 3

  /** Messages kept from one CE 128 response (the protocol sends one). */
  val MaxResponseMessages: Int = 4
