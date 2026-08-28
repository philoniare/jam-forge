package io.forge.jam.node

import java.util.concurrent.{ConcurrentHashMap, Executors, LinkedBlockingQueue, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamKind}

import scala.collection.mutable

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

  def decodeBlockRequest(bytes: Array[Byte]): (Hash, Boolean, Long) =
    (
      Hash(java.util.Arrays.copyOfRange(bytes, 0, 32)),
      bytes(32) == 0,
      decodeU32(bytes, 33)
    )

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
final class SyncService(chain: ChainManager) extends LazyLogging:
  import SyncCodec.*

  /** UP 0 streams by peer connection, for broadcasting announcements. */
  private val announceStreams = ConcurrentHashMap.newKeySet[JamnpStream]()

  /** Single-threaded import pipeline so block application stays sequential. */
  private val importExecutor = Executors.newSingleThreadExecutor { r =>
    val t = new Thread(r, "jam-sync-import")
    t.setDaemon(true)
    t
  }

  private def ourFinal: HashSlot =
    val f = chain.finalized
    HashSlot(f.hash, f.slot)

  private def ourHandshake: Array[Byte] =
    encodeHandshake(ourFinal, chain.leaves.map(h => HashSlot(h.hash, h.slot)))

  // =========================================================================
  // UP 0 — block announcement
  // =========================================================================

  /** Handler for peer-opened UP 0 streams (we are the acceptor). */
  def blockAnnouncementHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      var handshaken = false
      announceStreams.add(stream)
      stream.onClosed(() => announceStreams.remove(stream))
      stream.onMessage { msg =>
        if !handshaken then
          handshaken = true
          val (peerFinal, peerLeaves) = decodeHandshake(msg)
          stream.send(ourHandshake)
          onPeerLeaves(conn, peerLeaves)
        else onAnnouncement(conn, msg)
      }

  /** Open UP 0 to a peer we connected to (we are the initiator). */
  def openAnnouncementStream(conn: JamnpConnection): JamnpStream =
    val stream = conn
      .openStream(StreamKind.BlockAnnouncement)
      .get(10, TimeUnit.SECONDS)
    var handshaken = false
    announceStreams.add(stream)
    stream.onClosed(() => announceStreams.remove(stream))
    stream.onMessage { msg =>
      if !handshaken then
        handshaken = true
        val (peerFinal, peerLeaves) = decodeHandshake(msg)
        onPeerLeaves(conn, peerLeaves)
      else onAnnouncement(conn, msg)
    }
    stream.send(ourHandshake)
    stream

  /** Announce a newly imported/authored block on all UP 0 streams. */
  def announce(headerBytes: Array[Byte]): Unit =
    val msg = encodeAnnouncement(headerBytes, ourFinal)
    announceStreams.forEach(_.send(msg))

  private def onAnnouncement(conn: JamnpConnection, msg: Array[Byte]): Unit =
    val (headerBytes, _) = decodeAnnouncement(msg)
    val headerHash = io.forge.jam.core.Hashing.blake2b256(headerBytes)
    if !chain.hasBlock(headerHash) then
      logger.debug(s"announced block ${headerHash.toHex.take(18)} unknown; requesting")
      requestAndImport(conn, headerHash)

  private def onPeerLeaves(conn: JamnpConnection, leaves: List[HashSlot]): Unit =
    leaves.foreach { leaf =>
      if !chain.hasBlock(leaf.hash) then
        logger.debug(s"peer leaf ${leaf.hash.toHex.take(18)} unknown; requesting")
        requestAndImport(conn, leaf.hash)
    }

  // =========================================================================
  // CE 128 — block request
  // =========================================================================

  /** Handler serving peer block requests. */
  def blockRequestHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        val (from, ascending, maxBlocks) = decodeBlockRequest(msg)
        val max = math.min(maxBlocks, 256L).toInt
        val blocks =
          if ascending then chain.blocksAscending(from, max)
          else chain.blocksDescending(from, max)
        blocks.foreach(stream.send)
        stream.finish()
      }

  /** Fetch blocks descending (inclusive) from `from` on a fresh CE 128 stream
    * and import them oldest-first. Runs on the import executor.
    */
  private def requestAndImport(conn: JamnpConnection, from: Hash): Unit =
    importExecutor.submit(new Runnable {
      override def run(): Unit =
        try
          val stream = conn
            .openStream(StreamKind.BlockRequest)
            .get(10, TimeUnit.SECONDS)
          val received = new LinkedBlockingQueue[Array[Byte]]()
          val done = new LinkedBlockingQueue[java.lang.Boolean]()
          stream.onMessage(received.offer(_))
          stream.onClosed(() => done.offer(true))
          stream.send(encodeBlockRequest(from, ascending = false, maxBlocks = 64))
          stream.finish()

          if done.poll(20, TimeUnit.SECONDS) == null then
            logger.warn("block request timed out")
          else
            val blocks = mutable.ListBuffer.empty[Array[Byte]]
            var next = received.poll()
            while next != null do
              blocks += next
              next = received.poll()
            // Oldest first; skip blocks until one extends our best head.
            var imported = 0
            blocks.reverse.foreach { blockBytes =>
              chain.importBlock(blockBytes) match
                case Right(head) =>
                  imported += 1
                  chain.decodeBlock(blockBytes).foreach { b =>
                    announce(b.header.encode.toArray)
                  }
                case Left(err) =>
                  logger.debug(s"sync import skipped: $err")
            }
            if imported > 0 then
              logger.info(s"synced $imported block(s); best=${chain.best.slot}")
        catch
          case e: Exception =>
            logger.warn(s"block sync failed: ${e.getMessage}")
    })
    ()
