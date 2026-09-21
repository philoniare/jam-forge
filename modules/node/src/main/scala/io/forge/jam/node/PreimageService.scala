package io.forge.jam.node

import java.util.concurrent.{
  ExecutorService,
  LinkedBlockingQueue,
  TimeUnit
}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes, LittleEndian}
import io.forge.jam.core.primitives.{Hash, ServiceId}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.extrinsic.Preimage
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamHandler, StreamKind}
import io.forge.jam.protocol.accumulation.StateKey

/** CE 142 wire format (jamnp-s): Service Index (u32 LE) ++ Hash (32) ++
  * Length (u32 LE) = 40 bytes. CE 143 request/response are unframed: the
  * request is the bare 32-byte hash, the response is the preimage blob (or
  * no message + FIN when not held).
  */
object PreimageCodec:
  val MessageLength: Int = 40

  def encodeAnnouncement(serviceId: Long, hash: Hash, length: Long): Array[Byte] =
    LittleEndian.encode(serviceId, 4) ++ hash.bytes ++ LittleEndian.encode(length, 4)

  def decodeAnnouncement(bytes: Array[Byte]): Either[String, (Long, Hash, Long)] =
    if bytes.length != MessageLength then
      Left(s"CE142: expected $MessageLength bytes, got ${bytes.length}")
    else
      val serviceId = LittleEndian.get(bytes, 0, 4)
      val hash = Hash(java.util.Arrays.copyOfRange(bytes, 4, 36))
      val length = LittleEndian.get(bytes, 36, 4)
      Right((serviceId, hash, length))

final class PreimageService(
    chain: ChainManager,
    pools: ExtrinsicPools,
    egress: ExecutorService,
    announcedMetaCap: Int = PreimageService.DefaultAnnouncedMetaCap
) extends LazyLogging:

  private val announcedMeta =
    java.util.Collections.synchronizedMap(
      new java.util.LinkedHashMap[Hash, (Long, Long)](64, 0.75f, false) {
        override def removeEldestEntry(e: java.util.Map.Entry[Hash, (Long, Long)]) =
          size > announcedMetaCap
      }
    )

  private val fetchExecutor: ExecutorService =
    java.util.concurrent.Executors.newFixedThreadPool(
      4,
      (r: Runnable) => {
        val t = new Thread(r, "jam-preimage-fetch"); t.setDaemon(true); t
      }
    )

  def announceHandler: StreamHandler = new StreamHandler:
    def onStream(connection: JamnpConnection, stream: JamnpStream): Unit =
      stream.onMessage { msg =>
        try
          PreimageCodec.decodeAnnouncement(msg) match
            case Left(err) => logger.warn(err)
            case Right((serviceId, hash, length)) =>
              announcedMeta.put(hash, (serviceId, length))
              val view = chain.stateView()
              val infoKey =
                StateKey.computePreimageInfoStateKey(serviceId, length.toInt, JamBytes(hash.bytes))
              val solicited = view.storage.readTrie(infoKey).exists(StateKey.isUnprovidedRequest)
              if solicited then requestPreimage(connection, serviceId, hash, length)
        catch case e: Exception => logger.warn(s"CE142 handler failed: ${e.getMessage}")
        stream.finish()
      }

  def onImported(@annotation.unused head: ChainManager#Head, block: Block): Unit =
    block.extrinsic.preimages.foreach { p =>
      val hash = Hashing.blake2b256(p.blob.toArray)
      announcedMeta.put(hash, (p.requester.value.toLong, p.blob.length.toLong))
    }

  def requestHandler: StreamHandler = new StreamHandler:
    def onStream(connection: JamnpConnection, stream: JamnpStream): Unit =
      stream.onMessage { msg =>
        try
          if msg.length != 32 then logger.warn(s"CE143: expected 32-byte hash, got ${msg.length}")
          else
            val hash = Hash(msg)
            lookupBlob(hash) match
              case Some(blob) => stream.send(blob)
              case None       => logger.info(s"CE143: preimage ${hash.toHex.take(18)} not held")
        catch case e: Exception => logger.warn(s"CE143 handler failed: ${e.getMessage}")
        stream.finish()
      }

  private[node] def announcedMetaSize: Int = announcedMeta.size()
  private[node] def hasAnnouncedMeta(hash: Hash): Boolean = announcedMeta.containsKey(hash)

  private[node] def lookupBlob(hash: Hash): Option[Array[Byte]] =
    pools.findPreimage(hash).map(_.blob.toArray).orElse {
      Option(announcedMeta.get(hash)).flatMap { case (serviceId, _) =>
        val key = StateKey.computeServiceDataStateKey(serviceId, 0xfffffffeL, JamBytes(hash.bytes))
        chain.readRawState(key).map(_.toArray)
      }
    }

  def announce(serviceId: Long, blob: Array[Byte], peers: Iterable[JamnpConnection]): Unit =
    val hash = Hashing.blake2b256(blob)
    val length = blob.length.toLong
    announcedMeta.put(hash, (serviceId, length))
    val payload = PreimageCodec.encodeAnnouncement(serviceId, hash, length)
    peers.foreach { conn =>
      submitSend(
        () => conn.openStream(StreamKind.PreimageAnnouncement),
        StreamKind.PreimageAnnouncement,
        payload
      )
    }

  private def requestPreimage(
      conn: JamnpConnection,
      serviceId: Long,
      hash: Hash,
      expectedLength: Long
  ): Unit =
    try
      fetchExecutor.submit(new Runnable {
        def run(): Unit =
          try
            val stream = conn.openStream(StreamKind.PreimageRequest).get(10, TimeUnit.SECONDS)
            val collected = new LinkedBlockingQueue[Array[Byte]]()
            stream.onMessage(collected.put)
            stream.send(hash.bytes)
            stream.finish()
            val blob = collected.poll(10, TimeUnit.SECONDS)
            if blob != null && blob.length.toLong == expectedLength
              && Hashing.blake2b256(blob) == hash
            then
              pools.addPreimage(Preimage(requester = ServiceId(serviceId.toInt), blob = JamBytes(blob)))
              announcedMeta.put(hash, (serviceId, expectedLength))
            else logger.warn(s"CE143 response invalid for ${hash.toHex.take(18)}")
          catch case e: Exception => logger.warn(s"CE143 fetch failed: ${e.getMessage}")
      })
    catch
      case _: java.util.concurrent.RejectedExecutionException =>
        logger.warn(s"preimage-fetch executor rejected CE143 fetch for ${hash.toHex.take(18)} (shutting down)")

  def shutdown(): Unit =
    fetchExecutor.shutdown()
    fetchExecutor.awaitTermination(10, TimeUnit.SECONDS)

  private[node] def submitSend(
      open: () => java.util.concurrent.CompletableFuture[JamnpStream],
      kind: Byte,
      payload: Array[Byte]
  ): Unit =
    try
      egress.submit(new Runnable {
        def run(): Unit =
          try
            val stream = open().get(10, TimeUnit.SECONDS)
            stream.send(payload)
            stream.finish()
          catch
            case e: Exception =>
              logger.warn(s"preimage announcement on ${StreamKind.name(kind)} failed: ${e.getMessage}")
      })
    catch
      case _: java.util.concurrent.RejectedExecutionException =>
        logger.warn(s"egress rejected ${StreamKind.name(kind)} send (shutting down)")

object PreimageService:
  val DefaultAnnouncedMetaCap: Int = 4096
