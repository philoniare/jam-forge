package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, ExecutorService, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes, constants}
import io.forge.jam.core.primitives.{Ed25519Signature, Hash, ValidatorIndex}
import io.forge.jam.crypto.Ed25519
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, GuaranteeExtrinsic, Preimage}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamHandler, StreamKind}
import io.forge.jam.core.codec.Codec

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Distribution protocols feeding the extrinsic pools:
  *   - CE 135 work-report distribution (guaranteed work-report → authors)
  *   - CE 136 work-report request, served from a bounded known-report cache
  *   - CE 141 assurance distribution (assurer → authors)
  *   - CE 142/143 preimage announcement/request live in [[PreimageService]]
  *
  * CE 135 carries the graypaper guarantee encoding; CE 141 carries
  * `Anchor ++ Bitfield ++ Signature`, the assurance without its validator
  * index (JAMNP-S CE 141), which the receiver recovers from the signer.
  *
  * @param assurerKeys the Ed25519 keys assurances are checked against (the
  *   active set κ, by validator index)
  */
final class DistributionService(
    pools: ExtrinsicPools,
    coresCount: Int,
    egress: ExecutorService,
    assurerKeys: () => Seq[Array[Byte]] = () => Nil
) extends LazyLogging:

  private val connections =
    java.util.concurrent.ConcurrentHashMap.newKeySet[JamnpConnection]()

  /** Track a peer connection for outgoing distribution. */
  def trackConnection(conn: JamnpConnection): Unit =
    connections.add(conn)

  /** Currently tracked open peer connections. */
  def peers: List[JamnpConnection] =
    connections.asScala.filter(_.isOpen).toList

  /** Distinct peers: one reached over two connections counts once. */
  def peerCount: Int = peers.map(c => JamBytes(c.peerKey)).distinct.size

  // =========================================================================
  // CE 135 — guaranteed work-report distribution
  // =========================================================================

  def workReportHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        decodeItem[GuaranteeExtrinsic](msg) match
          case Right(g) =>
            logger.debug(s"pooled guarantee for core ${g.report.coreIndex.toInt}")
            pools.addGuarantee(g)
            recordReport(g.report)
            stream.finish()
          case Left(err) =>
            logger.warn(s"bad CE135 payload: $err")
            stream.reset()
      }

  /** Send a signed guarantee to a peer (typically the upcoming author). */
  def distributeGuarantee(conn: JamnpConnection, g: GuaranteeExtrinsic): Unit =
    sendItem(conn, StreamKind.WorkReportDistribution, g.encode.toArray)

  def distributeGuaranteeToAll(g: GuaranteeExtrinsic): Unit =
    connections.forEach(c => if c.isOpen then distributeGuarantee(c, g))

  // =========================================================================
  // CE 136 — work-report request
  // =========================================================================
  private val knownReports =
    java.util.Collections.synchronizedMap(
      new java.util.LinkedHashMap[Hash, WorkReport](64, 0.75f, false) {
        override def removeEldestEntry(e: java.util.Map.Entry[Hash, WorkReport]) = size > 256
      }
    )

  /** Record a report in the known-report cache, keyed by its blake2b-256
    * hash, so it can later be served over CE 136.
    */
  def recordReport(report: WorkReport): Unit =
    knownReports.put(Hashing.blake2b256(report.encode.toArray), report)

  /** CE 136: Work-Report Hash (32) -> Work-Report; reset when unknown. */
  def reportRequestHandler: StreamHandler = new StreamHandler:
    def onStream(connection: JamnpConnection, stream: JamnpStream): Unit =
      stream.onMessage { msg =>
        val report =
          if msg.length == 32 then Option(knownReports.get(Hash(msg)))
          else None
        report match
          case Some(r) =>
            stream.send(r.encode.toArray)
            stream.finish()
          case None => stream.reset()
      }

  // =========================================================================
  // CE 141 — assurance distribution
  // =========================================================================

  def assuranceHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        DistributionService.decodeAssurance(msg, coresCount) match
          case Right((anchor, bitfield, signature)) =>
            assurerIndex(conn, anchor, bitfield, signature) match
              case Some(idx) =>
                logger.debug(s"pooled assurance from validator $idx")
                pools.addAssurance(AssuranceExtrinsic(anchor, bitfield, ValidatorIndex(idx), signature))
                stream.finish()
              case None =>
                logger.warn("CE141: assurance signed by no active validator")
                stream.reset()
          case Left(err) =>
            logger.warn(s"bad CE141 payload: $err")
            stream.reset()
      }

  /** The assurer's validator index: the connection's peer key when it is an
    * active validator whose signature verifies (JAMNP-S expects validators to
    * use their Ed25519 key as network identity), else any active key the
    * signature verifies under, for devnet nodes that hold several keys.
    */
  private def assurerIndex(
      conn: JamnpConnection,
      anchor: Hash,
      bitfield: JamBytes,
      signature: Ed25519Signature
  ): Option[Int] =
    val keys = assurerKeys().toIndexedSeq
    val message = constants.JAM_AVAILABLE_BYTES ++ Hashing.blake2b256(anchor.bytes ++ bitfield.toArray).bytes
    def verifies(i: Int) = Ed25519.verify(keys(i), message, signature.bytes)
    val byPeer = keys.indices.find(i => java.util.Arrays.equals(keys(i), conn.peerKey))
    byPeer.filter(verifies).orElse(keys.indices.find(verifies))

  def distributeAssurance(conn: JamnpConnection, a: AssuranceExtrinsic): Unit =
    sendItem(conn, StreamKind.AssuranceDistribution, DistributionService.encodeAssurance(a))

  def distributeAssuranceToAll(a: AssuranceExtrinsic): Unit =
    connections.forEach(c => if c.isOpen then distributeAssurance(c, a))

  // =========================================================================
  // helpers
  // =========================================================================

  private def sendItem(conn: JamnpConnection, kind: Byte, payload: Array[Byte]): Unit =
    submitSend(() => conn.openStream(kind), kind, payload)

  private[node] def submitSend(
      open: () => CompletableFuture[JamnpStream],
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
              logger.warn(s"distribution on ${StreamKind.name(kind)} failed: ${e.getMessage}")
      })
    catch
      case _: java.util.concurrent.RejectedExecutionException =>
        logger.warn(s"egress rejected ${StreamKind.name(kind)} send (shutting down)")

  /** Run a request/response exchange on the egress pool. */
  private[node] def submitRequest(task: () => Unit): Unit =
    try
      egress.submit(new Runnable {
        def run(): Unit =
          try task()
          catch case e: Exception => logger.warn(s"request failed: ${e.getMessage}")
      })
    catch case _: java.util.concurrent.RejectedExecutionException => ()

  private def decodeItem[A](bytes: Array[Byte])(using codec: Codec[A]): Either[String, A] =
    codec.decodeAt(bytes, 0) match
      case Right((value, used)) if used == bytes.length => Right(value)
      case Right(_) => Left("trailing bytes")
      case Left(err)  => Left(err)

object DistributionService:

  /** CE 141 message: `Anchor(32) ++ Bitfield(ceil(C/8)) ++ Signature(64)`. */
  def encodeAssurance(a: AssuranceExtrinsic): Array[Byte] =
    a.anchor.bytes ++ a.bitfield.toArray ++ a.signature.bytes

  def decodeAssurance(bytes: Array[Byte], coresCount: Int): Either[String, (Hash, JamBytes, Ed25519Signature)] =
    val bitfieldSize = (coresCount + 7) / 8
    if bytes.length != 32 + bitfieldSize + 64 then
      Left(s"expected ${32 + bitfieldSize + 64} bytes, got ${bytes.length}")
    else
      Right(
        (
          Hash(java.util.Arrays.copyOfRange(bytes, 0, 32)),
          JamBytes(java.util.Arrays.copyOfRange(bytes, 32, 32 + bitfieldSize)),
          Ed25519Signature(java.util.Arrays.copyOfRange(bytes, 32 + bitfieldSize, bytes.length))
        )
      )
