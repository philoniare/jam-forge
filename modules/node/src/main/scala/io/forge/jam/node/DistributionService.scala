package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, ExecutorService, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, GuaranteeExtrinsic, Preimage}
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamKind}
import scodec.Codec
import scodec.bits.ByteVector

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Distribution protocols feeding the extrinsic pools:
  *   - CE 135 work-report distribution (guaranteed work-report → authors)
  *   - CE 141 assurance distribution (assurer → authors)
  *   - CE 142/143 preimage announcement/request live in [[PreimageService]]
  *
  * Wire content uses the corresponding graypaper extrinsic-item encodings.
  */
final class DistributionService(
    pools: ExtrinsicPools,
    coresCount: Int,
    egress: ExecutorService
) extends LazyLogging:

  private val connections =
    java.util.concurrent.ConcurrentHashMap.newKeySet[JamnpConnection]()

  /** Track a peer connection for outgoing distribution. */
  def trackConnection(conn: JamnpConnection): Unit =
    connections.add(conn)

  /** Currently tracked open peer connections. */
  def peers: List[JamnpConnection] =
    connections.asScala.filter(_.isOpen).toList

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
            stream.finish()
          case Left(err) =>
            logger.warn(s"bad CE135 payload: $err")
            stream.close()
      }

  /** Send a signed guarantee to a peer (typically the upcoming author). */
  def distributeGuarantee(conn: JamnpConnection, g: GuaranteeExtrinsic): Unit =
    sendItem(conn, StreamKind.WorkReportDistribution, g.encode.toArray)

  def distributeGuaranteeToAll(g: GuaranteeExtrinsic): Unit =
    connections.forEach(c => if c.isOpen then distributeGuarantee(c, g))

  // =========================================================================
  // CE 141 — assurance distribution
  // =========================================================================

  def assuranceHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        decodeItem[AssuranceExtrinsic](msg)(using
          AssuranceExtrinsic.codec(coresCount)
        ) match
          case Right(a) =>
            logger.debug(s"pooled assurance from validator ${a.validatorIndex.value}")
            pools.addAssurance(a)
            stream.finish()
          case Left(err) =>
            logger.warn(s"bad CE141 payload: $err")
            stream.close()
      }

  def distributeAssurance(conn: JamnpConnection, a: AssuranceExtrinsic): Unit =
    sendItem(
      conn,
      StreamKind.AssuranceDistribution,
      AssuranceExtrinsic.codec(coresCount).encode(a).require.toByteArray
    )

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

  private def decodeItem[A](bytes: Array[Byte])(using codec: Codec[A]): Either[String, A] =
    codec.decode(ByteVector(bytes).bits) match
      case scodec.Attempt.Successful(res) if res.remainder.isEmpty => Right(res.value)
      case scodec.Attempt.Successful(_) => Left("trailing bytes")
      case scodec.Attempt.Failure(err)  => Left(err.message)
