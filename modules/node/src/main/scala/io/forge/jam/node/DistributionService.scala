package io.forge.jam.node

import java.util.concurrent.ExecutorService

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes, constants}
import io.forge.jam.core.primitives.{Ed25519Signature, Hash, ValidatorIndex}
import io.forge.jam.crypto.Ed25519
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, GuaranteeExtrinsic}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamHandler, StreamKind}
import io.forge.jam.core.codec.Codec

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
  * @param admitGuarantee the receipt-time check of a CE 135 guarantee (see [[GuaranteeAdmission]]); a guarantee it
  *   refuses is never pooled. Without one, every received guarantee is refused.
  */
final class DistributionService(
    pools: ExtrinsicPools,
    coresCount: Int,
    egress: ExecutorService,
    assurerKeys: () => Seq[Array[Byte]] = () => Nil,
    admitGuarantee: GuaranteeExtrinsic => Either[String, Unit] = _ => Left("no guarantee verifier configured"),
    /** Where received guarantees and assurances are checked (signatures, state reads): off the network threads. */
    inbound: InboundWork = InboundWork.direct
) extends LazyLogging:

  // Open connections only: a connection leaves on its connection-down callback ([[untrackConnection]]), so the set
  // is bounded by the transport's connection limit however often peers reconnect.
  private val connections =
    java.util.concurrent.ConcurrentHashMap.newKeySet[JamnpConnection]()

  /** Track an open peer connection for outgoing distribution. A connection that already closed is not kept: its
    * connection-down callback has fired, so nothing would remove it.
    */
  def trackConnection(conn: JamnpConnection): Unit =
    if conn.isOpen then
      connections.add(conn)
      // Closed between the check and the add: the down callback may have run before the add.
      if !conn.isOpen then connections.remove(conn): Unit

  /** Forget a connection that closed. */
  def untrackConnection(conn: JamnpConnection): Unit =
    connections.remove(conn): Unit

  /** Currently tracked open peer connections. */
  def peers: List[JamnpConnection] =
    connections.asScala.filter(_.isOpen).toList

  /** Connections held, open or not: equals the open ones once the close callbacks have run. */
  private[node] def trackedConnectionCount: Int = connections.size

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
            val admitted = inbound.submit(conn, msg.length.toLong) {
              val taken =
                try receiveGuarantee(g)
                catch case e: Exception => Left(String.valueOf(e.getMessage))
              taken match
                case Right(_) => stream.finish()
                case Left(why) =>
                  logger.debug(s"CE135: guarantee for core ${g.report.coreIndex.toInt} refused: $why")
                  stream.reset()
            }
            if !admitted then stream.reset()
          case Left(err) =>
            logger.warn(s"bad CE135 payload: $err")
            stream.reset()
      }

  /** Pool a guarantee received from a peer. A report already pooled is kept as it is and costs no signature check;
    * anything else must pass `admitGuarantee` first. Right(true) when it was newly pooled.
    */
  private[node] def receiveGuarantee(g: GuaranteeExtrinsic): Either[String, Boolean] =
    if pools.hasGuarantee(g) then Right(false)
    else
      admitGuarantee(g).map { _ =>
        logger.debug(s"pooled guarantee for core ${g.report.coreIndex.toInt}")
        pools.addGuarantee(g)
        recordReport(g.report)
        true
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
            val admitted = inbound.submit(conn, msg.length.toLong) {
              val signer =
                try assurerIndex(conn, anchor, bitfield, signature)
                catch case _: Exception => None
              signer match
                case Some(idx) =>
                  logger.debug(s"pooled assurance from validator $idx")
                  pools.addAssurance(AssuranceExtrinsic(anchor, bitfield, ValidatorIndex(idx), signature))
                  stream.finish()
                case None =>
                  logger.warn("CE141: assurance signed by no active validator")
                  stream.reset()
            }
            if !admitted then stream.reset()
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
    submitSend(() => conn.openStream(kind), kind, payload, conn)

  private[node] def submitSend(
      open: () => java.util.concurrent.CompletableFuture[JamnpStream],
      kind: Byte,
      payload: Array[Byte],
      peer: AnyRef = null
  ): Unit =
    OneShotSend.submit(
      egress,
      peer,
      open,
      List(payload),
      why => logger.warn(s"distribution on ${StreamKind.name(kind)} failed: $why"),
      () => logger.warn(s"egress rejected ${StreamKind.name(kind)} send (shutting down)")
    )

  /** Request/response exchanges wait on a peer for up to half a minute each (10 s to open, 20 s for the answer).
    * They run here, a few at a time, and not on the egress thread, where one silent peer would hold up every
    * assurance, ticket, vote and judgment behind it. When more wait than the queue holds, the oldest gives way: its
    * caller asks again on its own schedule.
    */
  private val requests = new java.util.concurrent.ThreadPoolExecutor(
    DistributionService.RequestThreads,
    DistributionService.RequestThreads,
    30L,
    java.util.concurrent.TimeUnit.SECONDS,
    new java.util.concurrent.ArrayBlockingQueue[Runnable](DistributionService.MaxQueuedRequests),
    { r =>
      val t = new Thread(r, "jam-request"); t.setDaemon(true); t
    },
    new java.util.concurrent.ThreadPoolExecutor.DiscardOldestPolicy()
  )
  requests.allowCoreThreadTimeOut(true)

  /** Requests waiting for a request thread. */
  private[node] def queuedRequests: Int = requests.getQueue.size

  /** Run a request/response exchange off the egress thread. */
  private[node] def submitRequest(task: () => Unit): Unit =
    try
      requests.execute(() =>
        try task()
        catch case e: Exception => logger.warn(s"request failed: ${e.getMessage}")
      )
    catch case _: java.util.concurrent.RejectedExecutionException => ()

  def shutdown(): Unit =
    requests.shutdownNow()
    ()

  private def decodeItem[A](bytes: Array[Byte])(using codec: Codec[A]): Either[String, A] =
    codec.decodeAt(bytes, 0) match
      case Right((value, used)) if used == bytes.length => Right(value)
      case Right(_) => Left("trailing bytes")
      case Left(err)  => Left(err)

object DistributionService:

  /** Request/response exchanges in flight at once, and how many more may wait. */
  val RequestThreads: Int = 4
  val MaxQueuedRequests: Int = 64

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
