package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes, LittleEndian, constants}
import io.forge.jam.core.primitives.{Ed25519PublicKey, Ed25519Signature, Hash, Timeslot, ValidatorIndex}
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.types.dispute.GuaranteeSignature
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.crypto.{Ed25519, Ed25519ZebraWrapper}
import io.forge.jam.network.{PeerConnection, MessageStream}
import io.forge.jam.protocol.state.StateKey
import io.forge.jam.protocol.refine.{
  ComputedReport,
  ComputeReport,
  DaShards,
  HistoricalLookupService,
  ImportResolver,
  RefineFetch,
  WorkPackageBundle,
  WorkPackageValidator
}
import io.forge.jam.core.codec.Codec
import spire.math.UInt

import scala.collection.mutable
import scala.util.boundary, boundary.break

/** Historical preimage lookup Λ over the state at the work-package's lookup anchor:
  * the posterior state of the lookup-anchor block, found by its state root (the context's `lookupAnchorStateRoot`),
  * so every validator reads the same accounts whatever its best head. When that state is not stored (a node that
  * synced past it) the best state is read instead. Either way a blob is returned only when its request history
  * `a_l[(h, |p|)]` shows it available at the anchor timeslot t (`I(l, t)`).
  */
final class StateHistoricalLookup(chain: ChainReader, anchorStateRoot: Option[Hash] = None)
    extends HistoricalLookupService:

  private lazy val anchorState = anchorStateRoot.flatMap(chain.stateAtRoot)

  /** The lookup for a work-package whose lookup anchor has posterior state root `root`. */
  def forAnchor(root: Hash): StateHistoricalLookup = new StateHistoricalLookup(chain, Some(root))

  /** Whether the lookup anchor's state is held. When it is not, reads fall back to the head's state, which can give a
    * different refine result: callers check this first and refuse.
    */
  def anchorAvailable: Boolean =
    // A zero root is the newest history entry's placeholder, never a real state (though the empty trie "exists").
    anchorStateRoot.isEmpty || (!anchorStateRoot.contains(Hash.zero) && anchorState.isDefined)

  def serviceExists(serviceId: Long): Boolean =
    readRaw(StateKey.computeServiceAccountKey(serviceId)).isDefined

  private val answered: Option[ByteBoundedCache[(Long, Long, Hash), Option[Array[Byte]]]] =
    anchorStateRoot.map(_ =>
      new ByteBoundedCache(StateHistoricalLookup.CachedLookups, StateHistoricalLookup.CachedLookupBytes, _.fold(0L)(_.length.toLong))
    )

  def historicalLookup(
      serviceId: Long,
      lookupAnchorTimeslot: Long,
      hash: Hash
  ): Option[Array[Byte]] =
    val key = (serviceId, lookupAnchorTimeslot, hash)
    answered.flatMap(_.get(key)) match
      case Some(known) => known
      case None =>
        val found = readPreimage(serviceId, lookupAnchorTimeslot, hash)
        if !lost then answered.foreach(_.put(key, found))
        found

  private def readPreimage(serviceId: Long, lookupAnchorTimeslot: Long, hash: Hash): Option[Array[Byte]] =
    val h = JamBytes(hash.bytes.toArray)
    readRaw(StateKey.computeServiceDataStateKey(serviceId, 0xfffffffeL, h)).flatMap { blob =>
      val history = readRaw(StateKey.computePreimageInfoStateKey(serviceId, blob.length.toLong, h))
        .flatMap(v => scala.util.Try(StateKey.decodePreimageInfoValue(v)).toOption)
      if history.exists(StateHistoricalLookup.availableAt(_, lookupAnchorTimeslot)) then Some(blob.toArray)
      else None
    }

  @volatile private var lost = false

  def anchorLost: Boolean = lost

  def unlessLost[E, A](computed: Either[E, A]): Either[E | String, A] =
    computed.flatMap(c => if lost then Left("lookup anchor state not held") else Right(c))

  private def readRaw(stateKey: JamBytes): Option[JamBytes] =
    anchorState match
      case Some(trie) =>
        try trie.read(stateKey)
        catch
          case _: io.forge.jam.core.trie.MissingTrieNodeException =>
            lost = true
            None
      case None => chain.readRawState(stateKey)

object StateHistoricalLookup:

  val CachedLookups: Int = 256
  val CachedLookupBytes: Long = 64L << 20

  /** `I(l, t)`: whether request history `l` shows the preimage available at timeslot `t`. */
  def availableAt(l: List[Long], t: Long): Boolean =
    l match
      case List(x)       => x <= t
      case List(x, y)    => x <= t && t < y
      case List(x, y, z) => (x <= t && t < y) || z <= t
      case _             => false

/** The messages of one inbound stream, kept up to a count and a byte total, so that what a peer can make us hold
  * per stream is fixed however much it sends. Not thread safe: a stream's messages arrive in order on its event loop.
  */
private[node] final class StreamIntake(
    maxMessages: Int,
    maxBytes: Long,
    hold: Long => Boolean = _ => true,
    letGo: Long => Unit = _ => ()
):
  private val kept = mutable.ListBuffer.empty[Array[Byte]]
  private var total = 0L
  private var over = false
  private val held = new java.util.concurrent.atomic.AtomicLong()

  def release(): Unit =
    val bytes = held.getAndSet(0L)
    if bytes > 0 then letGo(bytes)

  /** Keep `msg`. False once the stream has sent more than it may, and from then on: nothing further is kept. */
  def offer(msg: Array[Byte]): Boolean =
    if over || kept.size >= maxMessages || total + msg.length > maxBytes || !hold(msg.length.toLong) then
      over = true
      false
    else
      kept += msg
      total += msg.length
      held.addAndGet(msg.length.toLong)
      true

  def size: Int = kept.size
  def bytes: Long = total
  def messages: List[Array[Byte]] = kept.toList

/** Guarantor role: accepts work-package submissions (CE 133), computes the
  * work report via the in-core pipeline (is-authorized + refine), signs it
  * with this node's assigned validator keys, tops the credentials up to the
  * required ≥ 2 via CE 134 co-signing with peer guarantors, and distributes
  * the guaranteed report via CE 135.
  */
final class GuarantorService(
    chain: ChainReader,
    distribution: DistributionService,
    pools: ExtrinsicPools,
    validatorKeys: Seq[ValidatorKeySet],
    shardStore: Option[io.forge.jam.db.ShardStore] = None,
    segments: SegmentPool = new SegmentPool(),
    /** CE 146 cumulative-message cap; overridable so tests can exercise the
      * breach path cheaply. See [[GuarantorService.DefaultMaxSubmissionBytes]].
      */
    maxSubmissionBytes: Long = GuarantorService.DefaultMaxSubmissionBytes,
    /** This node's network (TLS) Ed25519 key, preferred when signing CE 134. */
    ownNetworkKey: Option[Array[Byte]] = None,
    /** Where refining runs, shared with the auditor; None: a pool of this service's own. */
    sharedRefinePool: Option[RefinePool] = None,
    /** Rebuilds imported segments from the availability layer when no guarantor serves them. */
    segmentRecovery: Option[SegmentRecovery] = None,
    /** FAULT INJECTION, for testing disputes only: sign a deliberately wrong report (see [[tamper]]). */
    tamperReports: Boolean = false,
    /** A bundle rebuilt from the shards in our own custody, by erasure root: CE 147 once the bundle cache let it go. */
    localBundle: Hash => Option[Array[Byte]] = _ => None,
    /** Where a CE 147/148 request that has to rebuild from shards is answered, off the network threads. */
    serving: InboundWork = InboundWork.direct,
    fromValidator: PeerConnection => Boolean = _ => true,
    /** CE 134: whether the peer is a guarantor of the core it shares a package for ([[GuarantorService.guarantorsOf]]). */
    fromCoGuarantor: (PeerConnection, Int) => Boolean = (_, _) => true,
    exportersFirst: (Hash, List[PeerConnection]) => List[PeerConnection] = GuarantorIndex.unordered,
    /** Owner of this service's threads: the node's, or its own when made alone. */
    threads: NodeThreads = new NodeThreads,
    misbehaved: (PeerConnection, Offence) => Unit = (_, _) => (),
    dialValidators: Seq[io.forge.jam.core.types.epoch.ValidatorKey] => List[PeerConnection] = _ => Nil,
    telemetry: Option[Telemetry] = None
) extends LazyLogging:

  private type Ev = Option[Telemetry.EventRef]

  private def received(ev: Ev, coreIndex: Int, wp: WorkPackage): Unit =
    telemetry.foreach(_.workPackageReceived(ev, coreIndex, Telemetry.workPackageOutline(wp, wp.encoded.length, RefineFetch.workPackageHash(wp))))

  private def failed(ev: Ev, why: String): Unit = telemetry.foreach(_.workPackageFailed(ev, why))

  private def localSubmission(ce146: Boolean): Ev =
    telemetry.flatMap(_.workPackageSubmission(ownNetworkKey.getOrElse(new Array[Byte](32)), ce146))

  /** The report this guarantor signs: the computed one, or with `tamperReports` a wrong one (auth gas used + 1),
    * the same way every time so that two faulty co-guarantors agree on it. Only guaranteeing is affected: this
    * node's auditor still judges reports honestly.
    */
  private def tamper(computed: ComputedReport): ComputedReport =
    if !tamperReports then computed
    else
      logger.warn(s"FAULT INJECTION: signing a wrong report for package ${computed.report.packageSpec.hash.toHex.take(18)}")
      computed.copy(report =
        computed.report.copy(authGasUsed = io.forge.jam.core.primitives.Gas(computed.report.authGasUsed.toLong + 1))
      )

  private val computeReport = new ComputeReport(chain.config)
  private val accounts = new StateHistoricalLookup(chain)

  /** Refining runs off the netty event loops on the refine pool: a co-guarantor's CE 134 bundle ahead of a new
    * submission, since another guarantor is waiting on it. Waiting for co-signatures does not hold a refine thread.
    */
  private val refinePool = sharedRefinePool.getOrElse(new RefinePool("jam-guarantor"))

  /** Refine takes seconds and each queued submission holds its messages (up to 48 MiB for CE 146, 32 MiB for a
    * CE 134 bundle), so only a few may wait; one over the limit is rejected by resetting its stream.
    */
  private val admission =
    new WorkGate("guarantor intake", GuarantorService.MaxQueuedSubmissions, GuarantorService.MaxQueuedSubmissionBytes)

  /** CE 134 bundles to co-sign have their own limit, so a flood of submissions cannot crowd out co-signing. */
  private val coSignAdmission =
    new WorkGate("co-sign intake", GuarantorService.MaxQueuedSubmissions, GuarantorService.MaxQueuedSubmissionBytes)

  /** Submissions waiting for a refine thread. */
  private[node] def queuedSubmissions: Int = admission.queued + coSignAdmission.queued

  private val inFlight = java.util.concurrent.ConcurrentHashMap.newKeySet[Hash]()

  private[node] def packagesInFlight: Int = inFlight.size

  private def refusedAsCopy(packageHash: Hash): Option[String] =
    guaranteedLately(packageHash)
      .map(t => s"package already guaranteed at slot $t")
      .orElse(Option.when(inFlight.contains(packageHash))("package is already on its way to a guarantee here"))

  private val coSigned = new ByteBoundedCache[Hash, (Long, Array[Byte])](256, Long.MaxValue, _ => 0L)

  private def coSignedLately(request: Hash): Option[Array[Byte]] =
    val r = chain.config.rotationPeriod
    coSigned.get(request).filter((t, _) => chain.nextBlockSlot / r <= t / r + 1).map(_._2)

  private val guaranteesMadeCount = new java.util.concurrent.atomic.AtomicLong()

  def guaranteesMade: Long = guaranteesMadeCount.get

  /** Bounded (64 bundles, [[GuarantorService.HeldBundleBytes]]) insertion-evicting cache of full work-package bundles
    * this node holds — either its own refine output or a bundle re-executed
    * while co-signing for a peer guarantor via CE 134 — keyed by the
    * report's erasure root, so a peer's CE 147 request can be served
    * directly instead of falling back to shard reconstruction.
    */
  private val heldBundles = new ByteBoundedCache[Hash, Array[Byte]](64, GuarantorService.HeldBundleBytes, _.length.toLong)

  private val ourGuarantees = new ByteBoundedCache[Hash, java.lang.Long](1024, Long.MaxValue, _ => 0L)

  private def guaranteedLately(packageHash: Hash): Option[Long] =
    val r = chain.config.rotationPeriod
    ourGuarantees.get(packageHash).map(_.longValue).filter(t => chain.nextBlockSlot / r <= t / r + 1)

  /** Record a bundle in the held-bundle cache, keyed by its erasure root, so
    * it can later be served over CE 147.
    */
  def recordBundle(erasureRoot: Hash, bundleBytes: Array[Byte]): Unit =
    heldBundles.put(erasureRoot, bundleBytes)

  /** CE 147: Erasure-Root (32) -> Bundle; reset when we cannot provide it. From the bundle cache, else rebuilt from
    * the shards we custody (every validator's, for a report we guaranteed) and cached again: auditors of one report
    * ask at about the same time.
    */
  def bundleRequestHandler: io.forge.jam.network.PeerStreamHandler = new io.forge.jam.network.PeerStreamHandler:
    def onStream(connection: PeerConnection, stream: MessageStream): Unit =
      val receiving = telemetry.flatMap(_.receivingBundleRequest(connection.peerKey))
      def refuse(why: String): Unit =
        telemetry.foreach(_.bundleRequestFailed(receiving, why))
        stream.reset()
      def serve(b: Array[Byte]): Unit =
        stream.send(b)
        stream.finish()
        telemetry.foreach(_.bundleTransferred(receiving))
      stream.onMessage { msg =>
        if msg.length != 32 then refuse(s"malformed request of ${msg.length} bytes")
        else
          val root = Hash(msg)
          telemetry.foreach(_.bundleRequestReceived(receiving, root))
          heldBundles.get(root) match
            case Some(b) => serve(b)
            case None =>
              val taken = serving.submit(connection, msg.length.toLong) {
                val rebuilt =
                  try localBundle(root)
                  catch
                    case e: Exception =>
                      logger.warn(s"CE147 rebuild of ${root.toHex.take(18)} failed: ${e.getMessage}")
                      None
                rebuilt match
                  case Some(b) =>
                    heldBundles.put(root, b)
                    serve(b)
                  case None => refuse("bundle not held")
              }
              if !taken then refuse("too many requests waiting")
      }

  /** CE 148: repeated `(segments-root(32) ++ compact-len ++ [u16 LE index])`
    * groups; message 1 is every requested segment (`Csegmentsize` bytes
    * each) concatenated in request order, message 2 one import proof
    * (`len++[Hash]`, J_0 in GP 0.8.0 §14) per segment in the same order. An
    * unknown root, an out-of-range index or more than W_M segments rejects
    * the whole request with a reset.
    */
  def segmentRequestHandler: io.forge.jam.network.PeerStreamHandler = new io.forge.jam.network.PeerStreamHandler:
    def onStream(connection: PeerConnection, stream: MessageStream): Unit =
      val receiving = telemetry.flatMap(_.receivingSegmentRequest(connection.peerKey))
      stream.onMessage { msg =>
        val request =
          try decodeSegmentRequest(msg).filter(_.map(_._2.size).sum <= GuarantorService.MaxSegmentsPerRequest)
          catch case _: Exception => None
        request match
          case None =>
            telemetry.foreach(_.segmentRequestFailed(receiving, "malformed request, or too many segments"))
            stream.reset()
          // Every root still in the segment cache: answered here. Otherwise the missing ones are rebuilt from the
          // shards we custody, which is real work: off the network threads.
          case Some(groups) if groups.forall((root, indices) => segments.getWithProofs(root, indices).isDefined) =>
            telemetry.foreach(_.segmentRequestReceived(receiving, groups.map(_._2.size).sum))
            answerSegments(stream, groups, rebuild = false, receiving)
          case Some(groups) =>
            telemetry.foreach(_.segmentRequestReceived(receiving, groups.map(_._2.size).sum))
            if !serving.submit(connection, msg.length.toLong)(answerSegments(stream, groups, rebuild = true, receiving)) then
              telemetry.foreach(_.segmentRequestFailed(receiving, "too many requests waiting"))
              stream.reset()
      }

  /** Answer a CE 148 request: every requested segment from the segment cache, else (with `rebuild`) from our own
    * shards; a reset when any cannot be provided.
    */
  private def answerSegments(
      stream: MessageStream,
      groups: IndexedSeq[(Hash, Seq[Int])],
      rebuild: Boolean,
      receiving: Ev
  ): Unit =
    val served =
      try
        groups.foldLeft(Option(IndexedSeq.empty[(Array[Byte], List[Array[Byte]])])) { (acc, group) =>
          val (root, indices) = group
          acc.flatMap { collected =>
            segments
              .getWithProofs(root, indices)
              .orElse(
                if !rebuild then None
                else
                  segmentRecovery
                    .flatMap(_.recover(root, indices, localOnly = true))
                    .map(byIndex => indices.map(byIndex).toIndexedSeq)
              )
              .map(collected ++ _)
          }
        }
      catch
        case e: Exception =>
          logger.warn(s"CE148 failed: ${e.getMessage}")
          None
    served match
      case Some(found) =>
        val segs = new java.io.ByteArrayOutputStream()
        val proofs = new java.io.ByteArrayOutputStream()
        found.foreach { (seg, proof) =>
          segs.write(seg)
          proofs.write(JamCodecs.encodeCompactInteger(proof.size.toLong))
          proof.foreach(h => proofs.write(h))
        }
        stream.send(segs.toByteArray)
        stream.send(proofs.toByteArray)
        stream.finish()
        telemetry.foreach(_.segmentsTransferred(receiving))
      case None =>
        logger.debug("CE148: request not servable")
        telemetry.foreach(_.segmentRequestFailed(receiving, "a segment asked for is not held"))
        stream.reset()

  /** Decode CE 148's repeated `(root(32) ++ compact-len ++ [u16 LE index])`
    * groups. `None` on any structural malformation (short buffer, bad
    * compact prefix).
    */
  private def decodeSegmentRequest(msg: Array[Byte]): Option[IndexedSeq[(Hash, Seq[Int])]] =
    try
      var offset = 0
      val groups = mutable.ListBuffer.empty[(Hash, Seq[Int])]
      while offset < msg.length do
        if offset + 32 > msg.length then return None
        val root = Hash(java.util.Arrays.copyOfRange(msg, offset, offset + 32))
        offset += 32
        val (n, consumed) = JamCodecs.decodeCompactInteger(msg, offset)
        offset += consumed
        val byteLength = n.toInt * 2
        if n.toInt < 0 || offset + byteLength > msg.length then return None
        val indices = (0 until n.toInt).map { _ =>
          val idx = LittleEndian.get(msg, offset, 2).toInt
          offset += 2
          idx
        }
        groups += (root -> indices)
      Some(groups.toIndexedSeq)
    catch case _: Exception => None

  def shutdown(): Unit =
    importPool.shutdownNow()
    if sharedRefinePool.isEmpty then refinePool.shutdown()
    coSignPool.shutdownNow()
    ()

  private val ed25519ByPublic: Map[Ed25519PublicKey, ValidatorKeySet] =
    validatorKeys.map(k => k.ed25519Key -> k).toMap

  /** CE 133 handler: message 1 = core index (2 bytes LE) ++ work package,
    * message 2 = concatenated extrinsic blobs, then FIN.
    */
  def workPackageSubmissionHandler: io.forge.jam.network.PeerStreamHandler =
    (conn: PeerConnection, stream: MessageStream) =>
      // CE 133 is two messages; a stream sending more, or more bytes than a submission may hold, is dropped.
      val intake = new StreamIntake(2, maxSubmissionBytes, conn.holdInbound, conn.releaseInbound)
      var rejected = false
      val submission = telemetry.flatMap(_.workPackageSubmission(conn.peerKey, ce146 = false))
      stream.onMessage { msg =>
        if !rejected then
          // JAMNP-S CE 133: `Core Index ++ Work-Package` is at most 2 + 200 KiB,
          // so a guarantor never downloads a huge package before authorizing it.
          if intake.size == 0 && msg.length > 2 + GuarantorService.MaxWorkPackageBytes then
            rejected = true
            intake.release()
            logger.warn(s"CE133: work-package message of ${msg.length} bytes exceeds the limit")
            failed(submission, s"work-package message of ${msg.length} bytes exceeds the limit")
            stream.reset()
          else if !intake.offer(msg) then
            rejected = true
            intake.release()
            logger.warn("CE133: more than two messages or over the submission size cap; rejecting")
            failed(submission, "more than two messages, or over the submission size cap")
            stream.reset()
      }
      stream.onClosed { () =>
        if rejected then intake.release()
        else
          val messages = intake.messages
          val admitted = admission.tryExecute(refinePool.at(RefinePool.Priority.Submission), intake.bytes, conn) {
            try
              if messages.nonEmpty then
                val first = messages.head
                val coreIndex = ((first(0) & 0xff) | ((first(1) & 0xff) << 8))
                val wpBytes = java.util.Arrays.copyOfRange(first, 2, first.length)
                decodeWorkPackage(wpBytes) match
                  case Left(err) =>
                    logger.warn(s"CE133: bad work package: $err")
                    failed(submission, s"bad work package: $err")
                  case Right(wp) =>
                    received(submission, coreIndex, wp)
                    val extrinsicBlob = messages.drop(1).headOption.getOrElse(Array.emptyByteArray)
                    onWorkPackage(coreIndex, wp, extrinsicBlob, submission)
              else failed(submission, "no message")
            catch
              case e: Exception =>
                logger.error("CE133 handling failed", e)
                failed(submission, s"failed here: ${e.getMessage}")
          }
          intake.release()
          // `<-- FIN`: the submission was received in full. Reset: we are too busy to take it.
          if admitted then stream.finish()
          else
            failed(submission, "too many submissions waiting")
            stream.reset()
      }

  /** CE 146 handler: the multi-message sibling of CE 133, for builders that
    * hold (or have reconstructed) the full audit bundle rather than just the
    * work package. Five messages then FIN, replying FIN only:
    *
    *   - msg1: `coreIndex(u16 LE) ++ compactLen ++ [wpHash(32) ++ segmentsRoot(32)]`
    *     (the segments-root mappings)
    *   - msg2: work-package bytes
    *   - msg3: concatenated extrinsic data
    *   - msg4: concatenated `Csegmentsize`-byte imported segments
    *   - msg5: import justification paths (`compactLen ++ [hash(32)]` per
    *     import, driven by the work package's declared import counts)
    *
    * msg2..msg5 together are exactly a [[WorkPackageBundle]] encoding split
    * by section, so they're decoded by concatenating and reusing
    * `WorkPackageBundle.decode` rather than re-implementing that parsing.
    * Dispatches on the 5th message, or on stream close with >= 2 messages
    * received (a peer may omit empty trailing messages, as CE 133 permits
    * for its own optional second message). Malformed input is logged and
    * gets FIN with no crash; count consistency between msg4/msg5 and the
    * work package's declared imports is `WorkPackageBundle.decode`'s job,
    * not this handler's.
    *
    * The transport's `FrameDecoder` already caps each individual frame at
    * 32 MiB, but that alone permits ~160 MB (5 x 32 MiB) of buffered,
    * unvalidated bytes per stream before this handler runs any check — and
    * that multiplies by however many CE 146 streams are open concurrently.
    * `maxSubmissionBytes` bounds the running total across all messages seen
    * so far; a breach aborts immediately (logged, stream finished) rather
    * than waiting for the 5th message or stream close. CE 133's two-message
    * accumulation has the same unbounded-buffering shape but is out of
    * scope here — ledgered as a node-level follow-up.
    */
  def bundleSubmissionHandler: io.forge.jam.network.PeerStreamHandler =
    (conn: PeerConnection, stream: MessageStream) =>
      val messages = mutable.ListBuffer.empty[Array[Byte]]
      var dispatched = false
      var totalBytes = 0L
      val held = new java.util.concurrent.atomic.AtomicLong()
      val submission = telemetry.flatMap(_.workPackageSubmission(conn.peerKey, ce146 = true))

      def letGo(): Unit =
        val bytes = held.getAndSet(0L)
        if bytes > 0 then conn.releaseInbound(bytes)

      def dispatch(): Unit =
        if !dispatched then
          dispatched = true
          val snapshot = messages.toIndexedSeq
          val admitted = admission.tryExecute(refinePool.at(RefinePool.Priority.Submission), totalBytes, conn) {
            try
              decodeBundleSubmission(snapshot) match
                case Left(err) =>
                  logger.warn(s"CE146: $err")
                  failed(submission, err)
                case Right((coreIndex, lookup, wp, extrinsicData, importSegments, justifications)) =>
                  received(submission, coreIndex, wp)
                  telemetry.foreach(_.extrinsicDataReceived(submission))
                  verifyBuilderImports(wp, lookup, extrinsicData, importSegments, justifications) match
                    case Left(err) =>
                      logger.warn(s"CE146: imports refused: $err")
                      failed(submission, s"imports refused: $err")
                    case Right(ourLookup) =>
                      telemetry.foreach(_.importsReceived(submission))
                      guaranteeWorkPackage(coreIndex, wp, extrinsicData, importSegments, justifications, ourLookup, None, submission)
            catch
              case e: Exception =>
                logger.error("CE146 handling failed", e)
                failed(submission, s"failed here: ${e.getMessage}")
            finally stream.finish()
          }
          letGo()
          if !admitted then
            failed(submission, "too many submissions waiting")
            stream.reset()

      stream.onMessage { msg =>
        if !dispatched then
          totalBytes += msg.length
          if totalBytes > maxSubmissionBytes then
            dispatched = true // mark the stream dead: no further messages are buffered or dispatched
            letGo()
            logger.warn(
              s"CE146: cumulative submission size $totalBytes exceeds the $maxSubmissionBytes-byte cap; aborting"
            )
            failed(submission, s"over the $maxSubmissionBytes-byte submission cap")
            stream.finish()
          else if messages.size == 1 && msg.length > GuarantorService.MaxWorkPackageBytes then
            // JAMNP-S CE 146: the Work-Package message is at most 200 KiB.
            dispatched = true
            letGo()
            logger.warn(s"CE146: work-package message of ${msg.length} bytes exceeds the limit")
            failed(submission, s"work-package message of ${msg.length} bytes exceeds the limit")
            stream.reset()
          else if !conn.holdInbound(msg.length.toLong) then
            dispatched = true
            letGo()
            logger.warn("CE146: the sender already holds its share of memory with unfinished submissions; rejecting")
            failed(submission, "the sender holds its share of memory with unfinished submissions")
            stream.reset()
          else
            held.addAndGet(msg.length.toLong)
            messages += msg
            if messages.size == 5 then dispatch()
      }
      stream.onClosed { () =>
        if messages.size >= 2 then dispatch()
        else if !dispatched then
          logger.warn(s"CE146: stream closed with only ${messages.size} message(s)")
          failed(submission, s"stream closed with only ${messages.size} message(s)")
          stream.finish()
        letGo()
      }

  /** Parse CE 146's 5-message wire format. Missing trailing messages (index
    * >= `messages.size`) are treated as empty, mirroring CE 133's handling
    * of its optional second message. `None`-shaped errors surface as `Left`
    * for the caller to log; nothing here throws.
    */
  private[node] def decodeBundleSubmission(messages: IndexedSeq[Array[Byte]]): Either[
    String,
    (
        Int,
        Map[Hash, Hash],
        WorkPackage,
        IndexedSeq[IndexedSeq[Array[Byte]]],
        IndexedSeq[IndexedSeq[Array[Byte]]],
        IndexedSeq[IndexedSeq[List[Array[Byte]]]]
    )
  ] =
    try
      if messages.isEmpty then return Left("no messages received")
      val msg1 = messages(0)
      if msg1.length < 2 then return Left("msg1 too short for core index")
      val coreIndex = LittleEndian.get(msg1, 0, 2).toInt
      var offset = 2
      val (n, c) = JamCodecs.decodeCompactInteger(msg1, offset)
      offset += c
      if n.toInt < 0 then return Left("msg1: negative mapping count")
      val lookup = (0 until n.toInt).map { _ =>
        if offset + 64 > msg1.length then throw new RuntimeException("msg1: mapping list truncated")
        val wpHash = Hash(java.util.Arrays.copyOfRange(msg1, offset, offset + 32))
        val segRoot = Hash(java.util.Arrays.copyOfRange(msg1, offset + 32, offset + 64))
        offset += 64
        wpHash -> segRoot
      }.toMap
      if offset != msg1.length then return Left("msg1: trailing bytes after mappings")

      val msg4 = messages.lift(3).getOrElse(Array.emptyByteArray)
      if msg4.length % constants.Csegmentsize.toInt != 0 then
        return Left(s"msg4: length ${msg4.length} is not a multiple of Csegmentsize")

      val msg2 = messages.lift(1).getOrElse(Array.emptyByteArray)
      val msg3 = messages.lift(2).getOrElse(Array.emptyByteArray)
      val msg5 = messages.lift(4).getOrElse(Array.emptyByteArray)
      val bundleBytes = msg2 ++ msg3 ++ msg4 ++ msg5

      WorkPackageBundle.decode(bundleBytes) match
        case Left(err) => Left(s"bundle: $err")
        case Right(bundle) =>
          Right(
            (coreIndex, lookup, bundle.workPackage, bundle.extrinsicData, bundle.importSegments, bundle.justifications)
          )
    catch case e: Exception => Left(s"decode failed: ${e.getMessage}")

  private def decodeWorkPackage(bytes: Array[Byte]): Either[String, WorkPackage] =
    summon[Codec[WorkPackage]].decodeAt(bytes, 0) match
      case Right((value, used)) if used == bytes.length => Right(value)
      case Right(_)                                     => Left("trailing bytes")
      case Left(err)                                    => Left(err)

  /** Split the concatenated extrinsic blob into per-item lists using the
    * work items' declared (hash, len) references.
    */
  private def splitExtrinsics(
      wp: WorkPackage,
      blob: Array[Byte]
  ): Either[String, IndexedSeq[IndexedSeq[Array[Byte]]]] =
    var offset = 0
    boundary:
      val out = wp.items.map { item =>
        item.extrinsic.map { ref =>
          val len = ref.len.toInt
          if offset + len > blob.length then break(Left("extrinsic data truncated"))
          val data = java.util.Arrays.copyOfRange(blob, offset, offset + len)
          offset += len
          if Hashing.blake2b256(data) != ref.hash then break(Left("extrinsic hash mismatch"))
          data
        }.toIndexedSeq
      }.toIndexedSeq
      if offset != blob.length then Left("trailing extrinsic bytes")
      else Right(out)

  def guarantees(coreIndex: Int): Boolean = ownAssignedKeys(chain.nextBlockSlot, coreIndex).nonEmpty

  def submit(coreIndex: Int, packageBytes: Array[Byte], extrinsicBlob: Array[Byte]): Either[String, Unit] =
    if packageBytes.length > GuarantorService.MaxWorkPackageBytes then
      Left(s"work-package of ${packageBytes.length} bytes exceeds ${GuarantorService.MaxWorkPackageBytes}")
    else if packageBytes.length.toLong + extrinsicBlob.length > maxSubmissionBytes then
      Left(s"submission exceeds $maxSubmissionBytes bytes")
    else
      decodeWorkPackage(packageBytes).left.map(e => s"Codec error: $e").flatMap { wp =>
        refusedAsCopy(RefineFetch.workPackageHash(wp)) match
          case Some(why) => Left(why)
          case None => admitSubmission(coreIndex, wp, packageBytes, extrinsicBlob)
      }

  private def admitSubmission(
      coreIndex: Int,
      wp: WorkPackage,
      packageBytes: Array[Byte],
      extrinsicBlob: Array[Byte]
  ): Either[String, Unit] =
    val bytes = packageBytes.length.toLong + extrinsicBlob.length
    val submission = localSubmission(ce146 = false)
    received(submission, coreIndex, wp)
    val admitted = admission.tryExecute(refinePool.at(RefinePool.Priority.Submission), bytes, GuarantorService.Local) {
      try onWorkPackage(coreIndex, wp, extrinsicBlob, submission)
      catch
        case e: Exception =>
          logger.error("local work-package submission failed", e)
          failed(submission, s"failed here: ${e.getMessage}")
    }
    if admitted then Right(())
    else
      failed(submission, "too many submissions waiting")
      Left("too many submissions are waiting to be refined")

  def submitBundle(coreIndex: Int, bundleBytes: Array[Byte]): Either[String, Unit] =
    if bundleBytes.length > maxSubmissionBytes then Left(s"bundle of ${bundleBytes.length} bytes exceeds $maxSubmissionBytes")
    else
      WorkPackageBundle.decode(bundleBytes).left.map(e => s"Codec error: $e").flatMap { b =>
        refusedAsCopy(RefineFetch.workPackageHash(b.workPackage)).toLeft(()).flatMap { _ =>
        val submission = localSubmission(ce146 = true)
        received(submission, coreIndex, b.workPackage)
        telemetry.foreach(_.extrinsicDataReceived(submission))
        val admitted =
          admission.tryExecute(refinePool.at(RefinePool.Priority.Submission), bundleBytes.length.toLong, GuarantorService.Local) {
            try
              verifyBuilderImports(b.workPackage, Map.empty, b.extrinsicData, b.importSegments, b.justifications) match
                case Left(err) =>
                  logger.warn(s"local bundle submission: imports refused: $err")
                  failed(submission, s"imports refused: $err")
                case Right(lookup) =>
                  telemetry.foreach(_.importsReceived(submission))
                  guaranteeWorkPackage(coreIndex, b.workPackage, b.extrinsicData, b.importSegments, b.justifications, lookup, None, submission)
            catch
              case e: Exception =>
                logger.error("local bundle submission failed", e)
                failed(submission, s"failed here: ${e.getMessage}")
          }
        if admitted then Right(())
        else
          failed(submission, "too many submissions waiting")
          Left("too many submissions are waiting to be refined")
        }
      }

  /** Guarantee a submitted work package: refine, sign, pool and distribute.
    * CE 133 has no cross-package imports on hand, so it always calls this
    * with empty imports/justifications and an empty segments-root lookup;
    * CE 146 (`bundleSubmissionHandler`) calls [[guaranteeWorkPackage]]
    * directly with the richer inputs decoded from the builder's bundle.
    */
  def onWorkPackage(coreIndex: Int, wp: WorkPackage, extrinsicBlob: Array[Byte], ev: Ev = None): Unit =
    val checked = splitExtrinsics(wp, extrinsicBlob)
    if checked.isRight then telemetry.foreach(_.extrinsicDataReceived(ev))
    checked match
      case Left(err) =>
        logger.warn(s"work package rejected: $err")
        failed(ev, s"extrinsic data refused: $err")
      case Right(extrinsicData) if wp.items.forall(_.importSegments.isEmpty) =>
        val none = wp.items.map(_ => IndexedSeq.empty).toIndexedSeq
        telemetry.foreach(_.importsReceived(ev))
        guaranteeWorkPackage(coreIndex, wp, extrinsicData, none, none, Map.empty, None, ev)
      case Right(extrinsicData) =>
        // Imports are fetched (and checked) off the refine threads, then the package goes back to be refined.
        onceAnchored(wp, GuarantorService.heldBytes(wp, extrinsicData), RefinePool.Priority.Submission) {
          val resolved = for
            anchored <- precheck(wp, coreIndex, chain.nextBlockSlot)
            auth <- computeReport.authorize(wp, coreIndex, anchored).left.map(e => s"not authorized: $e")
            r <- ImportResolver.resolve(wp, reportedPackages)
          yield (auth, r)
          val name = RefineFetch.workPackageHash(wp).toHex.take(18)
          resolved match
            case Left(err) =>
              logger.warn(s"work package $name refused: $err")
              failed(ev, err.toString)
            case Right((auth, (imports, lookup))) =>
              telemetry.foreach { t =>
                t.authorized(ev, auth.cost)
                lookup.foreach((packageHash, segmentsRoot) => t.workPackageHashMapped(ev, packageHash, segmentsRoot))
              }
              try
                importPool.execute { () =>
                  importFetcher.fetch(imports, ev) match
                    case Left(err) =>
                      logger.warn(s"work package $name refused: $err")
                      failed(ev, err)
                    case Right((segs, proofs)) =>
                      telemetry.foreach(_.importsReceived(ev))
                      val bytes = GuarantorService.heldBytes(wp, extrinsicData) +
                        segs.iterator.flatten.map(_.length.toLong).sum + proofs.iterator.flatten.flatten.map(_.length.toLong).sum
                      val admitted = admission.tryExecute(refinePool.at(RefinePool.Priority.Submission), bytes, GuarantorService.Local) {
                        guaranteeWorkPackage(coreIndex, wp, extrinsicData, segs, proofs, lookup, Some(auth), ev)
                      }
                      if !admitted then
                        logger.warn(s"work package $name refused: too many submissions are waiting to be refined")
                        failed(ev, "too many submissions waiting")
                }
              catch
                case _: java.util.concurrent.RejectedExecutionException =>
                  logger.warn("too many work packages waiting for their imports; refused")
                  failed(ev, "too many work packages waiting for their imports")
        }

  /** Segments-roots of the work-packages reported in recent history (β), by package hash: what a ⊞ import can be
    * resolved through, since the report's segment-root lookup must be checkable on chain.
    */
  private def reportedPackages: Map[Hash, Hash] =
    chain.stateView().beta.history.flatMap(_.reported).map(r => r.hash -> r.exportsRoot).toMap

  /** A builder's CE 146 imports: its segment-root lookup must agree with what recent history says, and each imported
    * segment must verify by its justification. Returns the lookup the report will carry.
    */
  private def verifyBuilderImports(
      wp: WorkPackage,
      builderLookup: Map[Hash, Hash],
      extrinsicData: IndexedSeq[IndexedSeq[Array[Byte]]],
      importSegments: IndexedSeq[IndexedSeq[Array[Byte]]],
      justifications: IndexedSeq[IndexedSeq[List[Array[Byte]]]]
  ): Either[String, Map[Hash, Hash]] =
    for
      resolved <- ImportResolver.resolve(wp, reportedPackages)
      lookup = resolved._2
      _ <- Either.cond(
        builderLookup.forall((h, root) => lookup.get(h).forall(_ == root)),
        (),
        "segment-root lookup disagrees with recent history"
      )
      _ <- ImportResolver.verifyBundle(WorkPackageBundle(wp, extrinsicData, importSegments, justifications), lookup)
    yield lookup

  private val importFetcher = new ImportFetcher(
    segments,
    root => exportersFirst(root, distribution.peers),
    recover = (root, indices, importer) => segmentRecovery.flatMap(_.recover(root, indices, importer = importer)),
    lied = misbehaved(_, Offence.BadProof),
    telemetry = telemetry
  )

  /** Fetches of imported segments: a few at once, a bounded number waiting. */
  private val importPool = threads.own("jam-import-fetch")(factory => new java.util.concurrent.ThreadPoolExecutor(
    GuarantorService.ImportFetchThreads,
    GuarantorService.ImportFetchThreads,
    30L,
    java.util.concurrent.TimeUnit.SECONDS,
    new java.util.concurrent.LinkedBlockingQueue[Runnable](GuarantorService.MaxQueuedSubmissions),
    factory
  ))

  private val anchorWait =
    new WorkGate("anchor wait", GuarantorService.MaxQueuedSubmissions, GuarantorService.MaxQueuedSubmissionBytes)
  private val anchorWaiter = threads.scheduled("jam-anchor-wait")

  private def anchorsSettled(wp: WorkPackage): Boolean =
    val ctx = wp.context
    val history = chain.stateViewOf(chain.best).beta.history
    def known = history.exists(_.headerHash == ctx.anchor) && accounts.forAnchor(ctx.lookupAnchorStateRoot).anchorAvailable
    def tooOld = chain.slotOf(ctx.anchor).exists(s => history.headOption.exists(s < _.slot))
    known || tooOld

  private def onceAnchored(wp: WorkPackage, bytes: Long, priority: RefinePool.Priority, dropped: () => Unit = () => ())(
      k: => Unit
  ): Unit =
    if anchorsSettled(wp) then k
    else
      val name = RefineFetch.workPackageHash(wp).toHex.take(18)
      val deadline = System.currentTimeMillis() + chain.config.slotDuration * 1000L
      logger.info(s"work package $name is anchored at ${wp.context.anchor.toHex.take(18)}, not imported yet; waiting for it")
      val whenAnchored: java.util.concurrent.Executor = task =>
        def poll(): Unit =
          if anchorsSettled(wp) || System.currentTimeMillis() >= deadline then task.run()
          else anchorWaiter.schedule((() => poll()): Runnable, GuarantorService.AnchorPollMillis, java.util.concurrent.TimeUnit.MILLISECONDS): Unit
        anchorWaiter.schedule((() => poll()): Runnable, GuarantorService.AnchorPollMillis, java.util.concurrent.TimeUnit.MILLISECONDS): Unit
      val admitted = anchorWait.tryExecute(whenAnchored, bytes) {
        try refinePool.submit(priority)(k)
        catch case _: java.util.concurrent.RejectedExecutionException => dropped() // shutting down
      }
      if !admitted then
        logger.warn(s"work package $name refused: too many packages are waiting for their anchor")
        dropped()

  /** Run the work-report computation pipeline (Xi) and, on success, custody
    * the bundle/shards and sign+pool+distribute the resulting report. Shared
    * terminal step for both CE 133 (`onWorkPackage`) and CE 146
    * (`bundleSubmissionHandler`).
    */
  private def guaranteeWorkPackage(
      coreIndex: Int,
      wp: WorkPackage,
      extrinsicData: IndexedSeq[IndexedSeq[Array[Byte]]],
      importSegments: IndexedSeq[IndexedSeq[Array[Byte]]],
      justifications: IndexedSeq[IndexedSeq[List[Array[Byte]]]],
      segmentRootLookup: Map[Hash, Hash],
      authorized: Option[io.forge.jam.protocol.refine.IsAuthorizedResult],
      ev: Ev
  ): Unit =
    val bytes = GuarantorService.heldBytes(wp, extrinsicData) +
      importSegments.iterator.flatten.map(_.length.toLong).sum +
      justifications.iterator.flatten.flatten.map(_.length.toLong).sum
    val hash = RefineFetch.workPackageHash(wp)
    if !inFlight.add(hash) then
      logger.warn(s"work package ${hash.toHex.take(18)} refused: a copy is already on its way to a guarantee here")
      telemetry.foreach(_.duplicateWorkPackage(ev, coreIndex, hash))
    else
      val released = new java.util.concurrent.atomic.AtomicBoolean()
      val release = () => if released.compareAndSet(false, true) then inFlight.remove(hash): Unit
      try
        onceAnchored(wp, bytes, RefinePool.Priority.Submission, release) {
          var handedOn = false
          try
            val slot = chain.nextBlockSlot
            guaranteedLately(hash) match
              case Some(t) =>
                logger.warn(s"work package ${hash.toHex.take(18)} refused before refine: already guaranteed at slot $t")
                telemetry.foreach(_.duplicateWorkPackage(ev, coreIndex, hash))
              case None =>
                precheck(wp, coreIndex, slot) match
                  case Left(why) =>
                    logger.warn(s"work package ${hash.toHex.take(18)} refused before refine: $why")
                    failed(ev, why)
                  case Right(anchorState) =>
                    handedOn = refineAndGuarantee(
                      coreIndex, wp, extrinsicData, importSegments, justifications, segmentRootLookup, anchorState, authorized, ev, release
                    )
          finally if !handedOn then release()
        }
      catch
        case e: Throwable =>
          release()
          throw e

  /** GP §guaranteeing: everything that can be judged before refining. The work-package limits and the guarantee
    * extrinsic's refine-independent rules (authorizer in the pool, anchors) against the head, that one of our keys is
    * assigned to the core, and that we hold the lookup anchor's state. Returns that state's lookup.
    */
  private def precheck(wp: WorkPackage, coreIndex: Int, slot: Long): Either[String, StateHistoricalLookup] =
    val head = chain.best
    val view = chain.stateViewOf(head)
    val reports = io.forge.jam.protocol.state.TrieBackedJamStateBridges.ReportBridge.extract(view)
    val context = WorkPackageValidator.ChainContext(
      authPool = view.authPools.lift(coreIndex).getOrElse(Nil),
      recentHistory = view.beta.history,
      headStateRoot = head.stateRoot,
      slot = slot,
      services = reports.serviceInfo,
      knownPackages = io.forge.jam.protocol.report.ReportTransition.pipelinedPackages(reports.state)
    )
    val lookup = accounts.forAnchor(wp.context.lookupAnchorStateRoot)
    for
      _ <- WorkPackageValidator.validate(wp, coreIndex, context, chain.config)
      _ <- Either.cond(
        guaranteeSlot(coreIndex).isDefined,
        (),
        s"no key of ours is assigned to core $coreIndex in this rotation or the one before"
      )
      _ <- Either.cond(lookup.anchorAvailable, (), "lookup anchor state not held")
    yield lookup

  private def refineAndGuarantee(
      coreIndex: Int,
      wp: WorkPackage,
      extrinsicData: IndexedSeq[IndexedSeq[Array[Byte]]],
      importSegments: IndexedSeq[IndexedSeq[Array[Byte]]],
      justifications: IndexedSeq[IndexedSeq[List[Array[Byte]]]],
      segmentRootLookup: Map[Hash, Hash],
      lookup: StateHistoricalLookup,
      authorized: Option[io.forge.jam.protocol.refine.IsAuthorizedResult],
      ev: Ev,
      done: () => Unit
  ): Boolean =
    val computed: Either[Any, ComputedReport] =
      try
        val auth: Either[Any, io.forge.jam.protocol.refine.IsAuthorizedResult] = authorized match
          case Some(done) => Right(done)
          case None =>
            lookup.unlessLost(computeReport.authorize(wp, coreIndex, lookup)).left.map(e => s"not authorized: $e")
        if authorized.isEmpty then auth.foreach(a => telemetry.foreach(_.authorized(ev, a.cost)))
        auth.flatMap { a =>
          lookup.unlessLost(computeReport.compute(
            workPackage = wp,
            coreIndex = coreIndex,
            segmentRootLookup = segmentRootLookup,
            importSegments = importSegments,
            extrinsicData = extrinsicData,
            justifications = justifications,
            accounts = lookup,
            authorized = Some(a),
            deadlineMillis = Some(System.currentTimeMillis() + GuarantorService.RefineDeadlineMillis)
          ))
        }
      catch
        case e: (io.forge.jam.protocol.refine.ComputeReport.DeadlinePassed |
              io.forge.jam.protocol.pvmhost.PvmRunner.InternalFailure) => Left(e.getMessage)
    computed.map(tamper) match
      case Left(err) =>
        logger.warn(s"work package refused: $err")
        failed(ev, String.valueOf(err))
        false
      case Right(computed) =>
        reportBuilt(ev, computed)
        storeShards(computed)
        guaranteeSlot(coreIndex) match
          case None =>
            logger.warn(s"work package refused: no key of ours is assigned to core $coreIndex any more after refining")
            failed(ev, s"no key of ours is assigned to core $coreIndex any more after refining")
            false
          case Some(slot) =>
            try
              coSignPool.execute(() =>
                try signAndDistribute(computed, slot, ev)
                catch
                  case e: Exception =>
                    logger.error("guaranteeing failed", e)
                    failed(ev, s"failed here: ${e.getMessage}")
                finally done()
              )
              true
            catch case _: java.util.concurrent.RejectedExecutionException => false // shutting down

  private def reportBuilt(ev: Ev, computed: ComputedReport): Unit =
    telemetry.foreach { t =>
      t.refined(ev, computed.refineCosts)
      val spec = computed.report.packageSpec
      t.workReportBuilt(
        ev,
        Telemetry.workReportOutline(
          Hashing.blake2b256(computed.report.encode.toArray),
          computed.bundleBytes.length.toLong,
          spec.erasureRoot,
          spec.exportsRoot
        )
      )
    }

  /** Build and custody every validator's DA shards so assurers can pull
    * theirs via CE 137 (and auditors bundle shards via CE 138).
    */
  private def storeShards(computed: ComputedReport): Unit =
    recordBundle(computed.report.packageSpec.erasureRoot, computed.bundleBytes)
    // CE 148 sourcing: keyed by the exports-root, covers both guarantee
    // paths (own refine via onWorkPackage, and CE 134 co-sign re-execution
    // via respondCoSign — both funnel through storeShards).
    segments.put(computed.report.packageSpec.exportsRoot, computed.exportedSegments)
    shardStore.foreach { store =>
      DaShards.buildAll(
        computed.bundleBytes,
        computed.exportedSegments,
        chain.config
      ) match
        case Right(all) =>
          val root = computed.report.packageSpec.erasureRoot
          all.foreach(s => store.put(root, s.shardIndex, s.encode))
          logger.debug(s"custodied ${all.size} shard sets for ${root.toHex.take(18)}")
        case Left(err) =>
          logger.error(s"shard construction failed (report still distributed): $err")
    }

  private def guaranteeSlot(coreIndex: Int): Option[Long] =
    GuarantorService.guaranteeSlot(chain.nextBlockSlot, chain.config.rotationPeriod)(ownAssignedKeys(_, coreIndex).nonEmpty)

  /** Every held key assigned to `coreIndex` at `slot`, with its active-set
    * validator index.
    */
  private def ownAssignedKeys(slot: Long, coreIndex: Int): List[(Int, ValidatorKeySet)] =
    val (assignments, active) = GuarantorService.assignments(chain.stateView(), slot, chain.config)
    active.zipWithIndex.flatMap { case (vk, idx) =>
      if assignments(idx) == coreIndex then
        ed25519ByPublic.get(vk.ed25519).map(idx -> _)
      else None
    }.toList

  private def signAndDistribute(
      computed: ComputedReport,
      slot: Long,
      ev: Ev
  ): Unit =
    val report = computed.report
    val reportHash = Hashing.blake2b256(report.encode.toArray)
    val message = constants.JAM_GUARANTEE_BYTES ++ reportHash.bytes

    val ownSignatures = ownAssignedKeys(slot, report.coreIndex.toInt).map { (idx, keys) =>
      GuaranteeSignature(
        ValidatorIndex(idx),
        Ed25519Signature(Ed25519ZebraWrapper.sign(keys.ed25519Secret, message))
      )
    }

    // Share with the core's other guarantors via CE 134 unless we already
    // hold all three credentials.
    val signatures =
      (if ownSignatures.size >= 3 then ownSignatures
       else ownSignatures ++ requestCoSignatures(computed, slot, reportHash, ownSignatures.map(_.validatorIndex.toInt).toSet, ev))
        .distinctBy(_.validatorIndex.toInt)
        .sortBy(_.validatorIndex.toInt)

    if signatures.size < 2 then
      logger.warn(
        s"insufficient credentials for the report (have ${signatures.size} after CE 134); dropping"
      )
      failed(ev, s"insufficient credentials: ${signatures.size} after CE 134")
    else
      val guarantee =
        GuaranteeExtrinsic(report, Timeslot(UInt(slot.toInt)), signatures.take(3))
      ourGuarantees.put(report.packageSpec.hash, java.lang.Long.valueOf(slot))
      val built = telemetry.flatMap(_.guaranteeBuilt(ev, Telemetry.guaranteeOutline(guarantee)))
      pools.addGuarantee(guarantee)
      distribution.recordReport(report)
      distribution.distributeGuaranteeToAll(guarantee, built, () => telemetry.foreach(_.guaranteesDistributed(ev)))
      guaranteesMadeCount.incrementAndGet()
      logger.info(
        s"guaranteed package ${report.packageSpec.hash.toHex} on core ${report.coreIndex.toInt} " +
          s"with ${signatures.size} credentials: ${report.results.map(_.result).mkString(",")}, " +
          s"${report.packageSpec.exportsCount.toInt} export(s) under ${report.packageSpec.exportsRoot.toHex}"
      )

  // =========================================================================
  // CE 134 — work-package sharing (co-signing)
  // =========================================================================

  /** Responder: check the shard count, re-execute the shared bundle and
    * return one signature (`Work-Report Hash ++ Ed25519 Signature`) from a
    * held key assigned to the core: the key we use as network identity when
    * it is assigned, else the lowest assigned index we hold. Messages that
    * cannot be co-signed are rejected by resetting the stream.
    */
  def workPackageSharingHandler: io.forge.jam.network.PeerStreamHandler =
    (conn: PeerConnection, stream: MessageStream) =>
      // CE 134 is a header and a bundle. Only those two are kept: a peer that goes on sending, or sends more than
      // a submission may hold, gets a reset and nothing more of its stream is buffered.
      val shared = telemetry.flatMap(_.workPackageBeingShared(conn.peerKey))
      if !fromValidator(conn) then
        logger.debug("CE134: the sender is not a validator; rejecting")
        failed(shared, "the sender is not a validator")
        stream.reset()
      else
        val intake = new StreamIntake(2, maxSubmissionBytes, conn.holdInbound, conn.releaseInbound)
        var rejected = false
        stream.onMessage { msg =>
          // The header names the core: a validator that is not one of its guarantors is refused before its bundle
          // is kept. A header that does not decode is refused with the bundle, as before.
          val stranger = intake.size == 0 && !rejected &&
            GuarantorService.decodeSharingHeader(msg).exists((core, _, _) => !fromCoGuarantor(conn, core))
          if stranger then
            rejected = true
            logger.debug("CE134: the sender is not a guarantor of the core it names; rejecting")
            failed(shared, "the sender is not a guarantor of the core it names")
            stream.reset()
          else if rejected then ()
          else if intake.offer(msg) then
            if intake.size == 2 then
              val Seq(header, bundle) = intake.messages: @unchecked
              val admitted = coSignAdmission.tryExecute(refinePool.at(RefinePool.Priority.CoSign), intake.bytes, conn) {
                try respondCoSign(stream, header, bundle, shared)
                catch
                  case e: Exception =>
                    logger.error("CE134 handling failed", e)
                    failed(shared, s"failed here: ${e.getMessage}")
                    stream.reset()
              }
              intake.release()
              // Too many submissions waiting for the guarantor thread: rejected, once.
              if !admitted then
                rejected = true
                failed(shared, "too many bundles waiting to be co-signed")
                stream.reset()
          else if !rejected then
            rejected = true
            intake.release()
            logger.warn("CE134: more than two messages or over the submission size cap; rejecting")
            failed(shared, "more than two messages, or over the submission size cap")
            stream.reset()
        }
        stream.onClosed { () =>
          intake.release()
          if intake.size < 2 && !rejected then
            failed(shared, s"stream closed with ${intake.size} of 2 messages")
            stream.reset()
        }

  private def respondCoSign(stream: MessageStream, header: Array[Byte], bundleBytes: Array[Byte], shared: Ev): Unit =
    val request =
      Hashing.blake2b256(Hashing.blake2b256(header).bytes.toArray ++ Hashing.blake2b256(bundleBytes).bytes.toArray)
    coSignedLately(request) match
      case Some(answer) =>
        logger.debug(s"CE134: the same bundle was co-signed lately; sending that answer again")
        stream.send(answer)
        stream.finish()
        telemetry.foreach(_.workReportSignatureSent(shared))
      case None => refineAndCoSign(stream, header, bundleBytes, shared, request)

  private def refineAndCoSign(
      stream: MessageStream,
      header: Array[Byte],
      bundleBytes: Array[Byte],
      shared: Ev,
      request: Hash
  ): Unit =
    val decoded = for
      h <- GuarantorService.decodeSharingHeader(header)
      _ <- Either.cond(
        h._2 == chain.config.validatorCount,
        (),
        s"shard count ${h._2} is not the validator count ${chain.config.validatorCount}"
      )
      bundle <- WorkPackageBundle.decode(bundleBytes).left.map(e => s"cannot co-sign: $e")
    yield (h._1, h._3, bundle)
    decoded match
      case Left(err) =>
        logger.warn(s"CE134: $err")
        failed(shared, err)
        stream.reset()
      case Right((coreIndex, lookup, bundle)) =>
        received(shared, coreIndex, bundle.workPackage)
        telemetry.foreach(_.extrinsicDataReceived(shared))
        onceAnchored(bundle.workPackage, bundleBytes.length.toLong, RefinePool.Priority.CoSign) {
          try coSign(stream, coreIndex, lookup, bundle, shared, request)
          catch
            case e: Exception =>
              logger.error("CE134 handling failed", e)
              failed(shared, s"failed here: ${e.getMessage}")
              stream.reset()
        }

  private def coSign(
      stream: MessageStream,
      coreIndex: Int,
      lookup: Map[Hash, Hash],
      bundle: WorkPackageBundle,
      shared: Ev,
      request: Hash
  ): Unit =
    val slot = chain.nextBlockSlot
    val lastOfPrevious = slot / chain.config.rotationPeriod * chain.config.rotationPeriod - 1
    val held = ownAssignedKeys(slot, coreIndex) ++ (if lastOfPrevious >= 0 then ownAssignedKeys(lastOfPrevious, coreIndex) else Nil)
    if held.isEmpty then
      logger.debug(s"CE134: no key assigned to core $coreIndex at slot $slot or in the rotation before; rejecting")
      failed(shared, s"no key of ours is assigned to core $coreIndex")
      stream.reset()
    else
      val result = for
        anchored <- precheck(bundle.workPackage, coreIndex, slot)
        _ <- verifyBuilderImports(
          bundle.workPackage,
          lookup,
          bundle.extrinsicData,
          bundle.importSegments,
          bundle.justifications
        )
        _ = telemetry.foreach(_.importsReceived(shared))
        auth <- anchored
          .unlessLost(computeReport.authorize(bundle.workPackage, coreIndex, anchored))
          .left
          .map(e => s"not authorized: $e")
        _ = telemetry.foreach(_.authorized(shared, auth.cost))
        computed <- anchored
          .unlessLost(
            computeReport.compute(
              workPackage = bundle.workPackage,
              coreIndex = coreIndex,
              segmentRootLookup = lookup,
              importSegments = bundle.importSegments,
              extrinsicData = bundle.extrinsicData,
              justifications = bundle.justifications,
              accounts = anchored,
              authorized = Some(auth),
              deadlineMillis = Some(System.currentTimeMillis() + GuarantorService.RefineDeadlineMillis)
            )
          )
          .map(tamper)
          .left
          .map(e => s"re-execution refused: $e")
      yield computed

      result match
        case Left(err) =>
          logger.warn(s"CE134: cannot co-sign: $err")
          failed(shared, s"cannot co-sign: $err")
          stream.reset()
        case Right(computed) =>
          reportBuilt(shared, computed)
          storeShards(computed) // co-guarantors custody shards too
          val reportHash = Hashing.blake2b256(computed.report.encode.toArray)
          val message = constants.JAM_GUARANTEE_BYTES ++ reportHash.bytes
          val (_, keys) = held
            .find((_, k) => ownNetworkKey.exists(java.util.Arrays.equals(_, k.ed25519Public)))
            .getOrElse(held.minBy(_._1))
          val answer: Array[Byte] = reportHash.bytes ++ Ed25519ZebraWrapper.sign(keys.ed25519Secret, message)
          coSigned.put(request, (slot, answer))
          stream.send(answer)
          stream.finish()
          telemetry.foreach(_.workReportSignatureSent(shared))
          logger.info(s"CE134: co-signed report ${reportHash.toHex.take(18)}")

  /** Initiator: share the bundle with the other guarantors assigned to the
    * core (every peer when none can be told apart by key, as on devnets
    * whose nodes hold several validator keys), in parallel. Once two
    * credentials are in hand the rest get [[GuarantorService.ThirdGuarantorGraceMillis]]
    * to answer (JAMNP-S CE 135). A signature counts for the assigned
    * validator whose key it verifies under, preferring the peer's own key.
    */
  private def requestCoSignatures(
      computed: ComputedReport,
      slot: Long,
      reportHash: Hash,
      alreadyHave: Set[Int],
      ev: Ev
  ): List[GuaranteeSignature] =
    val report = computed.report
    recordBundle(report.packageSpec.erasureRoot, computed.bundleBytes)
    val (assignments, activeSet) = GuarantorService.assignments(chain.stateView(), slot, chain.config)
    val message = constants.JAM_GUARANTEE_BYTES ++ reportHash.bytes
    val assigned =
      activeSet.indices.filter(i => assignments.lift(i).contains(report.coreIndex.toInt) && !alreadyHave(i))
    if assigned.isEmpty then return Nil

    val header = GuarantorService.encodeSharingHeader(
      report.coreIndex.toInt,
      chain.config.validatorCount,
      report.segmentRootLookup.map(l => l.workPackageHash -> l.segmentTreeRoot)
    )
    def keyOf(i: Int) = JamBytes(activeSet(i).ed25519.bytes.toArray)
    val (connected, toDial) = GuarantorService.coSignTargets(
      distribution.peers,
      (c: PeerConnection) => JamBytes(c.peerKey),
      assigned.map(keyOf),
      holdsSeveralKeys = validatorKeys.size > 1
    )
    val dialed = if toDial.isEmpty then Nil else dialValidators(assigned.filter(i => toDial.contains(keyOf(i))).map(activeSet))
    val targets = (connected ++ dialed).distinctBy(c => JamBytes(c.peerKey))

    val responses = new java.util.concurrent.LinkedBlockingQueue[(PeerConnection, Either[String, Array[Byte]])]()
    targets.foreach { conn =>
      coSignPool.submit { (() => {
        telemetry.foreach(_.sharingWorkPackage(ev, conn.peerKey))
        val sent = () => telemetry.foreach(_.bundleSent(ev, conn.peerKey))
        responses.put(conn -> coSignRoundTrip(conn, header, computed.bundleBytes, sent))
      }): Runnable }
    }
    val answered = mutable.Set.empty[PeerConnection]
    def sharingFailed(conn: PeerConnection, why: String): Unit =
      answered += conn
      telemetry.foreach(_.workPackageSharingFailed(ev, conn.peerKey, why))

    val collected = mutable.ListBuffer.empty[GuaranteeSignature]
    val seen = mutable.Set.empty[Int] ++ alreadyHave
    var pending = targets.size
    val deadline = System.currentTimeMillis() + GuarantorService.CoSignTimeoutMillis
    var graceDeadline = Long.MaxValue
    while pending > 0 && seen.size < 3 && System.currentTimeMillis() < math.min(deadline, graceDeadline) do
      val wait = math.min(deadline, graceDeadline) - System.currentTimeMillis()
      Option(responses.poll(math.max(wait, 1), java.util.concurrent.TimeUnit.MILLISECONDS)).foreach { (conn, resp) =>
        pending -= 1
        resp.left.foreach(why => sharingFailed(conn, why))
        resp.foreach { r =>
          if r.length != 96 then
            logger.warn(s"CE134: bad response length ${r.length}")
            sharingFailed(conn, s"bad response length ${r.length}")
          else if Hash(java.util.Arrays.copyOfRange(r, 0, 32)) != reportHash then
            logger.warn("CE134: peer computed a different report; rejecting its signature")
            sharingFailed(conn, "the peer signed a different report")
            explainDifference(conn, report, Hash(java.util.Arrays.copyOfRange(r, 0, 32)))
            dumpMismatch(computed, Hash(java.util.Arrays.copyOfRange(r, 0, 32)))
          else
            val sig = java.util.Arrays.copyOfRange(r, 32, 96)
            def verifies(i: Int) = Ed25519.verify(activeSet(i).ed25519.bytes.toArray, message, sig)
            val candidates = assigned.filterNot(seen)
            val signer = candidates
              .find(i => java.util.Arrays.equals(activeSet(i).ed25519.bytes.toArray, conn.peerKey))
              .filter(verifies)
              .orElse(candidates.find(verifies))
            signer match
              case Some(idx) =>
                seen += idx
                collected += GuaranteeSignature(ValidatorIndex(idx), Ed25519Signature(sig))
                answered += conn
                telemetry.foreach(_.workReportSignatureReceived(ev, conn.peerKey))
                if seen.size >= 2 && graceDeadline == Long.MaxValue then
                  graceDeadline = System.currentTimeMillis() + GuarantorService.ThirdGuarantorGraceMillis
              case None =>
                logger.warn("CE134: signature from no assigned validator")
                sharingFailed(conn, "a signature from no assigned validator")
        }
      }
    targets.filterNot(answered).foreach(conn => sharingFailed(conn, "no signature in time, or not needed any more"))
    collected.toList

  /** With debug logging on, keep our report, bundle and exports and the peer's report hash under
    * `<tmpdir>/jam-forge-mismatch/`, to work out offline which rule the peer applied.
    */
  private def dumpMismatch(computed: ComputedReport, theirs: Hash): Unit =
    if logger.underlying.isDebugEnabled then
      try
        val dir = java.nio.file.Paths.get(System.getProperty("java.io.tmpdir"), "jam-forge-mismatch")
        java.nio.file.Files.createDirectories(dir)
        val base = computed.report.packageSpec.hash.toHex.stripPrefix("0x").take(16)
        def put(ext: String, bytes: Array[Byte]) = java.nio.file.Files.write(dir.resolve(s"$base.$ext"), bytes)
        put("report", summon[Codec[io.forge.jam.core.types.workpackage.WorkReport]].encode(computed.report))
        put("bundle", computed.bundleBytes)
        put("exports", computed.exportedSegments.flatten.toArray)
        put("theirs", theirs.bytes)
        logger.debug(s"CE134 mismatch kept in $dir/$base.*")
      catch case e: Exception => logger.debug(s"could not keep the mismatch: $e")

  /** Ask a co-guarantor that computed a different report for its report (CE 136) and log where the two differ: the
    * only way to see why two implementations disagree on a package. Off the waiting thread; best effort.
    */
  private def explainDifference(conn: PeerConnection, ours: io.forge.jam.core.types.workpackage.WorkReport, theirs: Hash): Unit =
    try
      coSignPool.execute { () =>
        StreamRequest.collect(conn, io.forge.jam.network.StreamKind.WorkReportRequest, theirs.bytes, 5000) match
          case Some(msg :: _) =>
            summon[Codec[io.forge.jam.core.types.workpackage.WorkReport]].decodeExact(msg) match
              case Right(report) =>
                val fields = ReportDiff.diff(ours, report)
                logger.warn(
                  s"CE134: report of package ${ours.packageSpec.hash.toHex.take(18)} differs from peer " +
                    s"${JamNode.hex(conn.peerKey).take(16)}'s in ${fields.size} field(s):\n  " + fields.mkString("\n  ")
                )
              case Left(err) => logger.warn(s"CE136: peer's report ${theirs.toHex.take(18)} does not decode: $err")
          case _ => logger.debug(s"CE136: peer did not serve its report ${theirs.toHex.take(18)}")
      }
    catch case _: java.util.concurrent.RejectedExecutionException => ()

  /** Runs CE 134 round trips in parallel, off the guarantor thread. */
  private val coSignPool = threads.cached("jam-cosign")

  /** One CE 134 round trip: header and bundle messages, FIN, then the
    * 96-byte response; None when the peer resets or times out.
    */
  private def coSignRoundTrip(
      conn: PeerConnection,
      header: Array[Byte],
      bundle: Array[Byte],
      sent: () => Unit
  ): Either[String, Array[Byte]] =
    // No response (the peer rejected it or lacks the protocol, a timeout, a failure): the stream is given up on
    // both sides.
    StreamRequest
      .exchange(
        conn,
        io.forge.jam.network.StreamKind.WorkPackageSharing,
        Seq(header, bundle),
        10_000,
        () => GuarantorService.CoSignTimeoutMillis,
        StreamRequest.Until.FirstMessage,
        maxMessages = 1,
        onSent = sent
      )
      .left
      .map(_.message)
      .flatMap(_.headOption.toRight("refused: no signature"))

object GuarantorService:
  /** Work packages fetching their imports at once. */
  val ImportFetchThreads: Int = 4

  val HeldBundleBytes: Long = 256L << 20

  val RefineDeadlineMillis: Long = 60_000L

  val AnchorPollMillis: Long = 100L

  def heldBytes(wp: WorkPackage, extrinsicData: IndexedSeq[IndexedSeq[Array[Byte]]]): Long =
    wp.items.map(_.payload.length.toLong).sum + extrinsicData.iterator.flatten.map(_.length.toLong).sum

  /** CE 133/134/146 submissions that may wait for the guarantor thread, and the message bytes they may hold
    * (two of the largest CE 146 submissions).
    */
  val MaxQueuedSubmissions: Int = 32
  val MaxQueuedSubmissionBytes: Long = 96L * 1024 * 1024

  /** CE 146's cumulative-across-messages byte cap (see `bundleSubmissionHandler`).
    * 48 MiB comfortably covers a legitimate submission (a realistic work
    * package + extrinsics + a handful of `Csegmentsize` import segments +
    * proofs) while bounding the per-stream — and, multiplied by concurrent
    * streams, per-node — memory a misbehaving or malicious builder can force
    * the node to buffer before any structural validation runs.
    */
  val DefaultMaxSubmissionBytes: Long = 48L * 1024 * 1024

  /** Largest encoded work-package a builder may send before authorization
    * (JAMNP-S CE 133 and CE 146: `200 * 1024` bytes).
    */
  val MaxWorkPackageBytes: Int = 200 * 1024

  def bundleSubmissionMessages(coreIndex: Int, lookup: Map[Hash, Hash], bundle: WorkPackageBundle): Seq[Array[Byte]] =
    val head = new java.io.ByteArrayOutputStream(3 + 64 * lookup.size)
    head.write(LittleEndian.encode(coreIndex.toLong, 2))
    head.write(JamCodecs.encodeCompactInteger(lookup.size.toLong))
    lookup.toSeq.sortBy(_._1.toHex).foreach { (wp, root) =>
      head.write(wp.bytes.toArray)
      head.write(root.bytes.toArray)
    }
    val justifications = new java.io.ByteArrayOutputStream()
    bundle.justifications.foreach(_.foreach { path =>
      justifications.write(JamCodecs.encodeCompactInteger(path.length.toLong))
      path.foreach(justifications.write)
    })
    Seq(
      head.toByteArray,
      RefineFetch.encodeWorkPackage(bundle.workPackage),
      Array.concat(bundle.extrinsicData.flatten*),
      Array.concat(bundle.importSegments.flatten*),
      justifications.toByteArray
    )

  private val Local = new Object

  /** Most segments one CE 148 stream may request: W_M = 3072 (GP 0.8.0). */
  val MaxSegmentsPerRequest: Int = 3072

  /** Upper bound on one CE 134 exchange (the peer refines the bundle). */
  val CoSignTimeoutMillis: Long = 30000L

  /** How long the third guarantor gets once two credentials are in hand
    * (JAMNP-S CE 135: "a reasonable amount of time (e.g. two seconds)").
    */
  val ThirdGuarantorGraceMillis: Long = 2000L

  /** CE 134 first message: `Core Index(u16) ++ Shard Count(u16) ++
    * len++[Work-Package Hash ++ Segments-Root]` (JAMNP-S CE 134).
    */
  def encodeSharingHeader(coreIndex: Int, shardCount: Int, mappings: Seq[(Hash, Hash)]): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream(4 + 1 + 64 * mappings.size)
    out.write(LittleEndian.encode(coreIndex.toLong, 2))
    out.write(LittleEndian.encode(shardCount.toLong, 2))
    out.write(JamCodecs.encodeCompactInteger(mappings.size.toLong))
    mappings.foreach { (wp, root) =>
      out.write(wp.bytes)
      out.write(root.bytes)
    }
    out.toByteArray

  def decodeSharingHeader(bytes: Array[Byte]): Either[String, (Int, Int, Map[Hash, Hash])] =
    try
      if bytes.length < 5 then Left(s"header too short (${bytes.length} bytes)")
      else
        val core = LittleEndian.get(bytes, 0, 2).toInt
        val shards = LittleEndian.get(bytes, 2, 2).toInt
        val (n, c) = JamCodecs.decodeCompactInteger(bytes, 4)
        val end = 4 + c + 64 * n
        if n < 0 || end != bytes.length then Left("mapping list length does not match the message")
        else
          val mappings = (0 until n.toInt).map { i =>
            val off = 4 + c + 64 * i
            Hash(java.util.Arrays.copyOfRange(bytes, off, off + 32)) ->
              Hash(java.util.Arrays.copyOfRange(bytes, off + 32, off + 64))
          }.toMap
          Right((core, shards, mappings))
    catch case e: Exception => Left(s"bad header: ${e.getMessage}")

  def coSignTargets[P, K](peers: List[P], keyOf: P => K, assigned: Seq[K], holdsSeveralKeys: Boolean): (List[P], Seq[K]) =
    val connected = peers.filter(p => assigned.contains(keyOf(p)))
    if connected.isEmpty && holdsSeveralKeys then (peers, Nil)
    else (connected, assigned.filterNot(k => connected.exists(p => keyOf(p) == k)))

  /** η′₂ and κ′ in force at `slot`, from the state of the best block. When `slot` opens a new epoch, the block at
    * `slot` rotates them (η′₂ = η₁ and κ′ = γ_P on an epoch change), so a guarantee made at the first
    * slot of an epoch must use those, not the head state's η₂ and κ.
    */
  def assignmentContext(
      view: io.forge.jam.protocol.state.TrieBackedJamState,
      slot: Long,
      config: io.forge.jam.core.ChainConfig
  ): (Hash, IndexedSeq[io.forge.jam.core.types.epoch.ValidatorKey]) =
    rotatedForSlot(view.timeslot, view.entropy.pool, view.validators.current, view.validators.nextEpoch, slot, config)

  /** [[assignmentContext]] over plain values: the state's slot, η, κ and γ_P. */
  def rotatedForSlot[K](
      stateSlot: Long,
      entropy: List[Hash],
      current: IndexedSeq[K],
      pending: IndexedSeq[K],
      slot: Long,
      config: io.forge.jam.core.ChainConfig
  ): (Hash, IndexedSeq[K]) =
    if slot / config.epochLength > stateSlot / config.epochLength then (entropy(1), pending)
    else (entropy(2), current)

  /** The validators that may share a package for `core` around `slot` (CE 134): those assigned to it in the slot's
    * rotation, and those of the rotation before, who may still be finishing one. A rotation of the epoch before the
    * state's is the previous set's (λ, with η_3).
    */
  def guarantorsOf[K](
      stateSlot: Long,
      entropy: List[Hash],
      previous: IndexedSeq[K],
      current: IndexedSeq[K],
      pending: IndexedSeq[K],
      core: Int,
      slot: Long,
      config: io.forge.jam.core.ChainConfig
  ): Set[K] =
    def at(s: Long): Set[K] =
      val (cores, validators) = assignmentsAt(stateSlot, entropy, previous, current, pending, s, config)
      validators.zip(cores).collect { case (v, c) if c == core => v }.toSet
    val before = (slot / config.rotationPeriod - 1) * config.rotationPeriod
    at(slot) ++ (if before >= 0 then at(before) else Set.empty)

  def assignments(
      view: io.forge.jam.protocol.state.TrieBackedJamState,
      slot: Long,
      config: io.forge.jam.core.ChainConfig
  ): (IndexedSeq[Int], IndexedSeq[io.forge.jam.core.types.epoch.ValidatorKey]) =
    val v = view.validators
    assignmentsAt(view.timeslot, view.entropy.pool, v.previous, v.current, v.nextEpoch, slot, config)

  def assignmentsAt[K](
      stateSlot: Long,
      entropy: List[Hash],
      previous: IndexedSeq[K],
      current: IndexedSeq[K],
      pending: IndexedSeq[K],
      slot: Long,
      config: io.forge.jam.core.ChainConfig
  ): (IndexedSeq[Int], IndexedSeq[K]) =
    if slot / config.epochLength < stateSlot / config.epochLength then
      val cores = io.forge.jam.protocol.report.GuarantorAssignment.coreAssignments(entropy(3), slot, previous.size, config)
      (cores.toIndexedSeq, previous)
    else assignmentsFor(stateSlot, entropy, current, pending, slot, config)

  def guaranteeSlot(next: Long, rotationPeriod: Int)(assignedAt: Long => Boolean): Option[Long] =
    if assignedAt(next) then Some(next)
    else
      val lastOfPrevious = next / rotationPeriod * rotationPeriod - 1
      Option.when(lastOfPrevious >= 0 && assignedAt(lastOfPrevious))(lastOfPrevious)

  def assignmentsFor[K](
      stateSlot: Long,
      entropy: List[Hash],
      current: IndexedSeq[K],
      pending: IndexedSeq[K],
      slot: Long,
      config: io.forge.jam.core.ChainConfig
  ): (IndexedSeq[Int], IndexedSeq[K]) =
    val (randomness, validators) = rotatedForSlot(stateSlot, entropy, current, pending, slot, config)
    val cores = io.forge.jam.protocol.report.GuarantorAssignment.coreAssignments(randomness, slot, validators.size, config)
    (cores.toIndexedSeq, validators)
