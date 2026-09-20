package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes, LittleEndian, constants}
import io.forge.jam.core.primitives.{Ed25519Signature, Hash, Timeslot, ValidatorIndex}
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.types.dispute.GuaranteeSignature
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.crypto.{Ed25519, Ed25519ZebraWrapper}
import io.forge.jam.network.{JamnpConnection, JamnpStream}
import io.forge.jam.protocol.accumulation.StateKey
import io.forge.jam.protocol.refine.{
  ComputedReport,
  ComputeReport,
  DaShards,
  HistoricalLookupService,
  WorkPackageBundle
}
import scodec.Codec
import scodec.bits.ByteVector
import spire.math.UInt

import scala.collection.mutable

/** Historical preimage lookup over the node's current chain state: service
  * existence via the account-info key, preimage blobs via the 0xFFFFFFFE
  * service-data discriminator.
  *
  * TODO(historical): consult the preimage-request timeslots for availability
  * at the lookup anchor; the current view serves preimages present in the
  * best state, which is correct for preimages available since genesis.
  */
final class StateHistoricalLookup(chain: ChainManager) extends HistoricalLookupService:

  def serviceExists(serviceId: Long): Boolean =
    readRaw(StateKey.computeServiceAccountKey(serviceId)).isDefined

  def historicalLookup(
      serviceId: Long,
      lookupAnchorTimeslot: Long,
      hash: Hash
  ): Option[Array[Byte]] =
    val blobKey = StateKey.computeServiceDataStateKey(
      serviceId,
      0xfffffffeL,
      JamBytes(hash.bytes.toArray)
    )
    readRaw(blobKey).map(_.toArray)

  private def readRaw(stateKey: JamBytes): Option[JamBytes] =
    chain.readRawState(stateKey)

/** Guarantor role: accepts work-package submissions (CE 133), computes the
  * work report via the in-core pipeline (is-authorized + refine), signs it
  * with this node's assigned validator keys, tops the credentials up to the
  * required ≥ 2 via CE 134 co-signing with peer guarantors, and distributes
  * the guaranteed report via CE 135.
  */
final class GuarantorService(
    chain: ChainManager,
    distribution: DistributionService,
    pools: ExtrinsicPools,
    validatorKeys: Seq[ValidatorKeySet],
    shardStore: Option[io.forge.jam.db.ShardStore] = None,
    segments: SegmentPool = new SegmentPool(),
    /** CE 146 cumulative-message cap; overridable so tests can exercise the
      * breach path cheaply. See [[GuarantorService.DefaultMaxSubmissionBytes]].
      */
    maxSubmissionBytes: Long = GuarantorService.DefaultMaxSubmissionBytes
) extends LazyLogging:

  private val computeReport = new ComputeReport(chain.config)
  private val accounts = new StateHistoricalLookup(chain)

  /** Refine/co-sign work happens off the netty event loops: PVM execution is
    * slow and CE 134 needs blocking round-trips to other peers.
    */
  private val executor = java.util.concurrent.Executors.newSingleThreadExecutor(r =>
    val t = new Thread(r, "jam-guarantor")
    t.setDaemon(true)
    t
  )

  /** Bounded (cap 64) insertion-evicting cache of full work-package bundles
    * this node holds — either its own refine output or a bundle re-executed
    * while co-signing for a peer guarantor via CE 134 — keyed by the
    * report's erasure root, so a peer's CE 147 request can be served
    * directly instead of falling back to shard reconstruction. Same
    * `LinkedHashMap` idiom as [[DistributionService.knownReports]].
    */
  private val heldBundles =
    java.util.Collections.synchronizedMap(
      new java.util.LinkedHashMap[Hash, Array[Byte]](64, 0.75f, false) {
        override def removeEldestEntry(e: java.util.Map.Entry[Hash, Array[Byte]]) = size > 64
      }
    )

  /** Record a bundle in the held-bundle cache, keyed by its erasure root, so
    * it can later be served over CE 147.
    */
  def recordBundle(erasureRoot: Hash, bundleBytes: Array[Byte]): Unit =
    heldBundles.put(erasureRoot, bundleBytes)

  /** CE 147: Erasure-Root (32) -> Bundle. */
  def bundleRequestHandler: io.forge.jam.network.StreamHandler = new io.forge.jam.network.StreamHandler:
    def onStream(connection: JamnpConnection, stream: JamnpStream): Unit =
      stream.onMessage { msg =>
        try
          if msg.length == 32 then
            Option(heldBundles.get(Hash(msg))).foreach(b => stream.send(b))
        catch case e: Exception => logger.warn(s"CE147 failed: ${e.getMessage}")
        stream.finish()
      }

  /** CE 148: repeated `(segments-root(32) ++ compact-len ++ [u16 LE index])`
    * groups; message 1 is every requested segment (`Csegmentsize` bytes
    * each) concatenated in request order, message 2 is the import-proof
    * list (`compact-len ++ [hash(32)]` per proof). Any unknown root or
    * out-of-range index for any group aborts the whole request (FIN, no
    * messages) rather than returning a partial result.
    *
    * TODO(proofs): message 2 is currently always an empty list —
    * generating proofs over the export Merkle tree is follow-up work;
    * acceptable for devnet, where peers re-verify a served segment by
    * re-executing the work package instead of trusting an unproven one.
    */
  def segmentRequestHandler: io.forge.jam.network.StreamHandler = new io.forge.jam.network.StreamHandler:
    def onStream(connection: JamnpConnection, stream: JamnpStream): Unit =
      stream.onMessage { msg =>
        try
          decodeSegmentRequest(msg) match
            case Some(groups) =>
              val resolved = groups.foldLeft(Option(IndexedSeq.empty[Array[Byte]])) { (acc, group) =>
                val (root, indices) = group
                acc.flatMap(collected => segments.get(root, indices).map(collected ++ _))
              }
              resolved match
                case Some(segs) =>
                  val out = new java.io.ByteArrayOutputStream()
                  segs.foreach(out.write)
                  stream.send(out.toByteArray)
                  stream.send(JamCodecs.encodeCompactInteger(0))
                  logger.info(
                    "CE148: served segments with an empty import-proof list " +
                      "(proof generation not yet implemented; peer must re-verify via re-execution)"
                  )
                case None => () // unknown root or out-of-range index anywhere in the request
            case None =>
              logger.warn("CE148: malformed segment request")
        catch case e: Exception => logger.warn(s"CE148 failed: ${e.getMessage}")
        stream.finish()
      }

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
    executor.shutdown()
    executor.awaitTermination(15, java.util.concurrent.TimeUnit.SECONDS)

  private val ed25519ByPublic: Map[Seq[Byte], ValidatorKeySet] =
    validatorKeys.map(k => k.ed25519Public.toSeq -> k).toMap

  /** CE 133 handler: message 1 = core index (2 bytes LE) ++ work package,
    * message 2 = concatenated extrinsic blobs, then FIN.
    */
  def workPackageSubmissionHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      val messages = mutable.ListBuffer.empty[Array[Byte]]
      stream.onMessage(messages += _)
      stream.onClosed { () =>
        executor.submit { (() =>
          try
            if messages.nonEmpty then
              val first = messages.head
              val coreIndex = ((first(0) & 0xff) | ((first(1) & 0xff) << 8))
              val wpBytes = java.util.Arrays.copyOfRange(first, 2, first.length)
              decodeWorkPackage(wpBytes) match
                case Left(err) =>
                  logger.warn(s"CE133: bad work package: $err")
                case Right(wp) =>
                  val extrinsicBlob = messages.drop(1).headOption.getOrElse(Array.emptyByteArray)
                  onWorkPackage(coreIndex, wp, extrinsicBlob)
          catch case e: Exception => logger.error("CE133 handling failed", e)
        ): Runnable }
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
  def bundleSubmissionHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      val messages = mutable.ListBuffer.empty[Array[Byte]]
      var dispatched = false
      var totalBytes = 0L

      def dispatch(): Unit =
        if !dispatched then
          dispatched = true
          val snapshot = messages.toIndexedSeq
          executor.submit { (() =>
            try
              decodeBundleSubmission(snapshot) match
                case Left(err) =>
                  logger.warn(s"CE146: $err")
                case Right((coreIndex, lookup, wp, extrinsicData, importSegments, justifications)) =>
                  guaranteeWorkPackage(coreIndex, wp, extrinsicData, importSegments, justifications, lookup)
            catch case e: Exception => logger.error("CE146 handling failed", e)
            finally stream.finish()
          ): Runnable }

      stream.onMessage { msg =>
        if !dispatched then
          totalBytes += msg.length
          if totalBytes > maxSubmissionBytes then
            dispatched = true // mark the stream dead: no further messages are buffered or dispatched
            logger.warn(
              s"CE146: cumulative submission size $totalBytes exceeds the $maxSubmissionBytes-byte cap; aborting"
            )
            stream.finish()
          else
            messages += msg
            if messages.size == 5 then dispatch()
      }
      stream.onClosed { () =>
        if messages.size >= 2 then dispatch()
        else if !dispatched then
          logger.warn(s"CE146: stream closed with only ${messages.size} message(s)")
          stream.finish()
      }

  /** Parse CE 146's 5-message wire format. Missing trailing messages (index
    * >= `messages.size`) are treated as empty, mirroring CE 133's handling
    * of its optional second message. `None`-shaped errors surface as `Left`
    * for the caller to log; nothing here throws.
    */
  private def decodeBundleSubmission(messages: IndexedSeq[Array[Byte]]): Either[
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
    summon[Codec[WorkPackage]].decode(ByteVector(bytes).bits) match
      case scodec.Attempt.Successful(res) if res.remainder.isEmpty => Right(res.value)
      case scodec.Attempt.Successful(_) => Left("trailing bytes")
      case scodec.Attempt.Failure(err)  => Left(err.message)

  /** Split the concatenated extrinsic blob into per-item lists using the
    * work items' declared (hash, len) references.
    */
  private def splitExtrinsics(
      wp: WorkPackage,
      blob: Array[Byte]
  ): Either[String, IndexedSeq[IndexedSeq[Array[Byte]]]] =
    var offset = 0
    val out = wp.items.map { item =>
      item.extrinsic.map { ref =>
        val len = ref.len.toInt
        if offset + len > blob.length then return Left("extrinsic data truncated")
        val data = java.util.Arrays.copyOfRange(blob, offset, offset + len)
        offset += len
        if Hashing.blake2b256(data) != ref.hash then
          return Left("extrinsic hash mismatch")
        data
      }.toIndexedSeq
    }.toIndexedSeq
    if offset != blob.length then Left("trailing extrinsic bytes")
    else Right(out)

  /** Guarantee a submitted work package: refine, sign, pool and distribute.
    * CE 133 has no cross-package imports on hand, so it always calls this
    * with empty imports/justifications and an empty segments-root lookup;
    * CE 146 (`bundleSubmissionHandler`) calls [[guaranteeWorkPackage]]
    * directly with the richer inputs decoded from the builder's bundle.
    */
  def onWorkPackage(coreIndex: Int, wp: WorkPackage, extrinsicBlob: Array[Byte]): Unit =
    splitExtrinsics(wp, extrinsicBlob) match
      case Left(err) =>
        logger.warn(s"work package rejected: $err")
      case Right(extrinsicData) =>
        val importSegments = wp.items.map(_ => IndexedSeq.empty[Array[Byte]]).toIndexedSeq
        val justifications = wp.items.map(_ => IndexedSeq.empty[List[Array[Byte]]]).toIndexedSeq
        guaranteeWorkPackage(coreIndex, wp, extrinsicData, importSegments, justifications, Map.empty)

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
      segmentRootLookup: Map[Hash, Hash]
  ): Unit =
    computeReport.compute(
      workPackage = wp,
      coreIndex = coreIndex,
      segmentRootLookup = segmentRootLookup,
      importSegments = importSegments,
      extrinsicData = extrinsicData,
      justifications = justifications,
      accounts = accounts
    ) match
      case Left(err) =>
        logger.warn(s"work package refused: $err")
      case Right(computed) =>
        storeShards(computed)
        val slot = chain.best.slot + 1
        signAndDistribute(computed, slot)

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
          all.foreach(s => store.put(root, s.validatorIndex, s.encode))
          logger.debug(s"custodied ${all.size} shard sets for ${root.toHex.take(18)}")
        case Left(err) =>
          logger.error(s"shard construction failed (report still distributed): $err")
    }

  /** Every held key assigned to `coreIndex` at `slot`, with its active-set
    * validator index.
    */
  private def ownAssignedKeys(slot: Long, coreIndex: Int): List[(Int, ValidatorKeySet)] =
    val view = chain.stateView()
    val assignments = GuarantorService.coreAssignments(view.entropy.pool(2), slot, chain.config)
    view.validators.current.zipWithIndex.flatMap { case (vk, idx) =>
      if assignments(idx) == coreIndex then
        ed25519ByPublic.get(vk.ed25519.bytes.toArray.toSeq).map(idx -> _)
      else None
    }

  private def signAndDistribute(
      computed: ComputedReport,
      slot: Long
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

    // Top up to the required >= 2 credentials via CE 134 co-signing.
    val signatures =
      (if ownSignatures.size >= 2 then ownSignatures
       else ownSignatures ++ requestCoSignatures(computed, slot, reportHash, ownSignatures.map(_.validatorIndex.value.toInt).toSet))
        .distinctBy(_.validatorIndex.value.toInt)
        .sortBy(_.validatorIndex.value.toInt)

    if signatures.size < 2 then
      logger.warn(
        s"insufficient credentials for the report (have ${signatures.size} after CE 134); dropping"
      )
    else
      val guarantee =
        GuaranteeExtrinsic(report, Timeslot(UInt(slot.toInt)), signatures.take(3))
      pools.addGuarantee(guarantee)
      distribution.recordReport(report)
      distribution.distributeGuaranteeToAll(guarantee)
      logger.info(
        s"guaranteed package ${report.packageSpec.hash.toHex.take(18)} on core ${report.coreIndex.toInt} " +
          s"with ${signatures.size} credentials"
      )

  // =========================================================================
  // CE 134 — work-package sharing (co-signing)
  // =========================================================================

  /** Responder: re-execute the shared bundle and return signatures from every
    * held key assigned to the core.
    */
  def workPackageSharingHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        executor.submit { (() =>
          try respondCoSign(stream, msg)
          catch case e: Exception => logger.error("CE134 handling failed", e)
        ): Runnable }
      }

  private def respondCoSign(stream: JamnpStream, msg: Array[Byte]): Unit =
    val slot = (msg(0) & 0xffL) | ((msg(1) & 0xffL) << 8) |
      ((msg(2) & 0xffL) << 16) | ((msg(3) & 0xffL) << 24)
    val coreIndex = (msg(4) & 0xff) | ((msg(5) & 0xff) << 8)
    var offset = 6
    val (n, c) = JamCodecs.decodeCompactInteger(msg, offset)
    offset += c
    val lookup = (0 until n.toInt).map { _ =>
      val wpHash = Hash(java.util.Arrays.copyOfRange(msg, offset, offset + 32))
      val segRoot = Hash(java.util.Arrays.copyOfRange(msg, offset + 32, offset + 64))
      offset += 64
      wpHash -> segRoot
    }.toMap
    val bundleBytes = java.util.Arrays.copyOfRange(msg, offset, msg.length)

    val held = ownAssignedKeys(slot, coreIndex)
    if held.isEmpty then
      logger.debug(s"CE134: no key assigned to core $coreIndex at slot $slot; ignoring")
      stream.finish()
      return

    val result = for
      bundle <- WorkPackageBundle.decode(bundleBytes)
      computed <- computeReport
        .compute(
          workPackage = bundle.workPackage,
          coreIndex = coreIndex,
          segmentRootLookup = lookup,
          importSegments = bundle.importSegments,
          extrinsicData = bundle.extrinsicData,
          justifications = bundle.justifications,
          accounts = accounts
        )
        .left
        .map(e => s"re-execution refused: $e")
    yield computed

    result match
      case Left(err) =>
        logger.warn(s"CE134: cannot co-sign: $err")
        stream.finish()
      case Right(computed) =>
        storeShards(computed) // co-guarantors custody shards too
        val reportHash = Hashing.blake2b256(computed.report.encode.toArray)
        val message = constants.JAM_GUARANTEE_BYTES ++ reportHash.bytes
        val out = new java.io.ByteArrayOutputStream()
        out.write(reportHash.bytes.toArray)
        out.write(JamCodecs.encodeCompactInteger(held.length.toLong))
        held.foreach { (idx, keys) =>
          out.write(idx & 0xff)
          out.write((idx >> 8) & 0xff)
          out.write(Ed25519ZebraWrapper.sign(keys.ed25519Secret, message))
        }
        stream.send(out.toByteArray)
        stream.finish()
        logger.info(s"CE134: co-signed report ${reportHash.toHex.take(18)} with ${held.size} key(s)")

  /** Initiator: ask every peer for co-signatures, accepting only signatures
    * from core-assigned validators over our own report hash.
    */
  private def requestCoSignatures(
      computed: ComputedReport,
      slot: Long,
      reportHash: Hash,
      alreadyHave: Set[Int]
  ): List[GuaranteeSignature] =
    val report = computed.report
    recordBundle(report.packageSpec.erasureRoot, computed.bundleBytes)
    val view = chain.stateView()
    val assignments = GuarantorService.coreAssignments(view.entropy.pool(2), slot, chain.config)
    val activeSet = view.validators.current
    val message = constants.JAM_GUARANTEE_BYTES ++ reportHash.bytes

    val request = new java.io.ByteArrayOutputStream(computed.bundleBytes.length + 64)
    request.write((slot & 0xff).toInt); request.write(((slot >> 8) & 0xff).toInt)
    request.write(((slot >> 16) & 0xff).toInt); request.write(((slot >> 24) & 0xff).toInt)
    request.write(report.coreIndex.toInt & 0xff)
    request.write((report.coreIndex.toInt >> 8) & 0xff)
    request.write(JamCodecs.encodeCompactInteger(report.segmentRootLookup.length.toLong))
    report.segmentRootLookup.foreach { l =>
      request.write(l.workPackageHash.bytes.toArray)
      request.write(l.segmentTreeRoot.bytes.toArray)
    }
    request.write(computed.bundleBytes)
    val requestBytes = request.toByteArray

    val collected = mutable.ListBuffer.empty[GuaranteeSignature]
    val seen = mutable.Set.empty[Int] ++ alreadyHave
    distribution.peers.iterator.takeWhile(_ => seen.size < 3).foreach { conn =>
      coSignRoundTrip(conn, requestBytes).foreach { response =>
        try
          val theirHash = Hash(java.util.Arrays.copyOfRange(response, 0, 32))
          if theirHash == reportHash then
            var off = 32
            val (n, c) = JamCodecs.decodeCompactInteger(response, off)
            off += c
            (0 until n.toInt).foreach { _ =>
              val idx = (response(off) & 0xff) | ((response(off + 1) & 0xff) << 8)
              val sig = java.util.Arrays.copyOfRange(response, off + 2, off + 66)
              off += 66
              val valid = !seen.contains(idx) &&
                assignments.lift(idx).contains(report.coreIndex.toInt) &&
                activeSet.lift(idx).exists(vk =>
                  Ed25519.verify(vk.ed25519.bytes.toArray, message, sig)
                )
              if valid then
                seen += idx
                collected += GuaranteeSignature(ValidatorIndex(idx), Ed25519Signature(sig))
            }
          else logger.warn(s"CE134: peer computed a different report; rejecting its signatures")
        catch case e: Exception => logger.warn(s"CE134: bad response: ${e.getMessage}")
      }
    }
    collected.toList

  /** One CE 134 request/response round trip. */
  private def coSignRoundTrip(
      conn: io.forge.jam.network.JamnpConnection,
      request: Array[Byte]
  ): Option[Array[Byte]] =
    try
      val stream = conn
        .openStream(io.forge.jam.network.StreamKind.WorkPackageSharing)
        .get(10, java.util.concurrent.TimeUnit.SECONDS)
      val response = new java.util.concurrent.CompletableFuture[Array[Byte]]()
      stream.onMessage(response.complete(_))
      stream.onClosed(() => response.complete(null)) // peer declined/lacks the protocol
      stream.send(request)
      val r = response.get(30, java.util.concurrent.TimeUnit.SECONDS)
      stream.finish()
      Option(r)
    catch
      case e: Exception =>
        logger.debug(s"CE134 round trip failed: ${e.getMessage}")
        None

object GuarantorService:
  /** CE 146's cumulative-across-messages byte cap (see `bundleSubmissionHandler`).
    * 48 MiB comfortably covers a legitimate submission (a realistic work
    * package + extrinsics + a handful of `Csegmentsize` import segments +
    * proofs) while bounding the per-stream — and, multiplied by concurrent
    * streams, per-node — memory a misbehaving or malicious builder can force
    * the node to buffer before any structural validation runs.
    */
  val DefaultMaxSubmissionBytes: Long = 48L * 1024 * 1024

  /** Validator→core assignment (gp guarantor rotation). */
  def coreAssignments(
      randomness: Hash,
      slot: Long,
      config: io.forge.jam.core.ChainConfig
  ): List[Int] =
    val source =
      (0 until config.validatorCount)
        .map(i => (config.coresCount * i) / config.validatorCount)
        .toList
    val shuffled = io.forge.jam.core.Shuffle.jamComputeShuffle(config.validatorCount, randomness)
    val shift = (math.floorMod(slot, config.epochLength) / config.rotationPeriod).toInt
    shuffled.map(idx => math.floorMod(source(idx) + shift, config.coresCount))
