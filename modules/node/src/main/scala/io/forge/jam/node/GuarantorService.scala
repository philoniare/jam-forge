package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes, constants}
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
    shardStore: Option[io.forge.jam.db.ShardStore] = None
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

  /** Guarantee a submitted work package: refine, sign, pool and distribute. */
  def onWorkPackage(coreIndex: Int, wp: WorkPackage, extrinsicBlob: Array[Byte]): Unit =
    splitExtrinsics(wp, extrinsicBlob) match
      case Left(err) =>
        logger.warn(s"work package rejected: $err")
      case Right(extrinsicData) =>
        // Imports/justifications: segment fetching arrives with the DA work.
        val importSegments = wp.items.map(_ => IndexedSeq.empty[Array[Byte]]).toIndexedSeq
        val justifications = wp.items.map(_ => IndexedSeq.empty[List[Array[Byte]]]).toIndexedSeq

        computeReport.compute(
          workPackage = wp,
          coreIndex = coreIndex,
          segmentRootLookup = Map.empty,
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
