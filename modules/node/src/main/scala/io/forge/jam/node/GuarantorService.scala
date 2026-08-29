package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes, constants}
import io.forge.jam.core.primitives.{Ed25519Signature, Hash, Timeslot, ValidatorIndex}
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.types.dispute.GuaranteeSignature
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.crypto.Ed25519ZebraWrapper
import io.forge.jam.network.{JamnpConnection, JamnpStream}
import io.forge.jam.protocol.accumulation.StateKey
import io.forge.jam.protocol.refine.{ComputeReport, HistoricalLookupService}
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
  * with this node's assigned validator keys, and distributes the guaranteed
  * report via CE 135.
  *
  * Interim scope: a single guarantor node holding several assigned validator
  * keys provides the ≥ 2 credentials itself; CE 134 co-signing between
  * distinct guarantor nodes and CE 137 shard distribution follow.
  */
final class GuarantorService(
    chain: ChainManager,
    distribution: DistributionService,
    pools: ExtrinsicPools,
    validatorKeys: Seq[ValidatorKeySet]
) extends LazyLogging:

  private val computeReport = new ComputeReport(chain.config)
  private val accounts = new StateHistoricalLookup(chain)

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
            val slot = chain.best.slot + 1
            signAndDistribute(computed.report, slot)

  private def signAndDistribute(
      report: io.forge.jam.core.types.workpackage.WorkReport,
      slot: Long
  ): Unit =
    val view = chain.stateView()
    val reportHash = Hashing.blake2b256(report.encode.toArray)
    val message = constants.JAM_GUARANTEE_BYTES ++ reportHash.bytes

    // Sign with every assigned validator key we hold for this core.
    val assignments = GuarantorService.coreAssignments(
      view.entropy.pool(2),
      slot,
      chain.config
    )
    val activeSet = view.validators.current
    val signatures = activeSet.zipWithIndex.flatMap { case (vk, idx) =>
      if assignments(idx) == report.coreIndex.toInt then
        ed25519ByPublic.get(vk.ed25519.bytes.toArray.toSeq).map { keys =>
          GuaranteeSignature(
            ValidatorIndex(idx),
            Ed25519Signature(Ed25519ZebraWrapper.sign(keys.ed25519Secret, message))
          )
        }
      else None
    }.sortBy(_.validatorIndex.value.toInt)

    if signatures.size < 2 then
      logger.warn(
        s"insufficient assigned keys to credential the report (have ${signatures.size}); " +
          "CE 134 co-signing not yet implemented"
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
