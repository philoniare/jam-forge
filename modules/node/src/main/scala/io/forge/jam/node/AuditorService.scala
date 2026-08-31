package io.forge.jam.node

import java.util.concurrent.ConcurrentHashMap

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, constants}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.crypto.Ed25519ZebraWrapper
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamKind}
import io.forge.jam.protocol.refine.{ComputeReport, WorkPackageBundle}
import io.forge.jam.vrfs.BandersnatchWrapper

import scala.jdk.CollectionConverters.*

/** Auditor role: re-executes every work report
  * that lands on-chain and publishes the outcome.
  *
  * Per imported block, for each included guarantee: reconstruct the audit
  * bundle, re-run the full in-core pipeline; mismatches additionally
  * publish a negative judgment via CE 145 and pool it.
  */
final class AuditorService(
    chain: ChainManager,
    distribution: DistributionService,
    shards: ShardService,
    pools: ExtrinsicPools,
    validatorKeys: Seq[ValidatorKeySet]
) extends LazyLogging:

  private val computeReport = new ComputeReport(chain.config)
  private val accounts = new StateHistoricalLookup(chain)

  /** reportHash -> audit verdict from this node's re-execution. */
  private val verdicts = new ConcurrentHashMap[Hash, Boolean]()
  /** reportHash -> announcing validator indexes seen via CE 144. */
  private val seenAnnouncements = new ConcurrentHashMap[Hash, java.util.Set[Integer]]()

  def verdictFor(reportHash: Hash): Option[Boolean] = Option(verdicts.get(reportHash))
  def announcementsFor(reportHash: Hash): Int =
    Option(seenAnnouncements.get(reportHash)).map(_.size).getOrElse(0)

  private def ed25519ByPublic: Map[Seq[Byte], ValidatorKeySet] =
    validatorKeys.map(k => k.ed25519Public.toSeq -> k).toMap

  /** Import listener: audit every report included in this block. */
  def onImported(head: ChainManager#Head, block: Block): Unit =
    if block.extrinsic.guarantees.isEmpty then return
    val audited = block.extrinsic.guarantees.map { g =>
      val reportHash = Hashing.blake2b256(g.report.encode.toArray)
      val valid = auditReport(g.report)
      verdicts.put(reportHash, valid)
      if !valid then publishJudgment(reportHash, valid = false)
      (g.report.coreIndex.toInt, reportHash, valid)
    }
    announce(head.hash, audited)

  /** Re-execute a report's in-core pipeline; true when the recomputed report
    * hash matches.
    */
  def auditReport(report: WorkReport): Boolean =
    val spec = report.packageSpec
    val result =
      for
        bundleBytes <- shards.reconstructBundle(
          spec.erasureRoot,
          spec.length.toLong.toInt,
          distribution.peers
        )
        bundle <- WorkPackageBundle.decode(bundleBytes)
        recomputed <- computeReport
          .compute(
            workPackage = bundle.workPackage,
            coreIndex = report.coreIndex.toInt,
            segmentRootLookup =
              report.segmentRootLookup.map(l => l.workPackageHash -> l.segmentTreeRoot).toMap,
            importSegments = bundle.importSegments,
            extrinsicData = bundle.extrinsicData,
            justifications = bundle.justifications,
            accounts = accounts
          )
          .left
          .map(e => s"re-execution refused: $e")
      yield Hashing.blake2b256(recomputed.report.encode.toArray) ==
        Hashing.blake2b256(report.encode.toArray)
    result match
      case Right(matches) =>
        if !matches then
          logger.warn(s"AUDIT MISMATCH for package ${spec.hash.toHex.take(18)}")
        matches
      case Left(err) =>
        // Irretrievable or undecodable bundle is an audit failure.
        logger.warn(s"audit could not re-execute ${spec.hash.toHex.take(18)}: $err")
        false

  // =========================================================================
  // CE 144 — audit announcements
  // =========================================================================

  private def announce(headHash: Hash, audited: Seq[(Int, Hash, Boolean)]): Unit =
    withOwnIndex { (idx, keys) =>
      val body = new java.io.ByteArrayOutputStream()
      body.write(headHash.bytes.toArray)
      body.write(0) // tranche 0
      body.write(idx & 0xff)
      body.write((idx >> 8) & 0xff)
      body.write(JamCodecs.encodeCompactInteger(audited.length.toLong))
      audited.foreach { case (core, reportHash, _) =>
        body.write(core & 0xff)
        body.write((core >> 8) & 0xff)
        body.write(reportHash.bytes.toArray)
      }
      val bodyBytes = body.toByteArray
      val evidence = BandersnatchWrapper.ietfVrfSign(
        keys.bandersnatchSecret,
        "jam_audit".getBytes("UTF-8") ++ headHash.bytes.toArray,
        bodyBytes
      )
      val msg = bodyBytes ++ evidence
      distribution.peers.foreach(sendOneShot(_, StreamKind.AuditAnnouncement, msg))
    }

  def auditAnnouncementHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        try
          var off = 32 + 1 // header hash ++ tranche
          val idx = (msg(off) & 0xff) | ((msg(off + 1) & 0xff) << 8)
          off += 2
          val (n, c) = JamCodecs.decodeCompactInteger(msg, off)
          off += c
          (0 until n.toInt).foreach { _ =>
            off += 2 // core index
            val reportHash = Hash(java.util.Arrays.copyOfRange(msg, off, off + 32))
            off += 32
            seenAnnouncements
              .computeIfAbsent(reportHash, _ => ConcurrentHashMap.newKeySet[Integer]())
              .add(idx)
          }
          logger.debug(s"audit announcement from validator $idx (${n} report(s))")
        catch case e: Exception => logger.warn(s"bad CE144 payload: ${e.getMessage}")
        stream.finish()
      }

  // =========================================================================
  // CE 145 — judgment publication
  // =========================================================================

  private def publishJudgment(reportHash: Hash, valid: Boolean): Unit =
    withOwnIndex { (idx, keys) =>
      val context = if valid then constants.JAM_VALID_BYTES else constants.JAM_INVALID_BYTES
      val signature = Ed25519ZebraWrapper.sign(keys.ed25519Secret, context ++ reportHash.bytes)
      pools.addJudgment(reportHash, idx, valid, signature)
      val epoch = (chain.best.slot / chain.config.epochLength).toInt
      val msg = new java.io.ByteArrayOutputStream(103)
      msg.write(epoch & 0xff); msg.write((epoch >> 8) & 0xff)
      msg.write((epoch >> 16) & 0xff); msg.write((epoch >> 24) & 0xff)
      msg.write(idx & 0xff); msg.write((idx >> 8) & 0xff)
      msg.write(if valid then 1 else 0)
      msg.write(reportHash.bytes.toArray)
      msg.write(signature)
      distribution.peers.foreach(sendOneShot(_, StreamKind.JudgmentPublication, msg.toByteArray))
      logger.warn(s"published ${if valid then "positive" else "NEGATIVE"} judgment for ${reportHash.toHex.take(18)}")
    }

  def judgmentHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        if msg.length == 4 + 2 + 1 + 32 + 64 then
          val idx = (msg(4) & 0xff) | ((msg(5) & 0xff) << 8)
          val valid = msg(6) == 1
          val reportHash = Hash(java.util.Arrays.copyOfRange(msg, 7, 39))
          val signature = java.util.Arrays.copyOfRange(msg, 39, 103)
          val context = if valid then constants.JAM_VALID_BYTES else constants.JAM_INVALID_BYTES
          val signer = chain.stateView().validators.current.lift(idx)
          val genuine = signer.exists(vk =>
            io.forge.jam.crypto.Ed25519.verify(
              vk.ed25519.bytes.toArray,
              context ++ reportHash.bytes,
              signature
            )
          )
          if genuine then
            pools.addJudgment(reportHash, idx, valid, signature)
            logger.warn(s"pooled ${if valid then "positive" else "negative"} judgment from validator $idx")
          else logger.warn(s"rejected judgment with bad signature from claimed validator $idx")
        else logger.warn(s"bad CE145 length ${msg.length}")
        stream.finish()
      }

  // =========================================================================
  // helpers
  // =========================================================================

  /** Run `f` with this node's announcing identity: the lowest active-set
    * index whose Ed25519 key we hold.
    */
  private def withOwnIndex(f: (Int, ValidatorKeySet) => Unit): Unit =
    val byPublic = ed25519ByPublic
    chain
      .stateView()
      .validators
      .current
      .zipWithIndex
      .flatMap((vk, idx) => byPublic.get(vk.ed25519.bytes.toArray.toSeq).map(idx -> _))
      .headOption
      .foreach(f.tupled)

  private def sendOneShot(conn: JamnpConnection, kind: Byte, payload: Array[Byte]): Unit =
    try
      val stream = conn.openStream(kind).get(10, java.util.concurrent.TimeUnit.SECONDS)
      stream.send(payload)
      stream.finish()
    catch
      case e: Exception =>
        logger.warn(s"send on ${StreamKind.name(kind)} failed: ${e.getMessage}")
