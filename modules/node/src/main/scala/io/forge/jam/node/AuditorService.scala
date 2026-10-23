package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, ExecutorService, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, constants}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.crypto.{BandersnatchVrf, Ed25519, Ed25519ZebraWrapper}
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamKind}
import io.forge.jam.protocol.refine.{ComputeReport, WorkPackageBundle}
import io.forge.jam.vrfs.BandersnatchWrapper

import scala.collection.mutable

/** Auditor role. For every imported block the reports that just became available (W) are
  * tracked; each validator key we hold in the block's prior set κ draws its tranche-0 cores with the VRF s_0
  * (eq:initialaudit) and, every `C_tranche-seconds` after the block's slot, may join later tranches for reports
  * with no-shows or a negative judgment (eq:latertranches). Announcements go out on CE 144 and judgments on CE 145,
  * positive as well as negative. A block is audited once every report in its W is (U(w)); the GRANDPA voter asks
  * [[isAudited]] before voting for a block, and a block whose report a third of the validators judge invalid is
  * disregarded by fork choice.
  */
final class AuditorService(
    chain: ChainManager,
    distribution: DistributionService,
    shards: ShardService,
    pools: ExtrinsicPools,
    validatorKeys: Seq[ValidatorKeySet],
    egress: ExecutorService,
    /** Wall-clock millis at which tranche 0 of a block starts, from its slot and import time. */
    trancheOrigin: (Long, Long) => Long = (_, importedAt) => importedAt,
    clock: () => Long = () => System.currentTimeMillis(),
    tickMillis: Long = 500L
) extends LazyLogging:

  import AuditSelection.*

  private val computeReport = new ComputeReport(chain.config)
  private val accounts = new StateHistoricalLookup(chain)
  private val verdicts = new ConcurrentHashMap[Hash, java.lang.Boolean]()

  /** A block being audited: its prior validator set κ, Y(H_v) and the reports q (eq:auditselection). */
  private final class Tracked(
      val hash: Hash,
      val slot: Long,
      val origin: Long,
      val vrfOutput: Array[Byte],
      val validators: IndexedSeq[Array[Byte]],
      val bandersnatch: IndexedSeq[Array[Byte]],
      val q: IndexedSeq[Option[WorkReport]],
      val reportHashes: IndexedSeq[Option[Hash]]
  ):
    /** A_n(w): validators announcing each report, per tranche. */
    val announced = mutable.Map.empty[(Hash, Int), mutable.Set[Int]]

    /** The announcement each validator sent in each tranche (for no-show evidence). */
    val announcements = mutable.Map.empty[(Int, Int), Announcement]

    /** Reports each of our validator indices has announced, with the tranche. */
    val ours = mutable.Map.empty[Int, mutable.Map[Hash, Int]]
    var tranche0Done: Set[Int] = Set.empty
    var lastTranche: Int = -1
    def available: Seq[(Int, WorkReport, Hash)] =
      q.indices.flatMap(c => q(c).zip(reportHashes(c)).map((w, h) => (c, w, h)))

  /** Every access to the tracking state below runs on this thread. */
  private val exec = java.util.concurrent.Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "jam-audit"); t.setDaemon(true); t
  }
  private val workers = java.util.concurrent.Executors.newSingleThreadExecutor { r =>
    val t = new Thread(r, "jam-audit-exec"); t.setDaemon(true); t
  }
  private val tracked = mutable.LinkedHashMap.empty[Hash, Tracked]
  private val positive = mutable.Map.empty[Hash, mutable.Set[Int]]
  private val negative = mutable.Map.empty[Hash, mutable.Set[Int]]
  private val inFlight = mutable.Set.empty[Hash]
  private val pendingAnnouncements = mutable.Map.empty[Hash, mutable.ListBuffer[(Array[Byte], Array[Byte], Array[Byte])]]

  /** Readable from any thread: whether each tracked block is audited. Blocks we never tracked count as audited. */
  private val auditedBlocks = new ConcurrentHashMap[Hash, java.lang.Boolean]()
  private val announcementCounts = new ConcurrentHashMap[Hash, java.util.Set[Integer]]()
  @volatile private var disregard: Hash => Unit = _ => ()

  exec.scheduleWithFixedDelay(() => guarded(tick()), tickMillis, tickMillis, TimeUnit.MILLISECONDS)

  def verdictFor(reportHash: Hash): Option[Boolean] =
    Option(verdicts.get(reportHash)).map(_.booleanValue)
  def announcementsFor(reportHash: Hash): Int =
    Option(announcementCounts.get(reportHash)).map(_.size).getOrElse(0)

  /** isAudited for block `hash`. */
  def isAudited(hash: Hash): Boolean = auditedBlocks.getOrDefault(hash, java.lang.Boolean.TRUE).booleanValue

  /** Called with a block whose report a third or more of the validators judged invalid. */
  def onDisregard(f: Hash => Unit): Unit = disregard = f

  def shutdown(): Unit =
    exec.shutdownNow()
    workers.shutdownNow()

  private def guarded(f: => Unit): Unit =
    try f
    catch case e: Exception => logger.warn(s"auditor step failed: $e")

  private def ed25519ByPublic: Map[Seq[Byte], ValidatorKeySet] =
    validatorKeys.map(k => k.ed25519Public.toSeq -> k).toMap

  /** Import listener: track the reports that just became available in `block`. Runs on the import bus, so the
    * block's audited flag is set before any later listener (the GRANDPA voter) can ask for it.
    */
  def onImported(head: ChainManager#Head, block: Block): Unit =
    if head.slot <= chain.finalized.slot then return
    val parent = block.header.parent
    chain.stateViewAt(parent) match
      case None => ()
      case Some(prior) =>
        val kappa = prior.validators.current.toIndexedSeq
        val q = justBecameAvailable(block, prior.cores.reports.toIndexedSeq, kappa.size)
        val vrfOut = BandersnatchVrf.getVrfOutput(block.header.entropySource.toArray).getOrElse(Array.emptyByteArray)
        val t = new Tracked(
          head.hash,
          head.slot,
          trancheOrigin(head.slot, clock()),
          vrfOut,
          kappa.map(_.ed25519.bytes.toArray),
          kappa.map(_.bandersnatch.bytes.toArray),
          q,
          q.map(_.map(w => Hashing.blake2b256(w.encode.toArray)))
        )
        if t.available.nonEmpty then
          auditedBlocks.put(head.hash, java.lang.Boolean.FALSE)
          exec.execute(() =>
            guarded {
              tracked.put(t.hash, t)
              pendingAnnouncements.remove(t.hash).foreach(_.foreach((p, h, e) => applyAnnouncement(p, h, e)))
              tick()
            }
          )

  /** q of eq:auditselection: per core, the report in ρ that this block's assurances made available. */
  private def justBecameAvailable(
      block: Block,
      rho: IndexedSeq[Option[io.forge.jam.core.types.workpackage.AvailabilityAssignment]],
      validatorCount: Int
  ): IndexedSeq[Option[WorkReport]] =
    val counts = new Array[Int](rho.size)
    block.extrinsic.assurances.foreach { a =>
      val bits = a.bitfield.toArray
      rho.indices.foreach { c =>
        if c / 8 < bits.length && (bits(c / 8) & (1 << (c % 8))) != 0 then counts(c) += 1
      }
    }
    rho.indices.map(c => rho(c).filter(_ => 3 * counts(c) > 2 * validatorCount).map(_.report))

  // ---------------------------------------------------------------------------
  // tranches
  // ---------------------------------------------------------------------------

  private def tick(): Unit =
    val finalizedSlot = chain.finalized.slot
    val now = clock()
    forgetFinalized(finalizedSlot, now)
    tracked.values.foreach { t =>
      val n = trancheAt(now, t.origin)
      ownIndices(t).foreach { (v, keys) =>
        if !t.tranche0Done.contains(v) then
          t.tranche0Done += v
          initialTranche(t, v, keys)
        if n > 0 then laterTranche(t, v, keys, n)
      }
      t.lastTranche = n
      updateAudited(t, n)
    }

  /** Our keys in the block's prior validator set, by index. */
  private def ownIndices(t: Tracked): Seq[(Int, ValidatorKeySet)] =
    val byPublic = ed25519ByPublic
    t.validators.zipWithIndex.flatMap((k, i) => byPublic.get(k.toSeq).map(i -> _))

  private def initialTranche(t: Tracked, v: Int, keys: ValidatorKeySet): Unit =
    val s0 = BandersnatchWrapper.ietfVrfSign(keys.bandersnatchSecret, initialInput(t.vrfOutput), Array.emptyByteArray)
    BandersnatchVrf.getVrfOutput(s0).foreach { y =>
      val cores = initialCores(t.q, y)
      val items = cores.sorted.flatMap(c => t.reportHashes(c).map(c -> _))
      if items.nonEmpty then
        announce(t, v, keys, 0, items, encodeFirstEvidence(s0))
        logger.info(s"audit tranche 0 for ${t.hash.toHex.take(18)}: validator $v audits core(s) ${cores.mkString(",")}")
        items.foreach((_, h) => startAudit(t, h))
    }

  private def laterTranche(t: Tracked, v: Int, keys: ValidatorKeySet, n: Int): Unit =
    val mine = t.ours.getOrElseUpdate(v, mutable.Map.empty)
    val picks = t.available.filterNot((_, _, h) => mine.contains(h)).flatMap { (c, _, h) =>
      val prior = t.announced.getOrElse((h, n - 1), mutable.Set.empty[Int])
      val noShows = (prior -- positive.getOrElse(h, mutable.Set.empty)).toSeq.sorted
      val forced = negative.get(h).exists(_.nonEmpty)
      if !forced && noShows.isEmpty then None
      else
        val sig = BandersnatchWrapper.ietfVrfSign(
          keys.bandersnatchSecret,
          laterInput(t.vrfOutput, h, n),
          Array.emptyByteArray
        )
        val out = BandersnatchVrf.getVrfOutput(sig)
        if forced || out.exists(y => escalates(y, t.validators.size, noShows.size)) then
          val evidence = noShows.flatMap(i => t.announcements.get((n - 1, i)).map(NoShow(i, _)))
          Some((c, h, sig, evidence))
        else None
    }
    if picks.nonEmpty then
      val items = picks.map((c, h, _, _) => c -> h)
      announce(t, v, keys, n, items, encodeLaterEvidence(picks.map((_, _, s, e) => s -> e)))
      logger.info(
        s"audit tranche $n for ${t.hash.toHex.take(18)}: validator $v joins for core(s) ${items.map(_._1).mkString(",")}"
      )
      items.foreach((_, h) => startAudit(t, h))

  /** Sign, record and broadcast our announcement for tranche `n` (CE 144). */
  private def announce(
      t: Tracked,
      v: Int,
      keys: ValidatorKeySet,
      n: Int,
      items: Seq[(Int, Hash)],
      evidence: Array[Byte]
  ): Unit =
    val sig = Ed25519ZebraWrapper.sign(keys.ed25519Secret, announcementMessage(n, items, t.hash))
    val a = Announcement(items, sig)
    val mine = t.ours.getOrElseUpdate(v, mutable.Map.empty)
    items.foreach((_, h) => mine.put(h, n))
    recordAnnouncement(t, v, n, a)
    val head = encodeHead(t.hash, n, a)
    distribution.peers.foreach(c => sendTwo(c, StreamKind.AuditAnnouncement, head, evidence))

  private def recordAnnouncement(t: Tracked, v: Int, n: Int, a: Announcement): Unit =
    t.announcements.put((n, v), a)
    a.items.foreach { (_, h) =>
      t.announced.getOrElseUpdate((h, n), mutable.Set.empty) += v
      announcementCounts.computeIfAbsent(h, _ => ConcurrentHashMap.newKeySet[Integer]()).add(v)
    }

  /** Re-execute `reportHash` once, off the audit thread, then judge it. */
  private def startAudit(t: Tracked, reportHash: Hash): Unit =
    val report = t.available.collectFirst { case (_, w, h) if h == reportHash => w }
    report.foreach { w =>
      Option(verdicts.get(reportHash)) match
        case Some(valid) => judge(t, reportHash, valid.booleanValue)
        case None if inFlight.add(reportHash) =>
          workers.execute(() =>
            val outcome =
              try auditOutcome(w)
              catch
                case e: Exception =>
                  logger.warn(s"audit of ${reportHash.toHex.take(18)} failed: $e"); Some(false)
            outcome match
              case Some(valid) =>
                verdicts.put(reportHash, java.lang.Boolean.valueOf(valid))
                exec.execute(() =>
                  guarded {
                    inFlight -= reportHash
                    tracked.values.filter(_.reportHashes.contains(Some(reportHash))).foreach(judge(_, reportHash, valid))
                  }
                )
              case None =>
                // Not enough shards reachable yet (e.g. we are cut off): no judgment, try again while still tracked.
                exec.schedule(
                  (() =>
                    guarded {
                      inFlight -= reportHash
                      tracked.values.find(_.reportHashes.contains(Some(reportHash))).foreach(startAudit(_, reportHash))
                    }
                  ): Runnable,
                  AuditSelection.TrancheSeconds.toLong,
                  TimeUnit.SECONDS
                )
          )
        case None => ()
    }

  /** Stop auditing finalized blocks once audited, or after `GiveUpTranches` without: a node that was cut off missed
    * the others' announcements and judgments, which are never resent, so it would retry forever and keep their state.
    */
  private def forgetFinalized(finalizedSlot: Long, now: Long): Unit =
    val done = tracked.values
      .filter(t => t.slot <= finalizedSlot && (isAudited(t.hash) || trancheAt(now, t.origin) >= GiveUpTranches))
      .toList
    if done.nonEmpty then
      done.foreach(t => tracked.remove(t.hash))
      val live = tracked.values.flatMap(_.available.map(_._3)).toSet
      done.foreach { t =>
        if !isAudited(t.hash) then logger.debug(s"block ${t.hash.toHex.take(18)} finalized before we saw it audited")
        auditedBlocks.remove(t.hash)
        t.available.map(_._3).filterNot(live.contains).foreach { h =>
          positive.remove(h); negative.remove(h); verdicts.remove(h); announcementCounts.remove(h)
        }
      }

  /** Publish our judgment on `reportHash` for every key of ours that announced it in `t`. */
  private def judge(t: Tracked, reportHash: Hash, valid: Boolean): Unit =
    ownIndices(t).foreach { (v, keys) =>
      val announcedIt = t.ours.get(v).exists(_.contains(reportHash))
      val judged = (if valid then positive else negative).get(reportHash).exists(_.contains(v))
      if announcedIt && !judged then publishJudgment(reportHash, v, keys, valid)
    }
    updateAudited(t, t.lastTranche)

  private def updateAudited(t: Tracked, n: Int): Unit =
    val v = t.validators.size
    val reports = t.available.map(_._3)
    val ok = reports.forall { h =>
      val byTranche = t.announced.collect { case ((rh, tr), set) if rh == h => tr -> set.toSet }.toMap
      audited(byTranche, positive.getOrElse(h, Set.empty).toSet, negative.getOrElse(h, Set.empty).toSet, v, n)
    }
    val was = isAudited(t.hash)
    auditedBlocks.put(t.hash, java.lang.Boolean.valueOf(ok))
    if ok && !was then logger.info(s"block ${t.hash.toHex.take(18)} slot=${t.slot} audited")
    if reports.exists(h => disregards(negative.getOrElse(h, Set.empty).toSet, v)) then
      logger.warn(s"block ${t.hash.toHex.take(18)} has a report judged invalid by a third of validators; disregarding")
      disregard(t.hash)

  /** Re-execute a report's in-core pipeline; true when the recomputed report
    * hash matches.
    */
  def auditReport(report: WorkReport): Boolean = auditOutcome(report).getOrElse(false)

  /** The audit evaluation e_n: None when too few shards are reachable to rebuild the bundle,
    * which says nothing about the report (reconstruction is assumed to work); a bundle that fails to decode or verify
    * makes the report invalid.
    */
  def auditOutcome(report: WorkReport): Option[Boolean] =
    val spec = report.packageSpec
    val fetched = fetchBundleDirect(spec.erasureRoot, distribution.peers) match
      case Some(bytes) =>
        logger.debug(s"CE147: fetched bundle directly for ${spec.erasureRoot.toHex.take(18)}")
        Right(bytes)
      case None =>
        shards.reconstructBundle(
          spec.erasureRoot,
          spec.length.toLong.toInt,
          distribution.peers
        )
    fetched match
      case Left(err) if err.startsWith(ShardService.TooFewShards) =>
        logger.warn(s"audit of ${spec.hash.toHex.take(18)} postponed: $err")
        None
      case Left(err) =>
        logger.warn(s"audit could not re-execute ${spec.hash.toHex.take(18)}: $err")
        Some(false)
      case Right(bundleBytes) => Some(evaluate(report, bundleBytes))

  private def evaluate(report: WorkReport, bundleBytes: Array[Byte]): Boolean =
    val spec = report.packageSpec
    val result =
      for
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
            accounts = accounts.forAnchor(bundle.workPackage.context.lookupAnchorStateRoot)
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
        // A bundle that fails to decode or re-execute makes the report invalid.
        logger.warn(s"audit could not re-execute ${spec.hash.toHex.take(18)}: $err")
        false

  // =========================================================================
  // CE 147 — bundle request (client side)
  // =========================================================================
  def fetchBundleDirect(
      erasureRoot: Hash,
      peers: Iterable[JamnpConnection],
      totalBudgetMs: Long = 15000,
      maxPeers: Int = 3
  ): Option[Array[Byte]] =
    val deadline = System.currentTimeMillis() + totalBudgetMs
    val candidates = peers.iterator.filter(_.isOpen).take(maxPeers)
    var result: Option[Array[Byte]] = None
    while result.isEmpty && candidates.hasNext && System.currentTimeMillis() < deadline do
      result = fetchBundleFrom(candidates.next(), erasureRoot, deadline)
    result

  /** One CE 147 request/response attempt against `conn`, bounded by the
    * shared absolute `deadline` (not a fixed per-call duration) so a peer
    * that stalls on `openStream` doesn't get a fresh full timeout budget for
    * the response wait too. On any failure — including a timeout — the
    * stream is fully closed (not just locally `finish()`ed) so it doesn't
    * linger registered on the connection pipeline until an idle-out that a
    * busy connection may never trigger.
    */
  private def fetchBundleFrom(
      conn: JamnpConnection,
      erasureRoot: Hash,
      deadline: Long
  ): Option[Array[Byte]] =
    var stream: JamnpStream = null
    try
      val openTimeout = deadline - System.currentTimeMillis()
      if openTimeout <= 0 then return None
      stream = conn.openStream(StreamKind.BundleRequest).get(openTimeout, TimeUnit.MILLISECONDS)
      val response = new CompletableFuture[Array[Byte]]()
      stream.onMessage(response.complete(_))
      stream.onClosed(() => response.complete(null)) // peer holds nothing / declined
      stream.send(erasureRoot.bytes.toArray)
      stream.finish()
      val respTimeout = deadline - System.currentTimeMillis()
      if respTimeout <= 0 then
        stream.close()
        None
      else Option(response.get(respTimeout, TimeUnit.MILLISECONDS))
    catch
      case e: Exception =>
        logger.debug(s"CE147 direct bundle fetch failed: ${e.getMessage}")
        if stream != null then stream.close()
        None

  // =========================================================================
  // CE 144 — audit announcements
  // =========================================================================

  /** CE 144: `Header Hash ++ Tranche ++ Announcement`, then the evidence, then FIN. The announcer is the peer: its
    * connection key is its Ed25519 key in the block's prior set.
    */
  def auditAnnouncementHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      val parts = mutable.ListBuffer.empty[Array[Byte]]
      stream.onMessage { msg =>
        parts += msg
        if parts.size == 2 then
          val peer = conn.peerKey
          val (head, evidence) = (parts(0), parts(1))
          exec.execute(() =>
            guarded {
              decodeHead(head) match
                case Left(err) => logger.debug(s"CE144 from ${JamNode.hex(peer).take(16)}: $err")
                case Right((headerHash, _, _)) =>
                  if tracked.contains(headerHash) then applyAnnouncement(peer, head, evidence)
                  else if pendingAnnouncements.size < 256 then
                    pendingAnnouncements.getOrElseUpdate(headerHash, mutable.ListBuffer.empty) +=
                      ((peer, head, evidence))
            }
          )
          stream.finish()
      }

  /** Verify an announcement against the block it names (Ed25519 signature, VRF evidence, and that the VRF selects
    * the announced reports) and record it in A_n.
    */
  private def applyAnnouncement(peer: Array[Byte], head: Array[Byte], evidence: Array[Byte]): Unit =
    for
      (headerHash, n, a) <- decodeHead(head).toOption
      t <- tracked.get(headerHash)
      v = t.validators.indexWhere(java.util.Arrays.equals(_, peer))
      if v >= 0
    do
      val signed = Ed25519.verify(t.validators(v), announcementMessage(n, a.items, headerHash), a.signature)
      val known = a.items.forall((c, h) => t.reportHashes.lift(c).flatten.contains(h))
      val selected =
        if n == 0 then
          BandersnatchVrf
            .ietfVrfVerify(t.bandersnatch(v), initialInput(t.vrfOutput), Array.emptyByteArray, evidence)
            .exists(y => a.items.map(_._1).toSet.subsetOf(initialCores(t.q, y).toSet))
        else
          decodeLaterEvidence(evidence, a.items.size).toOption.exists { perReport =>
            a.items.zip(perReport).forall { case ((_, h), (sig, noShows)) =>
              BandersnatchVrf
                .ietfVrfVerify(t.bandersnatch(v), laterInput(t.vrfOutput, h, n), Array.emptyByteArray, sig)
                .exists(y =>
                  negative.get(h).exists(_.nonEmpty) ||
                    escalates(y, t.validators.size, noShows.map(_.validator).distinct.size)
                )
            }
          }
      if signed && known && selected then
        recordAnnouncement(t, v, n, a)
        logger.debug(s"audit announcement from validator $v, tranche $n, ${a.items.size} report(s)")
        updateAudited(t, t.lastTranche)
      else
        logger.warn(s"rejected CE144 from validator $v (signature $signed, reports $known, selection $selected)")
        logger.debug(s"CE144 rejected: key=${JamNode.hex(peer)} head=${JamNode.hex(head)} evidence=${JamNode.hex(evidence)}")

  // =========================================================================
  // CE 145 — judgment publication
  // =========================================================================

  /** Sign, record and send our judgment (eq:judgments) with validator index `v`. */
  private def publishJudgment(reportHash: Hash, v: Int, keys: ValidatorKeySet, valid: Boolean): Unit =
    val context = if valid then constants.JAM_VALID_BYTES else constants.JAM_INVALID_BYTES
    val signature = Ed25519ZebraWrapper.sign(keys.ed25519Secret, context ++ reportHash.bytes)
    recordJudgment(reportHash, v, valid, signature)
    val epoch = (chain.best.slot / chain.config.epochLength).toInt
    val msg = new java.io.ByteArrayOutputStream(103)
    msg.write(epoch & 0xff); msg.write((epoch >> 8) & 0xff)
    msg.write((epoch >> 16) & 0xff); msg.write((epoch >> 24) & 0xff)
    msg.write(v & 0xff); msg.write((v >> 8) & 0xff)
    msg.write(if valid then 1 else 0)
    msg.write(reportHash.bytes.toArray)
    msg.write(signature)
    distribution.peers.foreach(sendOneShot(_, StreamKind.JudgmentPublication, msg.toByteArray))
    if valid then logger.info(s"published positive judgment for ${reportHash.toHex.take(18)} as validator $v")
    else logger.warn(s"published NEGATIVE judgment for ${reportHash.toHex.take(18)} as validator $v")

  /** J⊤ / J⊥; negative judgments also go to the pool for a disputes verdict. */
  private def recordJudgment(reportHash: Hash, v: Int, valid: Boolean, signature: Array[Byte]): Unit =
    (if valid then positive else negative).getOrElseUpdate(reportHash, mutable.Set.empty) += v
    if !valid then pools.addJudgment(reportHash, v, valid, signature)

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
            Ed25519.verify(
              vk.ed25519.bytes.toArray,
              context ++ reportHash.bytes,
              signature
            )
          )
          if genuine then
            exec.execute(() =>
              guarded {
                recordJudgment(reportHash, idx, valid, signature)
                tracked.values.filter(_.reportHashes.contains(Some(reportHash))).foreach(t =>
                  updateAudited(t, t.lastTranche)
                )
              }
            )
            logger.debug(s"${if valid then "positive" else "negative"} judgment from validator $idx")
          else logger.warn(s"rejected judgment with bad signature from claimed validator $idx")
        else logger.warn(s"bad CE145 length ${msg.length}")
        stream.finish()
      }

  // =========================================================================
  // helpers
  // =========================================================================

  /** Two messages then FIN on a fresh stream (CE 144). */
  private def sendTwo(conn: JamnpConnection, kind: Byte, first: Array[Byte], second: Array[Byte]): Unit =
    try
      egress.submit(new Runnable {
        def run(): Unit =
          try
            val stream = conn.openStream(kind).get(10, TimeUnit.SECONDS)
            stream.send(first)
            stream.send(second)
            stream.finish()
          catch
            case e: Exception =>
              logger.debug(s"send on ${StreamKind.name(kind)} failed: ${e.getMessage}")
      })
    catch case _: java.util.concurrent.RejectedExecutionException => ()

  private def sendOneShot(conn: JamnpConnection, kind: Byte, payload: Array[Byte]): Unit =
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
              logger.warn(s"send on ${StreamKind.name(kind)} failed: ${e.getMessage}")
      })
    catch
      case _: java.util.concurrent.RejectedExecutionException =>
        logger.warn(s"egress rejected ${StreamKind.name(kind)} send (shutting down)")
