package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, ExecutorService, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, constants}
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.crypto.{BandersnatchVrf, Ed25519, Ed25519ZebraWrapper}
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamKind}
import io.forge.jam.protocol.refine.{ComputeReport, DaShards, ImportResolver, WorkPackageBundle}
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
    tickMillis: Long = 500L,
    /** Where re-execution runs, shared with the guarantor; None: a pool of this service's own. */
    sharedRefinePool: Option[RefinePool] = None
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
      val reportHashes: IndexedSeq[Option[Hash]],
      /** The epoch of the current validator set: judgments on this block's reports index into that epoch's set. */
      val epoch: Long
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
  /** Audits fetch their bundle here (network waits, several reports at once); re-execution itself runs on the refine
    * pool at audit priority, ahead of co-signing and new submissions.
    */
  private val workers = java.util.concurrent.Executors.newFixedThreadPool(
    AuditorService.FetchThreads,
    { r =>
      val t = new Thread(r, "jam-audit-fetch"); t.setDaemon(true); t
    }
  )
  private val refinePool = sharedRefinePool.getOrElse(new RefinePool("jam-audit-exec"))

  /** Peers' announcements and judgments waiting for the audit thread are bounded; the thread's own ticks, import
    * callbacks and audit results are not counted and never dropped.
    */
  private val intake = new WorkGate("auditor intake", AuditorService.MaxQueuedMessages, AuditorService.MaxQueuedBytes)

  /** Peer messages waiting for the audit thread. */
  private[node] def queuedMessages: Int = intake.queued
  private val tracked = mutable.LinkedHashMap.empty[Hash, Tracked]
  private val positive = mutable.Map.empty[Hash, mutable.Set[Int]]
  private val negative = mutable.Map.empty[Hash, mutable.Set[Int]]
  private val inFlight = mutable.Set.empty[Hash]

  /** Tracked blocks already disregarded: fork choice is told once, not at every tick. */
  private val disregarded = mutable.Set.empty[Hash]

  /** Announcements for blocks we do not track yet, per announcing peer and by (block, tranche), oldest first: at
    * most [[AuditorService.PendingAnnouncementsPerPeer]] per validator of the sets around the head.
    */
  private val pendingAnnouncements =
    mutable.Map.empty[Ed25519PublicKey, mutable.LinkedHashMap[(Hash, Int), (Array[Byte], Array[Byte])]]

  /** Genuine judgments on reports no tracked block holds yet, per validator index and by (report, validity), oldest
    * first: at most [[AuditorService.PendingJudgmentsPerValidator]] per validator. `positive` and `negative` hold
    * tracked reports only, so they are bounded by validators × reports awaiting audit.
    */
  private val pendingJudgments = mutable.Map.empty[Int, mutable.LinkedHashMap[(Hash, Boolean), (Array[Byte], Long)]]

  /** Readable from any thread: the slot of each unfinalized block we have looked at and whether it is audited. A
    * block above finality we have not looked at yet is not audited (see [[isAudited]]).
    */
  private val auditedBlocks = new ConcurrentHashMap[Hash, AuditorService.Mark]()
  private val announcementCounts = new ConcurrentHashMap[Hash, java.util.Set[Integer]]()
  @volatile private var disregard: Hash => Unit = _ => ()

  exec.scheduleWithFixedDelay(() => guarded(tick()), tickMillis, tickMillis, TimeUnit.MILLISECONDS)

  def verdictFor(reportHash: Hash): Option[Boolean] =
    Option(verdicts.get(reportHash)).map(_.booleanValue)
  def announcementsFor(reportHash: Hash): Int =
    Option(announcementCounts.get(reportHash)).map(_.size).getOrElse(0)

  /** isAudited for block `hash`. A finalized block is. An unfinalized block we have not looked at (its import listener
    * has not run yet, or it was imported before auditing was enabled or before a restart) is not: it is tracked now,
    * and counts once its reports are audited, or at once when it made none available.
    */
  def isAudited(hash: Hash): Boolean =
    val mark = auditedBlocks.get(hash)
    if mark != null then mark.audited
    else
      chain.slotOf(hash) match
        case Some(slot) if slot <= chain.finalized.slot => true
        case Some(slot) =>
          if auditedBlocks.putIfAbsent(hash, AuditorService.Mark(slot, false)) == null then
            exec.execute(() => guarded(chain.block(hash).foreach(b => track(hash, slot, b))))
          false
        case None => false

  /** Called with a block whose report a third or more of the validators judged invalid. */
  def onDisregard(f: Hash => Unit): Unit = disregard = f

  def shutdown(): Unit =
    exec.shutdownNow()
    workers.shutdownNow()
    if sharedRefinePool.isEmpty then refinePool.shutdownNow()

  private def guarded(f: => Unit): Unit =
    try f
    catch case e: Exception => logger.warn(s"auditor step failed: $e")

  private def ed25519ByPublic: Map[Ed25519PublicKey, ValidatorKeySet] =
    validatorKeys.map(k => k.ed25519Key -> k).toMap

  /** Import listener: track the reports that just became available in `block`. Runs on the import bus, so the
    * block's audited flag is set before any later listener (the GRANDPA voter) can ask for it.
    */
  def onImported(head: ChainManager#Head, block: Block): Unit =
    if head.slot > chain.finalized.slot then
      auditedBlocks.putIfAbsent(head.hash, AuditorService.Mark(head.slot, false))
      track(head.hash, head.slot, block)

  /** Start auditing block `hash` (marked unaudited already): it is audited at once when it made no report available,
    * and otherwise tracked through the tranches. Idempotent.
    */
  private def track(hash: Hash, slot: Long, block: Block): Unit =
    chain.stateViewAt(block.header.parent) match
      case None =>
        logger.warn(s"no prior state for ${hash.toHex.take(18)}; counting it as audited")
        auditedBlocks.put(hash, AuditorService.Mark(slot, true))
      case Some(prior) =>
        val kappa = prior.validators.current.toIndexedSeq
        val q = justBecameAvailable(block, prior.cores.reports.toIndexedSeq, kappa.size)
        val vrfOut = BandersnatchVrf.getVrfOutput(block.header.entropySource.toArray).getOrElse(Array.emptyByteArray)
        val t = new Tracked(
          hash,
          slot,
          trancheOrigin(slot, clock()),
          vrfOut,
          kappa.map(_.ed25519.bytes.toArray),
          kappa.map(_.bandersnatch.bytes.toArray),
          q,
          q.map(_.map(w => Hashing.blake2b256(w.encode.toArray))),
          prior.timeslot / chain.config.epochLength
        )
        if t.available.isEmpty then auditedBlocks.put(hash, AuditorService.Mark(slot, true))
        else
          exec.execute(() =>
            guarded {
              if !tracked.contains(t.hash) then
                tracked.put(t.hash, t)
                takePendingAnnouncements(t.hash).foreach((p, h, e) => applyAnnouncement(p, h, e))
                takePendingJudgments(t.available.map(_._3).toSet).foreach((h, v, valid, sig, epoch) =>
                  recordJudgment(h, epoch, v, valid, sig, tally = epoch == t.epoch)
                )
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
    t.validators.zipWithIndex.flatMap((k, i) => byPublic.get(Ed25519PublicKey(k)).map(i -> _))

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
      val seen = positive.getOrElse(h, mutable.Set.empty)
      val noShows = prior.iterator.filterNot(seen.contains).toSeq.sorted
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
                  // Our own failure says nothing about the report: no judgment, retried while still tracked.
                  logger.warn(s"audit of ${reportHash.toHex.take(18)} failed: $e"); None
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
    // Marks of finalized blocks that are not being tracked: isAudited answers for those from the finalized slot.
    auditedBlocks.entrySet.removeIf(e => e.getValue.slot <= finalizedSlot && !tracked.contains(e.getKey))
    val done = tracked.values
      .filter(t => t.slot <= finalizedSlot && (isAudited(t.hash) || trancheAt(now, t.origin) >= GiveUpTranches))
      .toList
    if done.nonEmpty then
      done.foreach(t => tracked.remove(t.hash))
      done.foreach(t => disregarded.remove(t.hash))
      val live = tracked.values.flatMap(_.available.map(_._3)).toSet
      done.foreach { t =>
        if !isAudited(t.hash) then logger.debug(s"block ${t.hash.toHex.take(18)} finalized before we saw it audited")
        auditedBlocks.remove(t.hash)
        val gone = t.available.map(_._3).filterNot(live.contains)
        gone.foreach { h =>
          positive.remove(h); negative.remove(h); verdicts.remove(h); announcementCounts.remove(h)
        }
        pools.removeJudgments(gone)
      }

  /** Publish our judgment on `reportHash` for every key of ours that announced it in `t`. */
  private def judge(t: Tracked, reportHash: Hash, valid: Boolean): Unit =
    ownIndices(t).foreach { (v, keys) =>
      val announcedIt = t.ours.get(v).exists(_.contains(reportHash))
      val judged = (if valid then positive else negative).get(reportHash).exists(_.contains(v))
      if announcedIt && !judged then publishJudgment(reportHash, t.epoch, v, keys, valid)
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
    auditedBlocks.put(t.hash, AuditorService.Mark(t.slot, ok))
    if ok && !was then logger.info(s"block ${t.hash.toHex.take(18)} slot=${t.slot} audited")
    if reports.exists(h => disregards(negative.getOrElse(h, Set.empty).toSet, v)) && disregarded.add(t.hash) then
      logger.warn(s"block ${t.hash.toHex.take(18)} has a report judged invalid by a third of validators; disregarding")
      disregard(t.hash)

  /** Re-execute a report's in-core pipeline; true when the recomputed report
    * hash matches.
    */
  def auditReport(report: WorkReport): Boolean = auditOutcome(report).getOrElse(false)

  /** The audit evaluation of a report. Only a bundle we know is the guaranteed one can make the report invalid:
    *   - A bundle a peer sends on CE 147 is unauthenticated (its erasure-root commitment also covers the exported
    *     segments, which we do not hold). If it reproduces the report it is the right one, short of a hash collision;
    *     if not, the peer may have sent a wrong bundle, so it is rebuilt from erasure-coded shards instead.
    *   - Shards are checked against the erasure root as they arrive, so a rebuilt bundle is authentic; if it fails to
    *     decode or reproduce the report, the report is invalid.
    *   - None (no judgment yet, retried while tracked) when too few shards are reachable or re-execution threw: neither
    *     says anything about the report.
    */
  def auditOutcome(report: WorkReport): Option[Boolean] =
    val spec = report.packageSpec
    val direct = fetchBundleDirect(spec.erasureRoot, distribution.peers)
      .filter(_.length.toLong == spec.length.toLong)
      .map(bytes => evaluate(report, bytes))
    direct match
      case Some(Right(true)) =>
        logger.debug(s"CE147: bundle fetched directly for ${spec.erasureRoot.toHex.take(18)} reproduces the report")
        Some(true)
      case other =>
        other.foreach(r =>
          logger.warn(
            s"CE147 bundle for ${spec.hash.toHex.take(18)} does not reproduce the report (${r.fold(identity, _ => "mismatch")}); rebuilding from shards"
          )
        )
        // Each validator holds one shard of the report, set by its index and the report's core: ask each peer for
        // its own first. The set may have rotated since the report was guaranteed, so both the current and the
        // previous one are tried.
        val validators = chain.stateView().validators
        val sets = List(validators.current, validators.previous)
        val core = report.coreIndex.toInt
        val rebuilt = shards.reconstructBundle(
          spec.erasureRoot,
          spec.length.toLong.toInt,
          distribution.peers,
          conn =>
            val key = conn.peerPublicKey
            sets.flatMap(_.zipWithIndex.collect { case (vk, v) if vk.ed25519 == key => v })
              .map(DaShards.shardIndexFor(core, _, chain.config))
        )
        rebuilt match
          case Left(err) if err.startsWith(ShardService.TooFewShards) =>
            logger.warn(s"audit of ${spec.hash.toHex.take(18)} postponed: $err")
            None
          case Left(err) =>
            // Authentic shards that do not rebuild a bundle of the committed length: the guarantor's spec is wrong.
            logger.warn(s"audit could not rebuild the bundle of ${spec.hash.toHex.take(18)}: $err")
            Some(false)
          case Right(bundleBytes) =>
            evaluate(report, bundleBytes) match
              case Right(matches) => Some(matches)
              case Left(err) =>
                // A guaranteed bundle that does not decode or is refused re-execution: the report is invalid.
                logger.warn(s"audit could not re-execute ${spec.hash.toHex.take(18)}: $err")
                Some(false)

  /** The lookup at the report's lookup anchor. Without that state re-execution could differ for reasons of ours, so
    * no judgment is given (the audit is retried).
    */
  private def anchoredLookup(root: Hash): StateHistoricalLookup =
    val lookup = accounts.forAnchor(root)
    if !lookup.anchorAvailable then throw new IllegalStateException(s"lookup anchor state ${root.toHex.take(18)} not held")
    lookup

  /** [[evaluate]] on the refine pool at audit priority; this thread waits for it. */
  private def evaluate(report: WorkReport, bundleBytes: Array[Byte]): Either[String, Boolean] =
    try
      CompletableFuture
        .supplyAsync(() => reExecute(report, bundleBytes), refinePool.at(RefinePool.Priority.Audit))
        .get()
    catch case e: java.util.concurrent.ExecutionException => throw e.getCause

  /** Whether `bundleBytes` re-executes to `report`; Left when it does not decode or re-execution is refused. */
  private def reExecute(report: WorkReport, bundleBytes: Array[Byte]): Either[String, Boolean] =
    val spec = report.packageSpec
    val result =
      for
        bundle <- WorkPackageBundle.decode(bundleBytes)
        // The guarantor vouched for these imports: each must verify against its segments-root, those named by package
        // hash through the report's own lookup.
        _ <- ImportResolver.verifyBundle(
          bundle,
          report.segmentRootLookup.map(l => l.workPackageHash -> l.segmentTreeRoot).toMap
        )
        recomputed <- computeReport
          .compute(
            workPackage = bundle.workPackage,
            coreIndex = report.coreIndex.toInt,
            segmentRootLookup =
              report.segmentRootLookup.map(l => l.workPackageHash -> l.segmentTreeRoot).toMap,
            importSegments = bundle.importSegments,
            extrinsicData = bundle.extrinsicData,
            justifications = bundle.justifications,
            accounts = anchoredLookup(bundle.workPackage.context.lookupAnchorStateRoot)
          )
          .left
          .map(e => s"re-execution refused: $e")
      yield Hashing.blake2b256(recomputed.report.encode.toArray) ==
        Hashing.blake2b256(report.encode.toArray)
    result.foreach(matches => if !matches then logger.warn(s"AUDIT MISMATCH for package ${spec.hash.toHex.take(18)}"))
    result

  // =========================================================================
  // CE 147 — bundle request (client side)
  // =========================================================================
  def fetchBundleDirect(
      erasureRoot: Hash,
      peers: Iterable[JamnpConnection],
      totalBudgetMs: Long = 15000,
      maxPeers: Int = 3
  ): Option[Array[Byte]] =
    AuditorService.fetchBundleDirect(erasureRoot, peers, totalBudgetMs, maxPeers)

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
        // Two messages make an announcement; anything the peer sends after them is not kept.
        if parts.size < 2 then
          parts += msg
          if parts.size == 2 then
            // Reset: the audit thread has too much waiting to take it.
            if receiveAnnouncement(conn.peerKey, parts(0), parts(1), conn) then stream.finish() else stream.reset()
      }

  /** An announcement from the peer with Ed25519 key `peer`: checked now when its block is tracked, kept for the
    * block's import otherwise. False when the audit thread has too many peer messages waiting (from the connection
    * `source`, or in all) and the announcement was not taken.
    */
  private[node] def receiveAnnouncement(
      peer: Array[Byte],
      head: Array[Byte],
      evidence: Array[Byte],
      source: AnyRef = null
  ): Boolean =
    intake.tryExecute(exec, head.length.toLong + evidence.length, source) {
      guarded {
        decodeHead(head) match
          case Left(err) => logger.debug(s"CE144 from ${JamNode.hex(peer).take(16)}: $err")
          case Right((headerHash, n, _)) =>
            if tracked.contains(headerHash) then applyAnnouncement(peer, head, evidence)
            else keepAnnouncement(Ed25519PublicKey(peer), headerHash, n, head, evidence)
      }
    }

  /** Keep an announcement for a block we have not imported, which cannot be checked without the block. Only a
    * validator can be an announcer, each announces once per block and tranche, and the wait is for an import a few
    * seconds away: a validator's oldest announcement goes when it has more than a few waiting.
    */
  private def keepAnnouncement(
      peer: Ed25519PublicKey,
      headerHash: Hash,
      tranche: Int,
      head: Array[Byte],
      evidence: Array[Byte]
  ): Unit =
    if head.length + evidence.length <= AuditorService.MaxPendingAnnouncementBytes && isValidator(peer) then
      val mine = pendingAnnouncements.getOrElseUpdate(peer, mutable.LinkedHashMap.empty)
      if !mine.contains((headerHash, tranche)) then
        mine.put((headerHash, tranche), (head, evidence))
        while mine.size > AuditorService.PendingAnnouncementsPerPeer do mine.remove(mine.head._1)

  /** In the validator sets a block near our head can be audited by: its prior set is the head's current or, across
    * an epoch change, its next or previous one.
    */
  private def isValidator(peer: Ed25519PublicKey): Boolean =
    val best = chain.best.hash
    if validatorsAt._1 != best then
      val v = chain.stateView().validators
      validatorsAt = (best, (v.current ++ v.nextEpoch ++ v.previous).map(_.ed25519).toSet)
    validatorsAt._2.contains(peer)

  /** The validator keys of [[isValidator]] at a best head, read once per head rather than per announcement. */
  private var validatorsAt: (Hash, Set[Ed25519PublicKey]) = (Hash.zero, Set.empty)

  private def takePendingAnnouncements(headerHash: Hash): Seq[(Array[Byte], Array[Byte], Array[Byte])] =
    val out = pendingAnnouncements.toSeq.flatMap { (peer, mine) =>
      mine.keys.filter(_._1 == headerHash).toList.flatMap(k => mine.remove(k)).map((h, e) => (peer.bytes.toArray, h, e))
    }
    pendingAnnouncements.filterInPlace((_, mine) => mine.nonEmpty)
    out

  private def takePendingJudgments(reports: Set[Hash]): Seq[(Hash, Int, Boolean, Array[Byte], Long)] =
    val out = pendingJudgments.toSeq.flatMap { (v, mine) =>
      mine.keys
        .filter(k => reports.contains(k._1))
        .toList
        .flatMap(k => mine.remove(k).map((sig, epoch) => (k._1, v, k._2, sig, epoch)))
    }
    pendingJudgments.filterInPlace((_, mine) => mine.nonEmpty)
    out

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
                    peerEscalates(y, t.validators.size, noShows.map(_.validator).distinct.size)
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

  /** Sign, record and send our judgment as validator `v` of the set of `epochIndex`. */
  private def publishJudgment(reportHash: Hash, epochIndex: Long, v: Int, keys: ValidatorKeySet, valid: Boolean): Unit =
    val context = if valid then constants.JAM_VALID_BYTES else constants.JAM_INVALID_BYTES
    val signature = Ed25519ZebraWrapper.sign(keys.ed25519Secret, context ++ reportHash.bytes)
    recordJudgment(reportHash, epochIndex, v, valid, signature, tally = true)
    val epoch = epochIndex.toInt
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

  /** Pool a genuine judgment by validator `v` of the set of `epoch` as dispute evidence, and with `tally` (its
    * epoch is the tracked block's) count it among the positive or negative judgments.
    */
  private def recordJudgment(
      reportHash: Hash,
      epoch: Long,
      v: Int,
      valid: Boolean,
      signature: Array[Byte],
      tally: Boolean
  ): Unit =
    if tally then (if valid then positive else negative).getOrElseUpdate(reportHash, mutable.Set.empty) += v
    pools.addJudgment(reportHash, epoch, v, valid, signature)

  def judgmentHandler: io.forge.jam.network.StreamHandler =
    (conn: JamnpConnection, stream: JamnpStream) =>
      stream.onMessage { msg =>
        receiveJudgment(msg, conn)
        stream.finish()
      }

  /** A CE 145 message: the signature is checked first, then the judgment is recorded when a tracked block holds
    * its report and kept for that block's import otherwise.
    */
  private[node] def receiveJudgment(msg: Array[Byte]): Unit = receiveJudgment(msg, null)

  /** As above, counted against the connection `source` it came on (null: not counted per source). */
  private[node] def receiveJudgment(msg: Array[Byte], source: AnyRef): Unit =
    if msg.length == 4 + 2 + 1 + 32 + 64 then
      val idx = (msg(4) & 0xff) | ((msg(5) & 0xff) << 8)
      val valid = msg(6) == 1
      val reportHash = Hash(java.util.Arrays.copyOfRange(msg, 7, 39))
      val signature = java.util.Arrays.copyOfRange(msg, 39, 103)
      val context = if valid then constants.JAM_VALID_BYTES else constants.JAM_INVALID_BYTES
      // The index is into the validator set of the judgment's epoch: the current one, or the previous one.
      val epoch = io.forge.jam.core.LittleEndian.get(msg, 0, 4)
      val view = chain.stateView()
      val bestEpoch = view.timeslot / chain.config.epochLength
      val set =
        if epoch == bestEpoch then Some(view.validators.current)
        else if epoch == bestEpoch - 1 then Some(view.validators.previous)
        else None
      val signer = set.flatMap(_.lift(idx))
      val genuine = signer.exists(vk =>
        Ed25519.verify(
          vk.ed25519.bytes.toArray,
          context ++ reportHash.bytes,
          signature
        )
      )
      if genuine then
        intake.tryExecute(exec, msg.length.toLong, source) {
          guarded {
            val holders = tracked.values.filter(_.reportHashes.contains(Some(reportHash))).toList
            if holders.nonEmpty then
              recordJudgment(reportHash, epoch, idx, valid, signature, tally = holders.exists(_.epoch == epoch))
              holders.foreach(t => updateAudited(t, t.lastTranche))
            else keepJudgment(reportHash, epoch, idx, valid, signature)
          }
        }
        logger.debug(s"${if valid then "positive" else "negative"} judgment from validator $idx")
      else logger.warn(s"rejected judgment with bad signature from claimed validator $idx")
    else logger.warn(s"bad CE145 length ${msg.length}")

  /** Keep a genuine judgment whose report no tracked block holds: the block that makes the report available may
    * not be imported yet, and judgments are not resent. Anyone can replay a validator's old judgments and a
    * validator can sign any hash, so each validator's oldest goes when it has more than a few waiting.
    */
  private def keepJudgment(reportHash: Hash, epoch: Long, v: Int, valid: Boolean, signature: Array[Byte]): Unit =
    val mine = pendingJudgments.getOrElseUpdate(v, mutable.LinkedHashMap.empty)
    if !mine.contains((reportHash, valid)) then
      mine.put((reportHash, valid), (signature, epoch))
      while mine.size > AuditorService.PendingJudgmentsPerValidator do mine.remove(mine.head._1)

  /** Sizes of the peer-fed audit state: (reports with a recorded judgment, recorded judgments, judgments waiting
    * for a block, announcements waiting for a block).
    */
  private[node] def bufferSizes: (Int, Int, Int, Int) =
    exec
      .submit(() =>
        (
          (positive.keySet ++ negative.keySet).size,
          positive.valuesIterator.map(_.size).sum + negative.valuesIterator.map(_.size).sum,
          pendingJudgments.valuesIterator.map(_.size).sum,
          pendingAnnouncements.valuesIterator.map(_.size).sum
        )
      )
      .get(30, TimeUnit.SECONDS)

  /** Validators recorded as judging `reportHash` valid and invalid. */
  private[node] def judgesOf(reportHash: Hash): (Set[Int], Set[Int]) =
    exec
      .submit(() =>
        (positive.getOrElse(reportHash, Set.empty).toSet, negative.getOrElse(reportHash, Set.empty).toSet)
      )
      .get(30, TimeUnit.SECONDS)

  // =========================================================================
  // helpers
  // =========================================================================

  /** Two messages then FIN on a fresh stream (CE 144). */
  private def sendTwo(conn: JamnpConnection, kind: Byte, first: Array[Byte], second: Array[Byte]): Unit =
    OneShotSend.submit(
      egress,
      conn,
      () => conn.openStream(kind),
      List(first, second),
      why => logger.debug(s"send on ${StreamKind.name(kind)} failed: $why")
    )

  private def sendOneShot(conn: JamnpConnection, kind: Byte, payload: Array[Byte]): Unit =
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
      why => logger.warn(s"send on ${StreamKind.name(kind)} failed: $why"),
      () => logger.warn(s"egress rejected ${StreamKind.name(kind)} send (shutting down)")
    )

object AuditorService extends LazyLogging:
  /** Audits fetching their bundles at once. */
  val FetchThreads: Int = 4

  /** A block's slot and whether it is audited. */
  private[node] final case class Mark(slot: Long, audited: Boolean)

  /** CE 144 announcements and CE 145 judgments that may wait for the audit thread, and the bytes they may hold. */
  val MaxQueuedMessages: Int = 4096
  val MaxQueuedBytes: Long = 32L * 1024 * 1024


  /** Announcements kept per validator for blocks not imported yet. A validator announces once per block and
    * tranche, a tranche lasts eight seconds and an announced block is at most an import away, so an honest validator
    * has one or two waiting (tranche 0 of the last block or two).
    */
  val PendingAnnouncementsPerPeer: Int = 4

  /** Largest announcement (head and evidence) kept for a block not imported yet. Tranche 0, the one that can outrun
    * an import, is under 600 bytes with all ten cores drawn (34 per report, a 64-byte signature, a 96-byte VRF proof);
    * this leaves room for a later tranche carrying a few dozen no-shows. With the per-validator count it bounds the
    * buffer at 16 KiB per validator.
    */
  val MaxPendingAnnouncementBytes: Int = 4096

  /** Judgments kept per validator for reports no tracked block holds yet. A validator draws at most
    * [[AuditSelection.InitialCores]] reports of a block in tranche 0; twice that leaves room for a second block
    * still being imported or for later-tranche picks.
    */
  val PendingJudgmentsPerValidator: Int = 2 * AuditSelection.InitialCores

  /** CE 147 against up to `maxPeers` open peers in turn, within `totalBudgetMs` overall. */
  private[node] def fetchBundleDirect(
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
    * the response wait too. Every exit without a bundle (the peer closed
    * with nothing, a timeout, an error) resets the stream, so neither side
    * keeps it; with a bundle in hand only the peer's FIN is outstanding.
    */
  private[node] def fetchBundleFrom(
      conn: JamnpConnection,
      erasureRoot: Hash,
      deadline: Long
  ): Option[Array[Byte]] =
    var stream: JamnpStream = null
    var bundle: Option[Array[Byte]] = None
    try
      val openTimeout = deadline - System.currentTimeMillis()
      if openTimeout > 0 then
        stream = conn.openStream(StreamKind.BundleRequest).get(openTimeout, TimeUnit.MILLISECONDS)
        val response = new CompletableFuture[Array[Byte]]()
        stream.onMessage(response.complete(_))
        stream.onClosed(() => response.complete(null)) // peer holds nothing / declined
        stream.send(erasureRoot.bytes.toArray)
        stream.finish()
        val respTimeout = deadline - System.currentTimeMillis()
        if respTimeout > 0 then bundle = Option(response.get(respTimeout, TimeUnit.MILLISECONDS))
    catch case e: Exception => logger.debug(s"CE147 direct bundle fetch failed: ${e.getMessage}")
    finally if stream != null && bundle.isEmpty then stream.reset()
    bundle
