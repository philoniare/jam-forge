package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes, constants}
import io.forge.jam.core.primitives.{Ed25519PublicKey, Ed25519Signature, Hash, ValidatorIndex}
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.crypto.Ed25519ZebraWrapper
import io.forge.jam.protocol.refine.DaShards

/** Assurer role: after each imported block, acquires and verifies DA custody
  * for every pending report, then signs a per-validator
  * availability bitfield of the cores whose shards it actually holds,
  * anchored at the new head, and pools/distributes the assurances (CE 141).
  */
final class AssurerService(
    chain: ChainManager,
    distribution: DistributionService,
    pools: ExtrinsicPools,
    validatorKeys: Seq[ValidatorKeySet],
    shards: ShardService,
    shardStore: io.forge.jam.db.ShardStore,
    /** Runs a distribution at its JAMNP-S time: about 2 s before the next
      * slot when the node keeps slot time, at once otherwise.
      */
    scheduleDistribution: (() => Unit) => Unit = f => f()
) extends LazyLogging:

  /** Assurances made at the latest import, not yet sent on CE 141. A newer
    * import replaces them, so only the freshest anchor is distributed.
    */
  private val pending = new java.util.concurrent.atomic.AtomicReference[List[AssuranceExtrinsic]](Nil)

  private val ed25519ByPublic: Map[Ed25519PublicKey, ValidatorKeySet] =
    validatorKeys.map(k => k.ed25519Key -> k).toMap

  /** The newest imported head not yet assured. Acquiring custody asks peers and can wait on them, so it runs on
    * its own thread rather than the one every post-import listener shares; only the newest head matters (an
    * assurance is anchored at the head), so a head that arrives while an older one is in hand replaces any waiting.
    */
  private val latest = new java.util.concurrent.atomic.AtomicReference[ChainManager#Head](null)
  private val worker = java.util.concurrent.Executors.newSingleThreadExecutor { r =>
    val t = new Thread(r, "jam-assurer"); t.setDaemon(true); t
  }

  /** Custody lookups of one head run side by side on these. */
  private val custodyPool = java.util.concurrent.Executors.newFixedThreadPool(
    AssurerService.CustodyLookupsAtOnce,
    r =>
      val t = new Thread(r, "jam-assurer-custody"); t.setDaemon(true); t
  )

  /** Import listener: acquire custody, then assure held cores, off the caller's thread. */
  def onImported(head: ChainManager#Head): Unit =
    if latest.getAndSet(head) == null then
      try
        worker.execute { () =>
          val h = latest.getAndSet(null)
          if h != null then
            try assure(h)
            catch case e: Exception => logger.error("assuring failed", e)
        }
      catch case _: java.util.concurrent.RejectedExecutionException => () // shutting down

  /** Stop assuring and wait for a lookup in flight: must run before the shard store closes. */
  def shutdown(): Unit =
    worker.shutdownNow()
    custodyPool.shutdownNow()
    worker.awaitTermination(15, java.util.concurrent.TimeUnit.SECONDS)
    custodyPool.awaitTermination(15, java.util.concurrent.TimeUnit.SECONDS)
    ()

  private[node] def assure(head: ChainManager#Head): Unit =
    // The state of the block being assured, not of whatever is best by now: the assurance is anchored at `head`.
    val view = chain.stateViewAtRoot(head.stateRoot)
    val reports = view.cores.reports
    if !reports.exists(_.isDefined) then return

    val activeSet = view.validators.current
    val heldIndexes = activeSet.zipWithIndex.flatMap { case (vk, idx) =>
      ed25519ByPublic.get(vk.ed25519).map(keys => idx -> keys)
    }
    if heldIndexes.isEmpty then return

    // Custody per (core, validator index): local store, else CE 137 pull. The pulls run side by side, so one
    // unreachable shard does not use up the time before the assurances are due.
    val peers = distribution.peers
    val lookups =
      for
        (r, core) <- reports.zipWithIndex
        assignment <- r.toList
        (idx, _) <- heldIndexes
      yield
        val root = assignment.report.packageSpec.erasureRoot
        val shardIndex = DaShards.shardIndexFor(core, idx, chain.config)
        val held: java.util.concurrent.Future[Boolean] =
          if shardStore.has(root, shardIndex) then java.util.concurrent.CompletableFuture.completedFuture(true)
          else custodyPool.submit(() => acquireCustody(root, shardIndex, peers))
        (core, idx) -> held
    val custody: Map[(Int, Int), Boolean] = lookups.map((k, f) => k -> f.get()).toMap

    val batch = List.newBuilder[AssuranceExtrinsic]
    var produced = 0
    heldIndexes.foreach { case (idx, keys) =>
      val bitfield = new Array[Byte]((chain.config.coresCount + 7) / 8)
      var any = false
      reports.zipWithIndex.foreach { case (r, core) =>
        if r.isDefined && custody.getOrElse((core, idx), false) then
          bitfield(core / 8) = (bitfield(core / 8) | (1 << (core % 8))).toByte
          any = true
      }
      if any then
        val dataHash = Hashing.blake2b256(head.hash.bytes ++ bitfield)
        val message = constants.JAM_AVAILABLE_BYTES ++ dataHash.bytes
        val assurance = AssuranceExtrinsic(
          anchor = head.hash,
          bitfield = JamBytes(bitfield),
          validatorIndex = ValidatorIndex(idx),
          signature =
            Ed25519Signature(Ed25519ZebraWrapper.sign(keys.ed25519Secret, message))
        )
        pools.addAssurance(assurance)
        batch += assurance
        produced += 1
    }
    if produced > 0 then
      pending.set(batch.result())
      // JAMNP-S CE 141: distribute approximately 2 seconds before each slot.
      scheduleDistribution(() => pending.getAndSet(Nil).foreach(distribution.distributeAssuranceToAll))
    if produced > 0 then
      logger.debug(
        s"assured ${reports.count(_.isDefined)} pending core(s) with $produced key(s) " +
          s"at anchor ${head.hash.toHex.take(18)}"
      )

  /** True when verified shards for (erasure-root, shard index) are in
    * custody, pulling them from peers via CE 137 when absent.
    */
  private def acquireCustody(erasureRoot: Hash, shardIndex: Int, peers: List[io.forge.jam.network.JamnpConnection]): Boolean =
    if shardStore.has(erasureRoot, shardIndex) then true
    else
      // Several peers at a time: the report's guarantors hold the shard, the rest refuse at once.
      shards.firstFrom(peers)(shards.fetchShards(_, erasureRoot, shardIndex, AssurerService.CustodyTimeoutMillis)) match
        case Some(s) =>
          shardStore.put(erasureRoot, shardIndex, s.encode)
          true
        case None =>
          logger.warn(
            s"no custody for shard $shardIndex of ${erasureRoot.toHex.take(18)}; withholding assurance"
          )
          false

object AssurerService:

  /** Custody lookups (one per pending core and held key) run at once. */
  val CustodyLookupsAtOnce: Int = 8

  /** How long one peer is waited on for a custody unit: under a slot, so the assurance can still be sent for it. */
  val CustodyTimeoutMillis: Long = 4000
