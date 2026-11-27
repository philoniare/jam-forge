package io.forge.jam.node

import scala.jdk.CollectionConverters.*

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
    chain: ChainReader,
    distribution: DistributionService,
    pools: ExtrinsicPools,
    validatorKeys: Seq[ValidatorKeySet],
    shards: ShardService,
    shardStore: io.forge.jam.db.ShardStore,
    /** Runs a distribution at its JAMNP-S time: about 2 s before the next
      * slot when the node keeps slot time, at once otherwise.
      */
    scheduleDistribution: (() => Unit) => Unit = f => f(),
    /** The Ed25519 keys of the guarantors of the report with this erasure-root; empty when not known. */
    guarantorsOf: Hash => Set[io.forge.jam.core.primitives.Ed25519PublicKey] = _ => Set.empty,
    /** Owner of this service's threads: the node's, or its own when made alone. */
    threads: NodeThreads = new NodeThreads,
    retryMillis: Long = AssurerService.RetryMillis
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
  private val latest = new java.util.concurrent.atomic.AtomicReference[ChainHead](null)
  private val worker = threads.single("jam-assurer")

  private val custodyPool = threads.own("jam-assurer-custody")(factory =>
    new java.util.concurrent.ThreadPoolExecutor(
      AssurerService.CustodyLookupsAtOnce,
      AssurerService.CustodyLookupsAtOnce,
      0L,
      java.util.concurrent.TimeUnit.MILLISECONDS,
      new java.util.concurrent.LinkedBlockingQueue[Runnable](chain.config.coresCount),
      factory
    )
  )

  private val lookups = new java.util.concurrent.ConcurrentHashMap[Hash, java.util.concurrent.CompletableFuture[Unit]]()

  private val failedAt = new java.util.concurrent.ConcurrentHashMap[Hash, java.lang.Long]()

  /** Import listener: acquire custody, then assure held cores, off the caller's thread. */
  def onImported(head: ChainHead): Unit =
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

  private[node] def assure(head: ChainHead): Unit =
    if chain.stateAtRoot(head.stateRoot).isEmpty then return
    // The state of the block being assured, not of whatever is best by now: the assurance is anchored at `head`.
    val view = chain.stateViewAtRoot(head.stateRoot)
    val reports = view.cores.reports
    if !reports.exists(_.isDefined) then return

    val activeSet = view.validators.current
    val heldIndexes = activeSet.zipWithIndex.flatMap { case (vk, idx) =>
      ed25519ByPublic.get(vk.ed25519).map(keys => idx -> keys)
    }
    if heldIndexes.isEmpty then return

    val units =
      for
        (r, core) <- reports.zipWithIndex
        assignment <- r.toList
        (idx, _) <- heldIndexes
      yield (core, idx) -> (assignment.report.packageSpec.erasureRoot, DaShards.shardIndexFor(core, idx, chain.config))
    val missing = units.map(_._2).filterNot((root, i) => shardStore.has(root, i)).groupMap(_._1)(_._2)
    val lookups = lookUp(missing, distribution.peers)
    // An assurance sent after its slot is of no use, so the lookups share one slot's time: a unit not held by then
    // is not assured (its lookup may still finish, for a later head).
    val deadline = System.currentTimeMillis() + chain.config.slotDuration * 1000L
    lookups.foreach { f =>
      try f.get(math.max(0L, deadline - System.currentTimeMillis()), java.util.concurrent.TimeUnit.MILLISECONDS)
      catch case _: java.util.concurrent.TimeoutException => ()
    }
    val custody: Map[(Int, Int), Boolean] = units.map { case (k, (root, i)) => k -> shardStore.has(root, i) }.toMap

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

  private[node] def lookupsQueuedOrRunning: Int = lookups.size

  private[node] def lookUp(
      missing: Map[Hash, Seq[Int]],
      peers: List[io.forge.jam.network.PeerConnection]
  ): Seq[java.util.concurrent.Future[?]] =
    failedAt.keySet.retainAll(missing.keySet.asJava)
    val now = System.currentTimeMillis()
    missing.toSeq.flatMap { (root, indexes) =>
      Option(lookups.get(root)).orElse {
        if Option(failedAt.get(root)).exists(now - _ < retryMillis) then None
        else
          val started = new java.util.concurrent.CompletableFuture[Unit]()
          if lookups.putIfAbsent(root, started) != null then Option(lookups.get(root))
          else
            try
              custodyPool.execute { () =>
                try
                  val held = indexes.map(i => acquireCustody(root, i, peers)).forall(identity)
                  if held then failedAt.remove(root) else failedAt.put(root, System.currentTimeMillis())
                finally
                  lookups.remove(root)
                  started.complete(()): Unit
              }
            catch
              case _: java.util.concurrent.RejectedExecutionException => // shutting down, or reports gone but still looked up fill it
                lookups.remove(root)
                started.complete(())
            Some(started)
      }
    }

  /** True when verified shards for (erasure-root, shard index) are in
    * custody, pulling them from peers via CE 137 when absent.
    */
  private def acquireCustody(erasureRoot: Hash, shardIndex: Int, peers: List[io.forge.jam.network.PeerConnection]): Boolean =
    if shardStore.has(erasureRoot, shardIndex) then true
    else
      val keys = guarantorsOf(erasureRoot)
      val guarantors = peers.filter(c => keys.contains(c.peerPublicKey))
      def fetch(conn: io.forge.jam.network.PeerConnection) =
        shards.fetchShards(conn, erasureRoot, shardIndex, AssurerService.CustodyTimeoutMillis)
      (if guarantors.nonEmpty then shards.firstFrom(guarantors, width = 1)(fetch) else shards.firstFrom(peers)(fetch)) match
        case Some(s) =>
          shardStore.put(erasureRoot, shardIndex, s.encode)
          true
        case None =>
          logger.warn(
            s"no custody for shard $shardIndex of ${erasureRoot.toHex.take(18)}; withholding assurance"
          )
          false

object AssurerService:

  /** Custody lookups (one per pending report) run at once. */
  val CustodyLookupsAtOnce: Int = 8

  val RetryMillis: Long = 4000

  /** How long one peer is waited on for a custody unit: under a slot, so the assurance can still be sent for it. */
  val CustodyTimeoutMillis: Long = 4000
