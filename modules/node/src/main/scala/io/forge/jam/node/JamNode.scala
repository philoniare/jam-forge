package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend, ShardStore}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.network.{JamnpConfig, JamnpConnection, JamnpNode, NodeIdentity, StreamKind}

/** Node runtime configuration. */
final case class NodeConfig(
    dataDir: Path,
    listenPort: Int = 0,
    /** 32-byte Ed25519 seed; random identity when absent. */
    ed25519Seed: Option[Array[Byte]] = None,
    /** JAM-common-era override for devnets whose genesis starts "now". */
    eraStartSeconds: Long = SlotClock.JamCommonEraSeconds,
    slotTicking: Boolean = true,
    /** SO_REUSEPORT on the listening socket (see [[JamnpConfig.reusePort]]). */
    reusePort: Boolean = false,
    /** JIP-3 telemetry server, from `--telemetry HOST:PORT`. */
    telemetry: Option[InetSocketAddress] = None,
    /** First retry delay for a lost bootnode connection; doubles per failed
      * attempt up to [[JamNode.MaxRedialMillis]].
      */
    redialMillis: Long = 1000L
)

/** A running JAM node: persistent chain (RocksDB), JAMNP-S networking with
  * block announcement/request protocols, and a slot clock. Block authoring
  * hooks onto the slot ticks (M2 authoring work).
  */
final class JamNode(
    val spec: ChainSpec,
    val nodeConfig: NodeConfig
) extends LazyLogging:

  val identity: NodeIdentity =
    nodeConfig.ed25519Seed
      .map(NodeIdentity.fromSeed)
      .getOrElse(NodeIdentity.generate())

  private def openStores(): (RocksDbTrieBackend, BlockStore, ShardStore) =
    // RocksDB creates only the leaf directory, so a fresh --data path must exist first.
    Files.createDirectories(nodeConfig.dataDir)
    val trie = RocksDbTrieBackend.open(nodeConfig.dataDir.resolve("state"))
    try
      val blocks = BlockStore.open(nodeConfig.dataDir.resolve("blocks"))
      try
        val shard = ShardStore.open(nodeConfig.dataDir.resolve("shards"))
        (trie, blocks, shard)
      catch
        case e: Throwable =>
          blocks.close()
          throw e
    catch
      case e: Throwable =>
        trie.close()
        throw e

  private val stores = openStores()
  private val trieBackend: RocksDbTrieBackend = stores._1
  private val blockStore: BlockStore = stores._2
  val shardStore: ShardStore = stores._3

  private val egress: java.util.concurrent.ExecutorService =
    java.util.concurrent.Executors.newSingleThreadExecutor(r =>
      val t = new Thread(r, "jam-egress"); t.setDaemon(true); t
    )

  val chain = new ChainManager(spec.config, trieBackend, blockStore)
  val sync = new SyncService(chain)
  val pools = new ExtrinsicPools
  val distribution = new DistributionService(pools, spec.config.coresCount, egress)
  val shards = new ShardService(shardStore, spec.config)
  val segments = new SegmentPool()
  val tickets = new TicketService(chain, pools, distribution, egress)
  val preimages = new PreimageService(chain, pools, egress)

  val slotClock = new SlotClock(
    eraStartSeconds = nodeConfig.eraStartSeconds,
    slotSeconds = spec.config.slotDuration
  )

  private val network = new JamnpNode(
    identity,
    JamnpConfig(genesisHashPrefix = spec.alpnPrefix, reusePort = nodeConfig.reusePort)
  )

  /** Peers this node keeps a connection to (bootnodes and `--peer`s): a lost
    * connection is redialed with backoff, so a node cut off for longer than
    * the QUIC idle timeout rejoins once the network heals.
    */
  private final class DialTarget(val address: InetSocketAddress, val key: Option[Array[Byte]]):
    @volatile var connection: JamnpConnection = null
    @volatile var nextAttemptMillis: Long = 0L
    @volatile var backoffMillis: Long = nodeConfig.redialMillis
    @volatile var lastFailure: String = ""

  private val dialTargets = new java.util.concurrent.CopyOnWriteArrayList[DialTarget]()
  @volatile private var redialer: java.util.concurrent.ScheduledExecutorService = null

  val telemetry: Option[Telemetry] = nodeConfig.telemetry.map { address =>
    new Telemetry(
      address,
      () =>
        Telemetry.nodeInformation(
          jamParameters = io.forge.jam.protocol.accumulation.ConstantsBlob.build(spec.config),
          genesisHash = spec.genesisHeaderHash,
          peerId = identity.publicKeyBytes,
          listenPort = listenPort,
          recompiler = false,
          implementationVersion = JamNode.ImplementationVersion,
          grayPaperVersion = JamNode.GrayPaperVersion,
          note = s"chain=${spec.id}"
        )
    )
  }
  @volatile private var lastBest: Hash = Hash.zero
  @volatile private var lastFinalized: Hash = Hash.zero
  @volatile private var lastInSync: Option[Boolean] = None

  @volatile private var slotTicker: AutoCloseable = null
  @volatile private var slotCallback: Long => Unit = _ => ()
  @volatile private var author: Option[BlockAuthor] = None
  @volatile private var guarantor: Option[GuarantorService] = None
  @volatile private var auditor: Option[AuditorService] = None
  @volatile private var shuttingDown: Boolean = false

  /** Hook invoked at every slot boundary (after any authoring attempt). */
  def onSlot(f: Long => Unit): Unit = slotCallback = f

  /** Enable block authoring with this node's validator keys, including
    * Safrole ticket generation for ticketed sealing.
    */
  def enableAuthoring(keys: Seq[ValidatorKeySet]): Unit =
    tickets.setValidatorKeys(keys)
    author = Some(new BlockAuthor(chain, keys, pools, tickets.ownTickets))
    if !keys.exists(k => java.util.Arrays.equals(k.ed25519Public, identity.publicKeyBytes)) then
      logger.warn(
        "network identity is not one of the authoring validators' Ed25519 keys; " +
          "peers cannot map this node to its validator index (JAMNP-S expects the validator key)"
      )

  /** Enable the guarantor role (CE 133 work-package intake → refine → sign →
    * CE 135 distribution) with this node's validator keys.
    */
  def enableGuaranteeing(keys: Seq[ValidatorKeySet]): Unit =
    val g = new GuarantorService(chain, distribution, pools, keys, Some(shardStore), segments)
    guarantor = Some(g)
    network.registerHandler(
      StreamKind.WorkPackageSubmission,
      g.workPackageSubmissionHandler
    )
    network.registerHandler(
      StreamKind.WorkPackageSharing,
      g.workPackageSharingHandler
    )
    network.registerHandler(
      StreamKind.BundleRequest,
      g.bundleRequestHandler
    )
    network.registerHandler(
      StreamKind.SegmentRequest,
      g.segmentRequestHandler
    )
    network.registerHandler(
      StreamKind.WorkPackageBundleSubmission,
      g.bundleSubmissionHandler
    )

  /** Enable the assurer role: after every imported block, pending cores are
    * assured with each held validator key and distributed via CE 141.
    */
  def enableAssuring(keys: Seq[ValidatorKeySet]): Unit =
    val a = new AssurerService(chain, distribution, pools, keys, shards, shardStore)
    chain.onImported((head, block) => a.onImported(head, block))

  /** Enable the auditor role: every included report is re-executed from its
    * reconstructed bundle; outcomes are announced (CE 144) and mismatches
    * judged (CE 145). Returns the service for verdict inspection.
    */
  def enableAuditing(keys: Seq[ValidatorKeySet]): AuditorService =
    val a = new AuditorService(chain, distribution, shards, pools, keys, egress)
    auditor = Some(a)
    network.registerHandler(StreamKind.AuditAnnouncement, a.auditAnnouncementHandler)
    network.registerHandler(StreamKind.JudgmentPublication, a.judgmentHandler)
    chain.onImported((head, block) => a.onImported(head, block))
    a

  /** The auditor service when auditing is enabled (for verdict queries). */
  def auditorService: Option[AuditorService] = auditor

  /** Attempt to author for `slot`; on success the block is imported and
    * announced
    */
  def authorSlot(slot: Long): Option[ChainManager#Head] =
    if shuttingDown then return None
    val regenerated = tickets.maybeGenerate()
    val slotInEpoch = slot % chain.config.epochLength
    val distributionSlot = math.max(chain.config.epochLength / 60, 1)
    // Distribute at the epoch's distribution slot, or at once when a reorg
    // forced new tickets after it.
    if slotInEpoch == distributionSlot || (regenerated && slotInEpoch > distributionSlot) then
      tickets.distributeTickets(distribution.peers, connectionForNextEpochValidator)
    author.flatMap { a =>
      a.tryAuthor(slot).flatMap { block =>
        val authoring = telemetry.flatMap(_.authoring(slot, block.header.parent))
        importAuthored(block, authoring) match
          case Some(head) => Some(head)
          case None if block.extrinsic.guarantees.nonEmpty ||
              block.extrinsic.assurances.nonEmpty ||
              block.extrinsic.preimages.nonEmpty =>
            logger.warn("authored block rejected; dropping pools and retrying empty")
            pools.clear()
            a.tryAuthor(slot).flatMap { retry =>
              importAuthored(retry, telemetry.flatMap(_.authoring(slot, retry.header.parent)))
            }
          case None => None
      }
    }

  /** CE 131 proxy lookup: the open connection whose peer key is the Ed25519
    * key of validator `index` in the next epoch's set (γ_k), whose ring the
    * tickets were made for.
    */
  private def connectionForNextEpochValidator(index: Int): Option[JamnpConnection] =
    chain.stateView().validators.nextEpoch.lift(index).flatMap { vk =>
      val key = vk.ed25519.bytes.toArray
      distribution.peers.find(c => java.util.Arrays.equals(c.peerKey, key))
    }

  private def importAuthored(
      block: io.forge.jam.core.types.block.Block,
      authoring: Option[Telemetry.EventRef]
  ): Option[ChainManager#Head] =
    val bytes = chain.encodeBlock(block)
    telemetry.foreach { t =>
      val x = block.extrinsic
      t.authored(
        authoring,
        Telemetry.blockOutline(
          bytes.length,
          chain.headerHashOf(block),
          x.tickets.size,
          x.preimages.size,
          x.preimages.map(_.blob.length.toLong).sum,
          x.guarantees.size,
          x.assurances.size,
          x.disputes.verdicts.size
        )
      )
    }
    importAndAnnounce(bytes) match
      case Right(head) =>
        pools.pruneAfterImport(
          block.extrinsic.guarantees,
          block.extrinsic.assurances,
          block.extrinsic.preimages,
          head.hash
        )
        Some(head)
      case Left(err) =>
        logger.error(s"authored block failed to import: $err")
        telemetry.foreach(_.authoringFailed(authoring, err))
        None

  def start(): JamNode =
    chain.initializeOrRestore(spec)

    if spec.devnetFinalityDepth > 0 then
      chain.onImported((_, _) =>
        chain.finalizeAtDepth(spec.devnetFinalityDepth, sync.peerBests, spec.finalityQuorum)
      )

    chain.onImported((head, block) => preimages.onImported(head, block))
    chain.onImported((head, block) => pruneTickets(head, block))
    telemetry.foreach(_ => chain.onImported((_, _) => reportHeads()))

    network.onConnectionChange { (conn, up) =>
      val peer = try JamNode.hex(conn.peerKey).take(16) catch case _: Exception => "?"
      val side = if conn.initiator then "dialed" else "accepted"
      val where = conn.remoteAddress.map(a => s" $a").getOrElse("")
      if up then logger.info(s"peer connected ($side) $peer$where")
      else
        logger.info(s"peer disconnected ($side) $peer$where")
        telemetry.foreach(t =>
          try t.disconnected(conn.peerKey, "connection closed")
          catch case _: Exception => ()
        )
    }

    // Track accepted peers for distribution when they open UP 0 to us.
    val announceHandler = sync.blockAnnouncementHandler
    network
      .registerHandler(
        StreamKind.BlockAnnouncement,
        (conn, stream) =>
          distribution.trackConnection(conn)
          announceHandler.onStream(conn, stream)
      )
      .registerHandler(StreamKind.BlockRequest, sync.blockRequestHandler)
      .registerHandler(StreamKind.WorkReportDistribution, distribution.workReportHandler)
      .registerHandler(StreamKind.WorkReportRequest, distribution.reportRequestHandler)
      .registerHandler(StreamKind.AssuranceDistribution, distribution.assuranceHandler)
      .registerHandler(StreamKind.ShardDistribution, shards.custodyHandler)
      .registerHandler(StreamKind.AuditShardRequest, shards.custodyHandler)
      .registerHandler(StreamKind.SegmentShardRequest, shards.segmentShardHandler)
      .registerHandler(StreamKind.SegmentShardRequestVerified, shards.custodyHandler)
      .registerHandler(StreamKind.TicketDistributionStep1, tickets.ticketHandler)
      .registerHandler(StreamKind.TicketDistributionStep2, tickets.ticketHandler)
      .registerHandler(StreamKind.PreimageAnnouncement, preimages.announceHandler)
      .registerHandler(StreamKind.PreimageRequest, preimages.requestHandler)
      .registerHandler(StreamKind.StateRequest, sync.stateRequestHandler)
      .start(new InetSocketAddress("0.0.0.0", nodeConfig.listenPort))

    logger.info(
      s"node up: chain=${spec.id} alpn=${JamnpConfig(spec.alpnPrefix).alpn} " +
        s"port=$listenPort identity=${identity.altName}"
    )
    telemetry.foreach(_.start())

    spec.bootnodes.foreach(bn => dialTargets.add(new DialTarget(bn.address, Some(bn.ed25519Key))))
    dialTargets.forEach(t => dial(t, initial = true))
    redialer = java.util.concurrent.Executors.newSingleThreadScheduledExecutor { r =>
      val t = new Thread(r, "jam-redial"); t.setDaemon(true); t
    }
    redialer.scheduleWithFixedDelay(() => redialLost(), 500, 500, java.util.concurrent.TimeUnit.MILLISECONDS)

    if nodeConfig.slotTicking then
      slotTicker = slotClock.scheduleSlotTicks { slot =>
        try
          authorSlot(slot)
          reportSyncStatus(slot)
          slotCallback(slot)
        catch case e: Exception => logger.error(s"slot $slot handler failed", e)
      }
    this

  def listenPort: Int = network.boundPort

  /** Keep a connection to `address` (redialed when lost), as for a bootnode. */
  def addPeer(address: InetSocketAddress, expectedEd25519: Option[Array[Byte]] = None): Unit =
    val target = new DialTarget(address, expectedEd25519)
    dialTargets.add(target)
    dial(target, initial = true)

  /** Dial targets whose connection is missing or closed, once their backoff
    * has passed. Runs on the redial thread.
    */
  private def redialLost(): Unit =
    if shuttingDown then return
    val now = System.currentTimeMillis()
    dialTargets.forEach { t =>
      val live = t.connection != null && t.connection.isOpen
      if !live && now >= t.nextAttemptMillis then dial(t, initial = false)
    }

  private def dial(t: DialTarget, initial: Boolean): Unit =
    val where = s"${t.address.getHostString}:${t.address.getPort}"
    try
      t.connection = connectPeer(t.address, t.key)
      t.backoffMillis = nodeConfig.redialMillis
      t.lastFailure = ""
      logger.info(s"${if initial then "connected" else "reconnected"} peer $where")
    catch
      case e: Exception =>
        val why = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
        // Log the first failure of a streak and changes, not every retry.
        if initial || why != t.lastFailure then logger.warn(s"peer $where unreachable: $why; retrying")
        t.lastFailure = why
        t.nextAttemptMillis = System.currentTimeMillis() + t.backoffMillis
        t.backoffMillis = math.min(t.backoffMillis * 2, JamNode.MaxRedialMillis)

  /** Drop pooled tickets that `block` included (whoever authored it) and,
    * at an epoch change, every ticket entered for an earlier epoch: their
    * proofs are bound to that epoch's η_2 and γ_z (GP 0.8.0 §safrole).
    */
  private def pruneTickets(head: ChainManager#Head, block: io.forge.jam.core.types.block.Block): Unit =
    val ids = block.extrinsic.tickets.flatMap(t =>
      io.forge.jam.crypto.BandersnatchVrf.ringVrfOutput(t.signature.toArray).map(Hash(_))
    )
    pools.removeTickets(ids)
    pools.pruneTicketsBefore(head.slot / spec.config.epochLength)
    tickets.processDeferred()

  /** JIP-3 events 11 and 12 when the best or finalized head moved. */
  private def reportHeads(): Unit =
    telemetry.foreach { t =>
      val best = chain.best
      if best.hash != lastBest then
        lastBest = best.hash
        t.bestBlockChanged(best.slot, best.hash)
      val fin = chain.finalized
      if fin.hash != lastFinalized then
        lastFinalized = fin.hash
        t.finalizedBlockChanged(fin.slot, fin.hash)
    }

  /** JIP-3 event 13: in sync while the best head is within one epoch's
    * ticket-submission lag (two slots) of the wall-clock slot and we have
    * at least one peer.
    */
  private def reportSyncStatus(slot: Long): Unit =
    telemetry.foreach { t =>
      val inSync = slot - chain.best.slot <= 2 && distribution.peers.nonEmpty
      if !lastInSync.contains(inSync) then
        lastInSync = Some(inSync)
        t.syncStatusChanged(inSync)
      reportHeads()
    }

  def connectPeer(
      address: InetSocketAddress,
      expectedEd25519: Option[Array[Byte]] = None
  ): JamnpConnection =
    val conn = network.connect(address).get(10, java.util.concurrent.TimeUnit.SECONDS)
    expectedEd25519.foreach { expected =>
      JamNode.verifyPeerKey(expected, conn.peerKey) match
        case Left(msg) =>
          conn.close()
          throw new SecurityException(msg)
        case Right(()) => ()
    }
    val expectedAlpn = network.config.alpn
    if conn.negotiatedAlpn != expectedAlpn then
      conn.close()
      throw new SecurityException(
        s"ALPN mismatch: expected $expectedAlpn, got ${conn.negotiatedAlpn}"
      )
    sync.openAnnouncementStream(conn)
    distribution.trackConnection(conn)
    conn

  /** Import a locally produced or externally obtained block and announce it. */
  def importAndAnnounce(blockBytes: Array[Byte]): Either[String, ChainManager#Head] =
    chain.importBlock(blockBytes).map { head =>
      chain.decodeBlock(blockBytes).foreach { b =>
        sync.announce(b.header.encode.toArray)
      }
      head
    }

  def shutdown(): Unit =
    shuttingDown = true
    if redialer != null then redialer.shutdownNow()
    telemetry.foreach(_.shutdown())
    if slotTicker != null then slotTicker.close() // waits for in-flight tick
    guarantor.foreach(_.shutdown()) // drains in-flight refine/co-sign work
    network.shutdown()
    sync.shutdown() // waits for an in-flight sync import
    egress.shutdown()
    egress.awaitTermination(10, java.util.concurrent.TimeUnit.SECONDS)
    preimages.shutdown() // its own executor for CE143 fetches, drained alongside egress
    shutdownStorageOnly()

  /** Close only the storage layers (for tests that never start networking). */
  def shutdownStorageOnly(): Unit =
    chain.shutdown()
    blockStore.close()
    shardStore.close()
    trieBackend.close()

object JamNode:

  /** Upper bound on the redial backoff. */
  val MaxRedialMillis: Long = 16000L

  val ImplementationVersion: String = "0.1.0"
  val GrayPaperVersion: String = "0.8.0"

  private[node] def hex(bytes: Array[Byte]): String =
    bytes.map(b => f"${b & 0xff}%02x").mkString
    
  def verifyPeerKey(expected: Array[Byte], actual: Array[Byte]): Either[String, Unit] =
    if java.util.Arrays.equals(expected, actual) then Right(())
    else Left(s"peer key mismatch: expected ${hex(expected)}, got ${hex(actual)}")
