package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend, ShardStore}
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.network.{JamnpConfig, PeerConnection, JamnpNode, NodeIdentity, StreamKind}

/** Node runtime configuration. */
final case class NodeConfig(
    dataDir: Path,
    listenPort: Int = 0,
    listenIp: java.net.InetAddress = JamNode.AnyAddress,
    externalIp: Option[java.net.InetAddress] = None,
    lowConnectivityGraceMillis: Long = LowConnectivityWatch.DefaultGraceMillis,
    /** 32-byte Ed25519 seed; random identity when absent. */
    ed25519Seed: Option[Array[Byte]] = None,
    /** JAM-common-era override for devnets whose genesis starts "now". */
    eraStartSeconds: Long = SlotClock.JamCommonEraSeconds,
    slotTicking: Boolean = true,
    /** SO_REUSEPORT on the listening socket (see [[JamnpConfig.reusePort]]). */
    reusePort: Boolean = false,
    /** JIP-3 telemetry server, from `--telemetry HOST:PORT`. */
    telemetry: Option[InetSocketAddress] = None,
    /** GRANDPA finality instead of the devnet quorum rule and following peers' finality. */
    grandpa: Boolean = false,
    dummyFinality: Boolean = false,
    /** How many distinct peers must announce a block as finalized (UP 0 `Final`) before this node finalizes it.
      * None: 2V/3 of the validators, which only a node connected to most of them reaches. A lower number is a
      * decision to trust those peers: their announcement is not a proof, and a finalized block is never reorged
      * away. For a node that can reach only a bootnode (validators on a private network), 1 follows that node.
      */
    finalityPeers: Option[Int] = None,
    /** Delete blocks more than a day (`maxLookupAnchorAge` slots) behind finality. Off by default: a node that
      * keeps them can serve the whole chain to a node joining from genesis, which is the only way to join until
      * there is a state-sync client; one that prunes can only serve the last day.
      */
    pruneFinalizedHistory: Boolean = false,
    /** Delete the state of blocks more than a day (`maxLookupAnchorAge` slots) behind finality. On by default;
      * nothing reads it.
      */
    pruneOldState: Boolean = true,
    stateSweepIntervalMillis: Long = ChainManager.StateSweepIntervalMillis,
    syncFrom: Option[Hash] = None,
    syncFromSetId: Option[Long] = None,
    /** FAULT INJECTION, `--fault-tamper-reports`: guarantee deliberately wrong reports, to test disputes. Never on a
      * real network: the node's guarantors become culprits.
      */
    tamperReports: Boolean = false,
    inlineJudgmentGuarantee: Boolean = false,
    /** Loopback port of the local submission endpoint ([[SubmitServer]]), from `--submit-port N`. */
    submitPort: Option[Int] = None,
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
    val trie = RocksDbTrieBackend.open(nodeConfig.dataDir.resolve("state"), trackGarbage = false)
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

  val telemetry: Option[Telemetry] = nodeConfig.telemetry.map { address =>
    new Telemetry(
      address,
      () =>
        Telemetry.nodeInformation(
          jamParameters = io.forge.jam.protocol.pvmhost.ConstantsBlob.build(spec.config),
          genesisHash = spec.genesisHeaderHash,
          peerId = identity.publicKeyBytes,
          listenPort = listenPort,
          recompiler = io.forge.jam.pvm.ExecutionMode.default == io.forge.jam.pvm.ExecutionMode.Recompiled,
          implementationVersion = JamNode.ImplementationVersion,
          grayPaperVersion = JamNode.GrayPaperVersion,
          note = s"chain=${spec.id}",
          advertisedAddress = nodeConfig.externalIp
        )
    )
  }

  /** Every pool of the node and its services: closed, newest first and each waited for, before the stores are. */
  private val threads = new NodeThreads
  private val trieBackend: RocksDbTrieBackend = stores._1
  private val blockStore: BlockStore = stores._2
  val shardStore: ShardStore = stores._3

  /** Outbound one-shot sends and request exchanges. Each waits on a peer (a stream open, up to 10 s), so a slow
    * peer backs the queue up: it is bounded, and when full the oldest send is dropped, since what it carries
    * (assurances, tickets, votes, announcements) goes stale first.
    */
  private val egress: WorkLimits.DropOldestExecutor =
    threads.own("jam-egress")(factory =>
      WorkLimits.dropOldestExecutor("jam-egress", threads = 1, capacity = WorkLimits.EgressQueueCapacity, factory)
    )

  /** Checks of received guarantees, assurances and tickets, kept off the network event loops. */
  private[node] val inbound: InboundWork = InboundWork.pooled(threads)
  private[node] val announcing: InboundWork = InboundWork.ordered(threads)

  val chain =
    new ChainManager(
      spec.config,
      trieBackend,
      blockStore,
      pruneFinalizedHistory = nodeConfig.pruneFinalizedHistory,
      pruneOldState = nodeConfig.pruneOldState,
      stateSweepIntervalMillis = nodeConfig.stateSweepIntervalMillis
    )
  /** CE 128 / CE 129 requests are answered here, not on the network event loops. */
  private[node] val serving: InboundWork = InboundWork.serving(threads)
  private[node] val scores: PeerScores = new PeerScores(
    exempt = key => peers.inHeadValidatorSets(key) || peers.isConfigured(key),
    onBan = key => network.closeConnectionsOf(key)
  )

  private def reportFault(key: Array[Byte], offence: Offence): Unit =
    scores.report(key, offence)
    if offence != Offence.Unanswered then telemetry.foreach(_.peerMisbehaved(key, offence.toString))
  private val misbehaved: (PeerConnection, Offence) => Unit = (conn, offence) => reportFault(conn.peerKey, offence)

  private val importedBlocks = new java.util.concurrent.atomic.AtomicLong()
  private val importFailures = new java.util.concurrent.atomic.AtomicLong()

  private def countImportFailure(outcome: Either[ChainError, ChainHead]): Unit = outcome match
    case Left(_: ChainError.InvalidBlock | _: ChainError.ImportFailed) => importFailures.incrementAndGet(): Unit
    case _                                                            => ()

  private val importTelemetry = new SyncService.ImportObserver:
    def importing(blockBytes: Array[Byte]): AnyRef | Null =
      telemetry.flatMap { t =>
        chain.decodeBlock(blockBytes).toOption.filterNot(b => chain.hasBlock(chain.headerHashOf(b))).map { block =>
          val hash = chain.headerHashOf(block)
          (t.importing(block.header.slot.toLong, Telemetry.blockOutline(block, blockBytes.length, hash)), hash)
        }
      }.orNull
    def imported(handle: AnyRef | Null, outcome: Either[ChainError, ChainHead]): Unit =
      countImportFailure(outcome)
      (handle, telemetry) match
        case ((ref: Option[?], hash: Hash), Some(t)) =>
          val importing = ref.collect { case r: Telemetry.EventRef => r }
          outcome match
            case Right(_) =>
              t.blockVerified(importing)
              t.blockExecuted(importing, chain.takeAccumulateCosts(hash).getOrElse(Map.empty))
            case Left(e: ChainError.InvalidBlock)   => t.blockVerificationFailed(importing, e.message)
            case Left(e: ChainError.ImportFailed)   => t.blockExecutionFailed(importing, e.message)
            case Left(_)                            => ()
        case _ => ()
  val sync = new SyncService(chain, serving, announcing, threads, misbehaved, importTelemetry, scores, telemetry)
  private val roleStore = RoleStore.over(blockStore)
  val pools = new ExtrinsicPools(store = roleStore)
  telemetry.foreach { t =>
    pools.onDiscard(
      (g, reason) => t.guaranteeDiscarded(Telemetry.guaranteeOutline(g), reason),
      (p, reason) => t.preimageDiscarded(io.forge.jam.core.Hashing.blake2b256(p.blob.toArray), p.blob.length.toLong, reason)
    )
  }
  val distribution = new DistributionService(
    pools,
    spec.config.coresCount,
    egress,
    assurerKeys = () => chain.stateView().validators.current.map(_.ed25519.bytes.toArray),
    admitGuarantee = new GuaranteeAdmission(chain, () => currentSlot).check,
    inbound = inbound,
    threads = threads,
    misbehaved = misbehaved,
    askOrder = scores.leastFaultedFirst(_)(_.peerKey),
    telemetry = telemetry
  )
  val shards = new ShardService(shardStore, spec.config, inbound, threads, misbehaved, telemetry = telemetry)
  val segments = new SegmentPool()
  val tickets = new TicketService(
    chain,
    pools,
    distribution,
    egress,
    forwardTargets = () => peers.currentValidatorConnections,
    inbound = inbound,
    threads = threads,
    misbehaved = misbehaved,
    telemetry = telemetry
  )
  val preimages = new PreimageService(
    chain,
    pools,
    egress,
    gossipTargets = () => distribution.peers.filter(c => peers.announcementWanted(c.peerKey)),
    inbound = inbound,
    serving = serving,
    threads = threads,
    misbehaved = misbehaved,
    telemetry = telemetry
  )

  val slotClock = new SlotClock(
    eraStartSeconds = nodeConfig.eraStartSeconds,
    slotSeconds = spec.config.slotDuration
  )

  // Slots are wall-clock only while the node ticks; a node driven slot by slot (tests) has no clock to bound by.
  if nodeConfig.slotTicking then chain.useWallClock(() => slotClock.currentSlot)

  private[node] val network = new JamnpNode(
    identity,
    JamnpConfig(genesisHashPrefix = spec.alpnPrefix, reusePort = nodeConfig.reusePort)
  )

  private[node] val peers = new PeerConnectivity(
    identity,
    network,
    chain,
    sync,
    distribution,
    spec,
    nodeConfig,
    currentSlot = () => currentSlot,
    upkeep = () =>
      if author.isDefined then maybeSendTickets(currentSlot)
      // A node started without GRANDPA takes its finality from its peers' UP 0 announcements, which are claims and
      // not proofs. With GRANDPA, voting or only observing, finality moves on a verified justification alone.
      if !nodeConfig.grandpa && !nodeConfig.dummyFinality then followPeerFinality(),
    threads = threads
  )

  @volatile private var assuranceTimer: java.util.concurrent.ScheduledExecutorService = null
  @volatile private var ticketsSentGeneration: Int = 0
  private val headsReport = new Object
  private var lastBest: Hash = Hash.zero
  private var lastFinalized: Hash = Hash.zero
  @volatile private var lastInSync: Option[Boolean] = None

  @volatile private var slotTicker: AutoCloseable = null
  @volatile private var slotCallback: Long => Unit = _ => ()
  @volatile private var author: Option[BlockAuthor] = None
  @volatile private var guarantor: Option[GuarantorService] = None
  private lazy val segmentRecovery = new SegmentRecovery(chain, shards, exportIndex, () => distribution.peers, telemetry)

  /** Erasure roots of recently included reports with exports, by segments-root. */
  private[node] val exportIndex = new ExportIndex(store = roleStore)
  private[node] val guarantorIndex = new GuarantorIndex()

  /** PVM-heavy work of the guarantor and auditor roles: audits first, then co-signing, then new submissions. */
  private lazy val refinePool = new RefinePool("jam-refine", owner = threads)

  /** The guarantor service when guaranteeing is enabled. */
  private[node] def guarantorService: Option[GuarantorService] = guarantor
  @volatile private var auditor: Option[AuditorService] = None
  @volatile private var assurer: Option[AssurerService] = None
  @volatile private var shuttingDown: Boolean = false

  /** Hook invoked at every slot boundary (after any authoring attempt). */
  def onSlot(f: Long => Unit): Unit = slotCallback = f

  private[node] def blockAuthor: Option[BlockAuthor] = author

  @volatile private var authoringGuard: Option[EquivocationGuard] = None
  private val watchingForTwins = new java.util.concurrent.atomic.AtomicBoolean(false)

  def authoringStopped: Option[String] = authoringGuard.flatMap(_.stopped)

  private def sealerOf(hash: Hash, block: io.forge.jam.core.types.block.Block): Option[Array[Byte]] =
    chain.stateViewAt(hash).flatMap(_.validators.current.lift(block.header.authorIndex.toInt)).map(_.bandersnatch.bytes.toArray)

  /** Enable block authoring with this node's validator keys, including
    * Safrole ticket generation for ticketed sealing. A slot is sealed for once, across restarts, and authoring stops
    * when a block sealed with these keys elsewhere is imported ([[EquivocationGuard]]).
    */
  def enableAuthoring(keys: Seq[ValidatorKeySet]): Unit =
    tickets.setValidatorKeys(keys)
    val guard = new EquivocationGuard(
      chain.meta,
      keys,
      why => telemetry.foreach(_.peerMisbehaved(identity.publicKeyBytes, s"duplicate authoring: $why"))
    )
    authoringGuard = Some(guard)
    if watchingForTwins.compareAndSet(false, true) then
      chain.onImported((head, block) => authoringGuard.foreach(_.observe(head.hash, block, () => sealerOf(head.hash, block))))
      chain.onSealedSideBlock((hash, block) => authoringGuard.foreach(_.observe(hash, block, () => None)))
    author = Some(new BlockAuthor(chain, keys, pools, tickets.ownTickets, guard))
    peers.holdValidatorKeys(keys.map(k => JamBytes(k.ed25519Public)).toSet)
    if !keys.exists(k => java.util.Arrays.equals(k.ed25519Public, identity.publicKeyBytes)) then
      logger.warn(
        "network identity is not one of the authoring validators' Ed25519 keys; " +
          "peers cannot map this node to its validator index (JAMNP-S expects the validator key)"
      )

  /** Enable the guarantor role (CE 133 work-package intake → refine → sign →
    * CE 135 distribution) with this node's validator keys.
    */
  def enableGuaranteeing(keys: Seq[ValidatorKeySet]): Unit =
    val g = new GuarantorService(
      chain,
      distribution,
      pools,
      keys,
      Some(shardStore),
      segments,
      ownNetworkKey = Some(identity.publicKeyBytes),
      sharedRefinePool = Some(refinePool),
      segmentRecovery = Some(segmentRecovery),
      tamperReports = nodeConfig.tamperReports,
      fromValidator = c => recentValidatorKeys._3.contains(JamBytes(c.peerKey)),
      fromCoGuarantor = isGuarantorOf,
      exportersFirst = (segmentsRoot, peers) =>
        exportIndex.get(segmentsRoot).fold(peers)(e => guarantorIndex.first(e.erasureRoot, peers)),
      // Our own custody only: a request must not make us fetch shards from the network on the requester's behalf.
      localBundle = root =>
        exportIndex.bundleLength(root).flatMap(length => shards.reconstructBundle(root, length, Nil).toOption),
      serving = serving,
      threads = threads,
      misbehaved = misbehaved,
      dialValidators = dialGuarantors,
      telemetry = telemetry
    )
    guarantor = Some(g)
    network.registerPeerHandler(
      StreamKind.WorkPackageSubmission,
      unlessCatchingUp(g.workPackageSubmissionHandler)
    )
    network.registerPeerHandler(
      StreamKind.WorkPackageSharing,
      unlessCatchingUp(g.workPackageSharingHandler)
    )
    network.registerPeerHandler(
      StreamKind.BundleRequest,
      g.bundleRequestHandler
    )
    network.registerPeerHandler(
      StreamKind.SegmentRequest,
      g.segmentRequestHandler
    )
    network.registerPeerHandler(
      StreamKind.WorkPackageBundleSubmission,
      unlessCatchingUp(g.bundleSubmissionHandler)
    )

  /** Enable the assurer role: after every imported block, pending cores are
    * assured with each held validator key and distributed via CE 141.
    */
  def enableAssuring(keys: Seq[ValidatorKeySet]): Unit =
    if nodeConfig.slotTicking && assuranceTimer == null then
      assuranceTimer = threads.scheduled("jam-assurance")
    val a = new AssurerService(
      chain,
      distribution,
      pools,
      keys,
      shards,
      shardStore,
      scheduleAssurances,
      guarantorsOf = guarantorIndex.guarantorsOf,
      threads = threads
    )
    assurer = Some(a)
    chain.onImported((head, _) => if !catchingUp then a.onImported(head))

  /** Enable the auditor role: reports that become available are audited per the tranche
    * rules, announced on CE 144 and judged on CE 145. Tranche 0 starts at the block's slot, or at import without
    * slot ticking. Returns the service for verdict inspection.
    */
  def enableAuditing(keys: Seq[ValidatorKeySet]): AuditorService =
    val origin: (Long, Long) => Long =
      if nodeConfig.slotTicking then (slot, _) => slotClock.slotStartMillis(slot)
      else (_, importedAt) => importedAt
    val a = new AuditorService(
      chain,
      distribution,
      shards,
      pools,
      keys,
      egress,
      trancheOrigin = origin,
      sharedRefinePool = Some(refinePool),
      store = roleStore,
      inbound = inbound,
      guarantorsFirst = guarantorIndex.first,
      misbehaved = reportFault,
      threads = threads,
      telemetry = telemetry,
      inlineGuarantee = nodeConfig.inlineJudgmentGuarantee
    )
    a.onDisregard(h =>
      chain.disregard(h) match
        case Right(head) => logger.warn(s"disregarded ${h.toHex.take(18)}; best is now ${head.hash.toHex.take(18)}")
        case Left(why)   => logger.debug(s"disregard ${h.toHex.take(18)}: $why")
    )
    auditor = Some(a)
    network.registerPeerHandler(StreamKind.AuditAnnouncement, a.auditAnnouncementHandler)
    network.registerPeerHandler(StreamKind.JudgmentPublication, a.judgmentHandler)
    chain.onImported((head, block) => a.onImported(head, block))
    a

  /** The auditor service when auditing is enabled (for verdict queries). */
  def auditorService: Option[AuditorService] = auditor

  /** Attempt to author for `slot`; on success the block is imported and
    * announced
    */
  def authorSlot(slot: Long): Option[ChainHead] =
    if shuttingDown then return None
    // On the clock the ring proofs are made off this thread, which must author by the slot's deadline; a node
    // driven slot by slot (no clock, as in tests) makes them here, so this slot's block already carries them.
    if nodeConfig.slotTicking then tickets.generateInBackground(slot) else tickets.maybeGenerate(slot): Unit
    peers.update(slot)
    maybeSendTickets(slot)
    peers.appliedSlotOf(slot / chain.config.epochLength).foreach(applied => tickets.releaseForwards(slot, applied))
    if catchingUp then return None
    author.flatMap { a =>
      a.tryAuthor(slot).flatMap { block =>
        val authoring = telemetry.flatMap(_.authoring(slot, block.header.parent))
        importAuthored(block, authoring) match
          case Right(head) => Some(head)
          // Only a block the pipeline found invalid says anything about its contents. Losing a race (a peer's block
          // got there first: side branch, already imported) or a failure of ours leaves the pools as they are.
          case Left(ChainError.InvalidBlock(error, _)) if hasContent(block) =>
            // The contents were checked against the state transitions before sealing, so this should not happen.
            authoringGuard.foreach(_.withdraw(slot))
            def retry(): Option[ChainHead] =
              a.tryAuthor(slot, withDisputes = false).flatMap { b =>
                importAuthored(b, telemetry.flatMap(_.authoring(slot, b.header.parent))) match
                  case Right(head) => Some(head)
                  case Left(ChainError.InvalidBlock(_, _)) =>
                    authoringGuard.foreach(_.withdraw(slot))
                    None
                  case Left(_) => None
              }
            val x = block.extrinsic
            val withoutGuarantees =
              if x.guarantees.isEmpty then None
              else
                logger.warn(s"authored block rejected ($error); dropping its ${x.guarantees.size} guarantee(s) and retrying")
                pools.removeGuarantees(x.guarantees)
                retry()
            withoutGuarantees.orElse {
              logger.warn(s"authored block rejected ($error); dropping its assurances and preimages and retrying")
              pools.removeAssurances(x.assurances)
              pools.removePreimages(x.preimages)
              retry()
            }
          case Left(ChainError.BodyMismatch) =>
            logger.error(s"authored block for slot $slot: ${ChainError.BodyMismatch.message}")
            None
          case Left(_) => None
      }
    }

  /** Send this epoch's tickets on CE 131 once (again at once when a reorg forced new tickets): as soon as the epoch's
    * entropy η_2 is final, i.e. a block of the epoch on our best chain is finalized, or else max(⌊E/60⌋, 1) slots
    * after the epoch's connectivity changes were applied (JAMNP-S CE 131). Runs at each slot and on the redial
    * thread, so a finalization mid-slot sends without waiting for the next slot.
    */
  private def maybeSendTickets(slot: Long): Unit = synchronized {
    val e = chain.config.epochLength
    val epoch = slot / e
    val fin = chain.finalized.slot
    val entropyFinal = finalityRunning && fin >= epoch * e && fin / e == epoch && chain.best.slot / e == epoch
    val step1Due = peers.appliedSlotOf(epoch).exists(applied => slot >= applied + math.max(e / 60, 1))
    // A ticket reaching the author at slot index Y − 1 or later misses the lottery.
    val inLottery = slot % e < chain.config.ticketCutoff - 1
    val generation = tickets.generation
    if (entropyFinal || step1Due) && inLottery && ticketsSentGeneration != generation && tickets.hasTicketsFor(epoch)
    then
      ticketsSentGeneration = generation
      logger.info(s"sending epoch $epoch tickets at slot index ${slot % e} (entropy final: $entropyFinal)")
      tickets.distributeTickets(distribution.peers, connectionForNextEpochValidator)
  }

  private val syncGate =
    new SyncGate(JamNode.SyncLagSlots, JamNode.SyncPatienceSlots * spec.config.slotDuration * 1000L)
  @volatile private var held = false

  @volatile private var checkpointSyncing = nodeConfig.syncFrom.isDefined
  private[node] def syncingFromCheckpoint: Boolean = checkpointSyncing

  private def startCheckpointSync(checkpoint: Hash): Unit =
    if !chain.atGenesis then
      logger.info(s"--sync-from ${checkpoint.toHex.take(18)} ignored: this node already has blocks")
      checkpointSyncing = false
    else
      checkpointSyncing = true
      sync.hold()
      val job =
        new CheckpointSync(chain, sync, checkpoint, () => distribution.peers, misbehaved, nodeConfig.syncFromSetId)
      threads.single("jam-checkpoint-sync").execute { () =>
        CheckpointSync.untilStarted(
          () => if chain.atGenesis then job.attempt() else Right(chain.best),
          running = () => checkpointSyncing,
          started = () =>
            try
              grandpaLock.synchronized {
                try if !chain.atGenesis then grandpaService.foreach(_.restartAtFinalized())
                finally checkpointSyncing = false
              }
            finally sync.release()
          ,
          retryMillis = JamNode.CheckpointRetryMillis
        )
      }

  private def credibleBestSlots: List[Long] =
    val claimed = sync.peerBestSlots
    if nodeConfig.slotTicking then claimed.filter(_ <= slotClock.currentSlot + ChainManager.FutureSlotTolerance)
    else claimed

  private def catchingUp: Boolean =
    if checkpointSyncing then return true
    val peerBests = credibleBestSlots
    val best = chain.best.slot
    val holding = syncGate.holding(best, peerBests, System.currentTimeMillis())
    if holding != held then
      held = holding
      if holding then logger.info(s"catching up with peers (best slot $best): authoring, guaranteeing and assuring wait")
      else logger.info(s"caught up (best slot $best): duties resume")
    holding

  private def unlessCatchingUp(handler: io.forge.jam.network.PeerStreamHandler): io.forge.jam.network.PeerStreamHandler =
    (conn, stream) => if catchingUp then stream.reset() else handler.onStream(conn, stream)

  private def finalityRunning: Boolean = nodeConfig.grandpa || nodeConfig.dummyFinality || spec.devnetFinalityDepth > 0

  /** CE 131 proxy lookup: the open connection whose peer key is the Ed25519
    * key of validator `index` in the next epoch's set (γ_k), whose ring the
    * tickets were made for.
    */
  private def connectionForNextEpochValidator(index: Int): Option[PeerConnection] =
    chain.stateView().validators.nextEpoch.lift(index).flatMap { vk =>
      val key = vk.ed25519.bytes.toArray
      distribution.connectionsTo(key).headOption
    }

  private def hasContent(block: io.forge.jam.core.types.block.Block): Boolean =
    val x = block.extrinsic
    x.guarantees.nonEmpty || x.assurances.nonEmpty || x.preimages.nonEmpty || x.disputes.verdicts.nonEmpty

  private def importAuthored(
      block: io.forge.jam.core.types.block.Block,
      authoring: Option[Telemetry.EventRef]
  ): Either[ChainError, ChainHead] =
    val bytes = chain.encodeBlock(block)
    val hash = chain.headerHashOf(block)
    telemetry.foreach(_.authored(authoring, Telemetry.blockOutline(block, bytes.length, hash)))
    importAndAnnounce(bytes) match
      case Right(head) =>
        telemetry.foreach(_.blockExecuted(authoring, chain.takeAccumulateCosts(hash).getOrElse(Map.empty)))
        pools.pruneAfterImport(
          block.extrinsic.guarantees,
          block.extrinsic.assurances,
          block.extrinsic.preimages,
          head.hash
        )
        Right(head)
      case Left(err: ChainError.StoredOnSideBranch) =>
        logger.warn(s"authored block is not the best head: $err")
        telemetry.foreach(_.authoringFailed(authoring, err.message))
        Left(err)
      case Left(err) =>
        countImportFailure(Left(err))
        logger.error(s"authored block failed to import: $err")
        telemetry.foreach(_.authoringFailed(authoring, err.message))
        Left(err)

  def start(): JamNode =
    chain.initializeOrRestore(spec)
    nodeConfig.syncFrom.foreach(startCheckpointSync)

    // Validators of the sets around the head keep their inbound slots whoever else connects.
    network.setPriorityPeers(peers.isValidator)

    noteRecentValidators()
    chain.onImported((_, _) => noteRecentValidators())
    chain.onImported((_, _) => importedBlocks.incrementAndGet(): Unit)

    if nodeConfig.dummyFinality then chain.onImported((_, _) => chain.finalizeAtDepth(1): Unit)
    else if !nodeConfig.grandpa then
      if spec.devnetFinalityDepth > 0 then
        chain.onImported((_, _) =>
          chain.finalizeAtDepth(spec.devnetFinalityDepth, sync.peerBests(countsForFinality), spec.finalityQuorum)
        )
      chain.onImported((_, _) => followPeerFinality())

    chain.onImported((head, block) => preimages.onImported(head, block))
    // Guarantor credentials of included reports: the culprits of a bad verdict, should one come. Their exports, so
    // segments can be rebuilt from the availability layer for a later import.
    chain.onImported((head, block) =>
      if block.extrinsic.guarantees.nonEmpty then
        pools.recordCredentials(block.header.parent, head.slot, block.extrinsic.guarantees)
        exportIndex.record(block.extrinsic.guarantees)
        chain.stateViewAt(block.header.parent).foreach { parent =>
          guarantorIndex.record(List(parent.validators.current, parent.validators.previous), block.extrinsic.guarantees)
        }
    )
    // Custody units are kept for 28 days (672 epochs) after they were last stored, as exported-segment shards
    // must be; bundle shards share the unit and so are kept as long, which is more than required.
    chain.onImported { (_, _) =>
      val deleted = shardStore.deleteStoredBefore(System.currentTimeMillis() / 1000 - JamNode.shardRetentionSeconds(spec.config))
      if deleted > 0 then logger.info(s"expired $deleted shard custody unit(s) older than 28 days")
    }
    chain.onImported((head, block) => pruneTickets(head, block))
    // A reorg's abandoned blocks give their guarantees and preimages back, for a later block of the new chain.
    chain.onAbandoned((abandoned, adopted) => pools.restoreAbandoned(abandoned.map(_.extrinsic), adopted.map(_.extrinsic)))
    // Peers' blocks consume pooled extrinsics too, not only our own.
    chain.onImported { (head, block) =>
      pools.pruneAfterImport(
        block.extrinsic.guarantees,
        block.extrinsic.assurances,
        block.extrinsic.preimages,
        chain.best.hash
      )
      // A later block takes guarantees of its own rotation or the one before, so older ones are dead.
      pools.pruneGuaranteesBefore(
        io.forge.jam.protocol.report.GuarantorAssignment.oldestReportableSlot(head.slot, chain.config)
      )
    }
    telemetry.foreach { _ =>
      chain.onImported((_, _) => reportHeads())
      chain.onFinalized(_ => reportHeads())
    }

    network.onConnectionChange((conn, up) => if up && scores.banned(conn.peerKey) then conn.close())
    network.onConnectionChange { (conn, up) =>
      val peer = try JamNode.hex(conn.peerKey).take(16) catch case _: Exception => "?"
      val side = if conn.initiator then "dialed" else "accepted"
      val where = conn.remoteAddress.map(a => s" $a").getOrElse("")
      if up then
        logger.info(s"peer connected ($side) $peer$where")
        telemetry.foreach(t =>
          try if conn.initiator then t.connectedOut(conn.peerKey, conn.remoteAddress) else t.connectedIn(conn.remoteAddress, conn.peerKey)
          catch case _: Exception => ()
        )
        // Accepted peers are reachable for CE traffic whether or not they
        // open UP 0 to us (UP 0 is limited to grid neighbours).
        if !conn.initiator then distribution.trackConnection(conn)
        peers.dropDuplicateConnections(conn)
      else
        logger.info(s"peer disconnected ($side) $peer$where")
        distribution.untrackConnection(conn)
        sync.forgetConnection(conn)
        telemetry.foreach(t =>
          try t.disconnected(conn.peerKey, "connection closed")
          catch case _: Exception => ()
        )
    }

    // Track accepted peers for distribution when they open UP 0 to us.
    val announceHandler = sync.blockAnnouncementHandler
    network
      .registerPeerHandler(
        StreamKind.BlockAnnouncement,
        (conn, stream) =>
          distribution.trackConnection(conn)
          announceHandler.onStream(conn, stream)
      )
      .registerPeerHandler(StreamKind.BlockRequest, sync.blockRequestHandler)
      .registerPeerHandler(StreamKind.WorkReportDistribution, distribution.workReportHandler)
      .registerPeerHandler(StreamKind.WorkReportRequest, distribution.reportRequestHandler)
      .registerPeerHandler(StreamKind.AssuranceDistribution, distribution.assuranceHandler)
      .registerPeerHandler(StreamKind.ShardDistribution, shards.shardDistributionHandler)
      .registerPeerHandler(StreamKind.AuditShardRequest, shards.auditShardHandler)
      .registerPeerHandler(StreamKind.SegmentShardRequest, shards.segmentShardHandler(verified = false))
      .registerPeerHandler(StreamKind.SegmentShardRequestVerified, shards.segmentShardHandler(verified = true))
      .registerPeerHandler(StreamKind.TicketDistributionStep1, tickets.ticketHandler)
      .registerPeerHandler(StreamKind.TicketDistributionStep2, tickets.ticketHandler)
      .registerPeerHandler(StreamKind.PreimageAnnouncement, preimages.announceHandler)
      .registerPeerHandler(StreamKind.PreimageRequest, preimages.requestHandler)
      .registerPeerHandler(StreamKind.StateRequest, sync.stateRequestHandler)
      .start(new InetSocketAddress(nodeConfig.listenIp, nodeConfig.listenPort))

    logger.info(
      s"node up: chain=${spec.id} alpn=${JamnpConfig(spec.alpnPrefix).alpn} " +
        s"port=$listenPort listen=${JamNode.endpointText(boundAddress)} " +
        s"advertised=${advertisedEndpoint.fold("unspecified (no --external-ip)")(JamNode.endpointText)} " +
        s"identity=${identity.altName}"
    )
    checkAdvertisedEndpoint()
    telemetry.foreach { t =>
      t.start()
      threads
        .scheduled("jam-telemetry-status")
        .scheduleWithFixedDelay(new Periodic("telemetry status")(reportStatus(t)), 2, 2, java.util.concurrent.TimeUnit.SECONDS)
    }
    submitServer = nodeConfig.submitPort.map(p => new SubmitServer(p, preimages).start())

    peers.start()

    if nodeConfig.slotTicking then
      slotTicker = slotClock.scheduleSlotTicks(threads) { slot =>
        try slotTick(slot, slotClock.currentSlot)
        catch case e: Exception => logger.error(s"slot $slot handler failed", e)
      }
    this

  private[node] def slotTick(slot: Long, clockSlot: Long): Unit =
    if syncGate.onTime(slot, clockSlot, slotClock.slotStartMillis(clockSlot + 2)) then authorSlot(slot)
    else logger.info(s"the tick for slot $slot arrived in slot $clockSlot: skipped, and duties wait for two slots")
    reportSyncStatus(slot)
    slotCallback(slot)

  def listenPort: Int = network.boundPort

  def boundAddress: InetSocketAddress = network.boundAddress

  def advertisedEndpoint: Option[InetSocketAddress] = nodeConfig.externalIp.map(new InetSocketAddress(_, listenPort))

  private def checkAdvertisedEndpoint(): Unit =
    advertisedEndpoint.foreach { advertised =>
      val validators = chain.stateView().validators
      val own = JamBytes(identity.publicKeyBytes)
      (validators.current.iterator ++ validators.nextEpoch.iterator)
        .find(v => JamBytes(v.ed25519.bytes.toArray) == own)
        .foreach { v =>
          Connectivity.endpoint(v.metadata.toArray) match
            case Some(onChain) if onChain == advertised => ()
            case onChain =>
              logger.warn(
                s"this validator's metadata on chain names ${onChain.fold("no endpoint")(JamNode.endpointText)}, " +
                  s"but it is reached at ${JamNode.endpointText(advertised)} (--external-ip): other validators dial " +
                  "the endpoint in the metadata"
              )
        }
    }

  /** Keep a connection to `address` (redialed when lost), as for a bootnode. */
  def addPeer(address: InetSocketAddress, expectedEd25519: Option[Array[Byte]] = None): Unit =
    peers.addPeer(address, expectedEd25519)

  private def currentSlot: Long =
    if nodeConfig.slotTicking then slotClock.currentSlot else chain.best.slot

  @volatile private var recentValidatorKeys: (AnyRef, AnyRef, Set[JamBytes]) = (null, null, Set.empty)

  private def noteRecentValidators(): Unit =
    val validators = chain.stateView().validators
    val (current, previous) = (validators.current, validators.previous)
    val memo = recentValidatorKeys
    if (memo._1 ne current) || (memo._2 ne previous) then
      recentValidatorKeys =
        (current, previous, (current.iterator ++ previous.iterator).map(v => JamBytes(v.ed25519.bytes.toArray)).toSet)

  @volatile private var currentValidatorKeys: (AnyRef, Set[JamBytes]) = (null, Set.empty)

  private def submitWorkPackage(core: Int, packageBytes: Array[Byte], extrinsics: Seq[Array[Byte]]): Either[String, Unit] =
    val blob = Array.concat(extrinsics*)
    if core < 0 || core >= spec.config.coresCount then Left(s"there is no core $core")
    else
      guarantor.filter(_.guarantees(core)) match
        case Some(g) => g.submit(core, packageBytes, blob)
        case None =>
          val first = Array((core & 0xff).toByte, ((core >> 8) & 0xff).toByte) ++ packageBytes
          handToGuarantors(core, StreamKind.WorkPackageSubmission, Seq(first, blob))

  private def submitWorkPackageBundle(core: Int, bundleBytes: Array[Byte]): Either[String, Unit] =
    if core < 0 || core >= spec.config.coresCount then Left(s"there is no core $core")
    else
      guarantor.filter(_.guarantees(core)) match
        case Some(g) => g.submitBundle(core, bundleBytes)
        case None =>
          for
            bundle <- io.forge.jam.protocol.refine.WorkPackageBundle.decode(bundleBytes).left.map(e => s"Codec error: $e")
            reported = chain.stateView().beta.history.flatMap(_.reported).map(r => r.hash -> r.exportsRoot).toMap
            resolved <- io.forge.jam.protocol.refine.ImportResolver.resolve(bundle.workPackage, reported)
            _ <- handToGuarantors(
              core,
              StreamKind.WorkPackageBundleSubmission,
              GuarantorService.bundleSubmissionMessages(core, resolved._2, bundle)
            )
          yield ()

  private def handToGuarantors(core: Int, kind: Byte, messages: Seq[Array[Byte]]): Either[String, Unit] =
    val (cores, validators) = GuarantorService.assignments(chain.stateView(), chain.nextBlockSlot, spec.config)
    val assigned = validators.zip(cores).collect { case (v, c) if c == core => v }
    val keys = assigned.map(v => JamBytes(v.ed25519.bytes.toArray)).toSet
    val connected = distribution.peers.filter(c => keys.contains(JamBytes(c.peerKey))).distinctBy(_.peerPublicKey)
    val guarantors = if connected.nonEmpty then connected else dialGuarantors(assigned)
    if guarantors.isEmpty then Left(s"no guarantor of core $core could be reached")
    else
      val taken = guarantors.count(conn => StreamRequest.exchange(conn, kind, messages, 5_000, () => 10_000).isRight)
      logger.info(s"RPC ${StreamKind.name(kind)} for core $core handed to $taken of ${guarantors.size} connected guarantor(s)")
      if taken > 0 then Right(()) else Left(s"no guarantor of core $core took the work-package")

  private lazy val guarantorDialer = threads.cached("jam-guarantor-dial")

  private def dialGuarantors(validators: Seq[io.forge.jam.core.types.epoch.ValidatorKey]): List[PeerConnection] =
    val dials = validators.flatMap { v =>
      Connectivity.endpoint(v.metadata.toArray).map { address =>
        val key = v.ed25519.bytes.toArray
        (address, java.util.concurrent.CompletableFuture.supplyAsync(() => connectPeer(address, Some(key)), guarantorDialer))
      }
    }
    val deadline = System.currentTimeMillis() + JamNode.GuarantorDialMillis
    val reached = dials.flatMap { (address, f) =>
      try Some(f.get(math.max(1L, deadline - System.currentTimeMillis()), java.util.concurrent.TimeUnit.MILLISECONDS))
      catch
        case e: Exception =>
          val cause = Option(e.getCause).getOrElse(e)
          logger.debug(s"guarantor at $address not reached: ${cause.getClass.getSimpleName} ${Option(cause.getMessage).getOrElse("")}")
          None
    }.toList
    logger.info(s"dialed ${reached.size} of ${validators.size} guarantor(s) (${dials.size} with an endpoint)")
    reached

  private def fetchSegments(segmentsRoot: Hash, indices: Seq[Int]): Option[IndexedSeq[Array[Byte]]] =
    segments.get(segmentsRoot, indices).orElse {
      segmentRecovery.recover(segmentsRoot, indices).flatMap { found =>
        Option.when(indices.forall(found.contains))(indices.map(found(_)._1).toIndexedSeq)
      }
    }

  /** Whether the peer's key is a guarantor of `core` now or in the rotation before (CE 134). */
  private def isGuarantorOf(conn: PeerConnection, core: Int): Boolean =
    val view = chain.stateView()
    val v = view.validators
    GuarantorService
      .guarantorsOf(
        view.timeslot,
        view.entropy.pool,
        v.previous.toIndexedSeq,
        v.current.toIndexedSeq,
        v.nextEpoch.toIndexedSeq,
        core,
        chain.nextBlockSlot,
        spec.config
      )
      .exists(k => java.util.Arrays.equals(k.ed25519.bytes.toArray, conn.peerKey))

  private def isCurrentValidator(conn: PeerConnection): Boolean =
    val validators = chain.stateView().validators.current
    var memo = currentValidatorKeys
    if memo._1 ne validators then
      memo = (validators, validators.iterator.map(v => JamBytes(v.ed25519.bytes.toArray)).toSet)
      currentValidatorKeys = memo
    memo._2.contains(JamBytes(conn.peerKey))

  /** Whose unsigned word counts towards finality on a node without GRANDPA: the current validators, by connection
    * key, and, when the operator named a number of peers to trust (`--finality-peers`), the peers this node was told
    * to keep a connection to as well: the chain spec's bootnodes and its own `--bootnode`s. A peer that merely
    * connected is never counted, or any host that can reach the port could decide what this node finalizes.
    */
  private def countsForFinality(conn: PeerConnection): Boolean =
    isCurrentValidator(conn) || (nodeConfig.finalityPeers.isDefined && peers.isConfigured(conn.peerKey))

  private def followPeerFinality(): Unit = sync.followPeerFinality(peerFinalityQuorum, countsForFinality)

  /** Peers whose announced finality we follow: with this node they make the finality quorum (2V/3+1). */
  private def peerFinalityQuorum: Int =
    nodeConfig.finalityPeers.map(math.max(_, 1)).getOrElse(math.max(spec.finalityQuorum - 1, 1))

  /** Run a CE 141 distribution about 2 s before the next slot (JAMNP-S CE
    * 141), or at once without slot ticking.
    */
  private def scheduleAssurances(f: () => Unit): Unit =
    val timer = assuranceTimer
    if timer == null then f()
    else
      val delay = slotClock.slotStartMillis(slotClock.currentSlot + 1) - 2000 - System.currentTimeMillis()
      if delay <= 0 then f()
      else
        try timer.schedule((() => f()): Runnable, delay, java.util.concurrent.TimeUnit.MILLISECONDS): Unit
        catch case _: java.util.concurrent.RejectedExecutionException => ()

  /** Drop pooled tickets that `block` included (whoever authored it) and,
    * at an epoch change, every ticket entered for an earlier epoch: their
    * proofs are bound to that epoch's η_2 and γ_z (GP 0.8.0 §safrole).
    */
  private def pruneTickets(head: ChainHead, block: io.forge.jam.core.types.block.Block): Unit =
    val ids = block.extrinsic.tickets.flatMap(t =>
      io.forge.jam.crypto.BandersnatchVrf.ringVrfOutput(t.signature.toArray).map(Hash(_))
    )
    pools.removeTickets(ids)
    pools.pruneTicketsBefore(head.slot / spec.config.epochLength)
    tickets.processDeferred()

  /** JIP-3 events 11 and 12 when the best or finalized head moved. Called from the import listener, the slot tick
    * and whichever thread finalized: one at a time, each reading the heads of one chain change inside the lock, so a
    * head is reported once, a later caller never reports an older head than an earlier one did, and the best head is
    * reported before the finalized head of the same change (the finalized block is never ahead of the best one
    * reported).
    */
  private def reportHeads(): Unit =
    telemetry.foreach { t =>
      headsReport.synchronized {
        val (best, fin) = chain.heads
        if best.hash != lastBest then
          lastBest = best.hash
          t.bestBlockChanged(best.slot, best.hash)
        if fin.hash != lastFinalized then
          lastFinalized = fin.hash
          t.finalizedBlockChanged(fin.slot, fin.hash)
      }
    }

  private def reportStatus(t: Telemetry): Unit =
    try
      val connected = distribution.peers
      val byCore = new Array[Byte](spec.config.coresCount)
      pools.pendingGuarantees.foreach { g =>
        val core = g.report.coreIndex.toInt
        if core >= 0 && core < byCore.length && (byCore(core) & 0xff) < 255 then byCore(core) = (byCore(core) + 1).toByte
      }
      val (shardUnits, shardBytes) = shardStore.estimatedUnitsAndBytes
      val pending = pools.pendingPreimages
      t.status(
        peers = connected.map(c => JamBytes(c.peerKey)).distinct.size,
        validatorPeers = connected.map(c => JamBytes(c.peerKey)).distinct.count(k => peers.isValidator(k.toArray)),
        announcingPeers = connected.count(sync.hasAnnouncementStream),
        guaranteesByCore = byCore,
        shards = shardUnits,
        shardBytes = shardBytes,
        preimages = pending.size,
        preimageBytes = pending.map(_.blob.length.toLong).sum
      )
    catch case e: Exception => logger.debug(s"telemetry status failed: ${e.getMessage}")

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
  ): PeerConnection = peers.connectPeer(address, expectedEd25519)

  /** Import a locally produced or externally obtained block and announce it. */
  def importAndAnnounce(blockBytes: Array[Byte]): Either[ChainError, ChainHead] =
    chain.importBlock(blockBytes).map { head =>
      chain.decodeBlock(blockBytes).foreach { b =>
        sync.announce(chain.headerCodec.encode(b.header))
      }
      head
    }

  /** Run GRANDPA, voting with `keys` when they are in the voter set. Set 0's voters are the validators of the genesis
    * state.
    */
  def enableGrandpa(keys: Option[ValidatorKeySet]): GrandpaService =
    val g = new GrandpaService(
      chain,
      sync,
      distribution,
      keys,
      audited = h => auditor.forall(_.isAudited(h)),
      serving = serving,
      threads = threads,
      misbehaved = misbehaved,
      paused = () => checkpointSyncing
    )
    // Start the voter before taking CE 149-153 streams, so no message finds it unset.
    // Set 0 is the genesis validators whatever the node's best state is now: a follower restarted as a voter after
    // the validators rotated must still check set 0's justifications.
    val (genesisKeys, genesisNextKeys) = spec.genesisValidatorKeys
    val initial =
      if genesisKeys.nonEmpty then genesisKeys else chain.stateView().validators.current.map(_.ed25519.bytes).toIndexedSeq
    grandpaLock.synchronized {
      g.start(spec.genesisHeaderHash, initial, if genesisNextKeys.nonEmpty then genesisNextKeys else initial)
      grandpaService = Some(g)
    }
    g.register(network)
    g

  @volatile private var grandpaService: Option[GrandpaService] = None

  private val grandpaLock = new Object
  @volatile private var rpcServer: Option[RpcServer] = None

  def enableRpc(
      port: Int,
      host: String = "127.0.0.1",
      allowedOrigins: Seq[String] = Nil,
      methods: RpcMethods = RpcMethods.Auto,
      limits: RpcServer.Limits = RpcServer.Limits()
  ): RpcServer =
    val server =
      new RpcServer(new java.net.InetSocketAddress(host, port), rpcApi, threads, allowedOrigins.toSet, methods, limits)
        .start()
    rpcServer = Some(server)
    server

  private[node] lazy val rpcApi: RpcApi =
    new RpcApi(
      chain,
      spec.genesisHeaderHash,
      peerCount = () => distribution.peerCount,
      syncing = () => catchingUp,
      submitPreimage = preimages.submit,
      submitWorkPackage = submitWorkPackage,
      submitBundle = submitWorkPackageBundle,
      knownReport = distribution.knownReport,
      fetchSegments = fetchSegments,
      slow = threads.fixed("jam-rpc-slow", 2, numbered = true)
    )
  @volatile private var submitServer: Option[SubmitServer] = None
  @volatile private var metricsServer: Option[MetricsServer] = None

  def enableMetrics(port: Int, host: String = "127.0.0.1"): MetricsServer =
    val server =
      new MetricsServer(new InetSocketAddress(host, port), () => metrics, () => health, threads.single("jam-metrics")).start()
    metricsServer = Some(server)
    server

  def validatorConnectivity: Option[ValidatorConnectivity] = peers.validatorConnectivity

  def health: NodeHealth =
    val (best, finalized) = chain.heads
    val peerCount = distribution.peerCount
    val syncing = catchingUp
    val behind = if nodeConfig.slotTicking then slotClock.currentSlot - best.slot else 0L
    val following = !syncing && (peerCount > 0 || peers.holdsWholeValidatorSet) && behind <= JamNode.HealthLagSlots
    NodeHealth(best, finalized, peerCount, syncing, following, validatorConnectivity)

  def metrics: NodeMetrics =
    val h = health
    val keys = distribution.peers.map(c => JamBytes(c.peerKey)).distinct
    NodeMetrics(
      health = h,
      validatorPeers = keys.count(k => peers.isValidator(k.toArrayUnsafe)),
      importedBlocks = importedBlocks.get,
      importFailures = importFailures.get,
      guaranteesMade = guarantor.fold(0L)(_.guaranteesMade),
      auditsDone = auditor.fold(0L)(_.auditsDone),
      telemetryDroppedEvents = telemetry.fold(0L)(_.droppedEvents),
      queuedSubmissions = guarantor.fold(0)(_.queuedSubmissions),
      slotsBehindBestPeer = math.max(0L, credibleBestSlots.maxOption.getOrElse(0L) - h.best.slot),
      timings = chain.timings.snapshot,
      sync = sync.stats,
      stages = io.forge.jam.protocol.pipeline.StageTimers.timings
    )

  /** Port of the local submission endpoint, when it runs. */
  def submitPort: Option[Int] = submitServer.map(_.boundPort)

  /** BEEFY prototype: sign finalized blocks with `keys` (when given) and aggregate peers' signatures into proofs. */
  def enableBeefy(keys: Option[ValidatorKeySet]): BeefyService =
    val b = new BeefyService(chain, distribution, keys, threads = threads)
    b.register(network)
    b.start()
    beefyService = Some(b)
    b

  @volatile private var beefyService: Option[BeefyService] = None

  /** The thread pools that were still running when the last [[shutdown]] closed the stores; none, normally. */
  @volatile private[node] var poolsLeftRunning: List[String] = Nil

  @volatile private[node] var shutdownFault: () => Unit = () => ()

  def shutdown(): Unit =
    shuttingDown = true
    var interrupted = false
    def step(name: String)(action: => Unit): Unit =
      try action
      catch
        case e: InterruptedException =>
          interrupted = true
          logger.error(s"shutdown: $name was interrupted", e)
        case e: Exception => logger.error(s"shutdown: $name failed", e)
    step("fault injection")(shutdownFault())
    step("peers (begin)")(peers.beginShutdown())
    step("submission endpoint")(submitServer.foreach(_.stop()))
    step("RPC server")(rpcServer.foreach(_.stop()))
    step("metrics server")(metricsServer.foreach(_.stop()))
    step("GRANDPA")(grandpaService.foreach(_.shutdown()))
    step("BEEFY")(beefyService.foreach(_.shutdown()))
    step("auditor")(auditor.foreach(_.shutdown()))
    step("assurer")(assurer.foreach(_.shutdown())) // before the shard store closes
    step("shards")(shards.shutdown())
    step("peers")(peers.shutdown())
    step("assurance timer")(if assuranceTimer != null then assuranceTimer.shutdownNow(): Unit)
    step("telemetry")(telemetry.foreach(_.shutdown()))
    step("slot ticker")(if slotTicker != null then slotTicker.close()) // waits for in-flight tick
    step("tickets")(tickets.shutdown())
    step("guarantor")(guarantor.foreach(_.shutdown()))
    step("refine pool")(refinePool.shutdown()) // drains in-flight refine work
    step("network")(network.shutdown())
    step("inbound work")(inbound.shutdown()) // received messages still being checked read the chain
    step("serving")(serving.shutdown()) // requests being answered read the store
    step("announcing")(announcing.shutdown())
    step("sync")(sync.shutdown()) // waits for an in-flight sync import
    step("distribution")(distribution.shutdown())
    step("egress")(egress.shutdown())
    step("egress (wait)")(egress.awaitTermination(10, java.util.concurrent.TimeUnit.SECONDS): Unit)
    step("preimages")(preimages.shutdown()) // its own executor for CE143 fetches, drained alongside egress
    // Several services only interrupt their threads. Wait for every pool, so none reads a store after it closes.
    step("thread pools") { poolsLeftRunning = threads.close() }
    shutdownStorageOnly()
    if interrupted then Thread.currentThread().interrupt()

  /** Close only the storage layers (for tests that never start networking). Each close runs whatever the others
    * did; a failure is logged.
    */
  def shutdownStorageOnly(): Unit =
    val pruneStopped =
      try chain.shutdown()
      catch
        case e: Exception =>
          logger.error("shutdown: the chain failed to stop; a state prune may still run, so the stores are left open", e)
          false
    def close(name: String)(action: => Unit): Unit =
      try action
      catch case e: Exception => logger.error(s"shutdown: closing the $name store failed", e)
    close("shard")(shardStore.close())
    if pruneStopped then
      close("block")(blockStore.close())
      close("state")(trieBackend.close())

object JamNode:

  val CheckpointRetryMillis: Long = 2000L

  /** How long shard custody units are kept: 672 complete epochs (28 days at the full configuration). */
  def shardRetentionSeconds(config: io.forge.jam.core.ChainConfig): Long =
    672L * config.epochLength * config.slotDuration

  val SyncLagSlots: Long = 2L
  val SyncPatienceSlots: Long = 5L

  /** Upper bound on the redial backoff. */
  val MaxRedialMillis: Long = 16000L

  val GuarantorDialMillis: Long = 5000L

  /** How long an accepted connection may go without UP 0 before we open it. */
  val UpZeroGraceMillis: Long = 3000L

  val ImplementationVersion: String = implementationVersion(BuildInfo.version, BuildInfo.gitSha)

  def implementationVersion(version: String, gitSha: Option[String]): String =
    gitSha.fold(version)(sha => s"$version+${sha.take(8)}")
  val GrayPaperVersion: String = "0.8.0"

  val AnyAddress: java.net.InetAddress = java.net.InetAddress.getByAddress(Array[Byte](0, 0, 0, 0))

  val HealthLagSlots: Long = 10L

  def endpointText(address: InetSocketAddress): String =
    address.getAddress match
      case v6: java.net.Inet6Address =>
        val b = v6.getAddress
        val groups = (0 until 8).map(i => ((b(2 * i) & 0xff) << 8) | (b(2 * i + 1) & 0xff))
        val (start, length) = (0 until 8).foldLeft((0, 0)) { case (best @ (_, bestLength), i) =>
          val run = groups.drop(i).takeWhile(_ == 0).size
          if (i == 0 || groups(i - 1) != 0) && run > bestLength then (i, run) else best
        }
        val hex = groups.map(Integer.toHexString)
        val text =
          if length < 2 then hex.mkString(":")
          else hex.take(start).mkString(":") + "::" + hex.drop(start + length).mkString(":")
        s"[$text]:${address.getPort}"
      case a if a != null => s"${a.getHostAddress}:${address.getPort}"
      case _              => s"${address.getHostString}:${address.getPort}"

  private[node] def hex(bytes: Array[Byte]): String =
    bytes.map(b => f"${b & 0xff}%02x").mkString
    
  def verifyPeerKey(expected: Array[Byte], actual: Array[Byte]): Either[String, Unit] =
    if java.util.Arrays.equals(expected, actual) then Right(())
    else Left(s"peer key mismatch: expected ${hex(expected)}, got ${hex(actual)}")
