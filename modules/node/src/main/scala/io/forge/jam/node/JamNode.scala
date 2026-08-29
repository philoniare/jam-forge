package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend}
import io.forge.jam.network.{JamnpConfig, JamnpConnection, JamnpNode, NodeIdentity, StreamKind}

/** Node runtime configuration. */
final case class NodeConfig(
    dataDir: Path,
    listenPort: Int = 0,
    /** 32-byte Ed25519 seed; random identity when absent. */
    ed25519Seed: Option[Array[Byte]] = None,
    /** JAM-common-era override for devnets whose genesis starts "now". */
    eraStartSeconds: Long = SlotClock.JamCommonEraSeconds,
    slotTicking: Boolean = true
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

  private val trieBackend =
    RocksDbTrieBackend.open(nodeConfig.dataDir.resolve("state"))
  private val blockStore = BlockStore.open(nodeConfig.dataDir.resolve("blocks"))

  val chain = new ChainManager(spec.config, trieBackend, blockStore)
  val sync = new SyncService(chain)
  val pools = new ExtrinsicPools
  val distribution = new DistributionService(pools)
  distribution.coresCountForDecode = spec.config.coresCount

  val slotClock = new SlotClock(
    eraStartSeconds = nodeConfig.eraStartSeconds,
    slotSeconds = spec.config.slotDuration
  )

  private val network = new JamnpNode(
    identity,
    JamnpConfig(genesisHashPrefix = spec.alpnPrefix)
  )

  @volatile private var slotTicker: AutoCloseable = null
  @volatile private var slotCallback: Long => Unit = _ => ()
  @volatile private var author: Option[BlockAuthor] = None
  @volatile private var guarantor: Option[GuarantorService] = None
  @volatile private var tickets: Option[TicketService] = None
  @volatile private var shuttingDown: Boolean = false

  /** Hook invoked at every slot boundary (after any authoring attempt). */
  def onSlot(f: Long => Unit): Unit = slotCallback = f

  /** Enable block authoring with this node's validator keys, including
    * Safrole ticket generation for ticketed sealing.
    */
  def enableAuthoring(keys: Seq[ValidatorKeySet]): Unit =
    val ts = new TicketService(chain, pools, keys)
    tickets = Some(ts)
    author = Some(new BlockAuthor(chain, keys, pools, ts.ownTickets))

  /** Enable the guarantor role (CE 133 work-package intake → refine → sign →
    * CE 135 distribution) with this node's validator keys.
    */
  def enableGuaranteeing(keys: Seq[ValidatorKeySet]): Unit =
    val g = new GuarantorService(chain, distribution, pools, keys)
    guarantor = Some(g)
    network.registerHandler(
      StreamKind.WorkPackageSubmission,
      g.workPackageSubmissionHandler
    )

  /** Enable the assurer role: after every imported block, pending cores are
    * assured with each held validator key and distributed via CE 141.
    */
  def enableAssuring(keys: Seq[ValidatorKeySet]): Unit =
    val a = new AssurerService(chain, distribution, pools, keys)
    chain.onImported((head, block) => a.onImported(head, block))

  /** Attempt to author for `slot`; on success the block is imported and
    * announced
    */
  def authorSlot(slot: Long): Option[ChainManager#Head] =
    if shuttingDown then return None
    tickets.foreach(_.maybeGenerate())
    author.flatMap { a =>
      a.tryAuthor(slot).flatMap { block =>
        importAuthored(block) match
          case Some(head) => Some(head)
          case None if block.extrinsic.guarantees.nonEmpty ||
              block.extrinsic.assurances.nonEmpty ||
              block.extrinsic.preimages.nonEmpty =>
            logger.warn("authored block rejected; dropping pools and retrying empty")
            pools.clear()
            a.tryAuthor(slot).flatMap(importAuthored)
          case None => None
      }
    }

  private def importAuthored(block: io.forge.jam.core.types.block.Block): Option[ChainManager#Head] =
    importAndAnnounce(chain.encodeBlock(block)) match
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
        None

  def start(): JamNode =
    Files.createDirectories(nodeConfig.dataDir)
    chain.initializeOrRestore(spec)

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
      .registerHandler(StreamKind.AssuranceDistribution, distribution.assuranceHandler)
      .start(new InetSocketAddress("0.0.0.0", nodeConfig.listenPort))

    logger.info(
      s"node up: chain=${spec.id} alpn=${JamnpConfig(spec.alpnPrefix).alpn} " +
        s"port=$listenPort identity=${identity.altName}"
    )

    spec.bootnodes.foreach { bn =>
      try
        val conn = connectPeer(bn.address)
        logger.info(s"connected bootnode ${bn.host}:${bn.port}")
      catch
        case e: Exception =>
          logger.warn(s"bootnode ${bn.host}:${bn.port} unreachable: ${e.getMessage}")
    }

    if nodeConfig.slotTicking then
      slotTicker = slotClock.scheduleSlotTicks { slot =>
        try
          authorSlot(slot)
          slotCallback(slot)
        catch case e: Exception => logger.error(s"slot $slot handler failed", e)
      }
    this

  def listenPort: Int = network.boundPort

  /** Connect to a peer and open the UP 0 announcement stream. */
  def connectPeer(address: InetSocketAddress): JamnpConnection =
    val conn = network.connect(address).get(15, java.util.concurrent.TimeUnit.SECONDS)
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
    if slotTicker != null then slotTicker.close() // waits for in-flight tick
    network.shutdown()
    sync.shutdown() // waits for an in-flight sync import
    shutdownStorageOnly()

  /** Close only the storage layers (for tests that never start networking). */
  def shutdownStorageOnly(): Unit =
    blockStore.close()
    trieBackend.close()
