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
    eraStartSeconds: Long = SlotClock.JamCommonEraSeconds
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

  /** Hook invoked at every slot boundary (authoring plugs in here). */
  def onSlot(f: Long => Unit): Unit = slotCallback = f

  def start(): JamNode =
    Files.createDirectories(nodeConfig.dataDir)
    chain.initializeOrRestore(spec)

    network
      .registerHandler(StreamKind.BlockAnnouncement, sync.blockAnnouncementHandler)
      .registerHandler(StreamKind.BlockRequest, sync.blockRequestHandler)
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

    slotTicker = slotClock.scheduleSlotTicks { slot =>
      try slotCallback(slot)
      catch case e: Exception => logger.error(s"slot $slot handler failed", e)
    }
    this

  def listenPort: Int = network.boundPort

  /** Connect to a peer and open the UP 0 announcement stream. */
  def connectPeer(address: InetSocketAddress): JamnpConnection =
    val conn = network.connect(address).get(15, java.util.concurrent.TimeUnit.SECONDS)
    sync.openAnnouncementStream(conn)
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
    if slotTicker != null then slotTicker.close()
    network.shutdown()
    shutdownStorageOnly()

  /** Close only the storage layers (for tests that never start networking). */
  def shutdownStorageOnly(): Unit =
    blockStore.close()
    trieBackend.close()
