package io.forge.jam.node

import java.net.InetSocketAddress
import java.util.concurrent.{CompletableFuture, Executors}
import java.util.concurrent.atomic.AtomicLong

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Ed25519PublicKey
import io.forge.jam.network.{MessageStream, PeerConnection}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ConnectivityTickSpec extends AnyFunSuite with Matchers with NodeFixture:

  private final class CountingConnection(key: Array[Byte], reads: AtomicLong) extends PeerConnection:
    val initiator: Boolean = false
    val openedAtMillis: Long = System.currentTimeMillis()
    def peerKey: Array[Byte] = { reads.incrementAndGet(); key }
    lazy val peerPublicKey: Ed25519PublicKey = Ed25519PublicKey(key)
    def negotiatedAlpn: String = "test"
    def holdInbound(bytes: Long): Boolean = true
    def releaseInbound(bytes: Long): Unit = ()
    def heldInboundBytes: Long = 0L
    def pendingOutboundBytes: Long = 0L
    def droppedOutboundMessages: Long = 0L
    def openStream(kind: Byte): CompletableFuture[? <: MessageStream] =
      CompletableFuture.failedFuture(new IllegalStateException("no streams in this test"))
    @volatile private var open = true
    def close(): Unit = open = false
    def isOpen: Boolean = open
    def openStreamCount: Int = 0
    def remoteAddress: Option[java.net.SocketAddress] = None

  test("a connectivity tick at 1,023 validators sorts the peers at most once and reads each peer's key a few times") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val spec = ChainSpec(
      id = "connectivity-tick",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dir = tempDir("jam-connectivity-tick")
    var node: JamNode = null
    val threads = new NodeThreads
    val egress = Executors.newSingleThreadExecutor()
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false))
      node.chain.initializeOrRestore(spec)
      val reads = new AtomicLong()
      val sorts = new AtomicLong()
      val scores = new PeerScores(exempt = _ => true)
      val distribution = new DistributionService(
        node.pools,
        spec.config.coresCount,
        egress,
        threads = threads,
        askOrder = peers =>
          sorts.incrementAndGet()
          scores.leastFaultedFirst(peers)(_.peerKey)
      )
      val peerCount = 1022
      val keys = (1 to peerCount).map(i => Array.tabulate[Byte](32)(j => ((i >> (8 * (j % 2))) + j).toByte))
      keys.headOption.foreach(scores.report(_, Offence.Unanswered)) // someone faulted: the ordering really sorts
      val connections = keys.map(k => new CountingConnection(k, reads))
      connections.foreach(distribution.trackConnection)
      val peers = new PeerConnectivity(
        node.identity,
        node.network,
        node.chain,
        node.sync,
        distribution,
        spec,
        NodeConfig(dataDir = dir, slotTicking = false),
        currentSlot = () => 0L,
        upkeep = () => (),
        threads = threads
      )
      try
        peers.update(0L) // the view of slot 0 applied: the ticks below leave the dials alone
        peers.keepConnectedTo(keys.map(k => JamBytes(k) -> new InetSocketAddress("127.0.0.1", 1)).toMap)
        (1 to 3).foreach(_ => peers.maintain()) // warm up
        val ticks = 20
        reads.set(0); sorts.set(0)
        val t0 = System.nanoTime()
        (1 to ticks).foreach(_ => peers.maintain())
        val perTickMs = (System.nanoTime() - t0) / 1e6 / ticks
        val readsPerTick = reads.get.toDouble / ticks
        val sortsPerTick = sorts.get.toDouble / ticks
        info(f"$peerCount peers: $perTickMs%.2f ms, $sortsPerTick%.1f sorts, $readsPerTick%.0f key reads per tick")
        sortsPerTick should be <= 1.0
        readsPerTick should be <= 4.0 * peerCount
      finally peers.shutdown()
    finally
      egress.shutdownNow()
      threads.close()
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)
  }
