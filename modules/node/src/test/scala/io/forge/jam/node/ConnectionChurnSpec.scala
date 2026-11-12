package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.network.{JamnpConfig, JamnpNode, NodeIdentity}
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Peers that connect and leave must not accumulate in the node: a closed connection is dropped from the
  * distribution service (and the sync service's per-connection maps) when it closes.
  */
class ConnectionChurnSpec extends AnyFunSuite with Matchers:

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def tempDir(p: String): Path = Files.createTempDirectory(p)
  private def cleanup(dir: Path): Unit =
    if Files.exists(dir) then
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  private def eventually(timeoutMillis: Long)(cond: => Boolean): Boolean =
    val deadline = System.currentTimeMillis() + timeoutMillis
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(20)
    cond

  test("2000 connect/disconnect cycles leave no closed connection behind, and a live peer is still tracked") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val spec = ChainSpec(
      id = "churn-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dir = tempDir("jam-churn")
    var node: JamNode = null
    // A handful of peer identities, reused: the growth on trial is per connection, not per key.
    val clients = (0 until 4).map(_ => new JamnpNode(NodeIdentity.generate(), JamnpConfig(genesisHashPrefix = spec.alpnPrefix)))
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false)).start()
      val address = new InetSocketAddress("127.0.0.1", node.listenPort)
      val cycles = 2000
      var maxTracked = 0
      for i <- 0 until cycles do
        val conn = clients(i % clients.size).connect(address).get(10, TimeUnit.SECONDS)
        conn.close()
        if i % 50 == 49 then
          // Let the node see the closes, as it would between real reconnects.
          eventually(10000)(node.distribution.trackedConnectionCount <= clients.size)
          maxTracked = math.max(maxTracked, node.distribution.trackedConnectionCount)
      withClue("closed connections still tracked: ") {
        eventually(20000)(node.distribution.trackedConnectionCount == 0) shouldBe true
      }
      maxTracked should be <= clients.size
      node.distribution.peers shouldBe empty

      // A peer that stays is tracked and listed.
      val live = clients.head.connect(address).get(10, TimeUnit.SECONDS)
      eventually(10000)(node.distribution.peers.size == 1) shouldBe true
      node.distribution.trackedConnectionCount shouldBe 1
      java.util.Arrays.equals(node.distribution.peers.head.peerKey, clients.head.identity.publicKeyBytes) shouldBe true
      node.distribution.peerCount shouldBe 1
      live.close()
      eventually(10000)(node.distribution.trackedConnectionCount == 0) shouldBe true
    finally
      clients.foreach(c =>
        try c.shutdown()
        catch case _: Exception => ()
      )
      if node != null then node.shutdown()
      cleanup(dir)
  }
