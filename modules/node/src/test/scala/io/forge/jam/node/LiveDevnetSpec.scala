package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Live-clock devnet soak: two nodes run on the real slot ticker (6 s slots,
  * era aligned so slot numbers continue from the genesis slot). The
  * validator node authors on its own clock — including Safrole tickets —
  * and the keyless peer follows over JAMNP.
  */
class LiveDevnetSpec extends AnyFunSuite with Matchers:

  private val baseDir =
    sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath =
    baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def tempDir(p: String): Path = Files.createTempDirectory(p)
  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  test("nodes author and follow on the live slot clock") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val spec = ChainSpec(
      id = "live-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

    // Align the era so the wall clock sits a little past the genesis slot:
    // the next tick authors genesisSlot + 1.
    val genesisSlot = genesis.header.slot.value.toLong
    val eraStart =
      System.currentTimeMillis() / 1000 - genesisSlot * spec.config.slotDuration - 1

    val dirA = tempDir("jam-live-a")
    val dirB = tempDir("jam-live-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(
        spec,
        NodeConfig(dataDir = dirA, eraStartSeconds = eraStart, slotTicking = true)
      ).start()
      nodeB = new JamNode(
        spec,
        NodeConfig(dataDir = dirB, eraStartSeconds = eraStart, slotTicking = true)
      ).start()
      nodeA.enableAuthoring((0 until 6).map(ValidatorKeySet.dev))

      nodeB.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeA.listenPort))

      // Let three slot boundaries pass (6 s slots).
      val target = genesisSlot + 3
      val deadline = System.currentTimeMillis() + 45000
      while nodeA.chain.best.slot < target && System.currentTimeMillis() < deadline do
        Thread.sleep(250)

      nodeA.chain.best.slot should be >= target

      // The peer follows to the same head shortly after.
      val syncDeadline = System.currentTimeMillis() + 15000
      while nodeB.chain.best.slot < target && System.currentTimeMillis() < syncDeadline do
        Thread.sleep(250)
      nodeB.chain.best.slot should be >= target
      nodeB.chain.best.stateRoot shouldBe nodeA.chain.best.stateRoot
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }
