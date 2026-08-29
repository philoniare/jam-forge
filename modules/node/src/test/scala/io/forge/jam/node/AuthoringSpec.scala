package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Block authoring on the dev genesis: a node holding all six tiny validators authors a
  * chain of blocks through the full import pipeline, and a keyless peer
  * follows it over JAMNP.
  */
class AuthoringSpec extends AnyFunSuite with Matchers:

  private val baseDir =
    sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath =
    baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def specFor(genesis: Genesis): ChainSpec =
    ChainSpec(
      id = "authoring-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

  private def tempDir(prefix: String): Path = Files.createTempDirectory(prefix)
  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  test("a validator node authors a chain of sealed blocks and a peer follows") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )

    val dirA = tempDir("jam-author-a")
    val dirB = tempDir("jam-author-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      val spec = specFor(genesis)
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false)).start()
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false)).start()

      // Node A holds all six tiny dev validators → it owns every slot.
      nodeA.enableAuthoring((0 until 6).map(ValidatorKeySet.dev))

      nodeB.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeA.listenPort))

      val genesisSlot = genesis.header.slot.value.toLong

      // Author five consecutive slots deterministically.
      for i <- 1 to 5 do
        val slot = genesisSlot + i
        val head = nodeA.authorSlot(slot)
        withClue(s"authoring slot $slot:") {
          head.isDefined shouldBe true
        }
        nodeA.chain.best.slot shouldBe slot

      // The keyless peer follows via announcements + CE 128.
      val deadline = System.currentTimeMillis() + 30000
      while nodeB.chain.best.hash != nodeA.chain.best.hash &&
        System.currentTimeMillis() < deadline
      do Thread.sleep(100)

      nodeB.chain.best.hash shouldBe nodeA.chain.best.hash
      nodeB.chain.best.slot shouldBe genesisSlot + 5
      nodeB.chain.best.stateRoot shouldBe nodeA.chain.best.stateRoot
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("ticket generation fills the accumulator and epoch 1 seals with tickets") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val dir = tempDir("jam-ticketed")
    var node: JamNode = null
    try
      node = new JamNode(specFor(genesis), NodeConfig(dataDir = dir, slotTicking = false))
      node.chain.initializeOrRestore(node.spec)
      node.enableAuthoring((0 until 6).map(ValidatorKeySet.dev))

      val genesisSlot = genesis.header.slot.value.toLong
      val epochLen = node.spec.config.epochLength // 12 (TINY)

      // Author through epoch 0. Tickets (18 candidates, ≤3 per block) fill
      // the 12-slot accumulator well before the cutoff at phase 10.
      var ticketsIncluded = 0
      for i <- 1 until epochLen do
        val slot = genesisSlot + i
        withClue(s"epoch-0 slot $slot:") {
          node.authorSlot(slot).isDefined shouldBe true
        }
        val block = node.chain
          .decodeBlock(node.chain.blockStore.getBlock(node.chain.best.hash).get)
          .toOption
          .get
        ticketsIncluded += block.extrinsic.tickets.size
      ticketsIncluded should be >= epochLen

      // Epoch 1: the sealer sequence is now tickets; sealing must use our
      // tracked ticket attempts.
      import io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys
      for i <- 0 until 4 do
        val slot = genesisSlot + epochLen + i
        withClue(s"epoch-1 slot $slot:") {
          node.authorSlot(slot).isDefined shouldBe true
        }
        if i == 0 then
          node.chain.stateView().gamma.st match
            case TicketsOrKeys.Tickets(_) => () // ticketed epoch confirmed
            case other                    => fail(s"expected ticketed sealer sequence, got Keys")
    finally
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)
  }

  test("a node without the sealing key does not author") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val dir = tempDir("jam-author-nokeys")
    var node: JamNode = null
    try
      node = new JamNode(specFor(genesis), NodeConfig(dataDir = dir, slotTicking = false))
      node.chain.initializeOrRestore(node.spec)
      // Keys from an index outside the tiny validator set.
      node.enableAuthoring(Seq(ValidatorKeySet.dev(17)))
      node.authorSlot(genesis.header.slot.value.toLong + 1) shouldBe None
    finally
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)
  }
