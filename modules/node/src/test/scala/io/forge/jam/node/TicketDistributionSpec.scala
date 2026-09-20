package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.types.tickets.TicketEnvelope
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UByte

class TicketDistributionSpec extends AnyFunSuite with Matchers:

  private def envelope(fill: Byte): TicketEnvelope =
    TicketEnvelope(UByte(1), JamBytes(Array.fill[Byte](784)(fill)))

  test("TicketCodec round-trips epoch ++ attempt ++ proof (789 bytes)") {
    val bytes = TicketCodec.encode(42L, envelope(7))
    bytes.length shouldBe 789
    val Right((epoch, env)) = TicketCodec.decode(bytes): @unchecked
    epoch shouldBe 42L
    env.attempt shouldBe UByte(1)
    env.signature.length shouldBe 784
  }

  test("TicketCodec rejects wrong-length messages") {
    TicketCodec.decode(new Array[Byte](788)).isLeft shouldBe true
  }

  test("proxy index is last-4-bytes big-endian of ticket id mod validator count") {
    // id ends in 0x00000101 (BE) = 257; 257 mod 6 = 5
    val id = Hash(Array.fill[Byte](28)(0) ++ Array[Byte](0, 0, 1, 1))
    TicketService.proxyIndexFor(id, 6) shouldBe 5
  }

  private val baseDir =
    sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath =
    baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def tempDir(prefix: String): Path = Files.createTempDirectory(prefix)

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  test("CE131/132: node A distributes a generated ticket and node B pools it") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )

    val spec = ChainSpec(
      id = "ticket-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-tix-a")
    val dirB = tempDir("jam-tix-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false)).start()
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false)).start()
      nodeA.enableAuthoring(devKeys)

      nodeA.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeB.listenPort))

      nodeA.tickets.maybeGenerate()
      nodeA.pools.ticketCount should be > 0

      nodeA.tickets.distributeTickets(nodeA.distribution.peers, _ => None)

      val deadline = System.currentTimeMillis() + 15000
      while nodeB.pools.ticketCount == 0 && System.currentTimeMillis() < deadline do
        Thread.sleep(50)

      nodeB.pools.ticketCount should be > 0
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }
