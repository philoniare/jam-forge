package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** What joining a running network needs: a chain spec of a size that is none of the built-in ones, catching up
  * from far behind, and following the finality the peers announce.
  */
class TestnetJoinSpec extends AnyFunSuite with Matchers:

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")
  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  private def eventually(what: String, seconds: Int = 60)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(100)
    assert(cond, s"timed out waiting for $what")

  private def le(v: Long, size: Int): String = (0 until size).map(i => f"${(v >> (8 * i)) & 0xff}%02x").mkString

  /** `protocol_parameters` in the layout a JIP-4 spec carries (122 bytes). */
  private def parameters(cores: Int, epoch: Int, rotation: Int, lookupAge: Long, tailStart: Int): String =
    le(10, 8) + le(1, 8) + le(100, 8) + le(cores, 2) + le(19200, 4) + le(epoch, 4) +
      le(10_000_000L, 8) + le(50_000_000L, 8) + le(5_000_000_000L, 8) + le(3_500_000_000L, 8) +
      le(8, 2) + le(16, 2) + le(8, 2) + le(16, 2) + le(lookupAge, 4) +
      le(8, 2) + le(6, 2) + le(80, 2) + le(rotation, 2) + le(128, 2) + le(5, 2) +
      le(64000, 4) + le(13_791_360L, 4) + le(4_000_000L, 4) + le(3072, 4) + le(49152, 4) + le(128, 4) + le(3072, 4) +
      le(tailStart, 4)

  private def specJson(params: String, validators: Int): String =
    // State key 8 (the current validator set): a length prefix, then 336 bytes per validator.
    val prefix = if validators < 128 then le(validators, 1) else f"${0x80 | (validators >> 8)}%02x${validators & 0xff}%02x"
    val kappa = prefix + "00" * (336 * validators)
    s"""{"id":"sized","genesis_header":"${"00" * 40}","protocol_parameters":"$params",
       |"genesis_state":{"08${"00" * 30}":"$kappa"},"bootnodes":[]}""".stripMargin

  test("a chain spec of a size that is not built in gets its config from protocol_parameters and the genesis validators") {
    val spec = ChainSpec.fromJson(specJson(parameters(10, 600, 10, 14400, 500), 30)).fold(e => fail(e), identity)
    val c = spec.config
    c.coresCount shouldBe 10
    c.validatorCount shouldBe 30
    c.epochLength shouldBe 600
    c.rotationPeriod shouldBe 10
    c.contestDuration shouldBe 500
    c.maxLookupAnchorAge shouldBe 14400L
    c.authQueueSize shouldBe 80
    c.maxBlockGas shouldBe 3_500_000_000L
    c.maxRefineGas shouldBe 5_000_000_000L
    c.ecPieceSize shouldBe ChainConfig.ecPieceSizeFor(30)
    c.numEcPiecesPerSegment shouldBe 4104 / ChainConfig.ecPieceSizeFor(30)

    // A size that is built in still resolves to the built-in config.
    ChainSpec.fromJson(specJson(parameters(341, 600, 10, 14400, 500), 1023)).map(_.config) shouldBe Right(ChainConfig.FULL)
    ChainSpec.fromJson(specJson(parameters(2, 12, 4, 24, 10), 6)).map(_.config) shouldBe Right(ChainConfig.TINY)
    // Parameters matching nothing, with no validator set to size the config by, are refused.
    val noValidators = s"""{"id":"x","genesis_header":"${"00" * 40}","protocol_parameters":"${parameters(10, 600, 10, 14400, 500)}","genesis_state":{},"bootnodes":[]}"""
    an[IllegalArgumentException] should be thrownBy ChainSpec.fromJson(noValidators)
  }

  test("a node 100 blocks behind catches up, and follows a peer's finality only when told how many peers to trust") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val spec = ChainSpec(
      id = "join-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dirs = List.fill(3)(Files.createTempDirectory("jam-join"))
    var nodes: List[JamNode] = Nil
    try
      val ahead = new JamNode(spec, NodeConfig(dataDir = dirs(0), slotTicking = false)).start()
      nodes = List(ahead)
      ahead.enableAuthoring((0 until 6).map(ValidatorKeySet.dev))
      // More blocks than one block request carries, so the tip's 64 predecessors do not reach genesis.
      for slot <- 1 to 100 do ahead.authorSlot(slot.toLong).isDefined shouldBe true
      ahead.chain.finalize(ahead.chain.ancestorAtHeight(ahead.chain.best.hash, 95).get).isRight shouldBe true
      val address = new InetSocketAddress("127.0.0.1", ahead.listenPort)

      // Default finality quorum (2V/3 of the validators): one peer's word is not enough.
      val cautious = new JamNode(spec, NodeConfig(dataDir = dirs(1), slotTicking = false)).start()
      nodes = cautious :: nodes
      cautious.connectPeer(address)
      eventually("the cautious node to catch up")(cautious.chain.best.hash == ahead.chain.best.hash)
      cautious.chain.best.slot shouldBe 100L

      // Told to trust one peer: it also adopts that peer's finalized block.
      val trusting = new JamNode(spec, NodeConfig(dataDir = dirs(2), slotTicking = false, finalityPeers = Some(1))).start()
      nodes = trusting :: nodes
      trusting.connectPeer(address)
      eventually("the trusting node to catch up")(trusting.chain.best.hash == ahead.chain.best.hash)
      eventually("the trusting node to follow the peer's finality")(trusting.chain.finalized.hash == ahead.chain.finalized.hash)
      trusting.chain.finalized.slot shouldBe 95L
      cautious.chain.finalized.slot shouldBe 0L
    finally
      nodes.foreach(_.shutdown())
      dirs.foreach(cleanup)
  }
