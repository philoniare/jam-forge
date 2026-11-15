package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.network.{JamnpConfig, JamnpNode, NodeIdentity, StreamKind}
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** A node that runs no GRANDPA voter follows the finality its peers announce. An announcement is a claim, not a
  * proof, so by default only the chain's validators count towards the quorum: anyone can open connections.
  */
class FollowerFinalitySpec extends AnyFunSuite with Matchers:

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")
  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  private def eventually(what: String, seconds: Int = 20)(cond: => Boolean): Unit =
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(seconds.toLong)
    while !cond && System.nanoTime() < deadline do Thread.sleep(20)
    assert(cond, s"timed out waiting for $what")

  private def spec(): ChainSpec =
    if !Files.exists(genesisPath) then cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    given Decoder[Genesis] = Genesis.decoder
    val genesis = decode[Genesis](Files.readString(genesisPath)).fold(e => fail(e.toString), identity)
    ChainSpec(
      id = "follower-finality-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

  /** A peer that connects to `node` and announces `final` as its finalized block. */
  private def announcer(identity: NodeIdentity, chainSpec: ChainSpec, node: JamNode, finalized: SyncCodec.HashSlot): JamnpNode =
    val peer = new JamnpNode(identity, JamnpConfig(genesisHashPrefix = chainSpec.alpnPrefix))
    val conn = peer.connect(new InetSocketAddress("127.0.0.1", node.listenPort)).get(10, TimeUnit.SECONDS)
    val up = conn.openStream(StreamKind.BlockAnnouncement).get(10, TimeUnit.SECONDS)
    up.onMessage(_ => ())
    up.send(SyncCodec.encodeHandshake(finalized, Nil))
    peer

  private def withFollower(config: Path => NodeConfig)(test: (JamNode, ChainSpec, SyncCodec.HashSlot) => Unit): Unit =
    val chainSpec = spec()
    val dir = Files.createTempDirectory("jam-follower-finality")
    var node: JamNode = null
    try
      node = new JamNode(chainSpec, config(dir)).start()
      // Blocks 1 to 3, authored locally; nothing is finalized by depth yet.
      val author = new BlockAuthor(node.chain, devKeys)
      for slot <- 1L to 3L do
        val block = author.tryAuthor(slot).getOrElse(fail(s"author slot $slot"))
        node.chain.importBlock(node.chain.encodeBlock(block)).isRight shouldBe true
      val genesisHash = node.chain.finalized.hash
      node.chain.finalized.slot shouldBe 0L
      val second = node.chain.parentHash(node.chain.best.hash).get
      test(node, chainSpec, SyncCodec.HashSlot(second, 2L))
      genesisHash should not be second
    finally
      if node != null then node.shutdown()
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  test("peers that are not validators do not move a follower's finality, however many announce it") {
    withFollower(dir => NodeConfig(dataDir = dir, slotTicking = false)) { (node, chainSpec, target) =>
      val strangers = (1 to 5).map(_ => announcer(NodeIdentity.generate(), chainSpec, node, target))
      try
        eventually("the announcements to arrive")(node.sync.peerFinalized.size == 5)
        Thread.sleep(1500) // three upkeep passes
        node.chain.finalized.slot shouldBe 0L
      finally strangers.foreach(_.shutdown())
    }
  }

  test("a quorum of validators announcing a finalized block moves a follower's finality") {
    withFollower(dir => NodeConfig(dataDir = dir, slotTicking = false)) { (node, chainSpec, target) =>
      // TINY: six validators, so the quorum is five and this node needs four peers.
      val validators = devKeys.take(4).map(k => announcer(NodeIdentity.fromSeed(k.ed25519Secret), chainSpec, node, target))
      try eventually("finality to follow the validators")(node.chain.finalized.hash == target.hash)
      finally validators.foreach(_.shutdown())
    }
  }

  test("with --finality-peers the operator's chosen number of peers is trusted, validators or not") {
    withFollower(dir => NodeConfig(dataDir = dir, slotTicking = false, finalityPeers = Some(1))) { (node, chainSpec, target) =>
      val bootnode = announcer(NodeIdentity.generate(), chainSpec, node, target)
      try eventually("finality to follow the trusted peer")(node.chain.finalized.hash == target.hash)
      finally bootnode.shutdown()
    }
  }
