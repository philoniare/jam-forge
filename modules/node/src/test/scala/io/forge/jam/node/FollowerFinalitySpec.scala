package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path}
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
class FollowerFinalitySpec extends AnyFunSuite with Matchers with NodeFixture:

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

  /** A peer that announces `finalized` and answers CE 130 for it with `justification`. */
  private def justifier(
      chainSpec: ChainSpec,
      node: JamNode,
      finalized: SyncCodec.HashSlot,
      justification: GrandpaMessages.Justification
  ): JamnpNode =
    val peer = new JamnpNode(NodeIdentity.generate(), JamnpConfig(genesisHashPrefix = chainSpec.alpnPrefix))
    peer.registerPeerHandler(
      StreamKind.GrandpaJustificationRequest,
      (_, stream) =>
        stream.onMessage { _ =>
          stream.send(GrandpaMessages.encodeJustification(justification))
          stream.finish()
        }
    )
    val conn = peer.connect(new InetSocketAddress("127.0.0.1", node.listenPort)).get(10, TimeUnit.SECONDS)
    val up = conn.openStream(StreamKind.BlockAnnouncement).get(10, TimeUnit.SECONDS)
    up.onMessage(_ => ())
    up.send(SyncCodec.encodeHandshake(finalized, Nil))
    peer

  /** A round 1, set 0 justification of `target`, precommitted by the first `signers` dev validators. */
  private def justificationOf(node: JamNode, target: SyncCodec.HashSlot, signers: Int): GrandpaMessages.Justification =
    import GrandpaMessages.*
    val vote = Vote(target.hash, node.chain.postStateRoot(target.hash).get, target.slot)
    Justification(1L, 0L, Commit(vote, devKeys.take(signers).map(k => sign(Precommit, vote, 1L, 0L, k)).toList), Nil)

  private def withFollower(config: Path => NodeConfig, chainSpec: ChainSpec = spec())(
      test: (JamNode, ChainSpec, SyncCodec.HashSlot) => Unit
  ): Unit =
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

  private def observing(dir: Path) = NodeConfig(dataDir = dir, slotTicking = false, grandpa = true)

  test("a follower that observes GRANDPA is not moved by validators announcing a finalized block") {
    withFollower(observing) { (node, chainSpec, target) =>
      node.enableGrandpa(None)
      val validators = devKeys.take(5).map(k => announcer(NodeIdentity.fromSeed(k.ed25519Secret), chainSpec, node, target))
      try
        eventually("the announcements to arrive")(node.sync.peerFinalized.size == 5)
        Thread.sleep(5000) // two passes of the voter's two-second look at its peers
        node.chain.finalized.slot shouldBe 0L
      finally validators.foreach(_.shutdown())
    }
  }

  test("a follower that observes GRANDPA finalizes a block any peer proves with a supermajority's justification") {
    withFollower(observing) { (node, chainSpec, target) =>
      node.enableGrandpa(None)
      val stranger = justifier(chainSpec, node, target, justificationOf(node, target, signers = 5))
      try eventually("finality to follow the justification")(node.chain.finalized.hash == target.hash)
      finally stranger.shutdown()
    }
  }

  test("a justification signed by fewer than a supermajority does not move a follower that observes GRANDPA") {
    withFollower(observing) { (node, chainSpec, target) =>
      node.enableGrandpa(None)
      val stranger = justifier(chainSpec, node, target, justificationOf(node, target, signers = 4))
      try
        eventually("the announcement to arrive")(node.sync.peerFinalized.size == 1)
        Thread.sleep(5000)
        node.chain.finalized.slot shouldBe 0L
      finally stranger.shutdown()
    }
  }

  /** A peer that announces `best` as its only leaf. */
  private def holder(identity: NodeIdentity, chainSpec: ChainSpec, node: JamNode, best: SyncCodec.HashSlot): JamnpNode =
    val peer = new JamnpNode(identity, JamnpConfig(genesisHashPrefix = chainSpec.alpnPrefix))
    val conn = peer.connect(new InetSocketAddress("127.0.0.1", node.listenPort)).get(10, TimeUnit.SECONDS)
    val up = conn.openStream(StreamKind.BlockAnnouncement).get(10, TimeUnit.SECONDS)
    up.onMessage(_ => ())
    up.send(SyncCodec.encodeHandshake(SyncCodec.HashSlot(chainSpec.genesisHeaderHash, 0L), List(best)))
    peer

  private def depthFinality(peerKeys: Seq[NodeIdentity])(check: JamNode => Unit): Unit =
    withFollower(dir => NodeConfig(dataDir = dir, slotTicking = false), spec().copy(devnetFinalityDepth = 1)) {
      (node, chainSpec, _) =>
        val best = SyncCodec.HashSlot(node.chain.best.hash, node.chain.best.slot)
        val peers = peerKeys.map(holder(_, chainSpec, node, best))
        try
          eventually("the peers' bests to arrive")(node.sync.peerBests.size == peerKeys.size)
          val block = new BlockAuthor(node.chain, devKeys).tryAuthor(4L).getOrElse(fail("author slot 4"))
          node.chain.importBlock(node.chain.encodeBlock(block)).isRight shouldBe true
          check(node)
        finally peers.foreach(_.shutdown())
    }

  test("the devnet depth rule does not count the announced best of peers that are not validators") {
    depthFinality((1 to 5).map(_ => NodeIdentity.generate())) { node =>
      Thread.sleep(500)
      node.chain.finalized.slot shouldBe 0L
    }
  }

  test("the devnet depth rule counts validators that announce a best on our chain") {
    depthFinality(devKeys.take(4).map(k => NodeIdentity.fromSeed(k.ed25519Secret))) { node =>
      eventually("depth finality with the validators' support")(node.chain.finalized.slot == 3L)
    }
  }

  test("with dummy finality, each new best block finalizes its parent and peers are not followed") {
    val dir = Files.createTempDirectory("jam-dummy-finality")
    val node = new JamNode(spec(), NodeConfig(dataDir = dir, slotTicking = false, dummyFinality = true)).start()
    try
      val author = new BlockAuthor(node.chain, devKeys)
      for slot <- 1L to 3L do
        val block = author.tryAuthor(slot).getOrElse(fail(s"author slot $slot"))
        node.chain.importBlock(node.chain.encodeBlock(block)).isRight shouldBe true
        eventually(s"slot ${slot - 1} to be finalized")(node.chain.finalized.slot == slot - 1)
      node.chain.finalized.hash shouldBe node.chain.parentHash(node.chain.best.hash).get
    finally
      node.shutdown()
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
  }

  test("a follower that observes GRANDPA asks for the justification of a block one ahead of its own finality") {
    withFollower(observing) { (node, chainSpec, second) =>
      node.enableGrandpa(None)
      val first = SyncCodec.HashSlot(node.chain.parentHash(second.hash).get, 1L)
      val stranger = justifier(chainSpec, node, first, justificationOf(node, first, signers = 5))
      try eventually("finality to reach the next block")(node.chain.finalized.hash == first.hash)
      finally stranger.shutdown()
    }
  }
