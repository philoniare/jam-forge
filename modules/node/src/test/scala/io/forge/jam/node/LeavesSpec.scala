package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Real fork-tree leaf tracking: `ChainManager.leaves` must reflect every
  * imported block without imported children (best first), not just the best
  * head. Uses `ForkChoiceSpec`'s two-branch fixture (node A extends with
  * tickets, node B forks bare); the leaf set across a fork and a reorg is
  * checked in `ForkChoiceSpec` itself.
  */
class LeavesSpec extends AnyFunSuite with Matchers:

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

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  test("restart rebuilds the leaf set from the block store; finalizing prunes a dead side-branch leaf at or below the finalized height") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val spec = ChainSpec(
      id = "leaves-restart-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-leaves-restart-a")
    val dirB = tempDir("jam-leaves-restart-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false))
      nodeA.chain.initializeOrRestore(spec)
      nodeB.chain.initializeOrRestore(spec)

      nodeA.enableAuthoring(devKeys)
      val bareAuthorB = new BlockAuthor(nodeB.chain, devKeys)
      val bareAuthorShared = new BlockAuthor(nodeA.chain, devKeys)

      val block1 = bareAuthorShared.tryAuthor(1).getOrElse(fail("author block 1"))
      val block1Bytes = nodeA.chain.encodeBlock(block1)
      nodeA.chain.importBlock(block1Bytes).isRight shouldBe true
      nodeB.chain.importBlock(block1Bytes).isRight shouldBe true

      // A's own block 2 (with tickets, so it differs from B's bare 2').
      nodeA.authorSlot(2).isDefined shouldBe true
      val headA2 = nodeA.chain.best

      // B's competing block 2' parks on a side branch: a dead tip that will
      // never be adoptable again once A's chain is finalized past it.
      val block2b = bareAuthorB.tryAuthor(2).getOrElse(fail("author 2'"))
      val block2bBytes = nodeB.chain.encodeBlock(block2b)
      val block2bHash = nodeB.chain.headerHashOf(block2b)
      nodeA.chain.importBlock(block2bBytes).isLeft shouldBe true

      def triples(heads: List[ChainManager#Head]) = heads.map(h => (h.hash, h.slot, h.stateRoot)).toSet
      val leavesBefore = triples(nodeA.chain.leaves)
      val bestBefore = nodeA.chain.best
      leavesBefore.map(_._1) shouldBe Set(headA2.hash, block2bHash)
      leavesBefore should contain((headA2.hash, headA2.slot, headA2.stateRoot))
      leavesBefore should contain((block2bHash, 2L, Hash.zero))

      nodeA.shutdownStorageOnly()
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeA.chain.initializeOrRestore(spec)
      nodeA.chain.best.hash shouldBe bestBefore.hash
      triples(nodeA.chain.leaves) shouldBe leavesBefore
      nodeA.chain.leaves.head.hash shouldBe bestBefore.hash

      // A extends past it: block2b (height 2) is now behind A's new best
      // (height 3), but it's still tracked as a (dead) leaf.
      val block3 = new BlockAuthor(nodeA.chain, devKeys).tryAuthor(3).getOrElse(fail("author block 3"))
      val headA3 = nodeA.chain.importBlock(nodeA.chain.encodeBlock(block3)).fold(e => fail(s"import 3: $e"), identity)
      nodeA.chain.leaves.map(_.hash).toSet shouldBe Set(headA3.hash, block2bHash)

      val finalizeResult = nodeA.chain.finalize(headA3.hash)
      withClue(s"finalize result: $finalizeResult") { finalizeResult.isRight shouldBe true }

      val leavesAfter = nodeA.chain.leaves
      leavesAfter.map(_.hash) shouldBe List(headA3.hash)
      leavesAfter.map(_.hash) should not contain block2bHash
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("finalized head carries the finalized block's real state root, and it survives a restart") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val spec = ChainSpec(
      id = "leaves-finalized-root-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-leaves-finalized-root-a")
    var nodeA: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeA.chain.initializeOrRestore(spec)

      // Before any finalize call beyond genesis, `finalized` is genesis,
      // whose real root is known from bootstrap.
      val genesisRoot = nodeA.chain.best.stateRoot
      nodeA.chain.finalized.hash shouldBe nodeA.chain.best.hash
      nodeA.chain.finalized.stateRoot shouldBe genesisRoot

      nodeA.enableAuthoring(devKeys)
      val block1 = new BlockAuthor(nodeA.chain, devKeys).tryAuthor(1).getOrElse(fail("author block 1"))
      val block1Bytes = nodeA.chain.encodeBlock(block1)
      val head1 = nodeA.chain.importBlock(block1Bytes).getOrElse(fail("import block 1"))
      head1.stateRoot should not be Hash.zero

      val finalizeResult = nodeA.chain.finalize(head1.hash)
      withClue(s"finalize result: $finalizeResult") { finalizeResult.isRight shouldBe true }

      nodeA.chain.finalized.hash shouldBe head1.hash
      nodeA.chain.finalized.stateRoot shouldBe head1.stateRoot

      // Restart: a fresh ChainManager over the same on-disk stores must still
      // report the real root, not the old Hash.zero placeholder.
      nodeA.shutdownStorageOnly()
      val nodeA2 = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false))
      try
        nodeA2.chain.initializeOrRestore(spec)
        nodeA2.chain.finalized.hash shouldBe head1.hash
        nodeA2.chain.finalized.stateRoot shouldBe head1.stateRoot
      finally nodeA2.shutdownStorageOnly()
      nodeA = null
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      cleanup(dirA)
  }
