package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Real fork-tree leaf tracking: `ChainManager.leaves` must reflect every
  * imported block without imported children (best first), not just the best
  * head. Reuses `ForkChoiceSpec`'s two-branch fixture (node A extends with
  * tickets, node B forks bare) and its genesis `cancel` guard.
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

  test("a fork produces two leaves (best first); a child of one tip drops it from the set") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val spec = ChainSpec(
      id = "leaves-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-leaves-a")
    val dirB = tempDir("jam-leaves-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false))
      nodeA.chain.initializeOrRestore(spec)
      nodeB.chain.initializeOrRestore(spec)

      // Before any block is imported, genesis is the sole leaf.
      nodeA.chain.leaves.map(_.hash) shouldBe List(nodeA.chain.best.hash)

      nodeA.enableAuthoring(devKeys)
      val bareAuthorB = new BlockAuthor(nodeB.chain, devKeys)
      val bareAuthorShared = new BlockAuthor(nodeA.chain, devKeys)

      // Shared block 1.
      val block1 = bareAuthorShared.tryAuthor(1).getOrElse(fail("author block 1"))
      val block1Bytes = nodeA.chain.encodeBlock(block1)
      nodeA.chain.importBlock(block1Bytes).isRight shouldBe true
      nodeB.chain.importBlock(block1Bytes).isRight shouldBe true

      // A's own block 2 (with tickets) becomes best; block1 is no longer a leaf.
      nodeA.authorSlot(2).isDefined shouldBe true
      val headA2 = nodeA.chain.best
      nodeA.chain.leaves.map(_.hash) shouldBe List(headA2.hash)

      // B's competing block 2' fed to A: parks on a side branch (equal height) —
      // now both tips are leaves, best (A's) first.
      val block2b = bareAuthorB.tryAuthor(2).getOrElse(fail("author 2'"))
      val block2bBytes = nodeB.chain.encodeBlock(block2b)
      val block2bHash = nodeB.chain.headerHashOf(block2b)
      val sideResult = nodeA.chain.importBlock(block2bBytes)
      sideResult.isLeft shouldBe true

      nodeA.chain.best.hash shouldBe headA2.hash
      val leavesAfterFork = nodeA.chain.leaves
      leavesAfterFork.head.hash shouldBe headA2.hash
      leavesAfterFork.map(_.hash).toSet shouldBe Set(headA2.hash, block2bHash)

      nodeB.chain.importBlock(block2bBytes).isRight shouldBe true
      val block3b = bareAuthorB.tryAuthor(3).getOrElse(fail("author 3'"))
      val block3bBytes = nodeB.chain.encodeBlock(block3b)
      val block3bHash = nodeB.chain.headerHashOf(block3b)
      val reorged = nodeA.chain.importBlock(block3bBytes)
      withClue(s"reorg result: $reorged") { reorged.isRight shouldBe true }

      nodeA.chain.best.hash shouldBe block3bHash
      val leavesAfterReorg = nodeA.chain.leaves
      leavesAfterReorg.head.hash shouldBe block3bHash
      leavesAfterReorg.map(_.hash).toSet shouldBe Set(block3bHash, headA2.hash)
      leavesAfterReorg.map(_.hash) should not contain block2bHash
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("restart rebuilds the leaf set from the block store") {
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

      nodeA.authorSlot(2).isDefined shouldBe true
      val headA2 = nodeA.chain.best

      val block2b = bareAuthorB.tryAuthor(2).getOrElse(fail("author 2'"))
      val block2bBytes = nodeB.chain.encodeBlock(block2b)
      val block2bHash = nodeB.chain.headerHashOf(block2b)
      nodeA.chain.importBlock(block2bBytes).isLeft shouldBe true

      val leavesBefore = nodeA.chain.leaves.map(_.hash).toSet
      val bestBefore = nodeA.chain.best.hash
      leavesBefore shouldBe Set(headA2.hash, block2bHash)

      nodeA.shutdownStorageOnly()
      val nodeA2 = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false))
      try
        nodeA2.chain.initializeOrRestore(spec)
        nodeA2.chain.best.hash shouldBe bestBefore
        nodeA2.chain.leaves.map(_.hash).toSet shouldBe leavesBefore
        nodeA2.chain.leaves.head.hash shouldBe bestBefore
      finally nodeA2.shutdownStorageOnly()
      nodeA = null // already shut down above
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
  }
