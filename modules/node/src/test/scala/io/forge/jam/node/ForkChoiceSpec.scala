package io.forge.jam.node

import java.nio.file.Files

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Fork choice: side branches are stored unvalidated; a strictly heavier
  * branch (ticket-sealed blocks, then height; GP 0.8.0 §bestchain) triggers
  * a reorg that rewinds to the common ancestor and replays with full
  * validation, unless the branch holds an equivocation.
  *
  * Setup: node A and node B share block 1. A extends with its own block 2;
  * B skips slot 2 and produces 3' and 4'. Feeding 3' to A parks it on a side
  * branch (equal height); 4' makes B's branch longer and A reorgs onto it.
  */
class ForkChoiceSpec extends AnyFunSuite with Matchers with NodeFixture:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  test("at equal ticket weight the head with the later slot is taken, though its chain is shorter") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val spec = ChainSpec(
      id = "fork-slots",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dirs = List("a", "b", "c").map(n => tempDir(s"jam-fork-slots-$n"))
    val nodes = dirs.map(d => new JamNode(spec, NodeConfig(dataDir = d, slotTicking = false)))
    try
      nodes.foreach(_.chain.initializeOrRestore(spec))
      val List(long, late, mid) = nodes.map(_.chain)
      def author(chain: ChainManager, slot: Long): Array[Byte] =
        val block = new BlockAuthor(chain, devKeys).tryAuthor(slot).getOrElse(fail(s"author slot $slot"))
        val bytes = chain.encodeBlock(block)
        chain.importBlock(bytes).isRight shouldBe true
        bytes
      val onA = List(1L, 2L, 3L).map(author(long, _))
      val five = author(late, 5L)
      val four = author(mid, 4L)
      long.best.slot shouldBe 3L
      long.heightOf(long.best.hash) shouldBe Some(3L)

      long.importBlock(five).isRight shouldBe true
      long.best.hash shouldBe late.best.hash
      long.best.slot shouldBe 5L
      long.heightOf(long.best.hash) shouldBe Some(1L)
      long.best.stateRoot shouldBe late.best.stateRoot

      long.importBlock(four) match
        case Left(ChainError.StoredOnSideBranch(_, slot, _, bestSlot)) =>
          slot shouldBe 4L
          bestSlot shouldBe 5L
        case other => fail(s"expected the slot-4 block to be stored on a side branch, got $other")
      long.best.slot shouldBe 5L

      onA.foreach(bytes => late.importBlock(bytes).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch])
      late.best.slot shouldBe 5L
      val six = author(mid, 6L) // on that chain: genesis, 4, 6
      late.importBlock(four).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      late.importBlock(six).isRight shouldBe true
      late.best.hash shouldBe mid.best.hash
      long.importBlock(six).isRight shouldBe true
      long.best.hash shouldBe mid.best.hash
    finally
      nodes.foreach(_.shutdownStorageOnly())
      dirs.foreach(cleanup)
  }

  test("a side branch is adopted once its head has the later slot, by a validated reorg") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val spec = ChainSpec(
      id = "fork-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-fork-a")
    val dirB = tempDir("jam-fork-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false))
      nodeA.chain.initializeOrRestore(spec)
      nodeB.chain.initializeOrRestore(spec)

      // Before any block is imported, genesis is the sole leaf.
      nodeA.chain.leaves.map(_.hash) shouldBe List(nodeA.chain.best.hash)

      // A authors with tickets; B authors bare (no ticket service) so the
      // branches diverge in content and therefore in hash.
      nodeA.enableAuthoring(devKeys)
      val bareAuthorB = new BlockAuthor(nodeB.chain, devKeys) // no shared pools/tickets

      // Shared block 1 (authored bare so both chains start identically).
      val bareAuthorShared = new BlockAuthor(nodeA.chain, devKeys)
      val block1 = bareAuthorShared.tryAuthor(1).getOrElse(fail("author block 1"))
      val block1Bytes = nodeA.chain.encodeBlock(block1)
      nodeA.chain.importBlock(block1Bytes).isRight shouldBe true
      nodeB.chain.importBlock(block1Bytes).isRight shouldBe true

      nodeA.authorSlot(3).isDefined shouldBe true
      val headA2 = nodeA.chain.best
      // Block 1 is no longer a leaf.
      nodeA.chain.leaves.map(_.hash) shouldBe List(headA2.hash)

      // B's competing chain: 2' and 4' (no block at slot 3, so the branches
      // do not equivocate).
      val block2b = bareAuthorB.tryAuthor(2).getOrElse(fail("author 2'"))
      val block2bBytes = nodeB.chain.encodeBlock(block2b)
      nodeB.chain.importBlock(block2bBytes).isRight shouldBe true
      val block3b = bareAuthorB.tryAuthor(4).getOrElse(fail("author 4'"))
      val block3bBytes = nodeB.chain.encodeBlock(block3b)
      nodeB.chain.importBlock(block3bBytes).isRight shouldBe true

      nodeB.chain.best.hash should not be headA2.hash

      // Sanity: 2' imports linearly on a fresh node C (isolates replay bugs
      // from block validity).
      val dirC = tempDir("jam-fork-c")
      val nodeC = new JamNode(spec, NodeConfig(dataDir = dirC, slotTicking = false))
      nodeC.chain.initializeOrRestore(spec)
      nodeC.chain.importBlock(block1Bytes).isRight shouldBe true
      val cRes = nodeC.chain.importBlock(block2bBytes)
      withClue(s"fresh-node import of 2': $cRes") { cRes.isRight shouldBe true }
      nodeC.shutdownStorageOnly()
      cleanup(dirC)

      // Feed B's branch to A: 2' parks on a side branch (its slot is earlier than A's head's)...
      val sideResult = nodeA.chain.importBlock(block2bBytes)
      sideResult.isLeft shouldBe true
      sideResult.left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      nodeA.chain.best.hash shouldBe headA2.hash
      // Both tips are leaves, best (A's) first.
      val block2bHash = nodeB.chain.headerHashOf(block2b)
      val leavesAfterFork = nodeA.chain.leaves
      leavesAfterFork.head.hash shouldBe headA2.hash
      leavesAfterFork.map(_.hash).toSet shouldBe Set(headA2.hash, block2bHash)

      // ...and 4' gives the branch the later slot → reorg onto B's chain.
      val left = scala.collection.mutable.ListBuffer.empty[(List[Hash], List[Hash])]
      nodeA.chain.onAbandoned((abandoned, adopted) =>
        left += ((abandoned.map(nodeA.chain.headerHashOf), adopted.map(nodeA.chain.headerHashOf)))
      )
      val reorged = nodeA.chain.importBlock(block3bBytes)
      withClue(s"reorg result: $reorged") {
        reorged.isRight shouldBe true
      }
      // A's block was left behind for 2' and 4', whose extrinsics the pools must not take back.
      left.toList shouldBe List((List(headA2.hash), List(block2bHash, nodeB.chain.best.hash)))
      nodeA.chain.best.hash shouldBe nodeB.chain.best.hash
      nodeA.chain.best.stateRoot shouldBe nodeB.chain.best.stateRoot
      nodeA.chain.best.slot shouldBe 4L
      // After the reorg the new best leads the leaves; 2' has a child and is no leaf; A's block still is.
      val leavesAfterReorg = nodeA.chain.leaves
      leavesAfterReorg.head.hash shouldBe nodeB.chain.headerHashOf(block3b)
      leavesAfterReorg.map(_.hash).toSet shouldBe Set(nodeB.chain.headerHashOf(block3b), headA2.hash)
      leavesAfterReorg.map(_.hash) should not contain block2bHash

      // The abandoned block is still stored but no longer canonical; the new
      // chain keeps extending.
      nodeA.chain.hasBlock(headA2.hash) shouldBe true
      val block5 = new BlockAuthor(nodeA.chain, devKeys).tryAuthor(5).getOrElse(fail("author 5"))
      nodeA.chain.importBlock(nodeA.chain.encodeBlock(block5)).isRight shouldBe true
      nodeA.chain.best.slot shouldBe 5L

      // After the reorg each block's ancestor set is its own branch's, as the uncached store walk gives, whether the
      // headers come from the import-filled cache or from the store.
      val chainA = nodeA.chain
      val stored = List(block1, block2b, block3b, block5).map(chainA.headerHashOf) :+ headA2.hash
      stored.foreach { h =>
        val slot = chainA.slotOf(h).get
        val root = chainA.postStateRoot(h).getOrElse(Hash.zero)
        val expected = AncestryOracle.walk(chainA.blockStore, chainA.config, h, root, slot + 1)
        chainA.ancestorsOf(h, root, slot + 1) shouldBe expected
        chainA.headerLinks.clear()
        chainA.ancestorsOf(h, root, slot + 1) shouldBe expected
      }
      val onAbandoned = chainA.ancestorsOf(headA2.hash, Hash.zero, 6L).map(_.headerHash)
      onAbandoned should contain(headA2.hash)
      onAbandoned should not contain chainA.headerHashOf(block2b)
      chainA.ancestorsOf(chainA.best.hash, chainA.best.stateRoot, 6L).map(_.headerHash) should not contain headA2.hash
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("a disregarded best block is rewound and the best remaining branch adopted") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val spec = ChainSpec(
      id = "disregard-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dirA = tempDir("jam-disregard-a")
    val dirB = tempDir("jam-disregard-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false))
      nodeA.chain.initializeOrRestore(spec)
      nodeB.chain.initializeOrRestore(spec)
      val block1 = new BlockAuthor(nodeA.chain, devKeys).tryAuthor(1).getOrElse(fail("author block 1"))
      val block1Bytes = nodeA.chain.encodeBlock(block1)
      nodeA.chain.importBlock(block1Bytes).isRight shouldBe true
      nodeB.chain.importBlock(block1Bytes).isRight shouldBe true
      val headA1 = nodeA.chain.best

      // A: slots 3 then 4 on its own chain; B: slot 2 on block 1, parked on A as a side branch.
      val authorA = new BlockAuthor(nodeA.chain, devKeys)
      val a2 = authorA.tryAuthor(3).getOrElse(fail("author 3"))
      nodeA.chain.importBlock(nodeA.chain.encodeBlock(a2)).isRight shouldBe true
      val headA2 = nodeA.chain.best
      val a3 = authorA.tryAuthor(4).getOrElse(fail("author 4"))
      nodeA.chain.importBlock(nodeA.chain.encodeBlock(a3)).isRight shouldBe true
      val b4 = new BlockAuthor(nodeB.chain, devKeys).tryAuthor(2).getOrElse(fail("author 2'"))
      val b4Bytes = nodeB.chain.encodeBlock(b4)
      nodeB.chain.importBlock(b4Bytes).isRight shouldBe true
      nodeA.chain.importBlock(b4Bytes).isLeft shouldBe true // an earlier slot than A's head: stays a side branch

      // Disregarding A's first block drops both of A's and adopts 2'.
      nodeA.chain.disregard(headA2.hash) match
        case Right(head) => head.hash shouldBe nodeB.chain.best.hash
        case Left(err)   => fail(err.message)
      nodeA.chain.best.stateRoot shouldBe nodeB.chain.best.stateRoot
      nodeA.chain.parentHash(nodeA.chain.best.hash) shouldBe Some(headA1.hash)
      // The new chain extends; the disregarded block is refused from now on.
      val b5 = new BlockAuthor(nodeB.chain, devKeys).tryAuthor(5).getOrElse(fail("author 5"))
      nodeA.chain.importBlock(nodeA.chain.encodeBlock(b5)).isRight shouldBe true
      nodeA.chain.importBlock(nodeA.chain.encodeBlock(a2)) shouldBe Left(ChainError.Disregarded)
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
  }

  private def devSpec(id: String, genesis: io.forge.jam.protocol.traces.Genesis): ChainSpec =
    ChainSpec(
      id = id,
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

  test("a branch with an unfinalized equivocation is not adopted, and the best chain leaves one") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val spec = devSpec("equivocation-devnet", genesis)
    val dirA = tempDir("jam-equiv-a")
    val dirB = tempDir("jam-equiv-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false))
      nodeA.chain.initializeOrRestore(spec)
      nodeB.chain.initializeOrRestore(spec)
      nodeA.enableAuthoring(devKeys)
      val bareA = new BlockAuthor(nodeA.chain, devKeys)
      val bareB = new BlockAuthor(nodeB.chain, devKeys)

      val block1Bytes = nodeA.chain.encodeBlock(bareA.tryAuthor(1).getOrElse(fail("author 1")))
      nodeA.chain.importBlock(block1Bytes).isRight shouldBe true
      nodeB.chain.importBlock(block1Bytes).isRight shouldBe true

      // Slot 2 twice by the same sealer: A's (with tickets) and B's (bare).
      nodeA.authorSlot(2).isDefined shouldBe true
      val headA2 = nodeA.chain.best
      val b2 = nodeB.chain.encodeBlock(bareB.tryAuthor(2).getOrElse(fail("author 2'")))
      nodeB.chain.importBlock(b2).isRight shouldBe true
      val b3 = nodeB.chain.encodeBlock(bareB.tryAuthor(3).getOrElse(fail("author 3'")))
      nodeB.chain.importBlock(b3).isRight shouldBe true
      nodeB.chain.best.hash should not be headA2.hash

      val head1 = nodeA.chain.parentHash(headA2.hash).getOrElse(fail("parent of 2"))
      // Seeing 2' makes slot 2 equivocated: A's own block 2 is on its best chain above finality, so the best head
      // leaves it, for block 1 (B's branch holds the same slot and is not acceptable either).
      nodeA.chain.importBlock(b2).isLeft shouldBe true
      nodeA.chain.best.hash shouldBe head1
      val longer = nodeA.chain.importBlock(b3)
      withClue(s"import of 3': $longer") { longer.isLeft shouldBe true }
      nodeA.chain.best.hash shouldBe head1
      nodeA.authoringStopped.isDefined shouldBe true
      nodeA.authorSlot(4) shouldBe None
      // The chain goes on from block 1.
      val b4 = nodeA.chain.encodeBlock(bareA.tryAuthor(4).getOrElse(fail("author 4")))
      nodeA.chain.importBlock(b4).isRight shouldBe true
      nodeA.chain.parentHash(nodeA.chain.best.hash) shouldBe Some(head1)
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("depth finality waits for a quorum of peers holding the target, advances the finalized head and blocks reorgs below it") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val spec = devSpec("quorum-devnet", genesis)
    val dir = tempDir("jam-quorum")
    var node: JamNode = null
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false))
      node.chain.initializeOrRestore(spec)
      // Bare authoring: tickets play no part in depth finality.
      def author(slot: Long): Unit =
        val block = new BlockAuthor(node.chain, devKeys).tryAuthor(slot).getOrElse(fail(s"author $slot"))
        node.chain.importBlock(node.chain.encodeBlock(block)).isRight shouldBe true
      for slot <- 1L to 4L do author(slot)
      val best = node.chain.best
      val genesisHash = spec.genesisHeaderHash

      // Alone, or with a peer whose best is on another chain, quorum 2 is not met.
      node.chain.finalizeAtDepth(2, Nil, quorum = 2) shouldBe None
      node.chain.finalizeAtDepth(2, Seq(genesisHash), quorum = 2) shouldBe None
      node.chain.finalized.hash shouldBe genesisHash

      // Two peers on our chain at the same head count twice: three of three.
      val fin = node.chain
        .finalizeAtDepth(2, Seq(best.hash, best.hash), quorum = 3)
        .getOrElse(fail("not finalized"))
      fin.slot shouldBe 2L
      node.chain.ancestorAtHeight(best.hash, 2L) shouldBe Some(fin.hash)

      // Finalizing at depth on every import advances the finalized head with the chain.
      node.chain.onImported((_, _) => node.chain.finalizeAtDepth(2))
      author(5L)
      val finalityDeadline = System.currentTimeMillis() + 30000
      while node.chain.finalized.slot != 3L && System.currentTimeMillis() < finalityDeadline do
        Thread.sleep(50)
      node.chain.finalized.slot shouldBe 3L

      // Finalizing below the finalized head is rejected.
      node.chain.finalize(genesisHash).isLeft shouldBe true
    finally
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)
  }

  test("ChainManager refuses a trie backend that tracks garbage, since it never runs gc") {
    val dir = Files.createTempDirectory("jam-forkchoice-gc")
    val trie = io.forge.jam.db.RocksDbTrieBackend.open(dir.resolve("trie"))
    val blocks = io.forge.jam.db.BlockStore.open(dir.resolve("blocks"))
    try an[IllegalArgumentException] should be thrownBy new ChainManager(ChainConfig.TINY, trie, blocks)
    finally
      blocks.close()
      trie.close()
      cleanup(dir)
  }
