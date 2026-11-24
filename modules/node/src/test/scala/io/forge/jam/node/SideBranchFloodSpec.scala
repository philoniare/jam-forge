package io.forge.jam.node


import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash, Timeslot}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UByte

import scala.util.chaining.*

/** Blocks that do not extend the best head are stored without being executed, so a peer must not be able to make
  * the node store what it could have refused: forged side-branch blocks are refused when their seal can be checked
  * against a validated ancestor's state, and capped (per peer and in all) and pruned at finality when it cannot.
  * Honest forks are still stored and adopted.
  *
  * Chain: A holds 1-2-3. B holds 1-4'-5'-6' (TINY: epoch 12 slots, ticket cutoff 10).
  */
class SideBranchFloodSpec extends AnyFunSuite with Matchers with NodeFixture:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  private def spec(genesis: Genesis) = ChainSpec(
    id = "side-flood-devnet",
    config = ChainConfig.TINY,
    genesisHeaderBytes = Some(genesis.header.encode.toArray),
    explicitGenesisHash = None,
    genesisState = genesis.state.keyvals,
    bootnodes = Nil
  )

  /** `template` re-headed onto `parent` at `slot` with a seal nobody signed; `n` makes each one distinct. */
  private def forge(template: Block, parent: Hash, slot: Long, n: Int): Block =
    val seal = Hashing.blake2b256(s"seal-$n".getBytes("UTF-8")).bytes.toArray
    template.copy(header =
      template.header.copy(
        parent = parent,
        slot = Timeslot.fromLong(slot),
        seal = JamBytes(Array.fill(3)(seal).flatten)
      )
    )

  private def peer(i: Int) = Ed25519PublicKey(Array.fill(32)(i.toByte))

  test("a fork across an epoch boundary is seal-checked from an ancestor past the ticket cutoff, and adopted") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val chainSpec = spec(genesis)
    val dirA = tempDir("jam-sideepoch-a")
    val dirB = tempDir("jam-sideepoch-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(chainSpec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeB = new JamNode(chainSpec, NodeConfig(dataDir = dirB, slotTicking = false))
      nodeA.chain.initializeOrRestore(chainSpec)
      nodeB.chain.initializeOrRestore(chainSpec)
      val chainA = nodeA.chain
      val chainB = nodeB.chain
      def author(chain: ChainManager, slot: Long): (Block, Array[Byte]) =
        val block = new BlockAuthor(chain, devKeys).tryAuthor(slot).getOrElse(fail(s"author slot $slot"))
        val bytes = chain.encodeBlock(block)
        chain.importBlock(bytes).isRight shouldBe true
        (block, bytes)

      // Shared 1..10 (slot 10 is the ticket cutoff of epoch 0); A adds 13, B adds 12', 14' in epoch 1. At equal
      // ticket weight the head with the later slot is the best one: 12' is behind A's head, 14' ahead of it.
      for slot <- 1L to 10L do
        val (_, bytes) = author(chainA, slot)
        chainB.importBlock(bytes).isRight shouldBe true
      author(chainA, 13)
      val bestA = chainA.best
      val (b12, b12Bytes) = author(chainB, 12)
      val (_, b13Bytes) = author(chainB, 14)
      val hash12 = chainA.headerHashOf(b12)

      // 12' sits on validated block 10: checked against its state, through the epoch change, markers included.
      chainA.importBlock(b12Bytes).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      chainA.unverifiedCount shouldBe 0
      // The same block without its epoch marker is refused.
      val unmarked = b12.copy(header = b12.header.copy(epochMark = None))
      chainA.importBlock(chainA.encodeBlock(unmarked)).left.toOption.get shouldBe a[ChainError.SideBranchRefused]

      // Forged children of 12' in epoch 1: block 10 fixes the epoch-1 sealing keys, so they are refused.
      (0 until 200).foreach { n =>
        val f = forge(b12, hash12, 13 + n % 11, n)
        chainA.importBlock(chainA.encodeBlock(f), Some(peer(1))).left.toOption.get shouldBe a[ChainError.SideBranchRefused]
        chainA.hasBlock(chainA.headerHashOf(f)) shouldBe false
      }
      // In epoch 2 they cannot be checked: stored, counted, and counted again after a restart. Each has a later
      // slot than A's head, so a replay is tried and fails on the forged block.
      val unverifiable = (0 until 5).map(n => forge(b12, hash12, 24 + n, 1000 + n))
      unverifiable.foreach { f =>
        chainA.importBlock(chainA.encodeBlock(f), Some(peer(1))).left.toOption.get shouldBe a[ChainError.ReplayFailed]
        chainA.hasBlock(chainA.headerHashOf(f)) shouldBe true
      }
      chainA.unverifiedCount shouldBe 5
      chainA.best shouldBe bestA
      nodeA.shutdownStorageOnly()
      nodeA = new JamNode(chainSpec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeA.chain.initializeOrRestore(chainSpec) shouldBe false
      val restored = nodeA.chain
      restored.unverifiedCount shouldBe 5
      restored.best.hash shouldBe bestA.hash

      // The honest 14' (parent 12' unvalidated) verifies from block 10 and has the later slot: reorg.
      val reorged = restored.importBlock(b13Bytes)
      withClue(s"reorg across the epoch boundary: $reorged") { reorged.isRight shouldBe true }
      restored.best.hash shouldBe chainB.best.hash
      restored.best.stateRoot shouldBe chainB.best.stateRoot
      restored.unverifiedCount shouldBe 5

      // Finality at slot 14 leaves them (slots 24..28); finality past them removes them.
      restored.finalize(restored.best.hash).isRight shouldBe true
      restored.unverifiedCount shouldBe 5
      var slot = 15L
      while restored.best.slot < 28 && slot < 100 do
        new BlockAuthor(restored, devKeys)
          .tryAuthor(slot)
          .foreach(blk => restored.importBlock(restored.encodeBlock(blk)).isRight shouldBe true)
        slot += 1
      restored.finalize(restored.best.hash).isRight shouldBe true
      restored.unverifiedCount shouldBe 0
      unverifiable.exists(f => restored.hasBlock(restored.headerHashOf(f))) shouldBe false
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("a side-branch block whose seal could not be checked cannot make an honest block's slot equivocated") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val chainSpec = spec(genesis)
    val dirA = tempDir("jam-sideequiv-a")
    val dirB = tempDir("jam-sideequiv-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(chainSpec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeB = new JamNode(chainSpec, NodeConfig(dataDir = dirB, slotTicking = false))
      nodeA.chain.initializeOrRestore(chainSpec)
      nodeB.chain.initializeOrRestore(chainSpec)
      val chainA = nodeA.chain
      val chainB = nodeB.chain
      def author(chain: ChainManager, slot: Long): (Block, Array[Byte]) =
        val block = new BlockAuthor(chain, devKeys).tryAuthor(slot).getOrElse(fail(s"author slot $slot"))
        val bytes = chain.encodeBlock(block)
        chain.importBlock(bytes).isRight shouldBe true
        (block, bytes)

      // Shared 1..10. B's 12' is an honest, seal-checked side block on A, whose head (slot 13) is later; A's own
      // chain runs on into epoch 2.
      for slot <- 1L to 10L do
        val (_, bytes) = author(chainA, slot)
        chainB.importBlock(bytes).isRight shouldBe true
      author(chainA, 11)
      author(chainA, 13)
      val (b12, b12Bytes) = author(chainB, 12)
      chainA.importBlock(b12Bytes).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      val hash12 = chainA.headerHashOf(b12)
      val honest = (14L to 26L).map(slot => slot -> author(chainA, slot)._1).toMap
      val target = honest(25L)
      val best = chainA.best
      best.slot shouldBe 26L

      // A forged child of 12' at the honest block's slot, claiming its author. Slot 25 is two epochs past block 10,
      // the branch's validated ancestor, so the seal cannot be checked and the block is only stored.
      val forged = forge(b12, hash12, 25, 1).pipe(f => f.copy(header = f.header.copy(authorIndex = target.header.authorIndex)))
      val forgedBytes = chainA.encodeBlock(forged)
      chainA.importBlock(forgedBytes, Some(peer(1))).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      chainA.unverifiedCount shouldBe 1

      chainA.meta.equivocated(25L) shouldBe false
      chainA.best shouldBe best

      // Sending the stored block again changes nothing either.
      chainA.importBlock(forgedBytes, Some(peer(2))).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      chainA.meta.equivocated(25L) shouldBe false
      chainA.best shouldBe best
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("side-branch blocks whose seal could not be checked add no ticket weight and set no winning-tickets mark") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val chainSpec = spec(genesis)
    val dirA = tempDir("jam-sideweight-a")
    val dirB = tempDir("jam-sideweight-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(chainSpec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeB = new JamNode(chainSpec, NodeConfig(dataDir = dirB, slotTicking = false))
      nodeA.chain.initializeOrRestore(chainSpec)
      nodeB.chain.initializeOrRestore(chainSpec)
      val chainA = nodeA.chain
      val chainB = nodeB.chain
      def author(chain: ChainManager, slot: Long): (Block, Array[Byte]) =
        val block = new BlockAuthor(chain, devKeys).tryAuthor(slot).getOrElse(fail(s"author slot $slot"))
        val bytes = chain.encodeBlock(block)
        chain.importBlock(bytes).isRight shouldBe true
        (block, bytes)

      // Shared 1..10; B's 12' is a seal-checked side block on A, whose head (slot 13) is later; A's own chain runs
      // on to 26 and then has a block at slot 80 (no equivocation at slot 12).
      for slot <- 1L to 10L do
        val (_, bytes) = author(chainA, slot)
        chainB.importBlock(bytes).isRight shouldBe true
      author(chainA, 11)
      author(chainA, 13)
      val (b12, b12Bytes) = author(chainB, 12)
      chainA.importBlock(b12Bytes).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      val hash12 = chainA.headerHashOf(b12)
      (14L to 26L).foreach(slot => author(chainA, slot))
      author(chainA, 80)
      val best = chainA.best
      val bestWeight = chainA.weightOf(best.hash)

      // A forged branch on 12', one block per epoch from epoch 2 on, each claiming a winning-tickets marker that
      // would make the next one ticket-sealed. No seal can be checked (block 10 is the validated ancestor), and
      // every one has an earlier slot than A's head: only claimed weight could make the branch the better one.
      val mark = TicketMark(JamBytes(Array.fill(32)(7.toByte)), UByte(0))
      val forged = (0 until 5).scanLeft(Option.empty[Block] -> hash12) { case ((_, parent), i) =>
        val f = forge(b12, parent, 25 + 12L * i, 1 + i)
          .pipe(f => f.copy(header = f.header.copy(ticketsMark = Some(List.fill(ChainConfig.TINY.epochLength)(mark)))))
        val bytes = chainA.encodeBlock(f)
        val r = chainA.importBlock(bytes, Some(peer(1)))
        withClue(s"forged block at slot ${f.header.slot.toLong}: $r") {
          r.left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
        }
        Some(f) -> chainA.headerHashOf(f)
      }.drop(1).map((f, h) => (f.get, h))
      chainA.unverifiedCount shouldBe 5
      chainA.best shouldBe best
      forged.foreach { (_, h) =>
        chainA.meta.marked(h) shouldBe false
        chainA.weightOf(h) shouldBe chainA.weightOf(hash12)
      }
      chainA.weightOf(forged.last._2) should be <= bestWeight

      // Sending the stored blocks again changes nothing either.
      forged.foreach { (f, _) =>
        chainA.importBlock(chainA.encodeBlock(f), Some(peer(2))).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      }
      forged.foreach((_, h) => chainA.meta.marked(h) shouldBe false)
      chainA.weightOf(forged.last._2) shouldBe chainA.weightOf(hash12)
      chainA.best shouldBe best
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("forged side-branch blocks are refused or capped, pruned at finality, and honest forks still reorg") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val chainSpec = spec(genesis)
    val dirA = tempDir("jam-sideflood-a")
    val dirB = tempDir("jam-sideflood-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(chainSpec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeB = new JamNode(chainSpec, NodeConfig(dataDir = dirB, slotTicking = false))
      nodeA.chain.initializeOrRestore(chainSpec)
      nodeB.chain.initializeOrRestore(chainSpec)
      val chainA = nodeA.chain
      val chainB = nodeB.chain
      val authorA = new BlockAuthor(chainA, devKeys)
      val authorB = new BlockAuthor(chainB, devKeys)
      def authorOn(author: BlockAuthor, chain: ChainManager, slot: Long): (Block, Array[Byte]) =
        val block = author.tryAuthor(slot).getOrElse(fail(s"author slot $slot"))
        val bytes = chain.encodeBlock(block)
        chain.importBlock(bytes).isRight shouldBe true
        (block, bytes)

      val (block1, block1Bytes) = authorOn(authorA, chainA, 1)
      chainB.importBlock(block1Bytes).isRight shouldBe true
      val hash1 = chainA.headerHashOf(block1)
      authorOn(authorA, chainA, 2)
      authorOn(authorA, chainA, 7)
      val bestA = chainA.best
      val (b4, b4Bytes) = authorOn(authorB, chainB, 4)
      val (_, b5Bytes) = authorOn(authorB, chainB, 5)
      val (_, b6Bytes) = authorOn(authorB, chainB, 6)
      val (_, b8Bytes) = authorOn(authorB, chainB, 8)
      val leavesBefore = chainA.leaves.size
      val cachedBefore = chainA.headerLinks.size

      // --- 1. Seal checkable against the parent's state: 3000 forged children of block 1 -------------------------
      // Slots 2..11 are in block 1's epoch, 12.. in later ones; with block 1 as the parent every one is checked.
      val forgedOnValidated = (0 until 3000).map(n => forge(b4, hash1, 2 + n, n))
      forgedOnValidated.foreach { f =>
        val r = chainA.importBlock(chainA.encodeBlock(f), Some(peer(1)))
        withClue(s"forged child of block 1 at slot ${f.header.slot.toLong}: $r") {
          r.left.toOption.get shouldBe a[ChainError.SideBranchRefused]
        }
      }
      forgedOnValidated.exists(f => chainA.hasBlock(chainA.headerHashOf(f))) shouldBe false
      chainA.leaves.size shouldBe leavesBefore
      chainA.headerLinks.size shouldBe cachedBefore
      chainA.unverifiedCount shouldBe 0
      chainA.best shouldBe bestA

      // A slot at or before the parent's is refused whatever the seal.
      chainA.importBlock(chainA.encodeBlock(forge(b4, hash1, 1, -1))).left.toOption.get shouldBe a[ChainError.SideBranchRefused]

      // --- 2. The wall clock bounds the slot of an honest side block ----------------------------------------------
      @volatile var now = 2L
      chainA.useWallClock(() => now)
      chainA.importBlock(b4Bytes).left.toOption.get shouldBe a[ChainError.SideBranchRefused] // slot 4 > 2 + tolerance
      chainA.hasBlock(chainA.headerHashOf(b4)) shouldBe false
      now = 3L
      chainA.importBlock(b4Bytes).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch] // within tolerance
      now = 1L << 30
      val hash4 = chainA.headerHashOf(b4)
      chainA.hasBlock(hash4) shouldBe true
      chainA.unverifiedCount shouldBe 0 // its seal verified against block 1's state

      // --- 3. Parent unvalidated, seal still checkable: forged children of 4' in block 1's epoch ------------------
      (0 until 500).foreach { n =>
        val f = forge(b4, hash4, 5 + n % 7, 10000 + n) // slots 5..11
        chainA.importBlock(chainA.encodeBlock(f), Some(peer(1))).left.toOption.get shouldBe a[ChainError.SideBranchRefused]
        chainA.hasBlock(chainA.headerHashOf(f)) shouldBe false
      }
      chainA.unverifiedCount shouldBe 0

      // --- 4. Seal not checkable: forged children of 4' in the next epoch (block 1 is before the cutoff) ----------
      // Slots 12..23: each is later than A's head, so the branch is tried: 4' replays, the forged block fails, and
      // A's chain stays. The block is kept, and counted, so that it is not fetched and tried again.
      def unverifiable(n: Int) = forge(b4, hash4, 12 + n % 12, 20000 + n)
      val fromPeer1 = (0 until 1500).map(unverifiable)
      val results1 = fromPeer1.map(f => chainA.importBlock(chainA.encodeBlock(f), Some(peer(1))))
      results1.count(_.left.toOption.exists(_.isInstanceOf[ChainError.ReplayFailed])) shouldBe
        ChainManager.MaxUnverifiedPerPeer
      results1.count(_ == Left(ChainError.UnverifiedLimit("peer"))) shouldBe 1500 - ChainManager.MaxUnverifiedPerPeer
      chainA.unverifiedCount shouldBe ChainManager.MaxUnverifiedPerPeer

      // More peers: the node-wide cap holds.
      val others = (0 until 4000).map(n => (peer(2 + n % 8), unverifiable(5000 + n)))
      others.foreach((p, f) => chainA.importBlock(chainA.encodeBlock(f), Some(p)))
      chainA.unverifiedCount shouldBe ChainManager.MaxUnverifiedBlocks
      val allUnverifiable = fromPeer1 ++ others.map(_._2)
      allUnverifiable.count(f => chainA.hasBlock(chainA.headerHashOf(f))) shouldBe ChainManager.MaxUnverifiedBlocks
      chainA.leaves.size should be <= leavesBefore + 1 + ChainManager.MaxUnverifiedBlocks
      chainA.headerLinks.size should be <= cachedBefore + 1 + ChainManager.MaxUnverifiedBlocks
      chainA.best shouldBe bestA

      // A stored block sent again is not counted twice, is not refused by the cap, and is not tried again.
      val storedOne = fromPeer1.find(f => chainA.hasBlock(chainA.headerHashOf(f))).get
      chainA.importBlock(chainA.encodeBlock(storedOne), Some(peer(9))) shouldBe Left(ChainError.PreviouslyInvalid)
      chainA.unverifiedCount shouldBe ChainManager.MaxUnverifiedBlocks

      // --- 5. The honest fork is still adopted: 5' and 6' (parents unvalidated, checked against block 1), then 8'
      chainA.importBlock(b5Bytes).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      chainA.importBlock(b6Bytes).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      val reorged = chainA.importBlock(b8Bytes)
      withClue(s"reorg onto B's chain: $reorged") { reorged.isRight shouldBe true }
      chainA.best.hash shouldBe chainB.best.hash
      chainA.best.stateRoot shouldBe chainB.best.stateRoot
      chainA.unverifiedCount shouldBe ChainManager.MaxUnverifiedBlocks

      // --- 6. Finality past their slots deletes the unverified blocks; the validated chain is untouched -----------
      var slot = 9L
      while chainA.best.slot < 24 && slot < 100 do
        new BlockAuthor(chainA, devKeys).tryAuthor(slot).foreach(blk => chainA.importBlock(chainA.encodeBlock(blk)).isRight shouldBe true)
        slot += 1
      chainA.best.slot shouldBe 24L
      chainA.finalize(chainA.best.hash).isRight shouldBe true
      chainA.unverifiedCount shouldBe 0
      allUnverifiable.exists(f => chainA.hasBlock(chainA.headerHashOf(f))) shouldBe false
      chainA.hasBlock(hash4) shouldBe true
      chainA.hasBlock(bestA.hash) shouldBe true // A's abandoned block at slot 7: seal-verified, not subject to the prune
      chainA.leaves.map(_.hash) shouldBe List(chainA.best.hash)
      chainA.headerLinks.size should be <= cachedBefore + 40

      // The chain keeps extending.
      val next = new BlockAuthor(chainA, devKeys).tryAuthor(25).getOrElse(fail("author 25"))
      chainA.importBlock(chainA.encodeBlock(next)).isRight shouldBe true
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
  }

  private def withFork(name: String)(body: (ChainManager, ChainManager, (ChainManager, Long) => Array[Byte]) => Unit): Unit =
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val chainSpec = spec(genesis)
    val dirs = List(tempDir(s"jam-$name-a"), tempDir(s"jam-$name-b"))
    val nodes = dirs.map(dir => new JamNode(chainSpec, NodeConfig(dataDir = dir, slotTicking = false)))
    try
      nodes.foreach(_.chain.initializeOrRestore(chainSpec))
      val List(chainA, chainB) = nodes.map(_.chain)
      def author(chain: ChainManager, slot: Long): Array[Byte] =
        val block = new BlockAuthor(chain, devKeys).tryAuthor(slot).getOrElse(fail(s"author slot $slot"))
        chain.encodeBlock(block).tap(bytes => chain.importBlock(bytes).isRight shouldBe true)
      chainB.importBlock(author(chainA, 1)).isRight shouldBe true
      body(chainA, chainB, author)
    finally
      nodes.foreach(_.shutdownStorageOnly())
      dirs.foreach(cleanup)

  private val CheckedOfTheirs = 9
  private val KeptOfTheirs = CheckedOfTheirs + ChainManager.MaxUnverifiedPerPeer

  test("a branch with more blocks than are kept unexecuted is adopted at the cap, though the best head has the later slot") {
    withFork("sidelong") { (chainA, chainB, author) =>
      author(chainA, 2)
      author(chainA, 400)
      val own = chainA.best
      val theirs = (3L to 290L).map(author(chainB, _))
      val results = theirs.map(chainA.importBlock(_, Some(peer(1))))
      results.take(KeptOfTheirs).foreach(_.left.toOption.get shouldBe a[ChainError.StoredOnSideBranch])
      results.drop(KeptOfTheirs).zipWithIndex.foreach { (r, i) =>
        withClue(s"B's block at slot ${3 + KeptOfTheirs + i}: $r")(r.isRight shouldBe true)
      }
      chainA.best.hash shouldBe chainB.best.hash
      chainA.best.stateRoot shouldBe chainB.best.stateRoot
      chainA.best.slot shouldBe 290L
      chainA.unverifiedCount shouldBe 0
      chainA.leaves.map(_.hash).toSet shouldBe Set(chainB.best.hash, own.hash)
      chainA.importBlock(author(chainB, 291), Some(peer(1))) shouldBe Right(chainB.best)
    }
  }

  test("at the cap a branch with no more blocks than the best chain is refused, and adopted when its head has the later slot") {
    withFork("sideshort") { (chainA, chainB, author) =>
      author(chainA, 2)
      (301L to 570L).foreach(author(chainA, _))
      val own = chainA.best
      val theirs = (3L to 290L).map(author(chainB, _))
      val results = theirs.map(chainA.importBlock(_, Some(peer(1))))
      results.take(KeptOfTheirs).foreach(_.left.toOption.get shouldBe a[ChainError.StoredOnSideBranch])
      results(KeptOfTheirs) shouldBe Left(ChainError.UnverifiedLimit("peer"))
      results.drop(KeptOfTheirs + 1).foreach(_.left.toOption.get shouldBe a[ChainError.UnknownParent])
      chainA.best shouldBe own
      chainA.unverifiedCount shouldBe ChainManager.MaxUnverifiedPerPeer

      theirs.drop(KeptOfTheirs).foreach(chainA.importBlock(_, Some(peer(2))).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch])
      chainA.best shouldBe own
      chainA.importBlock(author(chainB, 571), Some(peer(2))) shouldBe Right(chainB.best)
      chainA.best.stateRoot shouldBe chainB.best.stateRoot
      chainA.unverifiedCount shouldBe 0
    }
  }
