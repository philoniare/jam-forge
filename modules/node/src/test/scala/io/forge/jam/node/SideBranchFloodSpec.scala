package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash, Timeslot}
import io.forge.jam.core.types.block.Block
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Blocks that do not extend the best head are stored without being executed, so a peer must not be able to make
  * the node store what it could have refused: forged side-branch blocks are refused when their seal can be checked
  * against a validated ancestor's state, and capped (per peer and in all) and pruned at finality when it cannot.
  * Honest forks are still stored and adopted.
  *
  * Chain: A holds 1-2-3. B holds 1-4'-5'-6' (TINY: epoch 12 slots, ticket cutoff 10).
  */
class SideBranchFloodSpec extends AnyFunSuite with Matchers:

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def tempDir(p: String): Path = Files.createTempDirectory(p)
  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

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

      // Shared 1..10 (slot 10 is the ticket cutoff of epoch 0); A adds 11, B adds 12', 13' in epoch 1.
      for slot <- 1L to 10L do
        val (_, bytes) = author(chainA, slot)
        chainB.importBlock(bytes).isRight shouldBe true
      author(chainA, 11)
      val bestA = chainA.best
      val (b12, b12Bytes) = author(chainB, 12)
      val (_, b13Bytes) = author(chainB, 13)
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
      // In epoch 2 they cannot be checked: stored, counted, and counted again after a restart. Each makes its
      // branch longer than A's chain, so a replay is tried and fails on the forged block.
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

      // The honest 13' (parent 12' unvalidated) verifies from block 10 and makes the branch longer: reorg.
      val reorged = restored.importBlock(b13Bytes)
      withClue(s"reorg across the epoch boundary: $reorged") { reorged.isRight shouldBe true }
      restored.best.hash shouldBe chainB.best.hash
      restored.best.stateRoot shouldBe chainB.best.stateRoot
      restored.unverifiedCount shouldBe 5

      // Finality at slot 13 leaves them (slots 24..28); finality past them removes them.
      restored.finalize(restored.best.hash).isRight shouldBe true
      restored.unverifiedCount shouldBe 5
      var slot = 14L
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
      authorOn(authorA, chainA, 3)
      val bestA = chainA.best
      val (b4, b4Bytes) = authorOn(authorB, chainB, 4)
      val (_, b5Bytes) = authorOn(authorB, chainB, 5)
      val (_, b6Bytes) = authorOn(authorB, chainB, 6)
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
      // Slots 12..23; all siblings at height 3, so none is heavier than A's chain.
      def unverifiable(n: Int) = forge(b4, hash4, 12 + n % 12, 20000 + n)
      val fromPeer1 = (0 until 1500).map(unverifiable)
      val results1 = fromPeer1.map(f => chainA.importBlock(chainA.encodeBlock(f), Some(peer(1))))
      results1.count(_.left.toOption.exists(_.isInstanceOf[ChainError.StoredOnSideBranch])) shouldBe
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

      // A stored block sent again is not counted twice and is not refused by the cap.
      val storedOne = fromPeer1.find(f => chainA.hasBlock(chainA.headerHashOf(f))).get
      chainA.importBlock(chainA.encodeBlock(storedOne), Some(peer(9))).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      chainA.unverifiedCount shouldBe ChainManager.MaxUnverifiedBlocks

      // --- 5. The honest fork is still adopted: 5' (parent 4' unvalidated, checked against block 1) then 6' -------
      chainA.importBlock(b5Bytes).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      val reorged = chainA.importBlock(b6Bytes)
      withClue(s"reorg onto B's chain: $reorged") { reorged.isRight shouldBe true }
      chainA.best.hash shouldBe chainB.best.hash
      chainA.best.stateRoot shouldBe chainB.best.stateRoot
      chainA.unverifiedCount shouldBe ChainManager.MaxUnverifiedBlocks

      // --- 6. Finality past their slots deletes the unverified blocks; the validated chain is untouched -----------
      var slot = 7L
      while chainA.best.slot < 24 && slot < 100 do
        new BlockAuthor(chainA, devKeys).tryAuthor(slot).foreach(blk => chainA.importBlock(chainA.encodeBlock(blk)).isRight shouldBe true)
        slot += 1
      chainA.best.slot shouldBe 24L
      chainA.finalize(chainA.best.hash).isRight shouldBe true
      chainA.unverifiedCount shouldBe 0
      allUnverifiable.exists(f => chainA.hasBlock(chainA.headerHashOf(f))) shouldBe false
      chainA.hasBlock(hash4) shouldBe true
      chainA.hasBlock(bestA.hash) shouldBe true // A's abandoned block 3: seal-verified, not subject to the prune
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
