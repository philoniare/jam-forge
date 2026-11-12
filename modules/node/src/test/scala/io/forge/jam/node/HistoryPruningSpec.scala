package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import io.forge.jam.db.ShardStore
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Finalized history more than `maxLookupAnchorAge` slots (24 in the tiny configuration) behind the finalized block
  * is deleted, with the side branches forking off it; the node keeps authoring, importing and reorging on what is
  * left, and comes back from a restart. Shard custody units expire by the time they were stored.
  */
class HistoryPruningSpec extends AnyFunSuite with Matchers:

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")
  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def tempDir(p: String): Path = Files.createTempDirectory(p)
  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)
  private val window = ChainConfig.TINY.maxLookupAnchorAge

  private def spec(id: String, genesis: Genesis): ChainSpec =
    ChainSpec(
      id = id,
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

  private def newNode(spec: ChainSpec, dir: Path, prune: Boolean = true, pruneState: Boolean = true): JamNode =
    val node = new JamNode(
      spec,
      NodeConfig(dataDir = dir, slotTicking = false, pruneFinalizedHistory = prune, pruneOldState = pruneState)
    )
    node.chain.initializeOrRestore(spec)
    node

  /** Author `slots` on `node`, finalizing two blocks behind the head as it goes; returns (slot, hash) per block. */
  private def grow(node: JamNode, slots: Range): List[(Long, Hash)] =
    slots.toList.map { slot =>
      val head = node.authorSlot(slot.toLong).getOrElse(fail(s"author slot $slot"))
      node.chain.finalizeAtDepth(2)
      slot.toLong -> head.hash
    }

  private def stored(node: JamNode, h: Hash): Boolean =
    node.chain.hasBlock(h) || node.chain.headerBytes(h).isDefined || node.chain.meta(s"height:${h.toHex}").isDefined

  test("blocks more than maxLookupAnchorAge slots behind finality are deleted; the rest, genesis and the chain's progress are kept") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val sp = spec("prune-devnet", genesis)
    val dir = tempDir("jam-prune")
    var node: JamNode = null
    try
      node = newNode(sp, dir)
      node.enableAuthoring(devKeys)
      val blocks = grow(node, 1 to 70)
      val fin = node.chain.finalized
      fin.slot shouldBe 68L
      val cutoff = fin.slot - window

      // Outside the window: body, header and metadata are gone. The one block just below the cutoff is the floor.
      val (old, kept) = blocks.partition(_._1 < cutoff)
      old.size should be > 30
      old.init.foreach((slot, h) => withClue(s"slot $slot: ") { stored(node, h) shouldBe false })
      node.chain.pruneFloorHeight shouldBe old.last._1 // one block per slot, so height == slot
      // Inside it: every block, with its header for the ancestor set.
      kept.foreach { (slot, h) =>
        withClue(s"slot $slot: ") {
          node.chain.hasBlock(h) shouldBe true
          node.chain.headerBytes(h).isDefined shouldBe true
        }
      }
      // Genesis stays.
      node.chain.headerBytes(sp.genesisHeaderHash).isDefined shouldBe true
      // The finalized-chain index holds only the kept range.
      node.chain.meta(f"fin:${5L}%016x") shouldBe None
      node.chain.meta(f"fin:${fin.slot}%016x").map(Hash(_)) shouldBe Some(fin.hash)

      // The chain goes on, and a block request for pruned history gets what is left rather than failing.
      grow(node, 71 to 80)
      node.chain.best.slot shouldBe 80L
      node.chain.blocksDescending(node.chain.best.hash, 64).size should be <= (window + 4).toInt
      node.chain.blocksDescending(node.chain.best.hash, 8).size shouldBe 8
    finally
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)
  }

  test("a pruned node restarts with its head, finality and leaves, and keeps importing") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val sp = spec("prune-restart", genesis)
    val dir = tempDir("jam-prune-restart")
    val dirAuthor = tempDir("jam-prune-restart-author")
    var node: JamNode = null
    var author: JamNode = null
    try
      // Another node authors the chain (and keeps all of it); the node under test imports it block by block.
      author = newNode(sp, dirAuthor, prune = false)
      author.enableAuthoring(devKeys)
      node = newNode(sp, dir)
      def follow(slots: Range): Unit =
        slots.foreach { slot =>
          val head = author.authorSlot(slot.toLong).getOrElse(fail(s"author slot $slot"))
          val bytes = author.chain.blocksDescending(head.hash, 1).head
          withClue(s"import of slot $slot: ") { node.chain.importBlock(bytes).isRight shouldBe true }
          node.chain.finalizeAtDepth(2)
        }
      follow(1 to 60)
      val best = node.chain.best
      val fin = node.chain.finalized
      val floor = node.chain.pruneFloorHeight
      floor should be > 20L
      node.shutdownStorageOnly()

      node = newNode(sp, dir)
      node.chain.best.hash shouldBe best.hash
      node.chain.finalized.hash shouldBe fin.hash
      node.chain.pruneFloorHeight shouldBe floor
      node.chain.leaves.map(_.hash) shouldBe List(best.hash)
      follow(61 to 75)
      node.chain.best.slot shouldBe 75L
      node.chain.best.hash shouldBe author.chain.best.hash
      node.chain.pruneFloorHeight should be > floor
    finally
      if node != null then node.shutdownStorageOnly()
      if author != null then author.shutdownStorageOnly()
      cleanup(dir)
      cleanup(dirAuthor)
  }

  test("a side branch is deleted once the block it forks from leaves the window") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val sp = spec("prune-forks", genesis)
    val dirA = tempDir("jam-prune-fork-a")
    val dirB = tempDir("jam-prune-fork-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = newNode(sp, dirA)
      nodeB = newNode(sp, dirB)
      nodeA.enableAuthoring(devKeys)
      val bare = new BlockAuthor(nodeA.chain, devKeys)
      val block1 = bare.tryAuthor(1).getOrElse(fail("author block 1"))
      val block1Bytes = nodeA.chain.encodeBlock(block1)
      nodeA.chain.importBlock(block1Bytes).isRight shouldBe true
      nodeB.chain.importBlock(block1Bytes).isRight shouldBe true

      // A's own block 2; B's competing 3' is parked on A as a side branch off block 1.
      nodeA.authorSlot(2).isDefined shouldBe true
      val authorB = new BlockAuthor(nodeB.chain, devKeys)
      val side = authorB.tryAuthor(3).getOrElse(fail("author 3'"))
      val sideBytes = nodeB.chain.encodeBlock(side)
      val sideHash = Hashing.blake2b256(nodeB.chain.headerCodec.encode(side.header))
      nodeA.chain.importBlock(sideBytes).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      nodeA.chain.hasBlock(sideHash) shouldBe true

      // While block 1 is inside the window the side branch stays.
      grow(nodeA, 3 to 20)
      nodeA.chain.hasBlock(sideHash) shouldBe true
      // Once block 1 is pruned the branch goes with it.
      grow(nodeA, 21 to 60)
      nodeA.chain.pruneFloorHeight should be > 1L
      stored(nodeA, sideHash) shouldBe false
      nodeA.chain.leaves.map(_.hash) shouldBe List(nodeA.chain.best.hash)
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("old states are deleted, and every kept block's state is whole and equal to an unpruned node's") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val sp = spec("prune-state", genesis)
    val dirFull = tempDir("jam-prune-state-full")
    val dirPruned = tempDir("jam-prune-state-pruned")
    var full: JamNode = null
    var pruned: JamNode = null
    try
      // One node authors and keeps everything; the other imports the same blocks and prunes.
      full = newNode(sp, dirFull, prune = false, pruneState = false)
      full.enableAuthoring(devKeys)
      pruned = newNode(sp, dirPruned)
      // A third keeps its blocks (the default) and must prune state just the same.
      val dirKeep = tempDir("jam-prune-state-keep")
      val keep = newNode(sp, dirKeep, prune = false)
      val blocks = (1 to 90).toList.map { slot =>
        val head = full.authorSlot(slot.toLong).getOrElse(fail(s"author slot $slot"))
        full.chain.finalizeAtDepth(2)
        val bytes = full.chain.blocksDescending(head.hash, 1).head
        withClue(s"import of slot $slot: ") { pruned.chain.importBlock(bytes).isRight shouldBe true }
        pruned.chain.finalizeAtDepth(2)
        keep.chain.importBlock(bytes).isRight shouldBe true
        keep.chain.finalizeAtDepth(2)
        slot.toLong -> head
      }
      pruned.chain.best.stateRoot shouldBe full.chain.best.stateRoot
      pruned.chain.pruneFloorHeight should be > 50L

      // Every key of every kept block's state, against the node that deleted nothing.
      val lo = io.forge.jam.core.JamBytes(new Array[Byte](31))
      val hi = io.forge.jam.core.JamBytes(Array.fill[Byte](31)(0xff.toByte))
      def allPairs(node: JamNode, h: Hash) =
        node.chain.stateAt(h).getOrElse(fail("state missing")).range(lo, hi, Int.MaxValue)._2
      val kept = blocks.filter(_._1 >= pruned.chain.pruneFloorHeight)
      kept.size should be >= window.toInt
      kept.foreach { (slot, head) =>
        withClue(s"state of slot $slot: ") {
          val mine = allPairs(pruned, head.hash)
          mine.size should be > 10
          mine shouldBe allPairs(full, head.hash)
        }
      }

      // And the old states are really gone from disk.
      val (nodesPruned, valuesPruned) = pruned.chain.storedStateCounts
      val (nodesFull, valuesFull) = full.chain.storedStateCounts
      nodesPruned should be < nodesFull
      valuesPruned should be <= valuesFull
      keep.chain.storedStateCounts shouldBe ((nodesPruned, valuesPruned))
      keep.chain.best.stateRoot shouldBe full.chain.best.stateRoot
      blocks.foreach((_, head) => keep.chain.hasBlock(head.hash) shouldBe true)
      keep.shutdownStorageOnly()
      cleanup(dirKeep)
      info(s"state nodes on disk after 90 blocks: $nodesPruned pruned vs $nodesFull unpruned; values $valuesPruned vs $valuesFull")
    finally
      if full != null then full.shutdownStorageOnly()
      if pruned != null then pruned.shutdownStorageOnly()
      cleanup(dirFull)
      cleanup(dirPruned)
  }

  test("with block pruning off every block stays, and old state is still deleted") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val sp = spec("prune-off", genesis)
    val dir = tempDir("jam-prune-off")
    var node: JamNode = null
    try
      node = newNode(sp, dir, prune = false)
      node.enableAuthoring(devKeys)
      val blocks = grow(node, 1 to 60)
      blocks.foreach { (_, h) =>
        node.chain.hasBlock(h) shouldBe true
        node.chain.headerBytes(h).isDefined shouldBe true
      }
      // The state floor still follows finality: states below it are gone, those above are readable.
      val floor = node.chain.pruneFloorHeight
      floor should be > 20L
      node.chain.stateAt(blocks(4)._2) shouldBe None
      blocks.filter(_._1 >= floor).foreach((slot, h) => withClue(s"slot $slot: ") { node.chain.stateAt(h).isDefined shouldBe true })
      node.chain.blocksDescending(node.chain.best.hash, 64).size shouldBe 60
    finally
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)
  }

  test("shard custody units expire by when their erasure-root was last stored") {
    val dir = tempDir("jam-shard-expiry")
    var now = 1000L
    val store = ShardStore.open(dir, clock = () => now)
    try
      val a = Hashing.blake2b256(Array[Byte](1))
      val b = Hashing.blake2b256(Array[Byte](2))
      val c = Hashing.blake2b256(Array[Byte](3))
      store.put(a, 0, Array[Byte](1)); store.put(a, 5, Array[Byte](2))
      now = 2000L
      store.put(b, 1, Array[Byte](3))
      now = 3000L
      store.put(c, 2, Array[Byte](4))
      // `a` is stored again later: it is kept as long as its latest unit.
      now = 3500L
      store.put(a, 7, Array[Byte](5))

      store.deleteStoredBefore(2500L) shouldBe 1 // only b
      store.heldIndexes(b) shouldBe Nil
      store.heldIndexes(a).sorted shouldBe List(0, 5, 7)
      store.heldIndexes(c) shouldBe List(2)

      store.deleteStoredBefore(3200L) shouldBe 1 // c
      store.heldIndexes(a).sorted shouldBe List(0, 5, 7)
      store.deleteStoredBefore(4000L) shouldBe 3 // a's three units
      store.heldIndexes(a) shouldBe Nil
      store.deleteStoredBefore(Long.MaxValue) shouldBe 0

      JamNode.shardRetentionSeconds(ChainConfig.FULL) shouldBe 28L * 24 * 3600
    finally
      store.close()
      cleanup(dir)
  }
