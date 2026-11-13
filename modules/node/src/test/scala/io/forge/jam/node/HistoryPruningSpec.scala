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

  /** Author `slot` on `node` with a bare [[BlockAuthor]] and import it. No ticket service, so no ring-VRF ticket
    * generation each epoch: the chain is key-sealed throughout, which is all pruning needs.
    */
  private def authorOn(node: JamNode, slot: Long): ChainManager#Head =
    val block = new BlockAuthor(node.chain, devKeys).tryAuthor(slot).getOrElse(fail(s"author slot $slot"))
    node.chain.importBlock(node.chain.encodeBlock(block)).fold(e => fail(s"import of slot $slot: $e"), identity)

  /** Author `slots` on `node`, finalizing two blocks behind the head as it goes; returns (slot, hash) per block. */
  private def grow(node: JamNode, slots: Range): List[(Long, Hash)] =
    slots.toList.map { slot =>
      val head = authorOn(node, slot.toLong)
      node.chain.finalizeAtDepth(2)
      slot.toLong -> head.hash
    }

  private def stored(node: JamNode, h: Hash): Boolean =
    node.chain.hasBlock(h) || node.chain.headerBytes(h).isDefined || node.chain.meta(s"height:${h.toHex}").isDefined

  test("blocks more than maxLookupAnchorAge slots behind finality are deleted with the side branches off them; the rest, genesis and the chain's progress are kept") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val sp = spec("prune-devnet", genesis)
    val dirA = tempDir("jam-prune")
    val dirB = tempDir("jam-prune-fork-b")
    var node: JamNode = null
    var nodeB: JamNode = null
    try
      node = newNode(sp, dirA)
      nodeB = newNode(sp, dirB)
      val block1 = new BlockAuthor(node.chain, devKeys).tryAuthor(1).getOrElse(fail("author block 1"))
      val block1Bytes = node.chain.encodeBlock(block1)
      val head1 = node.chain.importBlock(block1Bytes).fold(e => fail(s"import of block 1: $e"), identity)
      nodeB.chain.importBlock(block1Bytes).isRight shouldBe true

      // A's own block 2; B's competing 3' is parked on A as a side branch off block 1. A leaves slot 3 to it: two
      // blocks at one slot are an equivocation, and the best chain would leave its own.
      val head2 = authorOn(node, 2)
      val side = new BlockAuthor(nodeB.chain, devKeys).tryAuthor(3).getOrElse(fail("author 3'"))
      val sideHash = nodeB.chain.headerHashOf(side)
      node.chain.importBlock(nodeB.chain.encodeBlock(side)).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      node.chain.hasBlock(sideHash) shouldBe true

      // While block 1 is inside the window the side branch stays.
      val early = List(1L -> head1.hash, 2L -> head2.hash) ++ grow(node, 4 to 20)
      node.chain.hasBlock(sideHash) shouldBe true

      val blocks = early ++ grow(node, 21 to 40)
      val fin = node.chain.finalized
      fin.slot shouldBe 38L
      val cutoff = fin.slot - window

      // Outside the window: body, header and metadata are gone. The one block just below the cutoff is the floor.
      val (old, kept) = blocks.partition(_._1 < cutoff)
      old.size should be > 10
      old.init.foreach((slot, h) => withClue(s"slot $slot: ") { stored(node, h) shouldBe false })
      // Heights along A's chain: one block per slot, except the slot it left to 3'.
      def heightOf(h: Hash): Long = blocks.indexWhere(_._2 == h) + 1L
      node.chain.pruneFloorHeight shouldBe heightOf(old.last._2)
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
      node.chain.meta(f"fin:${heightOf(fin.hash)}%016x").map(Hash(_)) shouldBe Some(fin.hash)
      // Block 1 is pruned, so the side branch off it went with it.
      stored(node, sideHash) shouldBe false
      node.chain.leaves.map(_.hash) shouldBe List(node.chain.best.hash)

      // The chain goes on, and a block request for pruned history gets what is left rather than failing.
      grow(node, 41 to 45)
      node.chain.best.slot shouldBe 45L
      node.chain.blocksDescending(node.chain.best.hash, 64).size should be <= (window + 4).toInt
      node.chain.blocksDescending(node.chain.best.hash, 8).size shouldBe 8
    finally
      if node != null then node.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
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
      node = newNode(sp, dir)
      def follow(slots: Range): Unit =
        slots.foreach { slot =>
          val head = authorOn(author, slot.toLong)
          val bytes = author.chain.blocksDescending(head.hash, 1).head
          withClue(s"import of slot $slot: ") { node.chain.importBlock(bytes).isRight shouldBe true }
          node.chain.finalizeAtDepth(2)
        }
      follow(1 to 40)
      val best = node.chain.best
      val fin = node.chain.finalized
      val floor = node.chain.pruneFloorHeight
      floor should be > 10L
      node.shutdownStorageOnly()

      node = newNode(sp, dir)
      node.chain.best.hash shouldBe best.hash
      node.chain.finalized.hash shouldBe fin.hash
      node.chain.pruneFloorHeight shouldBe floor
      node.chain.leaves.map(_.hash) shouldBe List(best.hash)
      follow(41 to 48)
      node.chain.best.slot shouldBe 48L
      node.chain.best.hash shouldBe author.chain.best.hash
      node.chain.pruneFloorHeight should be > floor
    finally
      if node != null then node.shutdownStorageOnly()
      if author != null then author.shutdownStorageOnly()
      cleanup(dir)
      cleanup(dirAuthor)
  }

  test("old states are deleted, every kept block's state is whole and equal to an unpruned node's, and with block pruning off every block stays") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val sp = spec("prune-state", genesis)
    val dirFull = tempDir("jam-prune-state-full")
    val dirPruned = tempDir("jam-prune-state-pruned")
    var full: JamNode = null
    var pruned: JamNode = null
    try
      // One node authors and keeps everything; the other imports the same blocks and prunes.
      full = newNode(sp, dirFull, prune = false, pruneState = false)
      pruned = newNode(sp, dirPruned)
      // A third keeps its blocks (the default) and must prune state just the same.
      val dirKeep = tempDir("jam-prune-state-keep")
      val keep = newNode(sp, dirKeep, prune = false)
      val blocks = (1 to 45).toList.map { slot =>
        val head = authorOn(full, slot.toLong)
        full.chain.finalizeAtDepth(2)
        val bytes = full.chain.blocksDescending(head.hash, 1).head
        withClue(s"import of slot $slot: ") { pruned.chain.importBlock(bytes).isRight shouldBe true }
        pruned.chain.finalizeAtDepth(2)
        keep.chain.importBlock(bytes).isRight shouldBe true
        keep.chain.finalizeAtDepth(2)
        slot.toLong -> head
      }
      pruned.chain.best.stateRoot shouldBe full.chain.best.stateRoot
      pruned.chain.pruneFloorHeight should be > 15L

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

      // With block pruning off every block stays, with its header; the state floor still follows finality:
      // states below it are gone, those above are readable.
      blocks.foreach { (slot, head) =>
        withClue(s"kept block of slot $slot: ") {
          keep.chain.hasBlock(head.hash) shouldBe true
          keep.chain.headerBytes(head.hash).isDefined shouldBe true
        }
      }
      val keepFloor = keep.chain.pruneFloorHeight
      keepFloor shouldBe pruned.chain.pruneFloorHeight
      keep.chain.stateAt(blocks(4)._2.hash) shouldBe None
      blocks.filter(_._1 >= keepFloor).foreach((slot, head) =>
        withClue(s"state of kept slot $slot: ") { keep.chain.stateAt(head.hash).isDefined shouldBe true }
      )
      keep.chain.blocksDescending(keep.chain.best.hash, 64).size shouldBe 45
      keep.shutdownStorageOnly()
      cleanup(dirKeep)
      info(s"state nodes on disk after 45 blocks: $nodesPruned pruned vs $nodesFull unpruned; values $valuesPruned vs $valuesFull")
    finally
      if full != null then full.shutdownStorageOnly()
      if pruned != null then pruned.shutdownStorageOnly()
      cleanup(dirFull)
      cleanup(dirPruned)
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
