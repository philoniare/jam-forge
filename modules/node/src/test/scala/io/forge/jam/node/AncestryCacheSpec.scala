package io.forge.jam.node

import java.nio.file.{Files, Path}

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.Hash
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend}
import io.forge.jam.protocol.report.ReportTypes.{AncestorHeader, AncestorIndex}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The ancestor walk as it was before the header cache: one block-store read per header. The oracle for the cached
  * `ChainManager.ancestorsOf`.
  */
object AncestryOracle:
  def walk(store: BlockStore, config: ChainConfig, parent: Hash, parentPostRoot: Hash, slot: Long): List[AncestorHeader] =
    val out = List.newBuilder[AncestorHeader]
    var cursor = parent
    var postRoot = parentPostRoot
    var more = true
    while more do
      store.getHeader(cursor).filter(_.length >= 100) match
        case Some(h) =>
          val hSlot = io.forge.jam.core.LittleEndian.get(h, 96, 4)
          if slot - hSlot > config.maxLookupAnchorAge then more = false
          else
            out += AncestorHeader(hSlot, cursor, Some(postRoot))
            postRoot = Hash(h.slice(32, 64))
            val p = Hash(h.take(32))
            if p == Hash.zero then more = false else cursor = p
        case None => more = false
    out.result()

  /** The lookup-anchor predicate of ReportTransition before the index. */
  def contains(ancestry: List[AncestorHeader], hash: Hash, slot: Long, root: Hash): Boolean =
    ancestry.exists(a => a.headerHash == hash && a.slot == slot && a.postStateRoot.forall(_ == root))

/** The ancestor set A of `ChainManager.ancestorsOf` (used by the lookup-anchor check), served from the
  * in-memory header cache with the block store as fallback.
  */
class AncestryCacheSpec extends AnyFunSuite with Matchers:

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  private def withChain[A](config: ChainConfig)(f: ChainManager => A): A =
    val dir = Files.createTempDirectory("jam-ancestry")
    val trie = RocksDbTrieBackend.open(dir.resolve("trie"), trackGarbage = false)
    val blocks = BlockStore.open(dir.resolve("blocks"))
    try f(new ChainManager(config, trie, blocks))
    finally
      blocks.close()
      trie.close()
      cleanup(dir)

  private def randomHash(rnd: scala.util.Random): Hash =
    val b = new Array[Byte](32)
    rnd.nextBytes(b)
    Hash(b)

  /** Header bytes with the fields the walk reads: parent ++ prior state root ++ extrinsic hash ++ E_4(slot). */
  private def headerBytes(parent: Hash, priorRoot: Hash, slot: Long, rnd: scala.util.Random): Array[Byte] =
    val out = new Array[Byte](120)
    System.arraycopy(parent.bytes.toArray, 0, out, 0, 32)
    System.arraycopy(priorRoot.bytes.toArray, 0, out, 32, 32)
    val ext = new Array[Byte](32)
    rnd.nextBytes(ext)
    System.arraycopy(ext, 0, out, 64, 32)
    (0 until 4).foreach(i => out(96 + i) = ((slot >>> (8 * i)) & 0xff).toByte)
    out

  /** Store a header; `viaImport` also records it in the cache the way an import does (otherwise it is filled lazily). */
  private def store(chain: ChainManager, h: Hash, header: Array[Byte], parent: Hash, viaImport: Boolean): Unit =
    chain.blockStore.putBlock(h, parent, header, Array[Byte](1))
    if viaImport then chain.headerLinks.record(h, header)

  /** Store a linear chain of `n` headers at slots 1..n on top of a genesis header; returns (hash, slot) newest first. */
  private def storeLinearChain(chain: ChainManager, n: Int, rnd: scala.util.Random): List[(Hash, Long)] =
    val genesis = randomHash(rnd)
    store(chain, genesis, headerBytes(Hash.zero, Hash.zero, 0L, rnd), Hash.zero, viaImport = false)
    var parent = genesis
    var out = List((genesis, 0L))
    (1 to n).foreach { i =>
      val h = randomHash(rnd)
      store(chain, h, headerBytes(parent, randomHash(rnd), i.toLong, rnd), parent, viaImport = false)
      out = (h, i.toLong) :: out
      parent = h
    }
    out

  private def checkAgainstOracle(chain: ChainManager, h: Hash, slot: Long, root: Hash): Unit =
    val expected = AncestryOracle.walk(chain.blockStore, chain.config, h, root, slot)
    withClue(s"ancestors of ${h.toHex.take(18)} for slot $slot: ") {
      chain.ancestorsOf(h, root, slot) shouldBe expected
    }

  test("equivalence: on a random block tree with forks the ancestor set equals the uncached store walk") {
    withChain(ChainConfig.TINY) { chain =>
      val rnd = new scala.util.Random(20261111L)
      val L = chain.config.maxLookupAnchorAge
      val genesis = randomHash(rnd)
      store(chain, genesis, headerBytes(Hash.zero, Hash.zero, 0L, rnd), Hash.zero, viaImport = true)
      val blocks = scala.collection.mutable.ArrayBuffer((genesis, 0L))
      (1 to 800).foreach { _ =>
        // Mostly extend one of the newest blocks (long branches), sometimes fork far back.
        val (parent, parentSlot) =
          if rnd.nextInt(10) == 0 then blocks(rnd.nextInt(blocks.size))
          else blocks(math.max(0, blocks.size - 1 - rnd.nextInt(6)))
        val h = randomHash(rnd)
        val slot = parentSlot + 1 + rnd.nextInt(3)
        rnd.nextInt(50) match
          case 0 => // orphan: its parent is not stored, so the walk stops above it
            store(chain, h, headerBytes(randomHash(rnd), randomHash(rnd), slot, rnd), parent, rnd.nextBoolean())
          case 1 => // a stored header too short to carry a slot ends the walk
            chain.blockStore.putBlock(h, parent, new Array[Byte](40), Array[Byte](1))
          case _ =>
            store(chain, h, headerBytes(parent, randomHash(rnd), slot, rnd), parent, rnd.nextBoolean())
        blocks += ((h, slot))
      }
      def checkAll(): Unit =
        blocks.foreach { (h, slot) =>
          val root = randomHash(rnd)
          List(slot + 1, slot + L, slot + L + 1, slot + 2 * L).foreach(s => checkAgainstOracle(chain, h, s, root))
        }
      checkAll() // cache partly import-filled, partly cold
      checkAll() // warm
      chain.headerLinks.clear()
      checkAll() // cold
      // An unknown parent has no ancestors.
      chain.ancestorsOf(randomHash(rnd), randomHash(rnd), 10L) shouldBe Nil
    }
  }

  test("side branches see their own ancestors, not the other branch's or the best chain's") {
    withChain(ChainConfig.TINY) { chain =>
      val rnd = new scala.util.Random(3L)
      val trunk = storeLinearChain(chain, 10, rnd)
      val (fork, forkSlot) = trunk.head
      def branch(skip: Int): List[(Hash, Long)] =
        var parent = fork
        (1 to 30).map { i =>
          val h = randomHash(rnd)
          val slot = forkSlot + skip + i
          store(chain, h, headerBytes(parent, randomHash(rnd), slot, rnd), parent, viaImport = true)
          parent = h
          (h, slot)
        }.toList
      val x = branch(0)
      val y = branch(1)
      chain.headerLinks.advance(x.last._2) // x is "best"
      val xHashes = x.map(_._1).toSet
      val yHashes = y.map(_._1).toSet
      List(x, y).foreach(_.foreach((h, s) => checkAgainstOracle(chain, h, s + 1, Hash.zero)))
      val onY = chain.ancestorsOf(y.last._1, Hash.zero, y.last._2 + 1).map(_.headerHash)
      onY.toSet.intersect(xHashes) shouldBe empty
      onY.head shouldBe y.last._1
      onY shouldBe y.reverse.filter(_._2 >= y.last._2 + 1 - chain.config.maxLookupAnchorAge).map(_._1)
      val onX = chain.ancestorsOf(x.last._1, Hash.zero, x.last._2 + 1).map(_.headerHash)
      onX.toSet.intersect(yHashes) shouldBe empty
      // A short branch still reaches into the shared trunk.
      chain.ancestorsOf(y.head._1, Hash.zero, y.head._2 + 1).map(_.headerHash).tail.head shouldBe fork
    }
  }

  test("eviction bounds the cache and never changes the ancestor set (misses fall back to the store)") {
    withChain(ChainConfig.TINY) { chain =>
      val rnd = new scala.util.Random(11L)
      val window = chain.config.maxLookupAnchorAge + chain.config.epochLength
      val linear = storeLinearChain(chain, 5000, rnd)
      // Import-style fill while the horizon advances: the cache stays at its high-water mark, not at 5000.
      linear.reverse.foreach { (h, s) =>
        chain.headerLinks.record(h, chain.blockStore.getHeader(h).get)
        chain.headerLinks.advance(s)
      }
      chain.headerLinks.size should be <= 1024
      chain.headerLinks.sweep()
      chain.headerLinks.size.toLong should be <= window + 1
      // Old blocks (evicted) and new ones (cached) answer as the store walk does.
      linear.filter((_, s) => s % 97 == 0 || s > 4990).foreach((h, s) =>
        checkAgainstOracle(chain, h, s + 1, Hash.zero)
      )
      // Invalidating a header that left the store ends the walk there, as the store walk does.
      val (tip, tipSlot) = linear.head
      val (cut, _) = linear(5)
      chain.blockStore.deleteBlock(cut, linear(6)._1)
      chain.headerLinks.invalidate(cut)
      chain.ancestorsOf(tip, Hash.zero, tipSlot + 1).map(_.headerHash) shouldBe linear.take(5).map(_._1)
      checkAgainstOracle(chain, tip, tipSlot + 1, Hash.zero)
    }
  }

  test("lookup-anchor index answers exactly as the linear scan of the ancestor set") {
    withChain(ChainConfig.TINY) { chain =>
      val rnd = new scala.util.Random(5L)
      val linear = storeLinearChain(chain, 60, rnd)
      val (tip, tipSlot) = linear.head
      val root = randomHash(rnd)
      val ancestry = chain.ancestorsOf(tip, root, tipSlot + 1)
      val index = new AncestorIndex(ancestry)
      val tooOld = linear.find((_, s) => tipSlot + 1 - s > chain.config.maxLookupAnchorAge).get
      val queries =
        ancestry.flatMap(a =>
          val r = a.postStateRoot.get
          List((a.headerHash, a.slot, r), (a.headerHash, a.slot + 1, r), (a.headerHash, a.slot, randomHash(rnd)))
        ) ++ List((tooOld._1, tooOld._2, Hash.zero), (randomHash(rnd), tipSlot, root))
      queries.foreach((h, s, r) => index.containsLookupAnchor(h, s, r) shouldBe AncestryOracle.contains(ancestry, h, s, r))
      queries.count((h, s, r) => index.containsLookupAnchor(h, s, r)) shouldBe ancestry.size
      new AncestorIndex(Nil).containsLookupAnchor(tip, tipSlot, root) shouldBe false
    }
  }

  private def medianMicros(runs: Int)(f: => Any): Double =
    val samples = (1 to runs).map { _ =>
      val t0 = System.nanoTime()
      f
      (System.nanoTime() - t0) / 1000.0
    }.sorted
    samples(samples.size / 2)

  test("micro-measurement: ancestorsOf on a FULL-config chain of 20,000 stored headers") {
    withChain(ChainConfig.FULL) { chain =>
      val rnd = new scala.util.Random(7)
      val chainList = storeLinearChain(chain, 20_000, rnd)
      val (tip, tipSlot) = chainList.head
      chain.headerLinks.advance(tipSlot)
      val root = randomHash(rnd)
      val ancestry = chain.ancestorsOf(tip, root, tipSlot + 1)
      ancestry.size shouldBe 14_400
      ancestry shouldBe AncestryOracle.walk(chain.blockStore, chain.config, tip, root, tipSlot + 1)
      (1 to 5).foreach(_ => chain.ancestorsOf(tip, root, tipSlot + 1))
      val walk = medianMicros(30)(chain.ancestorsOf(tip, root, tipSlot + 1))
      val oracle = medianMicros(30)(AncestryOracle.walk(chain.blockStore, chain.config, tip, root, tipSlot + 1))
      // 341 guarantees (C in FULL), each looking up an anchor in A.
      val anchors = (0 until 341).map(i => ancestry(i * 40 % ancestry.size))
      val scan = medianMicros(30)(anchors.count(a => AncestryOracle.contains(ancestry, a.headerHash, a.slot, Hash.zero)))
      val indexed = medianMicros(30) {
        val index = new AncestorIndex(ancestry)
        anchors.count(a => index.containsLookupAnchor(a.headerHash, a.slot, Hash.zero))
      }
      info(f"ancestorsOf, cached (L = 14,400): median $walk%.0f us; uncached store walk: median $oracle%.0f us")
      info(f"341 lookup-anchor checks: linear scan median $scan%.0f us; index build + lookups median $indexed%.0f us")
    }
  }
