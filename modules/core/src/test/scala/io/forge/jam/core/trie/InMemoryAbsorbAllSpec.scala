package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.HashTables.{NodeTable, ValueTable}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class InMemoryAbsorbAllSpec extends AnyFlatSpec with Matchers:

  private def random(rng: Random): Array[Byte] = rng.nextBytes(32)

  private def crowded(rng: Random): Array[Byte] =
    val k = rng.nextBytes(32)
    k(7) = 0x13
    k(6) = 0xff.toByte
    k(5) = 0xff.toByte
    k(4) = (0xf0 | (k(4) & 0x0f)).toByte
    k

  private final case class Pending(nodes: NodeTable, values: ValueTable)

  private def pending(rng: Random, fresh: Seq[Array[Byte]], known: Seq[Array[Byte]]): Pending =
    val nodes = new NodeTable
    val values = new ValueTable
    fresh.foreach { k =>
      val slot = nodes.findOrInsert(k)
      nodes.setBranch(slot, rng.nextBytes(32), rng.nextBytes(32))
      nodes.setRef(slot, 1 + rng.nextInt(2))
      if rng.nextInt(3) == 0 then
        val vs = values.findOrInsert(k)
        values.setValue(vs, rng.nextBytes(40))
        values.addRef(vs, 1)
    }
    known.foreach { k =>
      val slot = nodes.findOrInsert(k)
      nodes.setRef(slot, nodes.ref(slot) + (if rng.nextBoolean() then -1 else 1))
      if nodes.ref(slot) == 0 then nodes.remove(slot)
      val vs = values.findOrInsert(k)
      values.addRef(vs, -1)
    }
    Pending(nodes, values)

  private def same(a: InMemoryTrieBackend, b: InMemoryTrieBackend, keys: Seq[Array[Byte]], clue: String): Unit =
    withClue(s"$clue: ") {
      a.nodeCount shouldBe b.nodeCount
      a.valueCount shouldBe b.valueCount
      keys.foreach { k =>
        val h = Hash.wrapUnchecked(k)
        a.nodeRefcount(h) shouldBe b.nodeRefcount(h)
        a.valueRefcount(h) shouldBe b.valueRefcount(h)
        a.readNode(h).map(n => (n.nodeType, n.left, n.right)) shouldBe b.readNode(h).map(n => (n.nodeType, n.left, n.right))
        a.readRawValue(h) shouldBe b.readRawValue(h)
      }
    }

  private def check(seed: Int, key: Random => Array[Byte], perTable: Int): Unit =
    val rng = new Random(seed)
    val whole = new InMemoryTrieBackend
    val oneByOne = new InMemoryTrieBackend
    var all = Vector.empty[Array[Byte]]
    for round <- 1 to 4 do
      val shared = key(rng)
      val tables = (0 until 6).map { t =>
        val size = if t == 5 then 10 else perTable
        val fresh = Vector.fill(size)(if rng.nextInt(8) == 0 then random(rng) else key(rng)) :+ shared
        val known = if all.isEmpty then Vector.empty else rng.shuffle(all).take(size / 3)
        (fresh, known)
      }
      def build(): Vector[Pending] =
        val r = new Random(seed * 100 + round)
        tables.map((fresh, known) => pending(r, fresh, known)).toVector
      val forWhole = build()
      whole.absorbAll(forWhole.map(_.nodes).toArray, forWhole.map(_.values).toArray)
      build().foreach(p => oneByOne.absorb(p.nodes, p.values))
      all = all ++ tables.flatMap(_._1)
      same(whole, oneByOne, all, s"seed $seed round $round")
      whole.gc()
      oneByOne.gc()
      whole.pendingDeadCandidates shouldBe 0
      same(whole, oneByOne, all, s"seed $seed round $round after gc")

  "absorbAll" should "store what absorbing the tables one after another stores" in {
    for seed <- 1 to 3 do check(seed, random, perTable = 3_000)
  }

  it should "place keys whose probes leave their range of the store" in {
    for seed <- 4 to 5 do check(seed, crowded, perTable = 2_000)
  }

  it should "take a pass too small to spread" in {
    check(6, random, perTable = 40)
  }
