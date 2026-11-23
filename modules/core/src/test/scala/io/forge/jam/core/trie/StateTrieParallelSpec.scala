package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.util.Random

/**
 * Updates large enough to be rebuilt as parallel subtrees ([[StateTrie.ParFrom]] keys, in-memory backend) against the
 * same updates applied in pieces too small to split: the same root after every round, the same nodes and values in
 * the backend after a save and a collection, and the same reads, before the save as well as after.
 */
class StateTrieParallelSpec extends AnyFlatSpec with Matchers:
  private type Batch = Vector[(JamBytes, Option[JamBytes])]

  private final class Pair:
    val split = new InMemoryTrieBackend
    val whole = new InMemoryTrieBackend
    var splitRoot: Hash = Hash.zero
    var wholeRoot: Hash = Hash.zero
    val model = mutable.HashMap[JamBytes, JamBytes]()

    /** Applies `updates` (one update call each) to both tries before one save, the reference in small pieces. */
    def round(updates: Seq[Batch], clue: String): Unit =
      val splitTrie = StateTrie.at(split, splitRoot)
      val wholeTrie = StateTrie.at(whole, wholeRoot)
      updates.foreach { batch =>
        splitTrie.update(batch)
        batch.grouped(StateTrie.ParFrom / 4).foreach(wholeTrie.update)
        batch.foreach {
          case (k, Some(v)) => model(k) = v
          case (k, None)    => model.remove(k)
        }
      }
      withClue(s"$clue: ") {
        splitTrie.rootHash shouldBe wholeTrie.rootHash
        // Before the save: pending nodes and values, wherever they are held, must be readable.
        val sample = model.iterator.take(500).toVector
        sample.foreach((k, v) => splitTrie.read(k) shouldBe Some(v))
        splitTrie.save()
        wholeTrie.save()
        split.gc()
        whole.gc()
        split.nodeCount shouldBe whole.nodeCount
        split.valueCount shouldBe whole.valueCount
        splitRoot = splitTrie.rootHash
        wholeRoot = wholeTrie.rootHash
        val saved = StateTrie.at(split, splitRoot)
        sample.foreach((k, v) => saved.read(k) shouldBe Some(v))
        saved.getKeyValues(JamBytes.wrap(new Array[Byte](31)), 0).size shouldBe model.size
      }

  private def key(rng: Random): JamBytes = JamBytes(rng.nextBytes(31))

  "A large update on the in-memory backend" should "leave the trie a piecewise update leaves" in {
    for seed <- 1 to 3 do
      val rng = new Random(seed)
      // A few long values shared by many keys, so that subtrees built on different threads hold the same raw value;
      // otherwise short (embedded) and long (regular) values of their own.
      val shared = Vector.fill(4)(JamBytes(rng.nextBytes(48)))
      def value(): JamBytes = rng.nextInt(4) match
        case 0 => shared(rng.nextInt(shared.size))
        case 1 => JamBytes(rng.nextBytes(rng.nextInt(33)))
        case _ => JamBytes(rng.nextBytes(33 + rng.nextInt(40)))
      val pair = new Pair
      def existing(n: Int): Vector[JamBytes] = rng.shuffle(pair.model.keys.toVector.sortBy(_.toHex)).take(n)

      // An empty trie filled in one batch.
      pair.round(Seq(Vector.fill(20_000)((key(rng), Some(value())))), s"seed $seed fill")
      // Inserts, overwrites, deletes, deletes of absent keys and a key written twice, in one batch.
      val mixed: Batch =
        Vector.fill(6_000)((key(rng), Some(value()))) ++
          existing(4_000).map(k => (k, Some(value()))) ++
          existing(3_000).map(k => (k, None)) ++
          Vector.fill(500)((key(rng), None))
      pair.round(Seq(rng.shuffle(mixed) :+ ((mixed.head._1, Some(value())))), s"seed $seed mixed")
      // Two updates before one save: the second, small and then large, meets nodes the first left pending.
      val first: Batch = Vector.fill(8_000)((key(rng), Some(value())))
      val undo: Batch = first.take(300).map((k, _) => (k, None)) ++ first.slice(300, 600).map((k, _) => (k, Some(value())))
      val second: Batch = Vector.fill(5_000)((key(rng), Some(value()))) ++ first.slice(600, 1_600).map((k, _) => (k, None))
      pair.round(Seq(first, undo, second), s"seed $seed two updates")
      // Nearly everything deleted: whole subtrees empty out and branches collapse above the split.
      val keep = existing(7)
      pair.round(Seq(pair.model.keys.toVector.filterNot(keep.contains).map(k => (k, None))), s"seed $seed drain")
      pair.model.size shouldBe keep.size
  }

  it should "give keys that crowd into a few subtrees the same trie" in {
    val rng = new Random(9)
    def crowded(): JamBytes =
      val a = rng.nextBytes(31)
      a(0) = (if rng.nextInt(10) == 0 then a(0) else 0x50).toByte // nine keys in ten under one 8-bit prefix
      JamBytes.wrap(a)
    val pair = new Pair
    pair.round(Seq(Vector.fill(10_000)((crowded(), Some(JamBytes(rng.nextBytes(40)))))), "crowded fill")
    pair.round(Seq(Vector.fill(6_000)((crowded(), Some(JamBytes(rng.nextBytes(20)))))), "crowded update")
  }

  it should "give a trie of one key the same trie" in {
    val rng = new Random(10)
    val pair = new Pair
    pair.round(Seq(Vector((key(rng), Some(JamBytes(rng.nextBytes(40)))))), "one key")
    pair.round(Seq(Vector.fill(9_000)((key(rng), Some(JamBytes(rng.nextBytes(40)))))), "onto one key")
  }

  private def keyUnder(rng: Random, first: Int): JamBytes =
    val a = rng.nextBytes(31)
    a(0) = first.toByte
    JamBytes.wrap(a)

  private def entry(rng: Random, first: Int): (JamBytes, Option[JamBytes]) =
    (keyUnder(rng, first), Some(JamBytes(rng.nextBytes(40))))

  it should "build new keys under an empty slot above the split depth" in {
    val rng = new Random(11)
    val pair = new Pair
    pair.round(Seq(Vector(entry(rng, 0x00), entry(rng, 0x04))), "seed 00, 04")
    pair.round(
      Seq(entry(rng, 0x02) +: Vector.fill(StateTrie.ParFrom - 1)(entry(rng, 0x80))),
      "one key under 02, the rest under 80"
    )
    pair.round(
      Seq(Vector.fill(StateTrie.ParFrom - 2)(entry(rng, 0x02)) ++ Vector.fill(2)(entry(rng, 0x40))),
      "most keys under 02, two under 40"
    )
  }

  it should "build new keys under an empty slot at the split depth" in {
    val rng = new Random(12)
    val pair = new Pair
    pair.round(Seq(Vector(0x00, 0x04, 0x10, 0x14).map(entry(rng, _))), "seed 00, 04, 10, 14")
    pair.round(
      Seq(entry(rng, 0x02) +: Vector.fill(StateTrie.ParFrom - 1)(entry(rng, 0x08 + rng.nextInt(8)))),
      "one key under 02, the rest under 08 to 0f"
    )
  }

  it should "build new keys beside a leaf above the split depth" in {
    val rng = new Random(13)
    val pair = new Pair
    pair.round(Seq(Vector(0x00, 0x04, 0x98).map(entry(rng, _))), "seed 00, 04, 98")
    pair.round(
      Seq(entry(rng, 0x02) +: Vector.fill(StateTrie.ParFrom - 1)(entry(rng, 0x90))),
      "one key under 02, the rest under 90"
    )
  }

  it should "give a state-shaped trie, where a new service writes many items in one block, the same trie" in {
    val rng = new Random(14)
    val pair = new Pair
    val components = (1 to 16).toVector.map(i => JamBytes.wrap(Array.tabulate[Byte](31)(j => if j == 0 then i.toByte else 0)))
    val accounts = Vector.fill(3)(keyUnder(rng, 0xff))
    def value(): Option[JamBytes] = Some(JamBytes(rng.nextBytes(40 + rng.nextInt(200))))
    pair.round(
      Seq((components ++ accounts).map((_, value())) ++ Vector.fill(300)(entry(rng, 0x00))),
      "genesis"
    )
    pair.round(
      Seq(
        Vector(3, 6, 11, 13).map(i => (components(i - 1), value())) ++
          Vector((accounts.head, value())) ++
          Vector.fill(StateTrie.ParFrom + 100)(entry(rng, 0x80))
      ),
      "a block with a new service's items"
    )
  }

  it should "leave sparse-topped tries the trie a piecewise update leaves" in {
    for seed <- 21 to 40 do
      val rng = new Random(seed)
      val pair = new Pair
      def firsts(n: Int): Vector[Int] = Vector.fill(n)(rng.nextInt(256))
      val seeded = firsts(1 + rng.nextInt(6))
      pair.round(Seq(Vector.fill(2 + rng.nextInt(40))(entry(rng, seeded(rng.nextInt(seeded.size))))), s"seed $seed seeded")
      for round <- 1 to 3 do
        val crowded = firsts(1 + rng.nextInt(3))
        val batch =
          Vector.fill(StateTrie.ParFrom + rng.nextInt(500))(entry(rng, crowded(rng.nextInt(crowded.size)))) ++
            Vector.fill(rng.nextInt(6))(entry(rng, rng.nextInt(256))) ++
            rng.shuffle(pair.model.keys.toVector.sortBy(_.toHex)).take(rng.nextInt(20)).map(k => (k, None))
        pair.round(Seq(rng.shuffle(batch)), s"seed $seed round $round")
  }
