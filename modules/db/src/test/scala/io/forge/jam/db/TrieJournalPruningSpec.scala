package io.forge.jam.db

import java.nio.file.{Files, Path}

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.StateTrie
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.util.Random

/** Pruning by journal deletes the nodes and values only old states reach, and never one a kept state reaches:
  * on random histories with forks, with keys set back to earlier values and values shared between keys.
  */
class TrieJournalPruningSpec extends AnyFunSuite with Matchers:

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  private def key(i: Int): JamBytes = JamBytes(Hashing.blake2b256(Array[Byte](1, (i >> 8).toByte, i.toByte)).bytes.toArray.take(31))
  private def label(name: String): Hash = Hashing.blake2b256(name.getBytes("UTF-8"))

  /** A few small (embedded) and large (raw) values, drawn again and again so keys often share one or return to one. */
  private def value(rng: Random): JamBytes =
    val id = rng.nextInt(12)
    val length = if id % 2 == 0 then 8 else 80
    JamBytes(Array.fill(length)(id.toByte))

  private type Model = Map[JamBytes, JamBytes]

  private final case class State(label: Hash, height: Long, root: Hash, model: Model)

  /** Save `model`'s difference from `parent` as block `name` at `height`. */
  private def step(backend: RocksDbTrieBackend, parent: State, name: String, height: Long, rng: Random, keys: Int): State =
    val updates = (1 to 1 + rng.nextInt(12)).map { _ =>
      val k = key(rng.nextInt(keys))
      k -> (if rng.nextInt(5) == 0 then None else Some(value(rng)))
    }
    val model = updates.foldLeft(parent.model) {
      case (m, (k, Some(v))) => m.updated(k, v)
      case (m, (k, None))    => m.removed(k)
    }
    val l = label(name)
    val root = backend.withJournal(l, height) {
      val trie = StateTrie.at(backend, parent.root)
      trie.update(updates)
      trie.save()
      trie.rootHash
    }
    State(l, height, root, model)

  private def check(backend: RocksDbTrieBackend, s: State, keys: Int, what: String): Unit =
    val trie = StateTrie.at(backend, s.root)
    (0 until keys).foreach { i =>
      val k = key(i)
      withClue(s"$what, height ${s.height}, key $i: ") { trie.read(k) shouldBe s.model.get(k) }
    }

  test("pruned histories keep every state at or above the floor intact, forks included") {
    val rng = new Random(7)
    for round <- 1 to 4 do
      val dir = Files.createTempDirectory("jam-trie-journal")
      val backend = RocksDbTrieBackend.open(dir, trackGarbage = false, new RocksDbMemory(16L << 20, 4L << 20))
      try
        val keys = 20 + rng.nextInt(60)
        val genesisModel: Model = (0 until keys / 2).map(i => key(i) -> value(rng)).toMap
        val genesisRoot = backend.withJournal(label("genesis"), 0L) {
          val t = StateTrie.at(backend, Hash.zero)
          t.update(genesisModel.toSeq.map((k, v) => k -> Some(v)))
          t.save()
          t.rootHash
        }
        val chain = mutable.ArrayBuffer(State(label("genesis"), 0L, genesisRoot, genesisModel))
        // forksAt(h): side-branch states whose first block is a child of the canonical block at height h.
        val forksAt = mutable.HashMap.empty[Long, List[State]]
        val length = 40
        for h <- 1 to length do
          chain += step(backend, chain.last, s"r$round-c$h", h.toLong, rng, keys)
          // Now and then a side branch of one to three blocks off a recent canonical block.
          if rng.nextInt(4) == 0 then
            val base = chain(math.max(0, chain.size - 1 - rng.nextInt(3)))
            var tip = base
            val branch = (1 to 1 + rng.nextInt(3)).map { d =>
              tip = step(backend, tip, s"r$round-f$h-$d", base.height + d, rng, keys)
              tip
            }.toList
            forksAt(base.height) = forksAt.getOrElse(base.height, Nil) ++ branch
          // The floor follows `window` blocks behind: the state below it goes, with the branches forking off it.
          val window = 5
          val floor = h - window
          if floor >= 1 then
            forksAt.remove(floor.toLong - 1).foreach(_.foreach(s => backend.dropJournal(s.label)))
            backend.pruneJournal(chain(floor).label, floor.toLong)
            // Everything kept reads exactly as it was written.
            chain.drop(floor).foreach(check(backend, _, keys, s"round $round canonical"))
            forksAt.foreach((base, states) => if base >= floor then states.foreach(check(backend, _, keys, s"round $round fork")))
      finally
        backend.close()
        cleanup(dir)
  }

  test("without forks, pruning to the newest state leaves exactly that state's nodes and values") {
    val rng = new Random(11)
    val dir = Files.createTempDirectory("jam-trie-journal-exact")
    val dirFresh = Files.createTempDirectory("jam-trie-journal-fresh")
    val memory = new RocksDbMemory(16L << 20, 4L << 20)
    val backend = RocksDbTrieBackend.open(dir, trackGarbage = false, memory)
    val fresh = RocksDbTrieBackend.open(dirFresh, trackGarbage = false, memory)
    try
      val keys = 200
      var state = State(label("g"), 0L, Hash.zero, Map.empty)
      state = step(backend, state, "g", 0L, rng, keys)
      val before = mutable.ArrayBuffer(state)
      for h <- 1 to 100 do
        state = step(backend, state, s"b$h", h.toLong, rng, keys)
        before += state
      val (nodesUnpruned, valuesUnpruned) = backend.storedCounts
      var deletedNodes = 0
      for h <- 1 to 100 do deletedNodes += backend.pruneJournal(before(h).label, h.toLong)._1
      check(backend, state, keys, "newest")

      // The same key set written once into an empty store: its trie is the newest state's, node for node.
      val t = StateTrie.at(fresh, Hash.zero)
      t.update(state.model.toSeq.map((k, v) => k -> Some(v)))
      t.save()
      t.rootHash shouldBe state.root
      val (nodes, values) = backend.storedCounts
      val (nodesFresh, valuesFresh) = fresh.storedCounts
      nodes shouldBe nodesFresh
      values shouldBe valuesFresh
      deletedNodes.toLong shouldBe nodesUnpruned - nodes
      nodesUnpruned should be > 3 * nodes
      valuesUnpruned should be >= values
    finally
      backend.close()
      fresh.close()
      cleanup(dir)
      cleanup(dirFresh)
  }

  test("a save outside a journal is never pruned, and a block without a journal prunes nothing") {
    val dir = Files.createTempDirectory("jam-trie-journal-none")
    val backend = RocksDbTrieBackend.open(dir, trackGarbage = false, new RocksDbMemory(16L << 20, 4L << 20))
    try
      val rng = new Random(3)
      // No journal for the first two saves.
      val t0 = StateTrie.at(backend, Hash.zero)
      t0.update((0 until 30).map(i => key(i) -> Some(value(rng))))
      t0.save()
      val root0 = t0.rootHash
      val model0 = (0 until 30).map(i => key(i) -> t0.read(key(i)).get).toMap
      // A journalled block replaces some of those nodes.
      val s1 = step(backend, State(label("x0"), 0L, root0, model0), "x1", 1L, rng, 30)
      backend.journalOf(s1.label) should not be empty
      val (nodesBefore, _) = backend.storedCounts
      backend.pruneJournal(s1.label, 1L) shouldBe ((0, 0)) // every replaced node was added outside a journal
      backend.storedCounts._1 shouldBe nodesBefore
      backend.journalOf(s1.label) shouldBe Nil
      backend.pruneJournal(label("never-imported"), 5L) shouldBe ((0, 0))
      check(backend, State(label("x0"), 0L, root0, model0), 30, "unjournalled")
    finally
      backend.close()
      cleanup(dir)
  }

  test("only a node that a later save brings back gets a birth record, and that record keeps it through pruning") {
    val dir = Files.createTempDirectory("jam-trie-journal-reborn")
    val backend = RocksDbTrieBackend.open(dir, trackGarbage = false, new RocksDbMemory(16L << 20, 4L << 20))
    try
      def save(parent: Hash, name: String, height: Long, updates: Seq[(JamBytes, Option[JamBytes])]): Hash =
        backend.withJournal(label(name), height) {
          val trie = StateTrie.at(backend, parent)
          trie.update(updates)
          trie.save()
          trie.rootHash
        }
      val (first, second) = (JamBytes(Array.fill[Byte](80)(1)), JamBytes(Array.fill[Byte](80)(2)))
      val g = save(Hash.zero, "g", 1L, (0 until 40).map(i => key(i) -> Some(first)))
      val b1 = save(g, "b1", 2L, Seq(key(7) -> Some(second)))
      val replaced = backend.journalOf(label("b1"))
      replaced should not be empty
      replaced.foreach(n => backend.nodeBornAt(n) shouldBe 0L)
      backend.nodeBornAt(b1) shouldBe 0L

      val b2 = save(b1, "b2", 3L, Seq(key(7) -> Some(first)))
      b2 shouldBe g
      replaced.foreach(n => withClue(s"node ${n.toHex.take(12)}: ") { backend.nodeBornAt(n) shouldBe 3L })

      backend.pruneJournal(label("b1"), 2L) shouldBe ((0, 0))
      val kept = StateTrie.at(backend, b2)
      (0 until 40).foreach(i => kept.read(key(i)) shouldBe Some(first))
      val (nodes, values) = backend.pruneJournal(label("b2"), 3L)
      nodes should be > 0
      values shouldBe 1 // `second` is no leaf's value any more
      (0 until 40).foreach(i => StateTrie.at(backend, b2).read(key(i)) shouldBe Some(first))
      backend.readNode(b1) shouldBe None
    finally
      backend.close()
      cleanup(dir)
  }

  test("pruning on another thread while blocks are saved loses nothing a kept state reaches") {
    val rng = new Random(11)
    val dir = Files.createTempDirectory("jam-trie-journal")
    val backend = RocksDbTrieBackend.open(dir, trackGarbage = false, new RocksDbMemory(16L << 20, 4L << 20))
    val pruner = java.util.concurrent.Executors.newSingleThreadExecutor()
    try
      val keys = 24
      val genesisModel: Model = (0 until keys).map(i => key(i) -> value(rng)).toMap
      val genesisRoot = backend.withJournal(label("genesis"), 0L) {
        val t = StateTrie.at(backend, Hash.zero)
        t.update(genesisModel.toSeq.map((k, v) => k -> Some(v)))
        t.save()
        t.rootHash
      }
      val chain = mutable.ArrayBuffer(State(label("genesis"), 0L, genesisRoot, genesisModel))
      val window = 3
      val pending = mutable.ArrayBuffer[java.util.concurrent.Future[?]]()
      for h <- 1 to 1500 do
        chain += step(backend, chain.last, s"c$h", h.toLong, rng, keys)
        if h > window then
          val floor = chain(h - window)
          backend.schedulePrune(floor.label, floor.height)
          pending += pruner.submit((() => backend.pruneJournal(floor.label, floor.height): Unit): Runnable)
        if h % 100 == 0 then
          (h - window to h).foreach(i => check(backend, chain(i), keys, s"while pruning, at block $h"))
      pending.foreach(_.get())
      backend.scheduledPrunes shouldBe Nil
      (chain.length - 1 - window until chain.length).foreach(i => check(backend, chain(i), keys, "after pruning"))
      val (nodes, _) = backend.storedCounts
      nodes should be < 400L
    finally
      pruner.shutdownNow()
      backend.close()
      cleanup(dir)
  }

  test("a prune that was asked for is remembered across a reopen until it is done") {
    val rng = new Random(3)
    val dir = Files.createTempDirectory("jam-trie-journal")
    var backend = RocksDbTrieBackend.open(dir, trackGarbage = false, new RocksDbMemory(16L << 20, 4L << 20))
    try
      val keys = 30
      val genesis = State(label("genesis"), 0L, Hash.zero, Map.empty)
      val a = step(backend, genesis, "a", 1L, rng, keys)
      val b = step(backend, a, "b", 2L, rng, keys)
      val c = step(backend, b, "c", 3L, rng, keys)
      backend.schedulePrune(c.label, 3L)
      backend.schedulePrune(b.label, 2L)
      backend.close()
      backend = RocksDbTrieBackend.open(dir, trackGarbage = false, new RocksDbMemory(16L << 20, 4L << 20))
      backend.scheduledPrunes shouldBe List(b.label -> 2L, c.label -> 3L)
      backend.scheduledPrunes.foreach((l, h) => backend.pruneJournal(l, h))
      backend.scheduledPrunes shouldBe Nil
      check(backend, c, keys, "after the remembered prunes")
    finally
      backend.close()
      cleanup(dir)
  }

  test("a large save that brings back thousands of nodes records every one of them") {
    val dir = Files.createTempDirectory("jam-trie-journal")
    val backend = RocksDbTrieBackend.open(dir, trackGarbage = false, new RocksDbMemory(16L << 20, 4L << 20))
    try
      val keys = 2000
      def save(name: String, height: Long, parent: Hash, fill: Byte): Hash =
        backend.withJournal(label(name), height) {
          val t = StateTrie.at(backend, parent)
          t.update((0 until keys).map(i => key(i) -> Some(JamBytes(Array.fill(8)(fill)))))
          t.save()
          t.rootHash
        }
      val first = save("one", 1L, Hash.zero, 1)
      val second = save("two", 2L, first, 2)
      val third = save("three", 3L, second, 1)
      third shouldBe first
      backend.journalOf(label("two")).size should be > 2 * keys
      backend.journalOf(label("two")).foreach(h => backend.nodeBornAt(h) shouldBe 3L)
      backend.pruneJournal(label("two"), 2L) shouldBe ((0, 0))
      val t = StateTrie.at(backend, third)
      (0 until keys).foreach(i => t.read(key(i)) shouldBe Some(JamBytes(Array.fill(8)(1.toByte))))
    finally
      backend.close()
      cleanup(dir)
  }

  test("a block saved twice is pruned as if saved once: a value another key still uses stays") {
    val dir = Files.createTempDirectory("jam-trie-journal")
    val backend = RocksDbTrieBackend.open(dir, trackGarbage = false, new RocksDbMemory(16L << 20, 4L << 20))
    try
      val shared = JamBytes(Array.fill[Byte](80)(7)) // a raw value (over 32 bytes), held by two keys
      val other = JamBytes(Array.fill[Byte](80)(9))
      def save(name: String, height: Long, parent: Hash, updates: Seq[(JamBytes, Option[JamBytes])]): Hash =
        backend.withJournal(label(name), height) {
          val t = StateTrie.at(backend, parent)
          t.update(updates)
          t.save()
          t.rootHash
        }
      val a = save("a", 1L, Hash.zero, Seq(key(1) -> Some(shared)))
      val e = save("e", 2L, a, Seq(key(3) -> Some(shared)))
      val b = save("b", 3L, e, Seq(key(1) -> Some(other)))
      save("b", 3L, e, Seq(key(1) -> Some(other))) shouldBe b
      backend.pruneJournal(label("b"), 3L)
      val kept = StateTrie.at(backend, b)
      kept.read(key(1)) shouldBe Some(other)
      kept.read(key(3)) shouldBe Some(shared)
    finally
      backend.close()
      cleanup(dir)
  }

  test("saving a block a second time leaves its journal as the first save wrote it") {
    val dir = Files.createTempDirectory("jam-trie-journal")
    val backend = RocksDbTrieBackend.open(dir, trackGarbage = false, new RocksDbMemory(16L << 20, 4L << 20))
    try
      val rng = new Random(5)
      val keys = 60
      val genesis = step(backend, State(label("g"), 0L, Hash.zero, Map.empty), "g", 0L, new Random(1), keys)
      val a = step(backend, genesis, "a", 1L, rng, keys)
      val seed = rng.nextLong()
      val b = step(backend, a, "b", 2L, new Random(seed), keys)
      val first = backend.journalOf(b.label)
      first should not be empty
      first.distinct shouldBe first
      val c = step(backend, b, "c", 3L, rng, keys)
      step(backend, a, "b", 2L, new Random(seed), keys).root shouldBe b.root
      backend.journalOf(b.label) shouldBe first
      step(backend, a, "b", 2L, new Random(seed), keys).root shouldBe b.root
      backend.journalOf(b.label) shouldBe first
      backend.pruneJournal(b.label, 2L)
      backend.pruneJournal(c.label, 3L)
      check(backend, c, keys, "after saving b three times")
    finally
      backend.close()
      cleanup(dir)
  }

  test("pruned histories keep every state intact when some blocks are saved twice, forks included") {
    val rng = new Random(13)
    for round <- 1 to 4 do
      val dir = Files.createTempDirectory("jam-trie-journal")
      val backend = RocksDbTrieBackend.open(dir, trackGarbage = false, new RocksDbMemory(16L << 20, 4L << 20))
      try
        val keys = 20 + rng.nextInt(40)
        val genesisModel: Model = (0 until keys / 2).map(i => key(i) -> value(rng)).toMap
        val genesisRoot = backend.withJournal(label("genesis"), 0L) {
          val t = StateTrie.at(backend, Hash.zero)
          t.update(genesisModel.toSeq.map((k, v) => k -> Some(v)))
          t.save()
          t.rootHash
        }
        final case class Saved(parent: State, name: String, height: Long, seed: Long, state: State)
        val saves = mutable.ArrayBuffer[Saved]()
        var replays = 0
        def save(parent: State, name: String, height: Long): State =
          val seed = rng.nextLong()
          val s = step(backend, parent, name, height, new Random(seed), keys)
          saves += Saved(parent, name, height, seed, s)
          s
        val chain = mutable.ArrayBuffer(State(label("genesis"), 0L, genesisRoot, genesisModel))
        val forksAt = mutable.HashMap.empty[Long, List[State]]
        val length = 40
        val window = 5
        for h <- 1 to length do
          chain += save(chain.last, s"r$round-c$h", h.toLong)
          if rng.nextInt(4) == 0 then
            val base = chain(math.max(0, chain.size - 1 - rng.nextInt(3)))
            var tip = base
            val branch = (1 to 1 + rng.nextInt(3)).map { d =>
              tip = save(tip, s"r$round-f$h-$d", base.height + d)
              tip
            }.toList
            forksAt(base.height) = forksAt.getOrElse(base.height, Nil) ++ branch
          val recent = saves.filter(_.parent.height >= h - window + 2)
          if recent.nonEmpty && rng.nextInt(2) == 0 then
            val again = recent(rng.nextInt(recent.size))
            step(backend, again.parent, again.name, again.height, new Random(again.seed), keys).root shouldBe again.state.root
            replays += 1
          val floor = h - window
          if floor >= 1 then
            forksAt.remove(floor.toLong - 1).foreach(_.foreach(s => backend.dropJournal(s.label)))
            backend.pruneJournal(chain(floor).label, floor.toLong)
            chain.drop(floor).foreach(check(backend, _, keys, s"round $round canonical"))
            forksAt.foreach((base, states) => if base >= floor then states.foreach(check(backend, _, keys, s"round $round fork")))
        replays should be > 5
      finally
        backend.close()
        cleanup(dir)
  }
