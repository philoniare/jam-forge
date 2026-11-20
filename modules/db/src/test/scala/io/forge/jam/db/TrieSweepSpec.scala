package io.forge.jam.db

import java.nio.file.{Files, Path}

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{StateTrie, TrieNodeType}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.util.Random

class TrieSweepSpec extends AnyFunSuite with Matchers:

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  private def key(i: Int): JamBytes = JamBytes(Hashing.blake2b256(Array[Byte](2, (i >> 8).toByte, i.toByte)).bytes.toArray.take(31))
  private def label(name: String): Hash = Hashing.blake2b256(name.getBytes("UTF-8"))

  private def value(rng: Random): JamBytes =
    val id = rng.nextInt(40)
    JamBytes(Array.fill(if id % 2 == 0 then 8 else 80)(id.toByte) ++ Array.fill(4)(rng.nextInt(256).toByte))

  private type Model = Map[JamBytes, JamBytes]
  private type Updates = Seq[(JamBytes, Option[JamBytes])]
  private final case class State(label: Hash, height: Long, root: Hash, model: Model)

  private def updates(rng: Random, keys: Int): Updates =
    (1 to 1 + rng.nextInt(12)).map(_ => key(rng.nextInt(keys)) -> (if rng.nextInt(5) == 0 then None else Some(value(rng))))

  private def save(backend: RocksDbTrieBackend, parent: State, name: String, height: Long, ups: Updates): State =
    val model = ups.foldLeft(parent.model) {
      case (m, (k, Some(v))) => m.updated(k, v)
      case (m, (k, None))    => m.removed(k)
    }
    def body: Hash =
      val trie = StateTrie.at(backend, parent.root)
      trie.update(ups)
      trie.save()
      trie.rootHash
    val l = if name == null then null else label(name)
    val root = if l == null then body else backend.withJournal(l, height)(body)
    State(l, height, root, model)

  private def pairs(backend: RocksDbTrieBackend, root: Hash): Vector[(JamBytes, JamBytes)] =
    StateTrie.at(backend, root).getKeyValues(JamBytes.empty, 0).toVector

  private def reachable(backend: RocksDbTrieBackend, roots: Seq[Hash]): (Set[Hash], Set[Hash]) =
    val nodes = mutable.HashSet.empty[Hash]
    val values = mutable.HashSet.empty[Hash]
    val stack = mutable.Stack.from(roots.filter(_ != Hash.zero))
    while stack.nonEmpty do
      val h = stack.pop()
      if nodes.add(h) then
        val n = backend.readNode(h).getOrElse(fail(s"node ${h.toHex} missing"))
        n.nodeType match
          case TrieNodeType.Branch =>
            Seq(n.left, n.right).map(b => Hash.wrap(b.toArray)).filter(_ != Hash.zero).foreach(stack.push)
          case TrieNodeType.RegularLeaf => values += Hash.wrap(n.right.toArray)
          case _                        => ()
    (nodes.toSet, values.toSet)

  private def open(dir: Path) = RocksDbTrieBackend.open(dir, trackGarbage = false, new RocksDbMemory(16L << 20, 4L << 20))

  private def genesis(backend: RocksDbTrieBackend, rng: Random, keys: Int): State =
    val model = (0 until keys / 2).map(i => key(i) -> value(rng))
    save(backend, State(null, 0L, Hash.zero, Map.empty), "genesis", 0L, model.map((k, v) => k -> Some(v)))

  test("garbage of abandoned branches goes, nodes they share with the kept chain stay, and every kept state reads as before") {
    val rng = new Random(5)
    val dir = Files.createTempDirectory("jam-sweep")
    val backend = open(dir)
    try
      val keys = 300
      val chain = mutable.ArrayBuffer(genesis(backend, rng, keys))
      val chainUpdates = mutable.ArrayBuffer[Updates](Nil)
      val sides = mutable.ArrayBuffer.empty[State]
      for h <- 1 to 40 do
        val ups = updates(rng, keys)
        chain += save(backend, chain.last, s"c$h", h.toLong, ups)
        chainUpdates += ups
        if h % 6 == 0 then
          var s = save(backend, chain(h - 1), s"s$h-1", h.toLong, ups)
          sides += s
          for j <- 2 to 3 do
            s = save(backend, s, s"s$h-$j", h.toLong + j - 1, updates(rng, keys))
            sides += s
      val floor = 25
      for h <- 1 to floor do backend.pruneJournal(chain(h).label, h.toLong)
      val kept = chain.drop(floor).toVector
      val before = kept.map(s => pairs(backend, s.root))
      val (nodesBefore, valuesBefore) = backend.storedCounts
      val (liveNodes, liveValues) = reachable(backend, kept.map(_.root))
      nodesBefore should be > liveNodes.size.toLong

      val keepJournal = kept.map(_.label).toSet ++ sides.filter(_.height > floor).map(_.label)
      val sweep = backend.beginSweep()
      val outcome =
        try sweep.run(kept.map(_.root), floor.toLong, keepJournal.contains, () => false)
        finally sweep.close()
      val stats = outcome match
        case RocksDbTrieBackend.SweepOutcome.Completed(st) => st
        case other                                         => fail(s"expected Completed, got $other")
      info(outcome.toString)

      kept.map(s => pairs(backend, s.root)) shouldBe before
      kept.foreach(s => s.model.foreach((k, v) => StateTrie.at(backend, s.root).read(k) shouldBe Some(v)))
      val (nodesAfter, valuesAfter) = backend.storedCounts
      nodesAfter shouldBe liveNodes.size.toLong + stats.nodesLeft
      stats.nodesLeft should be < stats.nodesDeleted / 20
      stats.nodesDeleted shouldBe nodesBefore - nodesAfter
      valuesAfter should be >= liveValues.size.toLong
      valuesAfter should be < valuesBefore
      kept.drop(1).foreach(s => backend.journalOf(s.label) should not be empty)
      sides.filter(_.height <= floor).foreach(s => backend.journalOf(s.label) shouldBe Nil)
      val again = backend.beginSweep()
      try
        again.run(kept.map(_.root), floor.toLong, keepJournal.contains, () => false) match
          case RocksDbTrieBackend.SweepOutcome.Completed(stats) => stats.nodesDeleted shouldBe 0L
          case other                                            => fail(s"second sweep: $other")
      finally again.close()
    finally
      backend.close()
      cleanup(dir)
  }

  for (journalled, what) <- List(true -> "in a journal above the floor", false -> "outside any journal") do
    test(s"a save $what that brings back unmarked nodes while the sweep runs keeps them") {
      val rng = new Random(9)
      val dir = Files.createTempDirectory("jam-sweep-readd")
      val backend = open(dir)
      try
        val keys = 200
        var chain = Vector(genesis(backend, rng, keys))
        for h <- 1 to 10 do chain :+= save(backend, chain.last, s"c$h", h.toLong, updates(rng, keys))
        val ups = updates(rng, keys) ++ updates(rng, keys)
        val side = save(backend, chain(5), if journalled then "side" else null, 6L, ups)
        val other = save(backend, chain(5), if journalled then "other" else null, 6L, updates(rng, keys))
        val floor = 5
        for h <- 1 to floor do backend.pruneJournal(chain(h).label, h.toLong)
        val kept = chain.drop(floor)
        val (sideNodes, _) = reachable(backend, Seq(side.root))
        val (keptNodes, _) = reachable(backend, kept.map(_.root))
        (sideNodes -- keptNodes) should not be empty

        val sweep = backend.beginSweep()
        val outcome =
          try
            sweep.run(
              kept.map(_.root),
              floor.toLong,
              _ => true,
              () => false,
              afterMark = () => save(backend, chain(5), if journalled then "again" else null, 11L, ups).root shouldBe side.root
            )
          finally sweep.close()
        outcome shouldBe a[RocksDbTrieBackend.SweepOutcome.Completed]
        side.model.foreach((k, v) => StateTrie.at(backend, side.root).read(k) shouldBe Some(v))
        pairs(backend, side.root).toMap shouldBe side.model
        backend.readNode(other.root) shouldBe None
        kept.foreach(s => pairs(backend, s.root).toMap shouldBe s.model)
      finally
        backend.close()
        cleanup(dir)
    }

  test("a kept state that is not whole stops the sweep before it deletes anything") {
    val rng = new Random(13)
    val dir = Files.createTempDirectory("jam-sweep-broken")
    val backend = open(dir)
    try
      var chain = Vector(genesis(backend, rng, 100))
      for h <- 1 to 4 do chain :+= save(backend, chain.last, s"c$h", h.toLong, updates(rng, 100))
      save(backend, chain(2), "side", 3L, updates(rng, 100))
      val root = backend.readNode(chain.last.root).get
      val lost = Hash.wrap(root.left.toArray)
      backend.loseNodesForTesting(lost)
      val counts = backend.storedCounts
      val sweep = backend.beginSweep()
      try
        sweep.run(Seq(chain.last.root), 4L, _ => true, () => false) match
          case RocksDbTrieBackend.SweepOutcome.Incomplete(hash, "node", _) => hash shouldBe lost
          case other                                                        => fail(s"expected Incomplete, got $other")
      finally sweep.close()
      backend.storedCounts shouldBe counts
    finally
      backend.close()
      cleanup(dir)
  }

  test("a sweep asked to stop deletes no more; one sweep at a time; saves made after it closed are not watched") {
    val rng = new Random(17)
    val dir = Files.createTempDirectory("jam-sweep-stop")
    val backend = open(dir)
    try
      var chain = Vector(genesis(backend, rng, 100))
      for h <- 1 to 4 do chain :+= save(backend, chain.last, s"c$h", h.toLong, updates(rng, 100))
      val counts = backend.storedCounts
      val sweep = backend.beginSweep()
      an[IllegalStateException] should be thrownBy backend.beginSweep()
      try sweep.run(Seq(chain.last.root), 4L, _ => true, () => true) shouldBe a[RocksDbTrieBackend.SweepOutcome.Stopped]
      finally sweep.close()
      backend.storedCounts shouldBe counts
      backend.beginSweep().close()
    finally
      backend.close()
      cleanup(dir)
  }

  test("a sweep stopped after deleting leaves leaves no journal that names them: the prune that follows keeps every value a kept leaf holds") {
    val dir = Files.createTempDirectory("jam-sweep-stopped")
    val backend = open(dir)
    try
      val n = 12_000
      def long(tag: Int, i: Int): JamBytes = JamBytes(Array.fill(80)(tag.toByte) ++ Array[Byte]((i >> 8).toByte, i.toByte))
      val genesis = save(
        backend, State(null, 0L, Hash.zero, Map.empty), "genesis", 0L, (0 until 2 * n).map(i => key(i) -> Some(long(7, i / 2)))
      )
      val b1 = save(backend, genesis, "b1", 1L, (0 until n).map(i => key(2 * i) -> Some(long(9, i))))
      backend.schedulePrune(b1.label, 1L)
      val before = pairs(backend, b1.root)
      val (nodesBefore, _) = backend.storedCounts

      val sweep = backend.beginSweep()
      try
        sweep.run(Seq(b1.root), 1L, _ => false, () => backend.storedCounts._1 < nodesBefore) shouldBe
          a[RocksDbTrieBackend.SweepOutcome.Stopped]
      finally sweep.close()
      backend.storedCounts._1 should be < nodesBefore

      backend.scheduledPrunes.foreach((block, height) => backend.pruneJournal(block, height))

      val (_, liveValues) = reachable(backend, Seq(b1.root))
      liveValues.count(v => backend.readRawValue(v).isEmpty) shouldBe 0
      pairs(backend, b1.root) shouldBe before

      val again = backend.beginSweep()
      try again.run(Seq(b1.root), 1L, _ => false, () => false) shouldBe a[RocksDbTrieBackend.SweepOutcome.Completed]
      finally again.close()
      val (liveNodes, _) = reachable(backend, Seq(b1.root))
      backend.storedCounts shouldBe (liveNodes.size.toLong, liveValues.size.toLong)
      pairs(backend, b1.root) shouldBe before
    finally
      backend.close()
      cleanup(dir)
  }

  test("a journal kept for a save that records births only (a download) stays empty however many saves it makes") {
    val rng = new Random(19)
    val dir = Files.createTempDirectory("jam-sweep-download")
    val backend = open(dir)
    try
      val target = label("target")
      val trie = StateTrie.empty(backend)
      for batch <- 0 until 20 do
        backend.withJournal(target, 7L, replaced = false) {
          trie.update((0 until 30).map(i => key(batch * 30 + i) -> Some(value(rng))))
          trie.save()
        }
      backend.journalOf(target) shouldBe Nil
      val other = label("journalled")
      val t2 = StateTrie.empty(backend)
      for batch <- 0 until 20 do
        backend.withJournal(other, 7L) {
          t2.update((0 until 30).map(i => key(1000 + batch * 30 + i) -> Some(value(rng))))
          t2.save()
        }
      backend.journalOf(other).size should be > 100
    finally
      backend.close()
      cleanup(dir)
  }
