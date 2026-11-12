package io.forge.jam.db

import java.nio.file.Files

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{BackendOp, InMemoryTrieBackend, StateTrie, TrieNode}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RocksDbTrieBackendSpec extends AnyFunSuite with Matchers:

  private def withBackend[A](f: RocksDbTrieBackend => A): A =
    val dir = Files.createTempDirectory("jam-rocksdb-test")
    val backend = RocksDbTrieBackend.open(dir)
    try f(backend)
    finally
      backend.close()
      deleteRecursively(dir)

  private def deleteRecursively(dir: java.nio.file.Path): Unit =
    Files
      .walk(dir)
      .sorted(java.util.Comparator.reverseOrder())
      .forEach(p => Files.deleteIfExists(p))

  private def key31(i: Int): JamBytes =
    val bytes = new Array[Byte](31)
    bytes(0) = (i & 0xff).toByte
    bytes(1) = ((i >> 8) & 0xff).toByte
    JamBytes(bytes)

  test("gc reclaims dead nodes and their regular-leaf values, keeps live ones") {
    withBackend { backend =>
      val liveValue = JamBytes(Array.fill[Byte](40)(1))
      val deadValue = JamBytes(Array.fill[Byte](40)(2))
      val liveNode = TrieNode.leaf(key31(1), liveValue)
      val deadNode = TrieNode.leaf(key31(2), deadValue)

      backend.batchUpdate(
        Seq(
          BackendOp.WriteNode(liveNode),
          BackendOp.writeRawValue(liveValue),
          BackendOp.NodeRefDelta(liveNode.hash, 1),
          BackendOp.RawValueRefDelta(Hashing.blake2b256(liveValue), 1),
          BackendOp.WriteNode(deadNode),
          BackendOp.writeRawValue(deadValue),
          BackendOp.NodeRefDelta(deadNode.hash, 1),
          BackendOp.RawValueRefDelta(Hashing.blake2b256(deadValue), 1)
        )
      )
      backend.batchUpdate(Seq(BackendOp.NodeRefDelta(deadNode.hash, -1)))
      backend.gc()

      backend.readNode(liveNode.hash) shouldBe Some(liveNode)
      backend.readNode(deadNode.hash) shouldBe None
      backend.readRawValue(Hashing.blake2b256(deadValue)) shouldBe None
      backend.readRawValue(Hashing.blake2b256(liveValue)) shouldBe Some(liveValue)
    }
  }

  test("a resurrected node survives gc") {
    withBackend { backend =>
      val value = JamBytes(Array.fill[Byte](40)(3))
      val node = TrieNode.leaf(key31(3), value)
      backend.batchUpdate(
        Seq(
          BackendOp.WriteNode(node),
          BackendOp.writeRawValue(value),
          BackendOp.NodeRefDelta(node.hash, 1),
          BackendOp.RawValueRefDelta(Hashing.blake2b256(value), 1)
        )
      )
      // Dip to zero, then resurrect before gc.
      backend.batchUpdate(Seq(BackendOp.NodeRefDelta(node.hash, -1)))
      backend.batchUpdate(Seq(BackendOp.NodeRefDelta(node.hash, 1)))
      backend.gc()
      backend.readNode(node.hash) shouldBe Some(node)
    }
  }

  test("StateTrie over RocksDB keeps the in-memory backend's nodes, values and counts across save/gc cycles") {
    withBackend { rocks =>
      val mem = new InMemoryTrieBackend
      val rng = new scala.util.Random(9)
      val written = scala.collection.mutable.LinkedHashSet[Hash]()
      val values = scala.collection.mutable.LinkedHashSet[Hash]()
      val live = scala.collection.mutable.HashMap[JamBytes, JamBytes]()
      val gone = scala.collection.mutable.ArrayBuffer[(JamBytes, JamBytes)]()
      var root = Hash.zero
      for round <- 1 to 30 do
        val updates = scala.collection.mutable.ArrayBuffer[(JamBytes, Option[JamBytes])]()
        for _ <- 0 until 20 do
          rng.nextInt(4) match
            case 0 if live.nonEmpty =>
              val k = live.keys.toVector(rng.nextInt(live.size))
              gone += ((k, live(k)))
              live.remove(k)
              updates += ((k, None))
            case 1 if gone.nonEmpty =>
              // A key deleted (and maybe collected) in an earlier round comes back with the same value.
              val (k, v) = gone(rng.nextInt(gone.size))
              live(k) = v
              updates += ((k, Some(v)))
            case _ =>
              val k = key31(rng.nextInt(400))
              val v = JamBytes(rng.nextBytes(20 + rng.nextInt(40)))
              live(k) = v
              updates += ((k, Some(v)))
        val tr = StateTrie.at(rocks, root)
        val tm = StateTrie.at(mem, root)
        tr.update(updates.toSeq)
        tm.update(updates.toSeq)
        tr.rootHash shouldBe tm.rootHash
        tm.save()
        tr.save()
        root = tr.rootHash
        // Every leaf and branch the in-memory backend holds after this round.
        val stack = scala.collection.mutable.Stack(root)
        while stack.nonEmpty do
          mem.readNode(stack.pop()).foreach { n =>
            written += n.hash
            if n.nodeType == io.forge.jam.core.trie.TrieNodeType.Branch then
              stack.push(Hash.wrap(n.left.toArray))
              stack.push(Hash.wrap(n.right.toArray))
            else if n.nodeType == io.forge.jam.core.trie.TrieNodeType.RegularLeaf then
              values += Hash.wrap(n.right.toArray)
          }
        if round % 3 == 0 then
          rocks.gc()
          mem.gc()
        written.foreach { h =>
          withClue(s"round $round node $h: ") {
            rocks.readNode(h).isDefined shouldBe mem.readNode(h).isDefined
            rocks.nodeRefcount(h) shouldBe mem.nodeRefcount(h)
          }
        }
        values.foreach { h =>
          withClue(s"round $round value $h: ") {
            rocks.readRawValue(h).isDefined shouldBe mem.readRawValue(h).isDefined
            rocks.valueRefcount(h) shouldBe mem.valueRefcount(h)
          }
        }
        live.foreach((k, v) => tr.read(k) shouldBe Some(v))
        gone.foreach((k, _) => if !live.contains(k) then tr.read(k) shouldBe None)
    }
  }

  test("refcount deltas add up without being read first, and persist across reopen") {
    val dir = Files.createTempDirectory("jam-rocksdb-merge")
    try
      val h = Hashing.blake2b256(JamBytes(Array[Byte](1)))
      val b1 = RocksDbTrieBackend.open(dir)
      b1.batchUpdate(Seq(BackendOp.NodeRefDelta(h, 3), BackendOp.RawValueRefDelta(h, 2)))
      b1.batchUpdate(Seq(BackendOp.NodeRefDelta(h, -1)))
      b1.batchUpdate(Seq(BackendOp.NodeRefDelta(h, -4)))
      b1.nodeRefcount(h) shouldBe -2L
      b1.valueRefcount(h) shouldBe 2L
      b1.close()
      val b2 = RocksDbTrieBackend.open(dir)
      try
        b2.nodeRefcount(h) shouldBe -2L
        b2.batchUpdate(Seq(BackendOp.NodeRefDelta(h, 2)))
        b2.nodeRefcount(h) shouldBe 0L
      finally b2.close()
    finally deleteRecursively(dir)
  }

  test("a node gc deletes is no longer served from the node cache") {
    withBackend { backend =>
      val node = TrieNode.branch(Hashing.blake2b256(JamBytes(Array[Byte](5))), Hash.zero)
      backend.batchUpdate(Seq(BackendOp.WriteNode(node), BackendOp.NodeRefDelta(node.hash, 1)))
      backend.readNode(node.hash) shouldBe Some(node) // now cached
      backend.batchUpdate(Seq(BackendOp.NodeRefDelta(node.hash, -1)))
      backend.gc()
      backend.readNode(node.hash) shouldBe None
    }
  }

  test("state persists across close/reopen; repeated open/close does not leak locks or handles") {
    val dir = Files.createTempDirectory("jam-rocksdb-leakshape")
    try
      val value = JamBytes(Array.fill[Byte](48)(0x5a))
      val node = TrieNode.leaf(key31(11), value)

      // Seed data so each reopen has something to read back.
      val seed = RocksDbTrieBackend.open(dir)
      seed.batchUpdate(
        Seq(
          BackendOp.WriteNode(node),
          BackendOp.writeRawValue(value),
          BackendOp.NodeRefDelta(node.hash, 1),
          BackendOp.RawValueRefDelta(Hashing.blake2b256(value), 1)
        )
      )
      seed.close()

      // Open/close repeatedly; a leaked lock throws here.
      (0 until 5).foreach { _ =>
        val b = RocksDbTrieBackend.open(dir)
        b.readNode(node.hash) shouldBe Some(node)
        b.readRawValue(Hashing.blake2b256(value)) shouldBe Some(value)
        b.nodeRefcount(node.hash) shouldBe 1L
        b.close()
      }

      // A fresh open on the same dir still succeeds and sees the data.
      val reopened = RocksDbTrieBackend.open(dir)
      try
        reopened.readNode(node.hash) shouldBe Some(node)
        reopened.readRawValue(Hashing.blake2b256(value)) shouldBe Some(value)
      finally reopened.close()
    finally deleteRecursively(dir)
  }

  /**
   * Saves `rounds` blocks of `perRound` overwritten keys each on `backend`, never calling gc. Returns every root saved
   * with its key-values, plus the dead-candidate count after each save.
   */
  private def saveRounds(
      backend: RocksDbTrieBackend,
      rounds: Int,
      perRound: Int
  ): (Vector[(Hash, Map[JamBytes, JamBytes])], Vector[Int]) =
    val rng = new scala.util.Random(17)
    var state = Map.empty[JamBytes, JamBytes]
    var root = Hash.zero
    val roots = Vector.newBuilder[(Hash, Map[JamBytes, JamBytes])]
    val pending = Vector.newBuilder[Int]
    for _ <- 1 to rounds do
      val updates = (0 until perRound).map(_ => key31(rng.nextInt(256)) -> JamBytes(rng.nextBytes(40)))
      val trie = StateTrie.at(backend, root)
      trie.update(updates.map((k, v) => k -> Some(v)))
      trie.save()
      root = trie.rootHash
      state = state ++ updates
      roots += ((root, state))
      pending += backend.pendingDeadCandidates
    (roots.result(), pending.result())

  test("with garbage tracking on and no gc, dead candidates grow with every save (what the node used to do)") {
    withBackend { backend =>
      val (_, pending) = saveRounds(backend, rounds = 50, perRound = 20)
      // Every save replaces the nodes on its touched paths, so the in-heap set only grows while gc does not run.
      pending.sliding(2).foreach { case Seq(a, b) => b should be >= a }
      pending.last should be > 20 * pending.head
      backend.gc()
      backend.pendingDeadCandidates shouldBe 0
    }
  }

  test("with garbage tracking off, no dead candidates are held, every earlier root stays readable, and gc refuses to run") {
    val dir = Files.createTempDirectory("jam-rocksdb-notrack-roots")
    try
      val backend = RocksDbTrieBackend.open(dir, trackGarbage = false)
      try
        backend.tracksGarbage shouldBe false
        val (roots, pending) = saveRounds(backend, rounds = 20, perRound = 15)
        pending.foreach(_ shouldBe 0)
        an[UnsupportedOperationException] should be thrownBy backend.gc()
        roots.foreach { (root, state) =>
          val trie = StateTrie.at(backend, root)
          state.foreach((k, v) => withClue(s"root $root key $k: ")(trie.read(k) shouldBe Some(v)))
          trie.getKeyValues(JamBytes(Array.emptyByteArray), 0).toMap shouldBe state
        }
        // Reorg shape: build a sibling of an old root and still read both branches.
        val (oldRoot, oldState) = roots(10)
        val side = StateTrie.at(backend, oldRoot)
        val sideUpdates = (0 until 15).map(i => key31(300 + i) -> JamBytes(Array.fill[Byte](40)(i.toByte)))
        side.update(sideUpdates.map((k, v) => k -> Some(v)))
        side.save()
        StateTrie.at(backend, side.rootHash).getKeyValues(JamBytes(Array.emptyByteArray), 0).toMap shouldBe
          oldState ++ sideUpdates
        val (lastRoot, lastState) = roots.last
        StateTrie.at(backend, lastRoot).getKeyValues(JamBytes(Array.emptyByteArray), 0).toMap shouldBe lastState
      finally backend.close()
    finally deleteRecursively(dir)
  }

  test("garbage tracking off writes the same nodes and refcounts as tracking on") {
    val dirOff = Files.createTempDirectory("jam-rocksdb-off")
    val dirOn = Files.createTempDirectory("jam-rocksdb-on")
    try
      val off = RocksDbTrieBackend.open(dirOff, trackGarbage = false)
      val on = RocksDbTrieBackend.open(dirOn)
      try
        val (rootsOff, _) = saveRounds(off, rounds = 15, perRound = 10)
        val (rootsOn, _) = saveRounds(on, rounds = 15, perRound = 10)
        rootsOff.map(_._1) shouldBe rootsOn.map(_._1)
        val stack = scala.collection.mutable.Stack.from(rootsOn.map(_._1))
        val seen = scala.collection.mutable.HashSet[Hash]()
        while stack.nonEmpty do
          val h = stack.pop()
          if seen.add(h) then
            val n = on.readNode(h)
            off.readNode(h) shouldBe n
            off.nodeRefcount(h) shouldBe on.nodeRefcount(h)
            n.filter(_.nodeType == io.forge.jam.core.trie.TrieNodeType.Branch).foreach { b =>
              stack.push(Hash.wrap(b.left.toArray))
              stack.push(Hash.wrap(b.right.toArray))
            }
      finally
        off.close()
        on.close()
    finally
      deleteRecursively(dirOff)
      deleteRecursively(dirOn)
  }

  test("clear empties everything") {
    withBackend { backend =>
      val value = JamBytes(Array.fill[Byte](40)(4))
      val node = TrieNode.leaf(key31(4), value)
      backend.batchUpdate(
        Seq(
          BackendOp.WriteNode(node),
          BackendOp.writeRawValue(value),
          BackendOp.NodeRefDelta(node.hash, 1)
        )
      )
      backend.clear()
      backend.readNode(node.hash) shouldBe None
      backend.readRawValue(Hashing.blake2b256(value)) shouldBe None
      backend.nodeRefcount(node.hash) shouldBe 0L
    }
  }

class BlockStoreSpec extends AnyFunSuite with Matchers:

  private def withStore[A](f: BlockStore => A): A =
    val dir = Files.createTempDirectory("jam-blockstore-test")
    val store = BlockStore.open(dir)
    try f(store)
    finally
      store.close()
      Files
        .walk(dir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(p => Files.deleteIfExists(p))

  private def hash(i: Int): Hash =
    Hash(Array.tabulate[Byte](32)(j => ((i + j) & 0xff).toByte))

  test("blocks, headers and children round-trip") {
    withStore { store =>
      val parent = hash(0)
      val child1 = hash(1)
      val child2 = hash(2)

      store.putBlock(child1, parent, Array[Byte](1, 1), Array[Byte](1, 1, 1))
      store.putBlock(child2, parent, Array[Byte](2, 2), Array[Byte](2, 2, 2))

      store.hasBlock(child1) shouldBe true
      store.getBlock(child1).get shouldBe Array[Byte](1, 1, 1)
      store.getHeader(child2).get shouldBe Array[Byte](2, 2)
      store.children(parent).toSet shouldBe Set(child1, child2)
      store.children(child1) shouldBe Nil

      // Idempotent child indexing.
      store.putBlock(child1, parent, Array[Byte](1, 1), Array[Byte](1, 1, 1))
      store.children(parent).length shouldBe 2
    }
  }

  test("head pointers persist across reopen") {
    val dir = Files.createTempDirectory("jam-blockstore-reopen")
    try
      val s1 = BlockStore.open(dir)
      s1.setHead(BlockStore.BestHead, hash(7))
      s1.setHead(BlockStore.FinalizedHead, hash(8))
      s1.close()

      val s2 = BlockStore.open(dir)
      try
        s2.getHead(BlockStore.BestHead) shouldBe Some(hash(7))
        s2.getHead(BlockStore.FinalizedHead) shouldBe Some(hash(8))
        s2.getHead("nonexistent") shouldBe None
      finally s2.close()
    finally
      Files
        .walk(dir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(p => Files.deleteIfExists(p))
  }

  test("deleteBlock unlinks the block from its parent's children") {
    withStore { store =>
      val parent = hash(0)
      store.putBlock(hash(1), parent, Array[Byte](1), Array[Byte](1))
      store.putBlock(hash(2), parent, Array[Byte](2), Array[Byte](2))
      store.deleteBlock(hash(1), parent)
      store.hasBlock(hash(1)) shouldBe false
      store.children(parent) shouldBe List(hash(2))
      store.deleteBlock(hash(2), parent)
      store.children(parent) shouldBe Nil
    }
  }

  test("concurrent putBlock of siblings keeps every child") {
    withStore { store =>
      val parent = hash(0)
      val threads = 8
      val perThread = 50
      val start = new java.util.concurrent.CountDownLatch(1)
      val workers = (0 until threads).map { t =>
        new Thread(() => {
          start.await()
          for i <- 0 until perThread do
            val h = Hash(Array.tabulate[Byte](32)(j => if j == 0 then t.toByte else if j == 1 then i.toByte else 7))
            store.putBlock(h, parent, Array[Byte](1), Array[Byte](1))
        })
      }
      workers.foreach(_.start())
      start.countDown()
      workers.foreach(_.join())
      store.children(parent).toSet.size shouldBe threads * perThread
    }
  }
