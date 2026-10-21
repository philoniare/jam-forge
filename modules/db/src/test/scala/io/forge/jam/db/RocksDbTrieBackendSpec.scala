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

  test("node and value round-trip through the store") {
    withBackend { backend =>
      val value = JamBytes(Array.fill[Byte](64)(0x2a)) // > 32 bytes → regular leaf
      val node = TrieNode.leaf(key31(1), value)

      backend.batchUpdate(
        Seq(
          BackendOp.WriteNode(node),
          BackendOp.writeRawValue(value),
          BackendOp.NodeRefDelta(node.hash, 1),
          BackendOp.RawValueRefDelta(Hashing.blake2b256(value), 1)
        )
      )

      backend.readNode(node.hash) shouldBe Some(node)
      backend.readRawValue(Hashing.blake2b256(value)) shouldBe Some(value)
      backend.nodeRefcount(node.hash) shouldBe 1L
    }
  }

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

  test("state persists across close/reopen") {
    val dir = Files.createTempDirectory("jam-rocksdb-reopen")
    try
      val value = JamBytes(Array.fill[Byte](50)(7))
      val node = TrieNode.leaf(key31(9), value)
      val backend1 = RocksDbTrieBackend.open(dir)
      backend1.batchUpdate(
        Seq(
          BackendOp.WriteNode(node),
          BackendOp.writeRawValue(value),
          BackendOp.NodeRefDelta(node.hash, 1),
          BackendOp.RawValueRefDelta(Hashing.blake2b256(value), 1)
        )
      )
      backend1.close()

      val backend2 = RocksDbTrieBackend.open(dir)
      try
        backend2.readNode(node.hash) shouldBe Some(node)
        backend2.readRawValue(Hashing.blake2b256(value)) shouldBe Some(value)
        backend2.nodeRefcount(node.hash) shouldBe 1L
      finally backend2.close()
    finally deleteRecursively(dir)
  }

  test("StateTrie over RocksDB produces the same roots as over the in-memory backend") {
    withBackend { rocks =>
      val mem = new InMemoryTrieBackend
      val trieRocks = StateTrie.empty(rocks)
      val trieMem = StateTrie.empty(mem)

      val updates1 = (0 until 50).map { i =>
        key31(i) -> Some(JamBytes(Array.fill[Byte](8 + i)( (i % 127).toByte)))
      }
      trieRocks.update(updates1); trieRocks.save()
      trieMem.update(updates1); trieMem.save()
      trieRocks.rootHash shouldBe trieMem.rootHash

      // Delete some, overwrite others.
      val updates2 =
        (0 until 20).map(i => key31(i) -> None) ++
          (20 until 35).map(i => key31(i) -> Some(JamBytes(Array.fill[Byte](100)(0x5c))))
      trieRocks.update(updates2); trieRocks.save()
      trieMem.update(updates2); trieMem.save()
      trieRocks.rootHash shouldBe trieMem.rootHash

      // Reads agree.
      trieRocks.read(key31(25)) shouldBe trieMem.read(key31(25))
      trieRocks.read(key31(5)) shouldBe None

      rocks.gc()
      mem.gc()
      trieRocks.read(key31(40)) shouldBe trieMem.read(key31(40))
      trieRocks.rootHash shouldBe trieMem.rootHash
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

  test("repeated open/close does not leak locks or handles; reopen succeeds") {
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

      // Open/close 50 times; a leaked lock or handle exhaustion throws here.
      (0 until 50).foreach { _ =>
        val b = RocksDbTrieBackend.open(dir)
        b.readNode(node.hash) shouldBe Some(node)
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
