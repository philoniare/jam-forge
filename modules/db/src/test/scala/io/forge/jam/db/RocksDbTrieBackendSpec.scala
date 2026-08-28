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
          BackendOp.WriteRawValue(value),
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
          BackendOp.WriteRawValue(liveValue),
          BackendOp.NodeRefDelta(liveNode.hash, 1),
          BackendOp.RawValueRefDelta(Hashing.blake2b256(liveValue), 1),
          BackendOp.WriteNode(deadNode),
          BackendOp.WriteRawValue(deadValue),
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
          BackendOp.WriteRawValue(value),
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
          BackendOp.WriteRawValue(value),
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

  test("clear empties everything") {
    withBackend { backend =>
      val value = JamBytes(Array.fill[Byte](40)(4))
      val node = TrieNode.leaf(key31(4), value)
      backend.batchUpdate(
        Seq(
          BackendOp.WriteNode(node),
          BackendOp.WriteRawValue(value),
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
