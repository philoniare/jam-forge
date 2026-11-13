package io.forge.jam.db

import java.nio.file.{Files, Path}

import io.forge.jam.core.Hashing
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Stores opened with one memory budget flush their write buffers at the shared limit and keep what they cache
  * within the shared cache.
  */
class RocksDbMemorySpec extends AnyFunSuite with Matchers:

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  private val MiB = 1024L * 1024

  /** Write `megabytes` of incompressible shards and return the largest write-buffer size seen on the way. */
  private def fill(store: ShardStore, megabytes: Int, seed: Int): Long =
    val rng = new scala.util.Random(seed)
    var peak = 0L
    for i <- 0 until megabytes * 4 do
      val shard = new Array[Byte](256 * 1024)
      rng.nextBytes(shard)
      store.put(Hashing.blake2b256(Array[Byte](seed.toByte, (i >> 8).toByte, i.toByte)), i % 1023, shard)
      peak = math.max(peak, store.memTableBytes)
    peak

  test("write buffers are flushed at the shared limit, across stores, and what was written reads back") {
    val memory = new RocksDbMemory(cacheBytes = 32 * MiB, writeBufferBytes = 8 * MiB)
    val dirA = Files.createTempDirectory("jam-rocks-mem-a")
    val dirB = Files.createTempDirectory("jam-rocks-mem-b")
    val a = ShardStore.open(dirA, memory)
    val b = ShardStore.open(dirB, memory)
    try
      // 24 MiB into each store, alternating. On its own a column family flushes at 64 MiB, so neither would flush.
      for round <- 0 until 3 do
        fill(a, 8, round)
        fill(b, 8, 100 + round)
      // Passing the limit schedules a flush that runs in the background, so how far the buffers overshoot while
      // writing depends on the disk and the machine's load. Once the flushes land the total is back near the limit,
      // far below the 48 MiB the two stores hold without a shared limit.
      val deadline = System.nanoTime() + 30_000_000_000L
      while a.memTableBytes + b.memTableBytes > 12 * MiB && System.nanoTime() < deadline do Thread.sleep(50)
      a.memTableBytes + b.memTableBytes should be <= 12 * MiB
      memory.usedBytes should be <= 40 * MiB

      val key = Hashing.blake2b256(Array[Byte](0, 0, 0))
      a.get(key, 0).map(_.length) shouldBe Some(256 * 1024)
      a.heldIndexes(key) shouldBe List(0)
    finally
      a.close()
      b.close()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("a store that has gone quiet gives up its write buffers when another store takes the total over the limit") {
    // RocksDB flushes only the store being written. Here the full store is the idle one.
    val memory = new RocksDbMemory(cacheBytes = 64 * MiB, writeBufferBytes = 16 * MiB)
    val dirA = Files.createTempDirectory("jam-rocks-mem-e")
    val dirB = Files.createTempDirectory("jam-rocks-mem-f")
    val a = ShardStore.open(dirA, memory)
    val b = ShardStore.open(dirB, memory)
    try
      // 12 MiB (13 with the index's buffer): a mebibyte under the 14 at which RocksDB flushes on a write, so it
      // all stays in memory.
      fill(a, 12, 1)
      a.memTableBytes should be > 11 * MiB
      // One 2 MiB write to the other store takes the total a mebibyte over, and nothing is written to the full
      // one again.
      b.put(Hashing.blake2b256(Array[Byte](9)), 0, new Array[Byte](2 * 1024 * 1024))
      val deadline = System.nanoTime() + 30_000_000_000L
      while a.memTableBytes > 2 * MiB && System.nanoTime() < deadline do Thread.sleep(50)
      a.memTableBytes should be <= 2 * MiB
      a.get(Hashing.blake2b256(Array[Byte](1, 0, 0)), 0).map(_.length) shouldBe Some(256 * 1024)
    finally
      a.close()
      b.close()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("without a shared limit the same writes stay in memory") {
    // The contrast for the test above: a budget too large to matter leaves each store its 64 MiB default.
    val memory = new RocksDbMemory(cacheBytes = 1024 * MiB, writeBufferBytes = 1024 * MiB)
    val dir = Files.createTempDirectory("jam-rocks-mem-c")
    val store = ShardStore.open(dir, memory)
    try fill(store, 48, 7) should be > 40 * MiB
    finally
      store.close()
      cleanup(dir)
  }

  test("the trie backend and the block store take the same budget and still work") {
    val memory = new RocksDbMemory(cacheBytes = 16 * MiB, writeBufferBytes = 4 * MiB)
    val dir = Files.createTempDirectory("jam-rocks-mem-d")
    val trie = RocksDbTrieBackend.open(dir.resolve("trie"), trackGarbage = false, memory)
    val blocks = BlockStore.open(dir.resolve("blocks"), memory)
    try
      blocks.setMeta("k", Array[Byte](1, 2, 3))
      blocks.getMeta("k").map(_.toList) shouldBe Some(List[Byte](1, 2, 3))
    finally
      blocks.close()
      trie.close()
      cleanup(dir)
  }
