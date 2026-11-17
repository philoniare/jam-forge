package io.forge.jam.db

import java.nio.file.{Files, Path, StandardCopyOption}

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.StateTrie
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TrieWalFlushSpec extends AnyFunSuite with Matchers:

  private def cleanup(dir: Path): Unit =
    if Files.exists(dir) then Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  private def key(i: Int): JamBytes = JamBytes(Hashing.blake2b256(Array[Byte](2, (i >> 8).toByte, i.toByte)).bytes.toArray.take(31))
  private def label(name: String): Hash = Hashing.blake2b256(name.getBytes("UTF-8"))
  private def value(tag: Int, i: Int): JamBytes = JamBytes(Array.fill[Byte](40)((tag * 31 + i).toByte))

  private def memory = new RocksDbMemory(64L << 20, 32L << 20)

  private def open(dir: Path, manual: Boolean, flushBytes: Long = RocksDbTrieBackend.Settings().manualWalFlushBytes) =
    RocksDbTrieBackend.open(
      dir,
      trackGarbage = false,
      memory,
      RocksDbTrieBackend.Settings(manualWalFlush = manual, manualWalFlushBytes = flushBytes)
    )

  private def crashImage(dir: Path): Path =
    val copy = Files.createTempDirectory("jam-trie-crash")
    val files = Files.list(dir)
    try files.forEach(f => Files.copy(f, copy.resolve(f.getFileName), StandardCopyOption.REPLACE_EXISTING))
    finally files.close()
    Files.deleteIfExists(copy.resolve("LOCK"))
    copy

  private def save(backend: RocksDbTrieBackend, parent: Hash, name: String, height: Long, tag: Int, keys: Range): Hash =
    backend.withJournal(label(name), height) {
      val t = StateTrie.at(backend, parent)
      t.update(keys.map(i => key(i) -> Some(value(tag, i))))
      t.save()
      t.rootHash
    }

  private def whole(backend: RocksDbTrieBackend, root: Hash, tag: Int => Int, keys: Range): Boolean =
    try
      val t = StateTrie.at(backend, root)
      keys.forall(i => t.read(key(i)).contains(value(tag(i), i)))
    catch case _: io.forge.jam.core.trie.MissingTrieNodeException => false

  test("manual WAL flush: a crash keeps everything up to the last sync and a prune request, and loses the save after") {
    val dir = Files.createTempDirectory("jam-trie-wal")
    var crashed: Path = null
    val backend = open(dir, manual = true)
    try
      val a = save(backend, Hash.zero, "a", 1L, 1, 0 until 200)
      backend.syncWal() // a chain change commits here: "trie WAL sync, then metadata batch"
      val b = save(backend, a, "b", 2L, 2, 0 until 50)
      backend.syncWal()
      backend.schedulePrune(label("b"), 2L)
      backend.pruneJournal(label("b"), 2L)
      val c = save(backend, b, "c", 3L, 3, 0 until 50)
      crashed = crashImage(dir)
      val after = open(crashed, manual = true)
      try
        whole(after, a, _ => 1, 0 until 200) shouldBe true
        whole(after, b, i => if i < 50 then 2 else 1, 0 until 200) shouldBe true
        after.readNode(c) shouldBe None
        after.scheduledPrunes shouldBe List(label("b") -> 2L)
        after.journalOf(label("b")) should not be empty
        after.scheduledPrunes.foreach((l, h) => after.pruneJournal(l, h))
        after.scheduledPrunes shouldBe Nil
        whole(after, b, i => if i < 50 then 2 else 1, 0 until 200) shouldBe true
        whole(after, a, _ => 1, 0 until 200) shouldBe false // a's state was the one pruned
      finally after.close()
    finally
      backend.close()
      cleanup(dir)
      if crashed != null then cleanup(crashed)
  }

  test("automatic WAL flush (the default before): a crash keeps every write, synced or not") {
    val dir = Files.createTempDirectory("jam-trie-wal")
    var crashed: Path = null
    val backend = open(dir, manual = false)
    try
      val a = save(backend, Hash.zero, "a", 1L, 1, 0 until 200)
      backend.syncWal()
      val b = save(backend, a, "b", 2L, 2, 0 until 50)
      crashed = crashImage(dir)
      val after = open(crashed, manual = false)
      try
        whole(after, a, _ => 1, 0 until 200) shouldBe true
        whole(after, b, i => if i < 50 then 2 else 1, 0 until 200) shouldBe true
      finally after.close()
    finally
      backend.close()
      cleanup(dir)
      if crashed != null then cleanup(crashed)
  }

  test("manual WAL flush: saves with no sync between them are flushed once their bytes pass the bound") {
    val dir = Files.createTempDirectory("jam-trie-wal")
    var crashed: Path = null
    val backend = open(dir, manual = true, flushBytes = 64L << 10)
    try
      var root = Hash.zero
      val roots = (0 until 40).map { n =>
        root = save(backend, root, s"d$n", 1L, n, n * 50 until (n + 1) * 50)
        root
      }
      backend.unflushedWalBytes should be < (64L << 10)
      crashed = crashImage(dir)
      val after = open(crashed, manual = true)
      try
        val kept = roots.indices.filter(n => after.readNode(roots(n)).isDefined)
        kept should not be empty
        kept shouldBe (0 until kept.size)
        kept.size should be > 30
        whole(after, roots(kept.last), i => i / 50, 0 until 50 * kept.size) shouldBe true
      finally after.close()
    finally
      backend.close()
      cleanup(dir)
      if crashed != null then cleanup(crashed)
  }

  test("manual WAL flush: close flushes and syncs what is buffered") {
    val dir = Files.createTempDirectory("jam-trie-wal")
    try
      val backend = open(dir, manual = true)
      val a = save(backend, Hash.zero, "a", 1L, 1, 0 until 100)
      backend.close()
      val again = open(dir, manual = true)
      try whole(again, a, _ => 1, 0 until 100) shouldBe true
      finally again.close()
    finally cleanup(dir)
  }
