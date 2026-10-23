package io.forge.jam.db.benchmark

import io.forge.jam.core.JamBytes
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrie, StateTrieBackend, StateTrieStore}
import io.forge.jam.db.RocksDbTrieBackend

import java.nio.file.{Files, Path}
import java.util.Locale
import scala.util.Random

/**
 * State trie + DB benchmark. For RocksDB and the in-memory backend: bootstrap N keys, then
 * B blocks of K dirty keys each (update, save, commit, gc, as BlockImporter does), then random reads.
 *
 * Args: `--keys=N` (1,000,000), `--blocks=B` (100), `--dirty=K` (1,000), `--reads=R` (100,000),
 * `--backend=rocksdb|memory|both` (both). Prints `RESULT name value unit` lines.
 */
object TrieDbBench:
  private val KeySize = 31
  private val ValueSize = 36 // over 32 bytes, so a regular leaf with its value stored apart, as most service data is

  private def result(name: String, value: Double, unit: String): Unit =
    println(f"RESULT $name $value%.3f $unit")

  private def timeMs[A](f: => A): (A, Double) =
    val t0 = System.nanoTime()
    val a = f
    (a, (System.nanoTime() - t0) / 1e6)

  private def percentile(sorted: IndexedSeq[Double], p: Double): Double =
    sorted(math.min(sorted.length - 1, (p * sorted.length).toInt))

  private def dirSize(p: Path): Long =
    val s = Files.walk(p)
    try s.filter(Files.isRegularFile(_)).mapToLong(Files.size(_)).sum()
    finally s.close()

  private def deleteDir(p: Path): Unit =
    if Files.exists(p) then
      val s = Files.walk(p)
      try s.sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
      finally s.close()

  final case class Params(keys: Int, blocks: Int, dirty: Int, reads: Int)

  private def run(name: String, open: () => StateTrieBackend, reopen: Option[() => StateTrieBackend], p: Params): Unit =
    val rng = new Random(42)
    val keys = Array.fill(p.keys)(JamBytes(rng.nextBytes(KeySize)))
    def value() = JamBytes(rng.nextBytes(ValueSize))
    val backend = open()
    val store = new StateTrieStore(backend)

    val (_, populateMs) = timeMs(store.bootstrap(keys.toSeq.map(k => (k, value()))))
    result(s"${name}_populate", populateMs, "ms")

    // Per block: 90% of the dirty keys overwrite existing ones, 10% are new.
    val inserted = p.dirty / 10
    var liveKeys = keys
    val updateMs, saveMs, gcMs, blockMs = new Array[Double](p.blocks)
    for b <- 0 until p.blocks do
      val overwrites = Seq.fill(p.dirty - inserted)(liveKeys(rng.nextInt(liveKeys.length)))
      val fresh = Array.fill(inserted)(JamBytes(rng.nextBytes(KeySize)))
      val updates = (overwrites ++ fresh).map(k => (k, Some(value())))
      val parent = store.currentRoot
      val t0 = System.nanoTime()
      val trie = store.at(parent)
      trie.update(updates)
      val t1 = System.nanoTime()
      trie.save()
      store.commitChild(parent, trie.rootHash)
      val t2 = System.nanoTime()
      store.gc()
      val t3 = System.nanoTime()
      updateMs(b) = (t1 - t0) / 1e6
      saveMs(b) = (t2 - t1) / 1e6
      gcMs(b) = (t3 - t2) / 1e6
      blockMs(b) = (t3 - t0) / 1e6
      liveKeys = liveKeys ++ fresh
    // The first tenth of the blocks is JIT warmup.
    val skip = p.blocks / 10
    def mean(a: Array[Double]) = a.drop(skip).sum / (a.length - skip)
    val sortedBlocks = blockMs.drop(skip).sorted.toIndexedSeq
    result(s"${name}_block_${p.dirty}_dirty_mean", mean(blockMs), "ms")
    result(s"${name}_block_${p.dirty}_dirty_p50", percentile(sortedBlocks, 0.5), "ms")
    result(s"${name}_block_${p.dirty}_dirty_p99", percentile(sortedBlocks, 0.99), "ms")
    result(s"${name}_block_update_mean", mean(updateMs), "ms")
    result(s"${name}_block_save_mean", mean(saveMs), "ms")
    result(s"${name}_block_gc_mean", mean(gcMs), "ms")

    val root = store.currentRoot
    def readAll(trie: StateTrie): Double =
      val probe = Array.fill(p.reads)(liveKeys(rng.nextInt(liveKeys.length)))
      val (_, ms) = timeMs {
        var i = 0
        while i < probe.length do
          if trie.read(probe(i)).isEmpty then throw new IllegalStateException("key missing")
          i += 1
      }
      ms * 1000 / p.reads
    readAll(StateTrie.at(backend, root))
    result(s"${name}_read_warm", readAll(StateTrie.at(backend, root)), "us/read")

    reopen.foreach { r =>
      backend match
        case c: AutoCloseable => c.close()
        case _ => ()
      val again = r()
      result(s"${name}_read_cold", readAll(StateTrie.at(again, root)), "us/read")
      again match
        case c: AutoCloseable => c.close()
        case _ => ()
    }

  def main(args: Array[String]): Unit =
    Locale.setDefault(Locale.US)
    org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME) match
      case l: ch.qos.logback.classic.Logger => l.setLevel(ch.qos.logback.classic.Level.OFF)
      case _ => ()
    val opts = args.collect { case s"--$k=$v" => k -> v }.toMap
    def int(k: String, d: Int) = opts.get(k).fold(d)(_.replace("_", "").toInt)
    val p = Params(int("keys", 1_000_000), int("blocks", 100), int("dirty", 1_000), int("reads", 100_000))
    val which = opts.getOrElse("backend", "both")
    println(s"TrieDbBench keys=${p.keys} blocks=${p.blocks} dirty=${p.dirty} reads=${p.reads}")
    if which == "both" || which == "rocksdb" then
      val dir = Files.createTempDirectory("trie-bench")
      try
        run("rocksdb", () => RocksDbTrieBackend.open(dir), Some(() => RocksDbTrieBackend.open(dir)), p)
        result("rocksdb_disk_size", dirSize(dir) / 1e6, "MB")
      finally deleteDir(dir)
    if which == "both" || which == "memory" then run("memory", () => new InMemoryTrieBackend, None, p)
