package io.forge.jam.core.benchmark

import io.forge.jam.core.JamBytes
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrie, StateTrieStore}

import scala.util.Random

/** Repeatable trie timings for before/after runs: populate 1M keys, insert 1M more as one batch and in 10k
  * batches, each on a fresh in-memory store, `--reps` times in one JVM (after one untimed pass). Prints min, median
  * and max per phase and the final root, which must not change between builds. Options: `--keys=N`, `--reps=R`.
  */
object TrieBatchBench:
  private def timeMs[A](f: => A): (A, Double) =
    val t0 = System.nanoTime()
    val a = f
    (a, (System.nanoTime() - t0) / 1e6)

  private type Entries = Vector[(JamBytes, JamBytes)]
  private type Updates = Vector[(JamBytes, Option[JamBytes])]

  private def populateThenOneBatch(first: Entries, second: Updates) =
    val store = new StateTrieStore(new InMemoryTrieBackend)
    val (_, p) = timeMs(store.bootstrap(first))
    val (r, o) = timeMs {
      val trie = StateTrie.at(store.backend, store.currentRoot)
      trie.update(second)
      trie.save()
      trie.rootHash
    }
    (p, o, r)

  private def tenKBatches(first: Entries, second: Updates) =
    val store = new StateTrieStore(new InMemoryTrieBackend)
    store.bootstrap(first)
    val (r, b) = timeMs {
      var root = store.currentRoot
      second.grouped(10_000).foreach { batch =>
        val trie = StateTrie.at(store.backend, root)
        trie.update(batch)
        trie.save()
        root = trie.rootHash
      }
      root
    }
    (b, r)

  def main(args: Array[String]): Unit =
    val opts = args.collect { case s"--$k=$v" => k -> v }.toMap
    val n = opts.get("keys").map(_.toInt).getOrElse(1_000_000)
    val reps = opts.get("reps").map(_.toInt).getOrElse(5)
    val rng = new Random(42)
    def entries() = Vector.fill(n)((JamBytes(rng.nextBytes(31)), JamBytes(rng.nextBytes(36))))
    val first = entries()
    val second = entries().map((k, v) => (k, Option(v)))
    val populate, oneBatch, batches = Vector.newBuilder[Double]
    var roots = Set.empty[String]
    for rep <- 0 to reps do // rep 0 is warmup
      // Each phase in its own method so its store is garbage before the next one is built.
      val (p, o, r1) = populateThenOneBatch(first, second)
      System.gc()
      val (b, r2) = tenKBatches(first, second)
      roots = roots + r1.toHex + r2.toHex
      if rep > 0 then { populate += p; oneBatch += o; batches += b }
      println(f"rep $rep populate=$p%.0f one_batch=$o%.0f batches_10k=$b%.0f ms")
      System.gc()
    def line(name: String, xs: Vector[Double]): Unit =
      val s = xs.sorted
      println(f"RESULT $name min=${s.head}%.0f median=${s(s.length / 2)}%.0f max=${s.last}%.0f ms")
    line("trie_populate_1m", populate.result())
    line("trie_insert_1m_one_batch", oneBatch.result())
    line("trie_insert_1m_10k_batches", batches.result())
    println(s"roots=${roots.size} ${roots.head.take(16)}")
