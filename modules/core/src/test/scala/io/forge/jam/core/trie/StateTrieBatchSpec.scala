package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.util.Random

/**
 * The batched [[StateTrie.update]] against the per-key update it replaced ([[LegacyStateTrie]]) and against a fresh
 * build of the final key set: same roots, same pending sets, and the same backend writes and ref deltas at every save,
 * over inserts, updates, deletes, long shared key prefixes and several save/gc cycles that bring deleted keys back.
 */
class StateTrieBatchSpec extends AnyFlatSpec with Matchers:

  /** Records what each save sends, normalised so two tries' saves compare equal when they do the same work. */
  private final class RecordingBackend extends StateTrieBackend:
    val inner = new InMemoryTrieBackend
    var lastOps: Seq[BackendOp] = Nil
    def readNode(hash: Hash): Option[TrieNode] = inner.readNode(hash)
    def readRawValue(hash: Hash): Option[JamBytes] = inner.readRawValue(hash)
    def batchUpdate(ops: Seq[BackendOp]): Unit =
      lastOps = ops
      inner.batchUpdate(ops)
    def gc(): Unit = inner.gc()
    def clear(): Unit = inner.clear()

    def summary: (Set[Hash], Set[Hash], Map[Hash, Long], Map[Hash, Long]) =
      (
        lastOps.collect { case BackendOp.WriteNode(n) => n.hash }.toSet,
        lastOps.collect { case BackendOp.WriteRawValue(h, _) => h }.toSet,
        lastOps.collect { case BackendOp.NodeRefDelta(h, d) => h -> d }.toMap,
        lastOps.collect { case BackendOp.RawValueRefDelta(h, d) => h -> d }.toMap,
      )

  /** GP 0.8.0 §D.2 M built directly from the sorted key set: the root a fresh trie of `model` must have. */
  private def freshRoot(model: collection.Map[JamBytes, JamBytes]): Hash =
    def bit(k: JamBytes, i: Int) = (k(i / 8) & (0x80 >>> (i % 8))) != 0
    def build(kvs: Vector[(JamBytes, JamBytes)], depth: Int): Hash =
      kvs match
        case Vector() => Hash.zero
        case Vector((k, v)) => TrieNode.leaf(k, v).hash
        case _ =>
          val (l, r) = kvs.partition((k, _) => !bit(k, depth))
          TrieNode.branch(build(l, depth + 1), build(r, depth + 1)).hash
    build(model.toVector, 0)

  /** Runs the same rounds through both tries, checking them against each other and the model after every round. */
  private def runRounds(seed: Int, rounds: Int, newKey: Random => JamBytes, gcEvery: Int): Unit =
    val rng = new Random(seed)
    val newBackend = new RecordingBackend
    val oldBackend = new RecordingBackend
    var newRoot = Hash.zero
    var oldRoot = Hash.zero
    val model = mutable.HashMap[JamBytes, JamBytes]()
    val deleted = mutable.ArrayBuffer[(JamBytes, JamBytes)]()

    def value(): JamBytes = JamBytes(rng.nextBytes(rng.nextInt(70))) // embedded (<= 32 bytes) and regular leaves

    for round <- 1 to rounds do
      val batch = mutable.ArrayBuffer[(JamBytes, Option[JamBytes])]()
      val keys = model.keys.toVector
      for _ <- 0 until rng.nextInt(60) do
        rng.nextInt(7) match
          case 0 | 1 => batch += ((newKey(rng), Some(value())))
          case 2 if keys.nonEmpty => batch += ((keys(rng.nextInt(keys.size)), Some(value())))
          case 3 if keys.nonEmpty =>
            val k = keys(rng.nextInt(keys.size))
            deleted += ((k, model(k)))
            batch += ((k, None))
          case 4 if deleted.nonEmpty =>
            // Back with the value it had: the leaf node a save and possibly a gc saw go away.
            val (k, v) = deleted(rng.nextInt(deleted.size))
            batch += ((k, Some(v)))
          case 5 if keys.nonEmpty => batch += ((keys(rng.nextInt(keys.size)), Some(model(keys(rng.nextInt(keys.size))))))
          case _ => batch += ((newKey(rng), None)) // delete of an absent key
      // The same key twice in a batch: the later update wins.
      if batch.nonEmpty && rng.nextBoolean() then batch += ((batch(rng.nextInt(batch.size))._1, Some(value())))
      batch.foreach {
        case (k, Some(v)) => model(k) = v
        case (k, None) => model.remove(k)
      }

      val newTrie = StateTrie.at(newBackend, newRoot)
      val oldTrie = new LegacyStateTrie(oldBackend, oldRoot)
      // Split a round across two update calls sometimes: pending state carries over between them.
      val (first, second) = batch.splitAt(if rng.nextBoolean() then batch.size / 2 else batch.size)
      Seq(first, second).filter(_.nonEmpty).foreach { part =>
        newTrie.update(part.toSeq)
        oldTrie.update(part.toSeq)
      }
      withClue(s"seed $seed round $round: ") {
        newTrie.rootHash shouldBe oldTrie.rootHash
        newTrie.rootHash shouldBe freshRoot(model)
        newTrie.pendingNodeCount shouldBe oldTrie.pendingNodeCount
        newTrie.pendingValueCount shouldBe oldTrie.pendingValueCount
        newTrie.save()
        oldTrie.save()
        newBackend.summary shouldBe oldBackend.summary
        model.foreach((k, v) => newTrie.read(k) shouldBe Some(v))
      }
      newRoot = newTrie.rootHash
      oldRoot = oldTrie.rootHash
      if round % gcEvery == 0 then
        newBackend.gc()
        oldBackend.gc()
        newBackend.inner.nodeCount shouldBe oldBackend.inner.nodeCount
        newBackend.inner.valueCount shouldBe oldBackend.inner.valueCount

  private def randomKey(rng: Random): JamBytes = JamBytes(rng.nextBytes(31))

  /** Keys that share all but their last two bytes, so splits run down long chains of one-sided branches. */
  private def sharedPrefixKey(rng: Random): JamBytes =
    val a = new Array[Byte](31)
    a(29) = rng.nextInt(4).toByte
    a(30) = rng.nextInt(256).toByte
    JamBytes.wrap(a)

  /** JAM storage keys of a few services: service-id bytes interleaved with a hash (GP 0.8.0 §D.1, C). */
  private def serviceKey(rng: Random): JamBytes =
    val a = rng.nextBytes(31)
    val service = rng.nextInt(3)
    a(0) = service.toByte
    a(2) = 0
    a(4) = 0
    a(6) = 0
    JamBytes.wrap(a)

  "StateTrie.update" should "match the per-key update and a fresh build over random batches" in {
    for seed <- 1 to 6 do runRounds(seed, rounds = 40, randomKey, gcEvery = 3)
  }

  it should "match them on keys with long shared prefixes" in {
    for seed <- 11 to 16 do runRounds(seed, rounds = 40, sharedPrefixKey, gcEvery = 2)
  }

  it should "match them on interleaved service keys" in {
    for seed <- 21 to 24 do runRounds(seed, rounds = 40, serviceKey, gcEvery = 4)
  }

  it should "match them when every round is saved and collected" in {
    for seed <- 31 to 34 do runRounds(seed, rounds = 60, sharedPrefixKey, gcEvery = 1)
  }

  it should "build a large trie in one batch with the root of a fresh build" in {
    val rng = new Random(5)
    val model = mutable.HashMap[JamBytes, JamBytes]()
    for _ <- 0 until 20000 do model(randomKey(rng)) = JamBytes(rng.nextBytes(rng.nextInt(50)))
    val trie = StateTrie.empty(new InMemoryTrieBackend)
    trie.update(model.toSeq.map((k, v) => (k, Some(v))))
    trie.rootHash shouldBe freshRoot(model)
  }

  it should "reject keys that are not 31 bytes" in {
    val trie = StateTrie.empty(new InMemoryTrieBackend)
    an[IllegalArgumentException] should be thrownBy trie.update(Seq((JamBytes(new Array[Byte](30)), None)))
  }
