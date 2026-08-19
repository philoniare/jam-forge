package io.forge.jam.protocol.traces

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{StateTrie, InMemoryTrieBackend}

/**
 * Property tests for the binary state trie. 
 */
class TriePropertySpec extends AnyFlatSpec with Matchers:

  private val rng = new scala.util.Random(20260530L)

  private def randKey(): JamBytes =
    val a = new Array[Byte](31); rng.nextBytes(a); JamBytes(a)

  private def randVal(len: Int): JamBytes =
    val a = new Array[Byte](len); rng.nextBytes(a); JamBytes(a)

  private def randMap(n: Int, maxValLen: Int): Map[JamBytes, JamBytes] =
    (0 until n).map(_ => randKey() -> randVal(rng.nextInt(maxValLen + 1))).toMap

  private def trieRootOf(kv: Map[JamBytes, JamBytes]): Hash =
    val trie = StateTrie.empty(new InMemoryTrieBackend())
    trie.update(kv.toSeq.map((k, v) => (k, Some(v): Option[JamBytes])))
    trie.rootHash

  private def kvList(m: Map[JamBytes, JamBytes]): List[KeyValue] =
    m.toList.map((k, v) => KeyValue(k, v))

  "StateTrie" should "produce the same root as StateMerklization over random key/value sets" in {
    for _ <- 0 until 50 do
      val kv = randMap(rng.nextInt(40), 64) // values up to 64B straddle the 32B embedded boundary
      trieRootOf(kv) shouldBe StateMerklization.stateMerklize(kvList(kv))
  }

  it should "agree with StateMerklization across the 32/33-byte embedded-leaf boundary" in {
    for valLen <- Seq(0, 1, 31, 32, 33, 64, 200) do
      val kv = (0 until 16).map(_ => randKey() -> randVal(valLen)).toMap
      withClue(s"valLen=$valLen: ") {
        trieRootOf(kv) shouldBe StateMerklization.stateMerklize(kvList(kv))
      }
  }

  it should "be insertion-order independent" in {
    for _ <- 0 until 30 do
      val kv = randMap(rng.nextInt(30) + 1, 48)
      val seq = kv.toSeq.map((k, v) => (k, Some(v): Option[JamBytes]))
      val t1 = StateTrie.empty(new InMemoryTrieBackend()); t1.update(seq)
      val t2 = StateTrie.empty(new InMemoryTrieBackend()); t2.update(rng.shuffle(seq))
      t1.rootHash shouldBe t2.rootHash
  }

  it should "return to the zero root after deleting all keys" in {
    for _ <- 0 until 30 do
      val kv = randMap(rng.nextInt(30) + 1, 48)
      val trie = StateTrie.empty(new InMemoryTrieBackend())
      trie.update(kv.toSeq.map((k, v) => (k, Some(v): Option[JamBytes])))
      trie.update(kv.keys.toSeq.map(k => (k, None: Option[JamBytes])))
      trie.rootHash shouldBe Hash.zero
  }

  it should "read back inserted values and reflect a delete" in {
    val kv = randMap(40, 64)
    val trie = StateTrie.empty(new InMemoryTrieBackend())
    trie.update(kv.toSeq.map((k, v) => (k, Some(v): Option[JamBytes])))
    kv.foreach((k, v) => trie.read(k) shouldBe Some(v))
    val dk = kv.head._1
    trie.update(Seq((dk, None: Option[JamBytes])))
    trie.read(dk) shouldBe None
    kv.tail.foreach((k, v) => trie.read(k) shouldBe Some(v))
  }

  it should "give the zero root for the empty trie" in {
    trieRootOf(Map.empty) shouldBe Hash.zero
    StateMerklization.stateMerklize(List.empty[KeyValue]) shouldBe Hash.zero
  }
