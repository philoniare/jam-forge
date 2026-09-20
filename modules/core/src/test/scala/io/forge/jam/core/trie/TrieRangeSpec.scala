package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TrieRangeSpec extends AnyFlatSpec with Matchers:

  /** 31-byte key that only differs in its last byte, so lexicographic order
    * (== bit order, since all preceding bytes are equal) matches `i`.
    */
  private def key(i: Int): JamBytes =
    val a = new Array[Byte](31)
    a(30) = i.toByte
    JamBytes(a)

  private def value(i: Int, len: Int): JamBytes =
    JamBytes(Array.fill[Byte](len)(i.toByte))

  private val ValueLen = 20

  private def buildTrie(): StateTrie =
    val trie = StateTrie.empty(new InMemoryTrieBackend())
    // Insert out of order to exercise the trie's insertion-order independence.
    val order = scala.util.Random(42).shuffle((1 to 10).toList)
    trie.update(order.map(i => (key(i), Some(value(i, ValueLen)): Option[JamBytes])))
    trie

  "range" should "return all 10 pairs sorted when the range covers the whole keyspace" in {
    val trie = buildTrie()
    val (_, pairs) = trie.range(JamBytes.zeros(31), JamBytes.fill(31)(0xff.toByte), maxSize = Int.MaxValue)
    pairs.map(_._1) shouldBe (1 to 10).map(key).toList
    pairs shouldBe (1 to 10).map(i => (key(i), value(i, ValueLen))).toList
  }

  it should "return exactly the keys contained in a sub-range" in {
    val trie = buildTrie()
    val (_, pairs) = trie.range(key(3), key(7), maxSize = Int.MaxValue)
    pairs.map(_._1) shouldBe (3 to 7).map(key).toList
    pairs shouldBe (3 to 7).map(i => (key(i), value(i, ValueLen))).toList
  }

  it should "return exactly one pair when maxSize is smaller than two values" in {
    val trie = buildTrie()
    // ValueLen = 20; 25 fits one value but not two.
    val (_, pairs) = trie.range(key(1), key(10), maxSize = 25)
    pairs shouldBe List((key(1), value(1, ValueLen)))
  }

  it should "always include the first pair even alone it exceeds maxSize" in {
    val trie = buildTrie()
    val (_, pairs) = trie.range(key(1), key(10), maxSize = 1)
    pairs shouldBe List((key(1), value(1, ValueLen)))
  }

  it should "return no pairs and no crash for a range beyond every stored key" in {
    val trie = buildTrie()
    val (_, pairs) = trie.range(key(20), key(30), maxSize = Int.MaxValue)
    pairs shouldBe empty
  }

  it should "produce boundary nodes that chain from the root hash down to the start key" in {
    val trie = buildTrie()
    val (boundary, pairs) = trie.range(key(3), key(7), maxSize = Int.MaxValue)

    boundary should not be empty
    pairs should not be empty
    boundary.head.hash shouldBe trie.rootHash

    def childHashes(n: TrieNode): Set[Hash] =
      Set(
        Hash.fromByteVectorUnchecked(n.left.toByteVector),
        Hash.fromByteVectorUnchecked(n.right.toByteVector)
      )

    for i <- 1 until boundary.length do
      val node = boundary(i)
      val priorLinks = boundary.take(i).flatMap(childHashes)
      withClue(s"boundary node $i (hash=${node.hash.toHex.take(12)}) should be a child link of a prior node: ") {
        priorLinks.contains(node.hash) shouldBe true
      }

    val allLinks = boundary.flatMap(childHashes).toSet
    val (firstKey, firstValue) = pairs.head
    val (lastKey, lastValue) = pairs.last
    allLinks.contains(TrieNode.leaf(firstKey, firstValue).hash) shouldBe true
    allLinks.contains(TrieNode.leaf(lastKey, lastValue).hash) shouldBe true
  }
