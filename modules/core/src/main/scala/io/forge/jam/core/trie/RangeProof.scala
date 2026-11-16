package io.forge.jam.core.trie

import io.forge.jam.core.{Blake2b256, JamBytes}
import io.forge.jam.core.primitives.Hash

object RangeProof:

  private val KeyBits = 248

  def verify(
      root: Hash,
      start: JamBytes,
      boundaryNodes: Seq[Array[Byte]],
      pairs: IndexedSeq[(JamBytes, JamBytes)]
  ): Either[String, Unit] =
    if start.length != 31 then Left("start key is not 31 bytes")
    else if boundaryNodes.exists(_.length != TrieNode.GpEncodingBytes) then Left("a boundary node is not 64 bytes")
    else if pairs.exists(_._1.length != 31) then Left("a key is not 31 bytes")
    else if pairs.nonEmpty && cmp(pairs.head._1, start) < 0 then Left("a key is before the start of the range")
    else if pairs.indices.drop(1).exists(i => cmp(pairs(i - 1)._1, pairs(i)._1) >= 0) then
      Left("keys are not in strictly increasing order")
    else
      val nodes = boundaryNodes.iterator.map(b => JamBytes.wrap(lossy(Blake2b256.hash(b, 0, b.length))) -> b).toMap
      val last = pairs.lastOption.map(_._1)
      val walk = new Walk(start, last, nodes, pairs)
      if walk.check(root.toArrayUnsafe, lossyHash = false, depth = 0, 0, pairs.length, onStart = true, last.isDefined)
      then Right(())
      else Left("the pairs and boundary nodes do not match the state root")

  private def cmp(a: JamBytes, b: JamBytes): Int = JamBytes.compareUnsigned(a.toArrayUnsafe, b.toArrayUnsafe)

  private def lossy(hash: Array[Byte]): Array[Byte] =
    val out = hash.clone()
    out(0) = (out(0) & 0x7f).toByte
    out

  private def bit(key: JamBytes, position: Int): Boolean =
    (key(position / 8) & (1 << (7 - position % 8))) != 0

  private final class Walk(
      start: JamBytes,
      last: Option[JamBytes],
      nodes: Map[JamBytes, Array[Byte]],
      pairs: IndexedSeq[(JamBytes, JamBytes)]
  ):
    private val zero = new Array[Byte](32)

    private def same(expected: Array[Byte], lossyHash: Boolean, actual: Array[Byte]): Boolean =
      if lossyHash then java.util.Arrays.equals(lossy(expected), lossy(actual))
      else java.util.Arrays.equals(expected, actual)

    private def split(lo: Int, hi: Int, depth: Int): Int =
      var a = lo
      var b = hi
      while a < b do
        val m = (a + b) >>> 1
        if bit(pairs(m)._1, depth) then b = m else a = m + 1
      a

    private def subtree(lo: Int, hi: Int, depth: Int): Array[Byte] =
      if lo == hi then zero
      else if hi - lo == 1 then TrieNode.leaf(pairs(lo)._1, pairs(lo)._2).hash.toArrayUnsafe
      else
        val mid = split(lo, hi, depth)
        TrieNode.branchHash(subtree(lo, mid, depth + 1), subtree(mid, hi, depth + 1))

    def check(
        expected: Array[Byte],
        lossyHash: Boolean,
        depth: Int,
        lo: Int,
        hi: Int,
        onStart: Boolean,
        onLast: Boolean
    ): Boolean =
      if !onStart && !onLast then same(expected, lossyHash, subtree(lo, hi, depth))
      else
        nodes.get(JamBytes.wrap(lossy(expected))) match
          case None =>
            same(expected, lossyHash, zero) && lo == hi
          case Some(node) if !same(expected, lossyHash, Blake2b256.hash(node, 0, node.length)) => false
          case Some(node) if (node(0) & 0x80) == 0 => // branch
            depth < KeyBits && {
              val mid = split(lo, hi, depth)
              val startGoesRight = onStart && bit(start, depth)
              val lastGoesLeft = onLast && !bit(last.get, depth)
              val left =
                if startGoesRight then lo == mid // every key there is before the range
                else
                  check(
                    java.util.Arrays.copyOfRange(node, 0, 32),
                    lossyHash = true,
                    depth + 1,
                    lo,
                    mid,
                    onStart,
                    lastGoesLeft
                  )
              val right =
                if lastGoesLeft then mid == hi // every key there is after the last
                else
                  check(
                    java.util.Arrays.copyOfRange(node, 32, 64),
                    lossyHash = false,
                    depth + 1,
                    mid,
                    hi,
                    startGoesRight,
                    onLast
                  )
              left && right
            }
          case Some(node) => // leaf
            val key = JamBytes.wrap(java.util.Arrays.copyOfRange(node, 1, 32))
            val inRange = (!onStart || cmp(key, start) >= 0) && (!onLast || cmp(key, last.get) <= 0)
            if !inRange then lo == hi
            else
              hi - lo == 1 && pairs(lo)._1 == key &&
              java.util.Arrays.equals(TrieNode.leaf(key, pairs(lo)._2).gpEncoding, node)
