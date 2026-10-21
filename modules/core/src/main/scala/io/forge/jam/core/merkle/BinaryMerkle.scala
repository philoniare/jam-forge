package io.forge.jam.core.merkle

import io.forge.jam.core.Hashing

/** General binary Merklization: the node function N, trace function T, well-balanced root
  * (merklizewb), constant-depth root (merklizecd) with its subtree-page
  * justification helpers, all over blake2b-256 by default.
  */
object BinaryMerkle:

  type Hasher = Array[Byte] => Array[Byte]

  val ZeroHash: Array[Byte] = new Array[Byte](32)
  private val NodePrefix = "node".getBytes("UTF-8")
  private val LeafPrefix = "leaf".getBytes("UTF-8")

  // The digest's own array: the Hash wrapping it is dropped here, so nothing else holds it.
  val blake2b: Hasher = data => Hashing.blake2b256(data).toArrayUnsafe

  private def hashConcat(h: Hasher, parts: Array[Byte]*): Array[Byte] =
    val total = parts.map(_.length).sum
    val buf = new Array[Byte](total)
    var off = 0
    parts.foreach { p =>
      System.arraycopy(p, 0, buf, off, p.length)
      off += p.length
    }
    h(buf)

  /** Node function N: zero hash for empty, the sole value
    * for singletons, else H("node" ++ N(left) ++ N(right)) with the split at
    * ceil(n/2).
    */
  def node(values: IndexedSeq[Array[Byte]], h: Hasher = blake2b): Array[Byte] =
    nodeOf(values, 0, values.length, h)

  /** N over `values(lo until hi)`, without slicing. */
  private def nodeOf(values: IndexedSeq[Array[Byte]], lo: Int, hi: Int, h: Hasher): Array[Byte] =
    hi - lo match
      case 0 => ZeroHash
      case 1 => values(lo)
      case n =>
        val mid = lo + (n + 1) / 2
        hashConcat(h, NodePrefix, nodeOf(values, lo, mid, h), nodeOf(values, mid, hi, h))

  /** Well-balanced binary Merkle root, merklizewb:
    * H(v0) for singletons, N(v) otherwise (zero hash for empty).
    */
  def merklizeWB(values: IndexedSeq[Array[Byte]], h: Hasher = blake2b): Array[Byte] =
    if values.length == 1 then h(values.head)
    else node(values, h)

  /** Trace function T: the sequence of opposite (sibling) nodes from the root
    * down to the leaf at `index`. Entries are either raw values (at the
    * bottom) or hashes.
    */
  def trace(
      values: IndexedSeq[Array[Byte]],
      index: Int,
      h: Hasher = blake2b
  ): List[Array[Byte]] =
    val out = List.newBuilder[Array[Byte]]
    var lo = 0
    var hi = values.length
    while hi - lo > 1 do
      val mid = lo + (hi - lo + 1) / 2
      if index < mid then
        out += nodeOf(values, mid, hi, h)
        hi = mid
      else
        out += nodeOf(values, lo, mid, h)
        lo = mid
    out.result()

  /**
   * N over `values`, with the value of every subtree kept, so its root and all its traces T cost one hash per inner
   * node in total. Calling [[trace]] for each of n leaves instead rehashes about n nodes per call.
   */
  final class Tree(values: IndexedSeq[Array[Byte]], h: Hasher = blake2b):
    // Children are null at the leaves.
    private final class Node(val value: Array[Byte], val left: Node, val right: Node)

    private def build(lo: Int, hi: Int): Node =
      hi - lo match
        case 0 => Node(ZeroHash, null, null)
        case 1 => Node(values(lo), null, null)
        case n =>
          val mid = lo + (n + 1) / 2
          val l = build(lo, mid)
          val r = build(mid, hi)
          Node(hashConcat(h, NodePrefix, l.value, r.value), l, r)

    private val top = build(0, values.length)

    /** N(values). */
    def root: Array[Byte] = top.value

    /** T(values, index): the sibling of each node from the root down to leaf `index`. */
    def trace(index: Int): List[Array[Byte]] =
      val out = List.newBuilder[Array[Byte]]
      var node = top
      var count = values.length
      var i = index
      while count > 1 do
        val mid = (count + 1) / 2
        val l = node.left
        val r = node.right
        if i < mid then
          out += r.value
          node = l
          count = mid
        else
          out += l.value
          node = r
          i -= mid
          count -= mid
      out.result()

  /** Verify a trace T(values, index) against a well-balanced root: recompute
    * the root from `leaf` (the raw value at `index` among `leafCount` leaves)
    * and the top-down sibling `path`, and compare
    */
  def verifyTrace(
      root: Array[Byte],
      leaf: Array[Byte],
      index: Int,
      leafCount: Int,
      path: List[Array[Byte]],
      h: Hasher = blake2b
  ): Boolean =
    if leafCount <= 1 || index < 0 || index >= leafCount then false
    else
      def recompute(idx: Int, count: Int, p: List[Array[Byte]]): Option[Array[Byte]] =
        if count == 1 then if p.isEmpty then Some(leaf) else None
        else
          p match
            case Nil => None
            case sibling :: rest =>
              val mid = (count + 1) / 2
              if idx < mid then
                recompute(idx, mid, rest).map(own => hashConcat(h, NodePrefix, own, sibling))
              else
                recompute(idx - mid, count - mid, rest).map(own =>
                  hashConcat(h, NodePrefix, sibling, own)
                )
      recompute(index, leafCount, path).exists(java.util.Arrays.equals(_, root))

  /** Constancy preprocessor C: hash each item with the "leaf" prefix and pad
    * with zero hashes to the next power of two (minimum 1).
    */
  def constancyPreprocess(
      values: IndexedSeq[Array[Byte]],
      h: Hasher = blake2b
  ): IndexedSeq[Array[Byte]] =
    val n = math.max(1, values.length)
    var size = 1
    while size < n do size <<= 1
    val out = new Array[Array[Byte]](size)
    var i = 0
    while i < values.length do
      out(i) = hashConcat(h, LeafPrefix, values(i))
      i += 1
    while i < size do
      out(i) = ZeroHash
      i += 1
    scala.collection.immutable.ArraySeq.unsafeWrapArray(out)

  /** Constant-depth binary Merkle root, merklizecd
    */
  def merklizeCD(values: IndexedSeq[Array[Byte]], h: Hasher = blake2b): Array[Byte] =
    node(constancyPreprocess(values, h), h)

  /** Merkle path justifying the subtree page of size 2^x containing leaf page
    * `pageIndex`: the first
    * max(0, ceil(log2(max(1, n))) - x) trace entries of T(C(v), 2^x·i).
    */
  def justifySubPath(
      values: IndexedSeq[Array[Byte]],
      x: Int,
      pageIndex: Int,
      h: Hasher = blake2b
  ): List[Array[Byte]] =
    val full = trace(constancyPreprocess(values, h), (1 << x) * pageIndex, h)
    full.take(pathLength(values.length, x))

  /**
   * For every 2^x-leaf page of `values` in order, its [[justifySubPath]] and its [[subtreePage]], from one
   * preprocessing and one tree: each segment is hashed once, not once per page.
   */
  def pagedJustifications(
      values: IndexedSeq[Array[Byte]],
      x: Int,
      h: Hasher = blake2b
  ): IndexedSeq[(List[Array[Byte]], IndexedSeq[Array[Byte]])] =
    val pageSize = 1 << x
    val leaves = constancyPreprocess(values, h)
    val tree = Tree(leaves, h)
    val depth = pathLength(values.length, x)
    (0 until (values.length + pageSize - 1) / pageSize).map { i =>
      val start = i * pageSize
      (tree.trace(start).take(depth), leaves.slice(start, math.min(values.length, start + pageSize)))
    }

  /** ceil(log2(max(1, n))) - x, floored at 0: the number of path elements
    * from the root to a 2^x-leaf subtree.
    */
  def pathLength(leafCount: Int, x: Int): Int =
    val n = math.max(1, leafCount)
    var log2 = 0
    while (1 << log2) < n do log2 += 1
    math.max(0, log2 - x)

  /** The 2^x-sized page of prefixed leaf hashes at `pageIndex`
    * Unpadded: the final page may be shorter.
    */
  def subtreePage(
      values: IndexedSeq[Array[Byte]],
      x: Int,
      pageIndex: Int,
      h: Hasher = blake2b
  ): IndexedSeq[Array[Byte]] =
    val pageSize = 1 << x
    val start = pageIndex * pageSize
    val end = math.min(values.length, start + pageSize)
    (start until end).map(i => hashConcat(h, LeafPrefix, values(i)))
