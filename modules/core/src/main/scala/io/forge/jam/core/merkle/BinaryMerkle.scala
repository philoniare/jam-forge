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

  val blake2b: Hasher = data => Hashing.blake2b256(data).bytes.toArray

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
    values.length match
      case 0 => ZeroHash
      case 1 => values.head
      case n =>
        val mid = (n + 1) / 2
        hashConcat(
          h,
          NodePrefix,
          node(values.slice(0, mid), h),
          node(values.slice(mid, n), h)
        )

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
    if values.length <= 1 then Nil
    else
      val mid = (values.length + 1) / 2
      val (opposite, into, nextIndex) =
        if index < mid then (values.slice(mid, values.length), values.slice(0, mid), index)
        else (values.slice(0, mid), values.slice(mid, values.length), index - mid)
      node(opposite, h) :: trace(into, nextIndex, h)

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
