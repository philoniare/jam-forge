package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash

/**
 * Flat open-addressing tables keyed by 32-byte hashes, used by [[StateTrie]] for its pending writes and by
 * [[InMemoryTrieBackend]] for its store. A slot's key, count, meta word and payload sit together in one `long[]`, so a
 * lookup touches one or two cache lines and a million entries cost the collector a few arrays, not millions of
 * objects. Not thread-safe.
 */
private[trie] object HashTables:
  private val LongLE: java.lang.invoke.VarHandle =
    java.lang.invoke.MethodHandles.byteArrayViewVarHandle(classOf[Array[Long]], java.nio.ByteOrder.LITTLE_ENDIAN)

  inline def word(a: Array[Byte], off: Int): Long =
    val w: Long = LongLE.get(a, off)
    w

  inline def putWord(a: Array[Byte], off: Int, w: Long): Unit = LongLE.set(a, off, w)

  /** A growable list of 32-byte hashes, four words each. */
  final class HashList:
    private var words = new Array[Long](64)
    var size: Int = 0

    def add(h: Array[Byte]): Unit =
      if (size + 1) * 4 > words.length then words = java.util.Arrays.copyOf(words, words.length * 2)
      var j = 0
      while j < 4 do
        words(size * 4 + j) = word(h, j * 8)
        j += 1
      size += 1

    def copyTo(i: Int, out: Array[Byte]): Unit =
      var j = 0
      while j < 4 do
        putWord(out, j * 8, words(i * 4 + j))
        j += 1

    def clear(): Unit =
      if words.length > 1024 then words = new Array[Long](64)
      size = 0

  // Slot layout, `stride` words per slot: the key's four words, the count, a meta word (1 key and count only, 2 + tag
  // for a slot holding a payload), then the subclass's payload words.
  inline val Ref = 4
  inline val Meta = 5
  inline val Head = 6
  /** Initial slots: small, since a StateTrie makes two tables per block and most blocks touch few nodes. */
  val SmallCapacity: Int = 1 << 5
  val LargeCapacity: Int = 1 << 10

  /**
   * Linear-probing table of 32-byte keys with a count and an optional payload per slot. Deletion shifts later entries
   * of the probe run back, so there are no tombstones. A slot index stays valid until the next insert or remove.
   *
   * Probing reads only `probe`, one word per slot (the key's first word, 0 for an empty slot), so a miss usually costs
   * one cache line; the full key and payload in `slots` are read only on a first-word match. The home slot is the top
   * bits of the first word: keys are Blake2b digests, so these are uniform, and slot order is key order at every
   * capacity, so copying one table into another in slot order sweeps the target's memory front to back.
   */
  abstract class Table(val stride: Int, initialCapacity: Int):
    protected var capacity: Int = initialCapacity
    private var shift = 64 - Integer.numberOfTrailingZeros(initialCapacity)
    private var probe: Array[Long] = new Array[Long](capacity)
    protected var slots: Array[Long] = new Array[Long](capacity * stride)
    private var count = 0
    var payloadCount: Int = 0

    protected def moveExtra(from: Int, to: Int): Unit = ()
    protected def clearExtra(slot: Int): Unit = ()

    /** After a resize: old slot `o` now lives at `moved(o)`, or was empty if that is -1. */
    protected def relocateExtra(oldCapacity: Int, moved: Array[Int]): Unit = ()

    private inline def mask: Int = capacity - 1

    private inline def home(k0: Long): Int = (k0 >>> shift).toInt

    // 0 marks an empty slot; a key whose first word is 0 probes as 1, which has the same home.
    private inline def probeWord(k0: Long): Long = if k0 == 0L then 1L else k0

    def size: Int = count

    /** Grows the table now, if needed, so that `n` more inserts will not resize it. */
    final def reserve(n: Int): Unit =
      var c = capacity
      while (count + n) * 4L > c * 3L do c *= 2
      if c != capacity then resize(c)

    /** The slot holding `h`, or -1. */
    final def find(h: Array[Byte]): Int =
      findWords(word(h, 0), word(h, 8), word(h, 16), word(h, 24))

    final def findWords(k0: Long, k1: Long, k2: Long, k3: Long): Int =
      val p = probe
      val s = slots
      val w = probeWord(k0)
      var i = home(k0)
      var pw = p(i)
      while pw != 0L do
        if pw == w then
          val b = i * stride
          if s(b) == k0 && s(b + 1) == k1 && s(b + 2) == k2 && s(b + 3) == k3 then return i
        i = (i + 1) & mask
        pw = p(i)
      -1

    /** The slot holding `h`, inserting it with count 0 and no payload if absent. */
    final def findOrInsert(h: Array[Byte]): Int =
      findOrInsertWords(word(h, 0), word(h, 8), word(h, 16), word(h, 24))

    final def findOrInsertWords(k0: Long, k1: Long, k2: Long, k3: Long): Int =
      if (count + 1) * 4 > capacity * 3 then resize(capacity * 2)
      val p = probe
      val s = slots
      val w = probeWord(k0)
      var i = home(k0)
      var pw = p(i)
      while pw != 0L do
        if pw == w then
          val b = i * stride
          if s(b) == k0 && s(b + 1) == k1 && s(b + 2) == k2 && s(b + 3) == k3 then return i
        i = (i + 1) & mask
        pw = p(i)
      p(i) = w
      val b = i * stride
      s(b) = k0
      s(b + 1) = k1
      s(b + 2) = k2
      s(b + 3) = k3
      s(b + Ref) = 0L
      s(b + Meta) = 1L
      count += 1
      i

    /** Whether `slot` is in use; for iterating slots `0 until slotCount`. */
    final def occupied(slot: Int): Boolean = probe(slot) != 0L
    final def slotCount: Int = capacity

    final def ref(slot: Int): Long = slots(slot * stride + Ref)

    final def setRef(slot: Int, c: Long): Unit = slots(slot * stride + Ref) = c

    final def addRef(slot: Int, d: Long): Long =
      val c = slots(slot * stride + Ref) + d
      slots(slot * stride + Ref) = c
      c

    final def keyWord(slot: Int, j: Int): Long = slots(slot * stride + j)

    final def keyHash(slot: Int): Hash =
      val out = new Array[Byte](32)
      val b = slot * stride
      var j = 0
      while j < 4 do
        putWord(out, j * 8, slots(b + j))
        j += 1
      Hash.wrapUnchecked(out)

    final def hasPayload(slot: Int): Boolean = slots(slot * stride + Meta) >= 2
    protected final def meta(slot: Int): Long = slots(slot * stride + Meta)

    protected final def setMeta(slot: Int, m: Long): Unit =
      val b = slot * stride + Meta
      if m >= 2 && slots(b) < 2 then payloadCount += 1
      slots(b) = m

    final def remove(slot: Int): Unit =
      if meta(slot) >= 2 then payloadCount -= 1
      var hole = slot
      var i = (slot + 1) & mask
      while probe(i) != 0L do
        val h = home(probe(i))
        // Entry i stays unless the hole lies cyclically between its home and i.
        val stays = if hole <= i then hole < h && h <= i else hole < h || h <= i
        if !stays then
          probe(hole) = probe(i)
          System.arraycopy(slots, i * stride, slots, hole * stride, stride)
          moveExtra(i, hole)
          hole = i
        i = (i + 1) & mask
      probe(hole) = 0L
      java.util.Arrays.fill(slots, hole * stride, hole * stride + stride, 0L)
      clearExtra(hole)
      count -= 1

    private def resize(newCapacity: Int): Unit =
      val oldCapacity = capacity
      val oldProbe = probe
      val old = slots
      capacity = newCapacity
      shift = 64 - Integer.numberOfTrailingZeros(capacity)
      probe = new Array[Long](capacity)
      slots = new Array[Long](capacity * stride)
      val moved = new Array[Int](oldCapacity)
      var o = 0
      while o < oldCapacity do
        val w = oldProbe(o)
        if w != 0L then
          var i = home(w)
          while probe(i) != 0L do i = (i + 1) & mask
          probe(i) = w
          System.arraycopy(old, o * stride, slots, i * stride, stride)
          moved(o) = i
        else moved(o) = -1
        o += 1
      relocateExtra(oldCapacity, moved)

    /** Empties the table, keeping its arrays unless they have grown large. */
    final def clear(): Unit =
      if capacity > (1 << 16) then
        capacity = initialCapacity
        shift = 64 - Integer.numberOfTrailingZeros(capacity)
        probe = new Array[Long](capacity)
        slots = new Array[Long](capacity * stride)
      else
        java.util.Arrays.fill(probe, 0L)
        java.util.Arrays.fill(slots, 0L)
      relocateExtra(0, new Array[Int](0))
      count = 0
      payloadCount = 0

    protected final def base(slot: Int): Int = slot * stride

  /** Payload: a trie node's 64 bytes as eight words; the meta word carries its tag. */
  final class NodeTable(initialCapacity: Int = SmallCapacity) extends Table(Head + 8, initialCapacity):
    def setNode(slot: Int, node: TrieNode): Unit =
      setMeta(slot, 2L + node.nodeType.tag)
      val l = node.left.toArrayUnsafe
      val r = node.right.toArrayUnsafe
      val b = base(slot) + Head
      var j = 0
      while j < 4 do
        slots(b + j) = word(l, j * 8)
        slots(b + 4 + j) = word(r, j * 8)
        j += 1

    def setBranch(slot: Int, left: Array[Byte], right: Array[Byte]): Unit =
      setMeta(slot, 2L + TrieNodeType.Branch.tag)
      val b = base(slot) + Head
      var j = 0
      while j < 4 do
        slots(b + j) = word(left, j * 8)
        slots(b + 4 + j) = word(right, j * 8)
        j += 1

    /** Copies the payload of `from`'s slot `fromSlot` into slot `slot`. */
    def setNodeFrom(slot: Int, from: NodeTable, fromSlot: Int): Unit =
      setMeta(slot, from.meta(fromSlot))
      System.arraycopy(from.slots, from.base(fromSlot) + Head, slots, base(slot) + Head, 8)

    /** Drops the payload, keeping the key and count. */
    def clearNode(slot: Int): Unit =
      val b = slot * stride + Meta
      if slots(b) >= 2 then payloadCount -= 1
      slots(b) = 1L

    /** The node at `slot`, whose key is `hash`, or null if the slot holds only a count. */
    def node(slot: Int, hash: Hash): TrieNode | Null =
      val m = meta(slot)
      if m < 2 then null
      else
        val l = new Array[Byte](32)
        val r = new Array[Byte](32)
        val b = base(slot) + Head
        var j = 0
        while j < 4 do
          putWord(l, j * 8, slots(b + j))
          putWord(r, j * 8, slots(b + 4 + j))
          j += 1
        TrieNode.withHash(TrieNodeType.fromTag((m - 2).toByte).get, JamBytes.wrap(l), JamBytes.wrap(r), hash)

    def isRegularLeaf(slot: Int): Boolean = meta(slot) == 2L + TrieNodeType.RegularLeaf.tag

    def copyRight(slot: Int, out: Array[Byte]): Unit =
      val b = base(slot) + Head + 4
      var j = 0
      while j < 4 do
        putWord(out, j * 8, slots(b + j))
        j += 1

  /** Payload: a raw value, kept beside the table at the same slot index. */
  final class ValueTable(initialCapacity: Int = SmallCapacity) extends Table(Head, initialCapacity):
    private var vals: Array[Array[Byte] | Null] = new Array[Array[Byte] | Null](capacity)

    override protected def moveExtra(from: Int, to: Int): Unit = vals(to) = vals(from)
    override protected def clearExtra(slot: Int): Unit = vals(slot) = null
    override protected def relocateExtra(oldCapacity: Int, moved: Array[Int]): Unit =
      val oldVals = vals
      vals = new Array[Array[Byte] | Null](capacity)
      var o = 0
      while o < oldCapacity do
        if moved(o) >= 0 then vals(moved(o)) = oldVals(o)
        o += 1

    def setValue(slot: Int, v: Array[Byte]): Unit =
      setMeta(slot, 2L)
      vals(slot) = v

    def value(slot: Int): Array[Byte] | Null = vals(slot)
