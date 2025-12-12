package io.forge.jam.core

import scala.collection.mutable.ArrayBuffer

/**
 * A mutable list of JamBytes items with value semantics for list operations.
 * 
 * It provides:
 * - Content-based equality comparisons using JamBytes semantics
 * - Clone-on-access to ensure immutability of items
 * - JAM codec serialization with compact integer length prefix
 * 
 * Unlike standard Scala collections, this class ensures that:
 * - Items are cloned when added or retrieved (defensive copying)
 * - Contains/indexOf use content-based comparison
 */
final class JamBytesList private (private val items: ArrayBuffer[JamBytes]) extends Cloneable:
  
  // ══════════════════════════════════════════════════════════════════════════
  // Size operations
  // ══════════════════════════════════════════════════════════════════════════
  
  /** Number of items in the list */
  def size: Int = items.size
  
  /** Alias for size */
  def length: Int = items.length
  
  /** Check if empty */
  def isEmpty: Boolean = items.isEmpty
  
  /** Check if non-empty */
  def nonEmpty: Boolean = items.nonEmpty
  
  // ══════════════════════════════════════════════════════════════════════════
  // Access operations (clone-on-read)
  // ══════════════════════════════════════════════════════════════════════════
  
  /** Get item at index (returns a clone) */
  def apply(index: Int): JamBytes = JamBytes(items(index).toArray)
  
  /** Get item at index as Option (returns a clone if exists) */
  def get(index: Int): Option[JamBytes] =
    if index >= 0 && index < items.size then Some(JamBytes(items(index).toArray))
    else None
  
  /** Get first item (clone) */
  def head: JamBytes = JamBytes(items.head.toArray)
  
  /** Get first item as Option (clone) */
  def headOption: Option[JamBytes] = items.headOption.map(b => JamBytes(b.toArray))
  
  /** Get last item (clone) */
  def last: JamBytes = JamBytes(items.last.toArray)
  
  /** Get last item as Option (clone) */
  def lastOption: Option[JamBytes] = items.lastOption.map(b => JamBytes(b.toArray))
  
  // ══════════════════════════════════════════════════════════════════════════
  // Mutation operations (clone-on-write)
  // ══════════════════════════════════════════════════════════════════════════
  
  /** Add an item to the end (stores a clone) */
  def add(element: JamBytes): Boolean =
    items += JamBytes(element.toArray)
    true
  
  /** Append operator */
  def +=(element: JamBytes): this.type =
    items += JamBytes(element.toArray)
    this
  
  /** Add an item at a specific index (stores a clone) */
  def add(index: Int, element: JamBytes): Unit =
    items.insert(index, JamBytes(element.toArray))
  
  /** Add all items from another collection (clones each) */
  def addAll(elements: IterableOnce[JamBytes]): Unit =
    elements.iterator.foreach(e => items += JamBytes(e.toArray))
  
  /** Remove item at index */
  def removeAt(index: Int): JamBytes =
    items.remove(index)
  
  /** Set item at index (stores a clone, returns old value) */
  def update(index: Int, element: JamBytes): JamBytes =
    val old = items(index)
    items(index) = JamBytes(element.toArray)
    old
  
  /** Clear all items */
  def clear(): Unit = items.clear()
  
  // ══════════════════════════════════════════════════════════════════════════
  // Search operations (content-based comparison)
  // ══════════════════════════════════════════════════════════════════════════
  
  /** Check if contains element using content comparison */
  def contains(element: JamBytes): Boolean =
    items.exists(_ == element)
  
  /** Find index of element using content comparison */
  def indexOf(element: JamBytes): Int =
    items.indexWhere(_ == element)
  
  /** Find last index of element using content comparison */
  def lastIndexOf(element: JamBytes): Int =
    items.lastIndexWhere(_ == element)
  
  // ══════════════════════════════════════════════════════════════════════════
  // Iteration
  // ══════════════════════════════════════════════════════════════════════════
  
  /** Iterator over cloned items */
  def iterator: Iterator[JamBytes] =
    items.iterator.map(b => JamBytes(b.toArray))
  
  /** Apply function to each item */
  def foreach[U](f: JamBytes => U): Unit =
    items.foreach(b => f(JamBytes(b.toArray)))
  
  /** Map over items */
  def map[B](f: JamBytes => B): Seq[B] =
    items.map(b => f(JamBytes(b.toArray))).toSeq
  
  /** Fold left over items */
  def foldLeft[B](z: B)(op: (B, JamBytes) => B): B =
    items.foldLeft(z)((acc, b) => op(acc, JamBytes(b.toArray)))
  
  // ══════════════════════════════════════════════════════════════════════════
  // Conversion
  // ══════════════════════════════════════════════════════════════════════════
  
  /** Convert to immutable list (clones all items) */
  def toList: List[JamBytes] = items.map(b => JamBytes(b.toArray)).toList
  
  /** Convert to immutable Seq (clones all items) */
  def toSeq: Seq[JamBytes] = items.map(b => JamBytes(b.toArray)).toSeq
  
  /** Convert to Vector (clones all items) */
  def toVector: Vector[JamBytes] = items.map(b => JamBytes(b.toArray)).toVector
  
  // ══════════════════════════════════════════════════════════════════════════
  // Cloning
  // ══════════════════════════════════════════════════════════════════════════
  
  /** Create a deep copy of this list */
  override def clone(): JamBytesList =
    val clonedItems = ArrayBuffer.from(items.map(b => JamBytes(b.toArray)))
    new JamBytesList(clonedItems)
  
  // ══════════════════════════════════════════════════════════════════════════
  // Encoding (JAM codec)
  // ══════════════════════════════════════════════════════════════════════════
  
  /**
   * Encode this list with compact integer length prefix.
   * Format: [compact-length][item0][item1]...
   */
  def encode(): Array[Byte] =
    val builder = JamBytes.newBuilder
    builder ++= codec.encodeCompactInteger(size)
    items.foreach(item => builder ++= item.toArray)
    builder.result().toArray
  
  /**
   * Encode without length prefix (for fixed-size arrays).
   */
  def encodeWithoutLength(): Array[Byte] =
    val builder = JamBytes.newBuilder
    items.foreach(item => builder ++= item.toArray)
    builder.result().toArray
  
  // ══════════════════════════════════════════════════════════════════════════
  // Equality (content-based)
  // ══════════════════════════════════════════════════════════════════════════
  
  override def equals(obj: Any): Boolean = obj match
    case that: JamBytesList =>
      this.size == that.size && 
        items.indices.forall(i => items(i) == that.items(i))
    case _ => false
  
  override def hashCode(): Int =
    items.foldLeft(1)((acc, item) => 31 * acc + item.hashCode())
  
  override def toString: String =
    val itemsStr = items.take(5).map(_.toString).mkString(", ")
    val suffix = if items.size > 5 then s", ... (${items.size - 5} more)" else ""
    s"JamBytesList(size=$size, items=[$itemsStr$suffix])"


object JamBytesList:
  
  /** Create an empty list */
  def empty: JamBytesList = new JamBytesList(ArrayBuffer.empty)
  
  /** Create from varargs (clones each) */
  def apply(items: JamBytes*): JamBytesList =
    val buffer = ArrayBuffer.from(items.map(b => JamBytes(b.toArray)))
    new JamBytesList(buffer)
  
  /** Create from a sequence (clones each) */
  def fromSeq(items: Seq[JamBytes]): JamBytesList =
    val buffer = ArrayBuffer.from(items.map(b => JamBytes(b.toArray)))
    new JamBytesList(buffer)
  
  /** Create from an iterable (clones each) */
  def from(items: IterableOnce[JamBytes]): JamBytesList =
    val buffer = ArrayBuffer.empty[JamBytes]
    items.iterator.foreach(b => buffer += JamBytes(b.toArray))
    new JamBytesList(buffer)
  
  /**
   * Decode a JamBytesList from bytes.
   * 
   * @param data     The byte array to decode from
   * @param offset   Starting offset in the byte array
   * @param itemSize Size of each item in bytes (default 32 for hashes)
   * @return Tuple of (decoded list, bytes consumed)
   */
  def fromBytes(data: Array[Byte], offset: Int = 0, itemSize: Int = 32): (JamBytesList, Int) =
    var currentOffset = offset
    val (length, lengthBytes) = codec.decodeCompactInteger(data, currentOffset)
    currentOffset += lengthBytes
    
    val list = JamBytesList.empty
    for _ <- 0 until length.toInt do
      val bytes = java.util.Arrays.copyOfRange(data, currentOffset, currentOffset + itemSize)
      list.add(JamBytes(bytes))
      currentOffset += itemSize
    
    (list, currentOffset - offset)
  
  /**
   * Decode from bytes using a custom item decoder.
   * 
   * @param data    The byte array to decode from
   * @param offset  Starting offset in the byte array
   * @param decoder Function that decodes one item, returns (JamBytes, bytesConsumed)
   * @return Tuple of (decoded list, bytes consumed)
   */
  def fromBytesWithDecoder(
    data: Array[Byte], 
    offset: Int,
    decoder: (Array[Byte], Int) => (JamBytes, Int)
  ): (JamBytesList, Int) =
    var currentOffset = offset
    val (length, lengthBytes) = codec.decodeCompactInteger(data, currentOffset)
    currentOffset += lengthBytes
    
    val list = JamBytesList.empty
    for _ <- 0 until length.toInt do
      val (item, itemBytes) = decoder(data, currentOffset)
      list.add(item)
      currentOffset += itemBytes
    
    (list, currentOffset - offset)
