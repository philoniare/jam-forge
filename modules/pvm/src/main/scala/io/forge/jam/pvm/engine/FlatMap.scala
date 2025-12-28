package io.forge.jam.pvm.engine

import scala.annotation.targetName
import spire.math.UInt

/**
 * A specialized map implementation optimized for flat, dense numeric keys.
 *
 * Provides O(1) lookup, insert, and access by using a pre-sized array.
 * Ideal for mapping program counters to compiled handler offsets where
 * keys are dense and bounded.
 *
 * @tparam T The type of values stored in the map
 */
final class FlatMap[T >: Null <: AnyRef] private (
  private val inner: Array[T]
):

  /**
   * Gets a value for the given key.
   *
   * @param key The key to look up
   * @return Some(value) if present, None if not set
   */
  def get(key: UInt): Option[T] =
    val idx = key.signed
    if idx >= 0 && idx < inner.length then
      Option(inner(idx))
    else
      None

  /**
   * Gets a value for the given key, returning null if not present.
   *
   * @param key The key to look up
   * @return The value if present, null otherwise
   */
  def getOrNull(key: UInt): T =
    val idx = key.signed
    if idx >= 0 && idx < inner.length then
      inner(idx)
    else
      null.asInstanceOf[T]

  /**
   * Returns the capacity of the map.
   *
   * @return The number of slots in the map
   */
  def len: UInt = UInt(inner.length)

  /**
   * Inserts a value at the specified key.
   *
   * @param key The key to insert at
   * @param value The value to insert
   */
  def insert(key: UInt, value: T): Unit =
    val idx = key.signed
    if idx >= 0 && idx < inner.length then
      inner(idx) = value

  /**
   * Clears all elements from the map by setting them to null.
   */
  def clear(): Unit =
    java.util.Arrays.fill(inner.asInstanceOf[Array[AnyRef]], null)

  /**
   * Checks if a key has a value.
   *
   * @param key The key to check
   * @return True if the key has a non-null value
   */
  def contains(key: UInt): Boolean =
    val idx = key.signed
    idx >= 0 && idx < inner.length && inner(idx) != null

object FlatMap:
  /**
   * Creates a new FlatMap with the specified capacity (UInt).
   *
   * @param capacity The number of slots to allocate
   * @return A new empty FlatMap
   */
  def create[T >: Null <: AnyRef: scala.reflect.ClassTag](capacity: UInt): FlatMap[T] =
    new FlatMap[T](new Array[T](capacity.signed))

  /**
   * Creates a new FlatMap with the specified capacity (Int).
   *
   * @param capacity The number of slots to allocate
   * @return A new empty FlatMap
   */
  @targetName("createFromInt")
  def createInt[T >: Null <: AnyRef: scala.reflect.ClassTag](capacity: Int): FlatMap[T] =
    new FlatMap[T](new Array[T](capacity))

  /**
   * Creates a new FlatMap reusing the existing memory.
   *
   * @param memory The existing FlatMap to reuse
   * @param capacity The new capacity
   * @return The resized FlatMap
   */
  def reuseMemory[T >: Null <: AnyRef: scala.reflect.ClassTag](
    memory: FlatMap[T],
    capacity: UInt
  ): FlatMap[T] =
    memory.clear()
    if memory.inner.length >= capacity.signed then
      memory
    else
      new FlatMap[T](new Array[T](capacity.signed))
