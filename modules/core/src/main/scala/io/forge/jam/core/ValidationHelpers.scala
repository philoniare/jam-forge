package io.forge.jam.core

import io.forge.jam.core.JamBytes.compareUnsigned

/**
 * Shared validation utilities for STF implementations.
 */
object ValidationHelpers:

  /**
   * Check if a list is sorted in strictly ascending order with no duplicates.
   * Uses a custom comparison function.
   *
   * @param items The list to validate
   * @param cmp Comparison function returning negative if a < b, 0 if equal, positive if a > b
   * @return true if sorted and unique, false otherwise
   */
  def isSortedUnique[A](items: List[A])(cmp: (A, A) => Int): Boolean =
    items.sliding(2).forall {
      case List(curr, next) => cmp(curr, next) < 0
      case _ => true
    }

  /**
   * Check if a list is sorted in strictly ascending order with no duplicates.
   * Uses the default Ordering for the type.
   *
   * @param items The list to validate
   * @return true if sorted and unique, false otherwise
   */
  def isSortedUniqueBy[A, B: Ordering](items: List[A])(f: A => B): Boolean =
    val ordering = summon[Ordering[B]]
    items.sliding(2).forall {
      case List(curr, next) => ordering.lt(f(curr), f(next))
      case _ => true
    }

  /**
   * Check if a list is sorted in strictly ascending order by integer key.
   *
   * @param items The list to validate
   * @param key Function to extract the integer key
   * @return true if sorted and unique by key, false otherwise
   */
  def isSortedUniqueByInt[A](items: List[A])(key: A => Int): Boolean =
    items.sliding(2).forall {
      case List(curr, next) => key(curr) < key(next)
      case _ => true
    }

  /**
   * Check if a list is sorted in strictly ascending order by long key.
   *
   * @param items The list to validate
   * @param key Function to extract the long key
   * @return true if sorted and unique by key, false otherwise
   */
  def isSortedUniqueByLong[A](items: List[A])(key: A => Long): Boolean =
    items.sliding(2).forall {
      case List(curr, next) => key(curr) < key(next)
      case _ => true
    }

  /**
   * Check if a list is sorted in strictly ascending order by byte array key.
   * Uses unsigned lexicographic comparison.
   *
   * @param items The list to validate
   * @param key Function to extract the byte array key
   * @return true if sorted and unique by key, false otherwise
   */
  def isSortedUniqueByBytes[A](items: List[A])(key: A => Array[Byte]): Boolean =
    items.sliding(2).forall {
      case List(curr, next) => compareUnsigned(key(curr), key(next)) < 0
      case _ => true
    }

  /**
   * Check if a list is sorted in strictly ascending order by JamBytes key.
   * Uses unsigned lexicographic comparison.
   *
   * @param items The list to validate
   * @param key Function to extract the JamBytes key
   * @return true if sorted and unique by key, false otherwise
   */
  def isSortedUniqueByJamBytes[A](items: List[A])(key: A => JamBytes): Boolean =
    items.sliding(2).forall {
      case List(curr, next) => key(curr) < key(next)
      case _ => true
    }

  /**
   * Check if a list is sorted (non-strictly, allows duplicates) by a key.
   *
   * @param items The list to validate
   * @param key Function to extract the comparable key
   * @return true if sorted (with possible duplicates), false otherwise
   */
  def isSortedBy[A, B: Ordering](items: List[A])(key: A => B): Boolean =
    val ordering = summon[Ordering[B]]
    items.sliding(2).forall {
      case List(curr, next) => ordering.lteq(key(curr), key(next))
      case _ => true
    }
