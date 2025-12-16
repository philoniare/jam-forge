package io.forge.jam.core

import primitives.Hash
import spire.math.UInt

/**
 * Shuffle algorithm for JAM protocol.
 *
 * Implements the Fisher-Yates shuffle with Blake2b-derived entropy
 */
object Shuffle:

  /**
   * Compute a JAM shuffle of a sequence [0, size) using the given entropy.
   *
   * @param size The size of the sequence to shuffle
   * @param entropy The 32-byte entropy source
   * @return A shuffled list of indices [0, size)
   */
  def jamComputeShuffle(size: Int, entropy: Hash): List[Int] =
    if size == 0 then List.empty
    else
      val sequence = (0 until size).toList
      val entropyNumbers = computeQ(entropy, size)
      computeShuffleEq329(sequence, entropyNumbers)

  /**
   * Fisher-Yates shuffle implementation.
   *
   * The algorithm picks a random element from the remaining sequence,
   * moves it to the front, and recursively shuffles the rest.
   */
  private def computeShuffleEq329(sequence: List[Int], entropyNumbers: List[Int]): List[Int] =
    if sequence.isEmpty then List.empty
    else
      val currentSize = sequence.size
      // Use unsigned modulo to select index
      val index = ((entropyNumbers.head.toLong & 0xFFFFFFFFL) % currentSize).toInt
      val head = sequence(index)

      // Replace selected element with the last element, then take all but last
      val sequenceArray = sequence.toArray
      sequenceArray(index) = sequenceArray(currentSize - 1)
      val remaining = sequenceArray.take(currentSize - 1).toList

      head :: computeShuffleEq329(remaining, entropyNumbers.tail)

  /**
   * Compute the Q function for generating random numbers from entropy.
   */
  private def computeQ(entropy: Hash, length: Int): List[Int] =
    (0 until length).map { i =>
      val counter = i / 8
      val counterBytes = intToLeBytes(counter)
      val preimage = JamBytes(entropy.bytes) ++ JamBytes(counterBytes)

      val hashed = Hashing.blake2b256(preimage)
      val offset = (4 * i) % 32
      fromLeBytesInt(hashed.bytes, offset)
    }.toList

  /**
   * Convert an integer to 4 bytes in little-endian format.
   */
  private def intToLeBytes(value: Int): Array[Byte] =
    Array(
      (value & 0xFF).toByte,
      ((value >> 8) & 0xFF).toByte,
      ((value >> 16) & 0xFF).toByte,
      ((value >> 24) & 0xFF).toByte
    )

  /**
   * Convert 4 bytes from little-endian format to Int.
   */
  private def fromLeBytesInt(bytes: Array[Byte], offset: Int): Int =
    (bytes(offset) & 0xFF) |
    ((bytes(offset + 1) & 0xFF) << 8) |
    ((bytes(offset + 2) & 0xFF) << 16) |
    ((bytes(offset + 3) & 0xFF) << 24)
