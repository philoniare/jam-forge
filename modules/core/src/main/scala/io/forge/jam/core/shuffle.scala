package io.forge.jam.core

import primitives.Hash

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
      jamComputeShuffleInPlace(size, entropy).toList

  /**
   * Optimized shuffle implementation matching the original algorithm.
   * Uses arrays instead of Lists to avoid O(n) allocations per iteration.
   *
   * Original algorithm (recursive):
   *   1. Pick index from [0, currentSize) using entropy
   *   2. The element at that index becomes the next output element
   *   3. Replace picked element with last element of remaining range
   *   4. Shrink the range and repeat
   *
   * @param size The size of the sequence to shuffle
   * @param entropy The 32-byte entropy source
   * @return A shuffled array of indices [0, size)
   */
  def jamComputeShuffleInPlace(size: Int, entropy: Hash): Array[Int] =
    if size == 0 then return Array.empty

    // Work array for the shuffle - elements get consumed from the back
    val sequence = Array.tabulate(size)(identity)
    val output = new Array[Int](size)
    val entropyNumbers = computeQArray(entropy, size)

    // Fisher-Yates variant: pick from shrinking range, output to front
    var remainingSize = size
    var i = 0
    while i < size do
      val randIndex = ((entropyNumbers(i).toLong & 0xFFFFFFFFL) % remainingSize).toInt
      // The picked element becomes output[i]
      output(i) = sequence(randIndex)
      // Replace picked element with the last element of remaining range
      sequence(randIndex) = sequence(remainingSize - 1)
      remainingSize -= 1
      i += 1

    output

  /**
   * Compute the Q function for generating random numbers from entropy.
   * Optimized version that returns Array[Int] and caches hash computations.
   */
  private def computeQArray(entropy: Hash, length: Int): Array[Int] =
    val result = new Array[Int](length)
    val entropyBytes = entropy.bytes
    var lastCounter = -1
    var cachedHash: Array[Byte] = null

    var i = 0
    while i < length do
      val counter = i / 8
      if counter != lastCounter then
        val counterBytes = intToLeBytes(counter)
        val preimage = JamBytes(entropyBytes) ++ JamBytes(counterBytes)
        cachedHash = Hashing.blake2b256(preimage).bytes
        lastCounter = counter
      val offset = (4 * i) % 32
      result(i) = fromLeBytesInt(cachedHash, offset)
      i += 1

    result

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
