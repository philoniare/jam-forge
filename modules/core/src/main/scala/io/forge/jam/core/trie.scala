package io.forge.jam.core

import primitives.Hash

/**
 * Merkle Trie implementation for JAM protocol.
 */
object MerkleTrie:

  /**
   * Compute the Merkle root of a key-value mapping.
   *
   * @param kvs The key-value pairs where keys are 32-byte hashes
   * @return The 32-byte Merkle root hash
   */
  def merkle(kvs: Map[JamBytes, JamBytes]): Hash =
    val kvsList = kvs.toList
    merkleInternal(kvsList, 0)

  /**
   * Internal recursive Merkle computation.
   *
   * @param kvs The list of key-value pairs
   * @param i The current bit position for splitting
   * @return The 32-byte hash of this subtrie
   */
  private def merkleInternal(kvs: List[(JamBytes, JamBytes)], i: Int): Hash =
    if kvs.isEmpty then
      // Empty trie returns 32 zero bytes
      Hash.zero
    else if kvs.size == 1 then
      // Single entry: encode as leaf, then hash
      val (key, value) = kvs.head
      val encoded = encodeLeaf(key, value)
      Hashing.blake2b256(encoded)
    else
      // Multiple entries: split into left/right based on bit at position i
      val (left, right) = kvs.partition { case (key, _) => !bit(key, i) }
      val leftHash = merkleInternal(left, i + 1)
      val rightHash = merkleInternal(right, i + 1)
      val encoded = encodeBranch(leftHash, rightHash)
      Hashing.blake2b256(encoded)

  /**
   * Get the bit at position i in the key.
   */
  private def bit(key: JamBytes, i: Int): Boolean =
    val byteIndex = i >> 3
    val bitIndex = 7 - (i & 7)
    (key.signedAt(byteIndex) & (1 << bitIndex)) != 0

  /**
   * Encode a branch node
   */
  private def encodeBranch(left: Hash, right: Hash): JamBytes =
    val leftBytes = left.bytes
    val rightBytes = right.bytes

    val result = new Array[Byte](64)
    // head = left[0] with MSB cleared (& 0x7F)
    result(0) = (leftBytes(0) & 0x7F).toByte
    // left[1:32] - remaining 31 bytes of left hash
    System.arraycopy(leftBytes, 1, result, 1, 31)
    // right[0:32] - full 32 bytes of right hash
    System.arraycopy(rightBytes, 0, result, 32, 32)

    JamBytes.wrap(result)

  /**
   * Encode a leaf node
   */
  private def encodeLeaf(key: JamBytes, value: JamBytes): JamBytes =
    val result = new Array[Byte](64)

    if value.length <= 32 then
      // Short value: embed directly
      result(0) = (0x80 | value.length).toByte
      // key[0:31] - first 31 bytes of key
      key.copyToArray(result, 1, 0, 31)
      // value bytes
      value.copyToArray(result, 32, 0, value.length)
      // padding with zeros (already zero-initialized)
    else
      // Long value: hash it
      result(0) = 0xC0.toByte
      // key[0:31] - first 31 bytes of key
      key.copyToArray(result, 1, 0, 31)
      // hash(value)
      val valueHash = Hashing.blake2b256(value)
      System.arraycopy(valueHash.bytes, 0, result, 32, 32)

    JamBytes.wrap(result)
