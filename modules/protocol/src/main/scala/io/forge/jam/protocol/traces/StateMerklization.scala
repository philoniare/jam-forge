package io.forge.jam.protocol.traces

import io.forge.jam.core.{JamBytes, Hashing}
import io.forge.jam.core.primitives.Hash

/**
 * State Merklization function implementing the JAM protocol state trie.
 *
 * This is a binary Merkle tree where:
 * - Keys are 31 bytes (248 bits)
 * - At each level, keys are split based on bit at position i
 * - Leaves are encoded differently based on value size (embedded vs regular)
 */
object StateMerklization:

  private val ZERO_HASH: Hash = Hash.zero

  /**
   * Compute state root from key-value pairs.
   *
   * @param kv Map of 31-byte keys to variable-length values
   * @param bitIndex Current bit position for tree split (0-247)
   * @return 32-byte state root hash
   */
  def stateMerklize(kv: Map[JamBytes, JamBytes], bitIndex: Int = 0): Hash =
    // Empty case - return zero hash
    if kv.isEmpty then
      return ZERO_HASH

    // Single leaf case
    if kv.size == 1 then
      val (key, value) = kv.head
      return Hashing.blake2b256(leaf(key, value))

    // Split keys based on bit at position bitIndex
    val (left, right) = kv.partition { case (key, _) => !getBit(key.toArray, bitIndex) }

    // Recurse and combine
    val leftHash = stateMerklize(left, bitIndex + 1)
    val rightHash = stateMerklize(right, bitIndex + 1)

    Hashing.blake2b256(branch(leftHash, rightHash))

  /**
   * Convenience method that takes a List[KeyValue] instead of Map.
   */
  def stateMerklize(keyvals: List[KeyValue]): Hash =
    val map = keyvals.map(kv => (kv.key, kv.value)).toMap
    stateMerklize(map)

  /**
   * Create branch node (64 bytes).
   * First byte has high bit cleared to indicate branch.
   */
  private def branch(left: Hash, right: Hash): JamBytes =
    val data = new Array[Byte](64)
    // Copy left hash, clearing high bit of first byte
    System.arraycopy(left.bytes, 0, data, 0, 32)
    data(0) = (data(0) & 0x7f).toByte // Clear high bit
    // Copy right hash
    System.arraycopy(right.bytes, 0, data, 32, 32)
    JamBytes(data)

  /**
   * Create leaf node (64 bytes).
   * Uses embedded format for values <= 32 bytes, regular format otherwise.
   */
  private def leaf(key: JamBytes, value: JamBytes): JamBytes =
    if value.length <= 32 then
      embeddedLeaf(key, value)
    else
      regularLeaf(key, value)

  /**
   * Embedded leaf format (value <= 32 bytes):
   * Byte 0: 0x80 | size (high bit set, size in lower 6 bits)
   * Bytes 1-31: key (31 bytes)
   * Bytes 32-63: value + zero padding
   */
  private def embeddedLeaf(key: JamBytes, value: JamBytes): JamBytes =
    require(key.length == 31, s"Key must be 31 bytes, got ${key.length}")
    require(value.length <= 32, s"Value must be <= 32 bytes for embedded leaf, got ${value.length}")

    val data = new Array[Byte](64)
    // First byte: 0x80 | size
    data(0) = (0x80 | value.length).toByte
    // Key (31 bytes)
    key.copyToArray(data, 1, 0, 31)
    // Value + padding (remaining bytes are already 0)
    value.copyToArray(data, 32, 0, value.length)
    JamBytes(data)

  /**
   * Regular leaf format (value > 32 bytes):
   * Byte 0: 0xC0 (high 2 bits set)
   * Bytes 1-31: key (31 bytes)
   * Bytes 32-63: blake2b(value)
   */
  private def regularLeaf(key: JamBytes, value: JamBytes): JamBytes =
    require(key.length == 31, s"Key must be 31 bytes, got ${key.length}")

    val data = new Array[Byte](64)
    // First byte: 0xC0
    data(0) = 0xc0.toByte
    // Key (31 bytes)
    key.copyToArray(data, 1, 0, 31)
    // Hash of value
    val valueHash = Hashing.blake2b256(value)
    System.arraycopy(valueHash.bytes, 0, data, 32, 32)
    JamBytes(data)

  /**
   * Get bit at position i from data.
   * Bit 0 is the MSB of byte 0, bit 7 is the LSB of byte 0, etc.
   */
  def getBit(data: Array[Byte], i: Int): Boolean =
    val byteIndex = i / 8
    if byteIndex >= data.length then
      return false
    val bitIndex = 7 - (i % 8) // MSB first
    (data(byteIndex) & (1 << bitIndex)) != 0
