package io.forge.jam.protocol.accumulation

import io.forge.jam.core.{JamBytes, Hashing}
import io.forge.jam.core.scodec.JamCodecs
import spire.math.UInt

/**
 * State key computation functions for accumulation.
 */
object StateKey:
  /**
   * Computes a generic service data state key.
   *
   * The result is a 31-byte state key constructed by:
   * 1. Hashing (discriminator || data) with Blake2b256
   * 2. Interleaving service index bytes with hash bytes
   *
   * @param serviceIndex Service index (UInt32)
   * @param discriminator Discriminator value (UInt32):
   *   - 0xFFFFFFFF for storage keys
   *   - 0xFFFFFFFE for preimage blob keys
   *   - preimage length for preimage info keys
   * @param data The data to hash (storage key or preimage hash)
   * @return 31-byte state key
   */
  def computeServiceDataStateKey(serviceIndex: Long, discriminator: Long, data: JamBytes): JamBytes =
    // Encode discriminator as 4-byte little-endian using codec
    val valEncoded = JamCodecs.encodeU32LE(UInt(discriminator.toInt))

    // h = valEncoded + data
    val h = valEncoded ++ data.toArray

    // a = blake2b256(h)
    val a = Hashing.blake2b256(h).bytes

    // Construct the state key by interleaving service index with hash
    val serviceBytes = JamCodecs.encodeU32LE(UInt(serviceIndex.toInt))

    val stateKey = new Array[Byte](31)

    // First 8 bytes: interleave service index with hash
    // Pattern: [s0, h0, s1, h1, s2, h2, s3, h3]
    var i = 0
    while i < 4 do
      stateKey(i * 2) = serviceBytes(i)
      stateKey(i * 2 + 1) = a(i)
      i += 1

    // Remaining bytes from hash (bytes 4-26, which is 23 bytes)
    System.arraycopy(a, 4, stateKey, 8, 23)

    JamBytes(stateKey)

  /**
   * Computes the state key for service storage.
   *
   * Uses discriminator 0xFFFFFFFF for storage entries.
   *
   * @param serviceIndex Service index
   * @param storageKey Storage key (arbitrary bytes)
   * @return 31-byte state key
   */
  def computeStorageStateKey(serviceIndex: Long, storageKey: JamBytes): JamBytes =
    computeServiceDataStateKey(serviceIndex, 0xFFFFFFFFL, storageKey)

  /**
   * Computes the state key for preimage info entry.
   *
   * Uses preimage length as discriminator.
   *
   * @param serviceIndex Service index
   * @param length Preimage length (used as discriminator)
   * @param preimageHash Hash of the preimage (32 bytes)
   * @return 31-byte state key
   */
  def computePreimageInfoStateKey(serviceIndex: Long, length: Int, preimageHash: JamBytes): JamBytes =
    computeServiceDataStateKey(serviceIndex, length.toLong, preimageHash)

  /**
   * Computes the state key for a service account (ServiceAccountDetails).
   *
   * The key is 31 bytes with:
   * - First byte: 0xFF (prefix for service account keys)
   * - Service index bytes interleaved at positions 1, 3, 5, 7
   * - Remaining bytes are zeros
   *
   * @param serviceIndex Service index
   * @return 31-byte service account key
   */
  def computeServiceAccountKey(serviceIndex: Long): JamBytes =
    val key = new Array[Byte](31)
    key(0) = 0xFF.toByte // Prefix for ServiceAccountKey

    // Interleave service index bytes (little-endian) at positions 1, 3, 5, 7
    val idx = serviceIndex.toInt
    key(1) = (idx & 0xFF).toByte
    key(3) = ((idx >> 8) & 0xFF).toByte
    key(5) = ((idx >> 16) & 0xFF).toByte
    key(7) = ((idx >> 24) & 0xFF).toByte

    JamBytes(key)

  /**
   * Encodes a preimage info value (list of 0-3 timeslots).
   *
   * Format: compact length + 4-byte LE timeslot values
   *
   * @param timeslots List of timeslot values (0-3 entries)
   * @return Encoded bytes as JamBytes
   */
  def encodePreimageInfoValue(timeslots: List[Long]): JamBytes =
    val builder = JamBytes.newBuilder

    // Compact length encoding (simple 1-byte count for 0-3 values)
    builder += timeslots.size.toByte

    // 4-byte LE timeslot values using codec
    for ts <- timeslots do
      builder ++= JamCodecs.encodeU32LE(UInt(ts.toInt))

    builder.result()

  /**
   * Decodes a preimage info value (list of 0-3 timeslots).
   *
   * @param data Encoded preimage info bytes
   * @return List of timeslot values
   */
  def decodePreimageInfoValue(data: JamBytes): List[Long] =
    if data.isEmpty then
      return List.empty

    val bytes = data.toArray
    val count = bytes(0).toInt & 0xFF
    if count == 0 then
      return List.empty

    val timeslots = scala.collection.mutable.ListBuffer[Long]()
    var i = 0
    while i < count do
      val offset = 1 + i * 4
      if offset + 4 <= bytes.length then
        val ts = JamCodecs.decodeU32LE(bytes, offset).toLong
        timeslots += ts
      i += 1

    timeslots.toList
