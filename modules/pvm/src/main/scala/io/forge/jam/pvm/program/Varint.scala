package io.forge.jam.pvm.program

import spire.math.{UByte, UInt, ULong}

/**
 * Varint encoding/decoding utilities for PVM program parsing.
 *
 * Supports both 32-bit and 64-bit varint encoding with proper sign extension
 * as specified in the Gray Paper.
 */
object Varint:

  /** Maximum varint length in bytes */
  val MaxVarintLength: Int = 5

  /** Maximum varint length for 64-bit values */
  val MaxVarint64Length: Int = 8

  /** Lookup table for shift amounts based on length */
  private val LengthToShift: Array[Int] = Array(32, 24, 16, 8, 0, 0, 0, 0, 0)

  /**
   * Read a simple varint with sign extension from a chunk.
   *
   * This performs sign extension based on the length:
   * - length 0: returns 0
   * - length 1: sign extends from 8 bits
   * - length 2: sign extends from 16 bits
   * - length 3: sign extends from 24 bits
   * - length 4+: no sign extension needed
   *
   * @param chunk The data chunk (low 32 bits contain the value)
   * @param length Number of bytes to read (0-4)
   * @return Sign-extended 32-bit value
   */
  def readSimpleVarint(chunk: Long, length: Int): Long =
    require(length >= 0 && length <= 4, s"Invalid varint length: $length")
    val shift = LengthToShift(length)
    // Shift left then arithmetic shift right to sign extend
    ((chunk << shift).toInt >> shift).toLong & 0xffffffffL

  /**
   * Count leading one bits in an unsigned 32-bit value.
   */
  def countLeadingOneBits(value: Long): Int =
    java.lang.Integer.numberOfLeadingZeros((~value.toInt))

  /**
   * Count leading one bits in an unsigned 64-bit value.
   */
  def countLeadingOneBits64(value: Long): Int =
    java.lang.Long.numberOfLeadingZeros(~value)

  /**
   * Get the number of bytes required to encode a value.
   *
   * Takes into account both positive values (based on leading zeros)
   * and negative values (based on leading ones).
   *
   * @param value The value to encode
   * @return Number of bytes required (0-4)
   */
  def getBytesRequired(value: Long): Int =
    val intValue = value.toInt
    val zeros = java.lang.Integer.numberOfLeadingZeros(intValue)

    if zeros == 32 then 0
    else if zeros > 24 then 1
    else if zeros > 16 then 2
    else if zeros > 8 then 3
    else if zeros != 0 then 4
    else
      // All zeros consumed, check leading ones
      val ones = java.lang.Integer.numberOfLeadingZeros(~intValue)
      if ones > 24 then 1
      else if ones > 16 then 2
      else if ones > 8 then 3
      else 4

  /**
   * Get the number of bytes required to encode a 64-bit value.
   *
   * @param value The 64-bit value to encode
   * @return Number of bytes required (0-8)
   */
  def getBytesRequired64(value: Long): Int =
    val zeros = java.lang.Long.numberOfLeadingZeros(value)

    if zeros == 64 then 0
    else if zeros > 56 then 1
    else if zeros > 48 then 2
    else if zeros > 40 then 3
    else if zeros > 32 then 4
    else if zeros > 24 then 5
    else if zeros > 16 then 6
    else if zeros > 8 then 7
    else if zeros != 0 then 8
    else
      // All zeros consumed, check leading ones
      val ones = java.lang.Long.numberOfLeadingZeros(~value)
      if ones > 56 then 1
      else if ones > 48 then 2
      else if ones > 40 then 3
      else if ones > 32 then 4
      else if ones > 24 then 5
      else if ones > 16 then 6
      else if ones > 8 then 7
      else 8

  /**
   * Get the varint length from the first byte prefix.
   *
   * @param firstByte The first byte of the varint
   * @return The length in bytes
   */
  def getVarintLength(firstByte: Int): Int =
    java.lang.Integer.numberOfLeadingZeros(~(firstByte | 0xFFFFFF00)) - 24

  /**
   * Read a variable-length integer from a byte array.
   *
   * @param input The byte array (after first byte)
   * @param firstByte The first byte of the varint
   * @return Some((length, value)) or None if invalid
   */
  def readVarint(input: Array[Byte], firstByte: Int): Option[(Int, Long)] =
    val fb = firstByte & 0xFF
    val length = java.lang.Integer.numberOfLeadingZeros(~(fb | 0xFFFFFF00)) - 24

    if input.length < length then None
    else
      val value = length match
        case 0 => fb.toLong
        case 1 => ((fb & 0x7F).toLong << 8) | (input(0) & 0xFF).toLong
        case 2 => ((fb & 0x3F).toLong << 16) | ((input(0) & 0xFF).toLong << 8) | (input(1) & 0xFF).toLong
        case 3 => ((fb & 0x1F).toLong << 24) | ((input(0) & 0xFF).toLong << 16) |
                  ((input(1) & 0xFF).toLong << 8) | (input(2) & 0xFF).toLong
        case 4 => (input(0) & 0xFF).toLong | ((input(1) & 0xFF).toLong << 8) |
                  ((input(2) & 0xFF).toLong << 16) | ((input(3) & 0xFF).toLong << 24)
        case _ => return None
      Some((length, value))

  /**
   * Write a simple varint to a buffer.
   *
   * @param value The value to write
   * @param buffer The output buffer
   * @return Number of bytes written
   */
  def writeSimpleVarint(value: Long, buffer: Array[Byte]): Int =
    val length = getBytesRequired(value)
    var i = 0
    while i < length do
      buffer(i) = ((value >> (8 * i)) & 0xFF).toByte
      i += 1
    length

  /**
   * Write a simple 64-bit varint to a buffer.
   *
   * @param value The 64-bit value to write
   * @param buffer The output buffer
   * @return Number of bytes written
   */
  def writeSimpleVarint64(value: Long, buffer: Array[Byte]): Int =
    val length = getBytesRequired64(value)
    var i = 0
    while i < length do
      buffer(i) = ((value >> (8 * i)) & 0xFF).toByte
      i += 1
    length

  /**
   * Calculate varint length from leading zeros.
   *
   * @param leadingZeros Number of leading zeros
   * @return Varint length
   */
  def getVarintLengthFromLeadingZeros(leadingZeros: Int): Int =
    val bitsRequired = 32 - leadingZeros
    val x = bitsRequired >> 3
    ((x + bitsRequired) ^ x) >> 3

  /**
   * Convert byte array to UInt in little-endian order.
   *
   * @param bytes The byte array
   * @param length Number of bytes to read (1-4)
   * @return The UInt value
   */
  def bytesToUIntLE(bytes: Array[Byte], length: Int): Long =
    require(length >= 1 && length <= 4, s"Invalid length: $length")
    var result = 0L
    var i = 0
    while i < length && i < bytes.length do
      result = result | ((bytes(i).toLong & 0xFF) << (8 * i))
      i += 1
    result
