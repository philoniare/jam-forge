package io.forge.jam.pvm.program

/**
 * 128-bit unsigned integer for chunk handling during instruction parsing.
 *
 * This is used to hold 16 bytes of instruction data for efficient argument extraction.
 * Operations are primarily right-shift and conversion to smaller types.
 *
 * @param low The lower 64 bits
 * @param high The upper 64 bits
 */
final case class U128(low: Long, high: Long):

  /**
   * Right shift by n bits.
   *
   * @param n Number of bits to shift (0-127)
   * @return New U128 with bits shifted right
   */
  def >>(n: Int): U128 =
    require(n >= 0, s"Shift amount must be non-negative: $n")
    if n >= 128 then U128(0L, 0L)
    else if n >= 64 then U128(high >>> (n - 64), 0L)
    else if n == 0 then this
    else
      val newLow = (low >>> n) | (high << (64 - n))
      val newHigh = high >>> n
      U128(newLow, newHigh)

  /**
   * Convert to ULong (lower 64 bits).
   */
  def toULong: Long = low

  /**
   * Convert to UInt (lower 32 bits).
   */
  def toUInt: Int = low.toInt

  /**
   * Convert to Long (lower 64 bits as signed).
   */
  def toLong: Long = low

  /**
   * Convert to Int (lower 32 bits as signed).
   */
  def toInt: Int = low.toInt

object U128:
  /** Zero value */
  val Zero: U128 = U128(0L, 0L)

  /**
   * Create U128 from a byte array in little-endian order.
   *
   * @param bytes The byte array
   * @param offset Starting offset in the array (default 0)
   * @return U128 value
   */
  def fromLEBytes(bytes: Array[Byte], offset: Int = 0): U128 =
    // Read low 64 bits (first 8 bytes)
    var low = 0L
    var i = 0
    while i < 8 && (offset + i) < bytes.length do
      low = low | ((bytes(offset + i).toLong & 0xFF) << (8 * i))
      i += 1

    // Read high 64 bits (next 8 bytes)
    var high = 0L
    i = 0
    while i < 8 && (offset + 8 + i) < bytes.length do
      high = high | ((bytes(offset + 8 + i).toLong & 0xFF) << (8 * i))
      i += 1

    U128(low, high)

  /**
   * Create U128 from two ULong values.
   *
   * @param low Lower 64 bits
   * @param high Upper 64 bits
   * @return U128 value
   */
  def apply(low: Long, high: Long): U128 = new U128(low, high)

  /**
   * Create U128 from a single Long value.
   *
   * @param value The value (becomes low bits, high = 0)
   * @return U128 value
   */
  def fromLong(value: Long): U128 = U128(value, 0L)
