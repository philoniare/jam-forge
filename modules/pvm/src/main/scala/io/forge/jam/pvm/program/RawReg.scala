package io.forge.jam.pvm.program

/**
 * Raw register value with safety checks.
 *
 * Wraps a 32-bit value and provides safe conversion to register indices.
 * Values greater than 12 are clamped to 12 (the maximum valid register index).
 *
 * @param value The raw value from instruction encoding
 */
opaque type RawReg = Int

object RawReg:
  /** Maximum valid register index */
  val MaxReg: Int = 12

  /**
   * Create a RawReg from an integer value.
   *
   * @param value The raw value (only lower 4 bits are used)
   * @return RawReg instance
   */
  def apply(value: Int): RawReg = value

  /**
   * Create a RawReg from a Long value.
   *
   * @param value The raw value (only lower 4 bits are used)
   * @return RawReg instance
   */
  def fromLong(value: Long): RawReg = value.toInt

  extension (r: RawReg)
    /**
     * Get the parsed register index (0-12).
     * Values > 12 are clamped to 12.
     */
    def get: Int =
      val parsed = r & 0xF
      if parsed > MaxReg then MaxReg else parsed

    /**
     * Get the raw unparsed value.
     */
    def rawUnparsed: Int = r

    /**
     * Convert to UInt (the register index).
     */
    def toIndex: Int = get

    /**
     * String representation showing the register number.
     */
    def show: String = s"r${get}"
