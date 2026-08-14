package io.forge.jam.protocol.accumulation

/**
 * Abstract interface for PVM instance interactions.
 *
 * This trait abstracts the PVM instance to allow the AccumulationHostCalls
 * to work without directly depending on the PVM module. Implementations
 * can wrap the actual PVM instance or provide mock implementations for testing.
 */
trait PvmInstance:
  /**
   * Get the value of a register.
   * @param regIdx Register index (0-12)
   * @return The register value as a Long
   */
  def reg(regIdx: Int): Long

  /**
   * Set the value of a register.
   * @param regIdx Register index (0-12)
   * @param value The value to set
   */
  def setReg(regIdx: Int, value: Long): Unit

  /**
   * Get remaining gas.
   * @return Remaining gas units
   */
  def gas: Long

  /**
   * Set remaining gas.
   * @param value New gas value
   */
  def setGas(value: Long): Unit

  /**
   * Read a byte from memory.
   * @param address Memory address
   * @return Some(byte) on success, None on failure
   */
  def readByte(address: Int): Option[Byte]

  /**
   * Write a byte to memory.
   * @param address Memory address
   * @param value Byte value to write
   * @return true on success, false on failure
   */
  def writeByte(address: Int, value: Byte): Boolean

  /**
   * Bulk-read a contiguous range of bytes from memory
   */
  def readBytes(address: Int, length: Int): Option[Array[Byte]]

  /**
   * Bulk-write a contiguous range of bytes to memory
   */
  def writeBytes(address: Int, data: Array[Byte]): Boolean

  /**
   * Check if memory is accessible at the given address and length.
   * @param address Starting address
   * @param length Number of bytes
   * @return true if memory is accessible
   */
  def isMemoryAccessible(address: Int, length: Int): Boolean

  def isMemoryReadable(address: Int, length: Int): Boolean

  /**
   * Check if memory is writable at the given address and length.
   *
   * @param address Starting address
   * @param length Number of bytes
   * @return true if memory is writable
   */
  def isMemoryWritable(address: Int, length: Int): Boolean
