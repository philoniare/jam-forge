package io.forge.jam.protocol.accumulation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.service.ServiceInfo
import spire.math.ULong

import scala.collection.mutable

/**
 * A simple mock implementation of PvmInstance for testing.
 */
class MockPvmInstance(
  memorySize: Int = 0x100000,
  initialGas: Long = 1000000L
) extends PvmInstance:

  private val registers = new Array[Long](13)
  private val memory = new Array[Byte](memorySize)
  private var _gas: Long = initialGas

  private val readOnlyRanges = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
  def markReadOnly(address: Int, length: Int): Unit =
    readOnlyRanges += ((address, length))
  private def overlapsReadOnly(address: Int, length: Int): Boolean =
    readOnlyRanges.exists { case (start, len) =>
      address < start + len && start < address + length
    }

  override def reg(regIdx: Int): Long = registers(regIdx)

  override def setReg(regIdx: Int, value: Long): Unit =
    if regIdx >= 0 && regIdx < registers.length then
      registers(regIdx) = value

  override def gas: Long = _gas

  override def setGas(value: Long): Unit =
    _gas = value

  override def readByte(address: Int): Option[Byte] =
    if address >= 0 && address < memory.length then
      Some(memory(address))
    else
      None

  override def writeByte(address: Int, value: Byte): Boolean =
    if address >= 0 && address < memory.length && !overlapsReadOnly(address, 1) then
      memory(address) = value
      true
    else
      false

  override def isMemoryAccessible(address: Int, length: Int): Boolean =
    address >= 0 && length >= 0 && (address + length) <= memory.length

  override def isMemoryReadable(address: Int, length: Int): Boolean =
    isMemoryAccessible(address, length)

  // Writable iff accessible AND not within an explicitly marked read-only range.
  override def isMemoryWritable(address: Int, length: Int): Boolean =
    isMemoryAccessible(address, length) && !overlapsReadOnly(address, length)

  /**
   * Bulk write to memory.
   */
  override def writeBytes(address: Int, data: Array[Byte]): Boolean =
    if !isMemoryWritable(address, data.length) then return false
    var i = 0
    while i < data.length do
      memory(address + i) = data(i)
      i += 1
    true

  /**
   * Bulk read from memory.
   */
  override def readBytes(address: Int, length: Int): Option[Array[Byte]] =
    if !isMemoryAccessible(address, length) then return None
    val result = new Array[Byte](length)
    var i = 0
    while i < length do
      result(i) = memory(address + i)
      i += 1
    Some(result)

  private var _growHeapBase: Long = 0L
  private var _growHeapMaxPage: Long = 0L
  private var _growHeapConfigured: Boolean = false
  private var _growHeapPagesGrown: Long = 0L

  def configureGrowHeap(hInitial: Long, maxPage: Long): Unit =
    _growHeapBase = hInitial
    _growHeapMaxPage = maxPage
    _growHeapConfigured = true
    _growHeapPagesGrown = 0L

  def growHeapPagesGrown: Long = _growHeapPagesGrown

  override def growHeapPageBounds: Option[(Long, Long)] =
    if _growHeapConfigured then Some((_growHeapBase + _growHeapPagesGrown, _growHeapMaxPage))
    else None

  override def growHeapPages(deltaPages: Long): Unit =
    _growHeapPagesGrown += deltaPages

  private var _forcedOutOfGas: Boolean = false
  override def forceOutOfGas(): Unit = _forcedOutOfGas = true
  override def isForcedOutOfGas: Boolean = _forcedOutOfGas
