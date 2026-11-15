package io.forge.jam.protocol.pvmhost

import io.forge.jam.pvm.MemoryResult
import io.forge.jam.pvm.engine.InterpretedInstance
import spire.math.{UInt, UByte}

/** Wrapper around InterpretedInstance to implement PvmInstance trait.
  */
class InterpretedInstanceWrapper(instance: InterpretedInstance)
    extends PvmInstance:
  override def reg(regIdx: Int): Long = instance.reg(regIdx)
  override def setReg(regIdx: Int, value: Long): Unit =
    instance.setReg(regIdx, value)
  override def gas: Long = instance.gas
  override def setGas(value: Long): Unit = instance.setGas(value)
  override def forceOutOfGas(): Unit = instance.forceOutOfGas()
  override def isForcedOutOfGas: Boolean = instance.isForcedOutOfGas

  override def readByte(address: Int): Option[Byte] =
    instance.basicMemory.loadU8(UInt(address)) match
      case MemoryResult.Success(v) => Some(v.toByte)
      case _                       => None

  override def writeByte(address: Int, value: Byte): Boolean =
    instance.basicMemory.storeU8(UInt(address), UByte(value)) match
      case MemoryResult.Success(_) => true
      case _                       => false

  override def readBytes(address: Int, length: Int): Option[Array[Byte]] =
    if length < 0 then None
    else
      instance.basicMemory.getMemorySlice(UInt(address), length) match
        case MemoryResult.Success(data) => Some(data)
        case _                          => None

  override def readInto(address: Int, dest: Array[Byte], destOffset: Int, length: Int): Boolean =
    if length < 0 || destOffset < 0 || destOffset + length > dest.length then false
    else
      instance.basicMemory.getMemorySliceInto(UInt(address), dest, destOffset, length) match
        case MemoryResult.Success(_) => true
        case _                       => false

  override def writeBytes(address: Int, data: Array[Byte]): Boolean =
    instance.basicMemory.setMemorySlice(UInt(address), data) match
      case MemoryResult.Success(_) => true
      case _                       => false

  override def isMemoryAccessible(address: Int, length: Int): Boolean =
    instance.basicMemory.getMemorySlice(UInt(address), length) match
      case MemoryResult.Success(_) => true
      case _                       => false

  override def isMemoryReadable(address: Int, length: Int): Boolean =
    length >= 0 && instance.basicMemory.isReadable(UInt(address), length)

  override def isMemoryWritable(address: Int, length: Int): Boolean =
    length >= 0 && instance.basicMemory.isWritable(UInt(address), length)

  override def growHeapPageBounds: Option[(Long, Long)] =
    Some(instance.growHeapPageBounds)

  override def growHeapPages(deltaPages: Long): Unit =
    if deltaPages > 0 then
      val pageBytes = instance.pageSize.signed.toLong & 0xFFFFFFFFL
      val growBytes = deltaPages * pageBytes
      instance.basicMemory.sbrk(UInt(growBytes.toInt)) match
        case Some(_) => ()
        case None =>
          throw new IllegalStateException(
            s"grow_heap invariant violated: BasicMemory.sbrk($growBytes) failed after the " +
              "spec bound b was already checked (r7 <= b) — page-domain bound and " +
              "maxHeapSize have diverged"
          )
