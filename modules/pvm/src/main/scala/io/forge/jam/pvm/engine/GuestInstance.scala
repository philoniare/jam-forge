package io.forge.jam.pvm.engine

import spire.math.UInt
import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.memory.GuestRam
import io.forge.jam.pvm.types.*

/** Interpreter instance for an inner-PVM guest.
  *
  * All control-flow, block-compilation and run-loop logic lives in
  * [[InterpreterCore]] (shared with [[InterpretedInstance]]); this class
  * contributes memory operations over [[GuestRam]] — a fully dynamic
  * page-granular memory whose access rights are controlled by the `pages`
  * host call — always-on gas metering, and `sbrk` panicking (guests have no
  * heap).
  */
final class GuestInstance private (
    module: InterpretedModule,
    val ram: GuestRam,
    regs: Array[Long],
    gas0: Long,
    sharedInstructions: scala.collection.mutable.ArrayBuffer[CompiledInstruction],
    sharedOffsetMap: Array[Int]
) extends InterpreterCore(
  module,
  regs,
  ProgramCounter.MaxValue,
  false, // programCounterValid
  None, // nextProgramCounter
  true, // nextProgramCounterChanged
  gas0,
  sharedOffsetMap,
  sharedInstructions,
  InterruptKind.Finished
):

  override protected def segfaultPageSize: UInt = UInt(GuestRam.PageSize)
  override protected def gasMeteringEnabled: Boolean = true
  override protected def stepTracingEnabled: Boolean = false

  // ============================================================================
  // Memory over GuestRam
  // ============================================================================

  private def loadImpl(
      pc: ProgramCounter,
      dst: Int,
      address: Int,
      width: Int,
      signedLoad: Boolean
  ): Int =
    val addr = address.toLong & 0xffffffffL
    ram.faultPage(addr, width.toLong, write = false) match
      case Some(page) => segfault(pc, UInt((page & 0xffffffffL).toInt))
      case None =>
        val buf = new Array[Byte](width)
        ram.readUnchecked(addr, buf)
        var value = 0L
        var i = 0
        while i < width do
          value |= (buf(i) & 0xffL) << (i * 8)
          i += 1
        if signedLoad then
          val shift = 64 - width * 8
          value = (value << shift) >> shift
        setReg64(dst, value)
        advance()

  private def storeImpl(
      pc: ProgramCounter,
      address: Int,
      value: Long,
      width: Int
  ): Int =
    val addr = address.toLong & 0xffffffffL
    ram.faultPage(addr, width.toLong, write = true) match
      case Some(page) => segfault(pc, UInt((page & 0xffffffffL).toInt))
      case None =>
        val buf = new Array[Byte](width)
        var i = 0
        while i < width do
          buf(i) = ((value >> (i * 8)) & 0xff).toByte
          i += 1
        ram.writeUnchecked(addr, buf)
        advance()

  override def loadU8(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadImpl(pc, dst, address.signed, 1, signedLoad = false)
  override def loadI8(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadImpl(pc, dst, address.signed, 1, signedLoad = true)
  override def loadU16(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadImpl(pc, dst, address.signed, 2, signedLoad = false)
  override def loadI16(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadImpl(pc, dst, address.signed, 2, signedLoad = true)
  override def loadU32(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadImpl(pc, dst, address.signed, 4, signedLoad = false)
  override def loadI32(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadImpl(pc, dst, address.signed, 4, signedLoad = true)
  override def loadU64(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadImpl(pc, dst, address.signed, 8, signedLoad = false)

  override def loadU8Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    loadImpl(pc, dst, address, 1, signedLoad = false)
  override def loadI8Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    loadImpl(pc, dst, address, 1, signedLoad = true)
  override def loadU16Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    loadImpl(pc, dst, address, 2, signedLoad = false)
  override def loadI16Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    loadImpl(pc, dst, address, 2, signedLoad = true)
  override def loadU32Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    loadImpl(pc, dst, address, 4, signedLoad = false)
  override def loadI32Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    loadImpl(pc, dst, address, 4, signedLoad = true)
  override def loadU64Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    loadImpl(pc, dst, address, 8, signedLoad = false)

  override def storeU8(pc: ProgramCounter, src: Int, address: UInt): Int =
    storeImpl(pc, address.signed, getReg(src), 1)
  override def storeU16(pc: ProgramCounter, src: Int, address: UInt): Int =
    storeImpl(pc, address.signed, getReg(src), 2)
  override def storeU32(pc: ProgramCounter, src: Int, address: UInt): Int =
    storeImpl(pc, address.signed, getReg(src), 4)
  override def storeU64(pc: ProgramCounter, src: Int, address: UInt): Int =
    storeImpl(pc, address.signed, getReg(src), 8)

  override def storeImmU8(pc: ProgramCounter, address: UInt, value: Byte): Int =
    storeImpl(pc, address.signed, value.toLong & 0xffL, 1)
  override def storeImmU16(pc: ProgramCounter, address: UInt, value: Short): Int =
    storeImpl(pc, address.signed, value.toLong & 0xffffL, 2)
  override def storeImmU32(pc: ProgramCounter, address: UInt, value: Int): Int =
    storeImpl(pc, address.signed, value.toLong & 0xffffffffL, 4)
  override def storeImmU64(pc: ProgramCounter, address: UInt, value: Long): Int =
    storeImpl(pc, address.signed, value, 8)

  override def storeU8Int(pc: ProgramCounter, src: Int, address: Int): Int =
    storeImpl(pc, address, getReg(src), 1)
  override def storeU16Int(pc: ProgramCounter, src: Int, address: Int): Int =
    storeImpl(pc, address, getReg(src), 2)
  override def storeU32Int(pc: ProgramCounter, src: Int, address: Int): Int =
    storeImpl(pc, address, getReg(src), 4)
  override def storeU64Int(pc: ProgramCounter, src: Int, address: Int): Int =
    storeImpl(pc, address, getReg(src), 8)

  override def storeImmU8Int(pc: ProgramCounter, address: Int, value: Byte): Int =
    storeImpl(pc, address, value.toLong & 0xffL, 1)
  override def storeImmU16Int(pc: ProgramCounter, address: Int, value: Short): Int =
    storeImpl(pc, address, value.toLong & 0xffffL, 2)
  override def storeImmU32Int(pc: ProgramCounter, address: Int, value: Int): Int =
    storeImpl(pc, address, value.toLong & 0xffffffffL, 4)
  override def storeImmU64Int(pc: ProgramCounter, address: Int, value: Long): Int =
    storeImpl(pc, address, value, 8)

  /** Guests have no heap: sbrk panics. */
  override def sbrk(dst: Int, size: UInt): Int =
    panic(_programCounter)

object GuestInstance:
  /** Create a guest instance over `ram` for one `invoke`: registers, gas and
    * entry pc are supplied by the invoke host call's 112-byte block.
    */
  def create(
      module: InterpretedModule,
      ram: GuestRam,
      registers: Array[Long],
      gas: Long,
      entryPc: ProgramCounter
  ): GuestInstance =
    val (sharedInstructions, sharedOffsetMap) = module.compiledState()
    val instance = new GuestInstance(
      module = module,
      ram = ram,
      regs = java.util.Arrays.copyOf(registers, 13),
      gas0 = gas,
      sharedInstructions = sharedInstructions,
      sharedOffsetMap = sharedOffsetMap
    )
    instance.setNextProgramCounter(entryPc)
    instance
