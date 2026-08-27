package io.forge.jam.pvm.engine

import scala.collection.mutable.ArrayBuffer
import spire.math.UInt
import io.forge.jam.pvm.{Abi, Instruction, InterruptKind, PvmConstants, SegfaultInfo}
import io.forge.jam.pvm.memory.GuestRam
import io.forge.jam.pvm.program.{InstructionDecoder, Program}
import io.forge.jam.pvm.types.*

final class GuestInstance private (
    val module: InterpretedModule,
    val ram: GuestRam,
    val regs: Array[Long],
    private var _programCounter: ProgramCounter,
    private var _programCounterValid: Boolean,
    private var _nextProgramCounter: Option[ProgramCounter],
    private var _nextProgramCounterChanged: Boolean,
    private var _gas: Long,
    private val compiledOffsetForBlock: Array[Int],
    private val compiledInstructions: ArrayBuffer[CompiledInstruction],
    private var _interrupt: InterruptKind
) extends ExecutionContext:

  private val _is64Bit: Boolean = module.is64Bit
  private var _compiledOffsetInt: Int = 0

  private inline val TargetAbsent: -1 = -1
  private inline def packTarget(offset: Int, isJumpTargetValid: Boolean): Int =
    val base = offset & 0x7fffffff
    if isJumpTargetValid then base | 0x80000000 else base
  private inline def targetAt(pcValue: UInt): Int =
    val idx = pcValue.signed
    if idx >= 0 && idx < compiledOffsetForBlock.length then
      compiledOffsetForBlock(idx)
    else TargetAbsent

  // ============================================================================
  // Public API
  // ============================================================================

  def reg(regIdx: Int): Long =
    if _is64Bit then regs(regIdx)
    else regs(regIdx) & 0xffffffffL

  def setReg(regIdx: Int, value: Long): Unit =
    if _is64Bit then regs(regIdx) = value
    else regs(regIdx) = value.toInt.toLong

  def gas: Long = _gas
  def setGas(value: Long): Unit = _gas = value

  def programCounter: Option[ProgramCounter] =
    if _programCounterValid then Some(_programCounter) else None

  def nextProgramCounter: Option[ProgramCounter] = _nextProgramCounter

  def setNextProgramCounter(pc: ProgramCounter): Unit =
    _programCounterValid = false
    _nextProgramCounter = Some(pc)
    _nextProgramCounterChanged = true

  def run(): Either[String, InterruptKind] =
    try Right(runImpl())
    catch case e: Exception => Left(e.getMessage)

  // ============================================================================
  // ExecutionContext: registers
  // ============================================================================

  override inline def getReg(idx: Int): Long = reg(idx)

  override def setReg32(idx: Int, value: UInt): Unit =
    setReg(idx, value.signed.toLong)

  override inline def setReg32Int(idx: Int, value: Int): Unit =
    setReg(idx, value.toLong)

  override inline def setReg64(idx: Int, value: Long): Unit =
    setReg(idx, value)

  // ============================================================================
  // ExecutionContext: control flow
  // ============================================================================

  override inline def advance(): Int =
    _compiledOffsetInt + 1

  override def resolveJump(pc: ProgramCounter): Int =
    val packed = targetAt(pc.value)
    if packed != TargetAbsent then
      val isValid = (packed >>> 31) == 1
      val offset = packed & 0x7fffffff
      if isValid then offset else panic(pc)
    else if !isJumpTargetValid(pc) then panic(pc)
    else compileBlock(pc)

  override def resolveFallthrough(pc: ProgramCounter): Int =
    val packed = targetAt(pc.value)
    if packed != TargetAbsent then packed & 0x7fffffff
    else compileBlock(pc)

  override def jumpIndirect(pc: ProgramCounter, address: UInt): Int =
    jumpIndirectInt(pc, address.signed)

  override def jumpIndirectInt(pc: ProgramCounter, address: Int): Int =
    if address == Abi.VmAddrReturnToHost.signed then
      _programCounter = pc
      _programCounterValid = true
      finished()
    else
      module.blob.jumpTable.getByAddress(address) match
        case Some(targetInt) => resolveJump(ProgramCounter(targetInt))
        case None            => panic(pc)

  override def branch(
      condition: Boolean,
      pc: ProgramCounter,
      target: Int,
      nextPc: ProgramCounter
  ): Int =
    if condition then
      val r = resolveJump(ProgramCounter(target))
      if r < 0 then panic(pc) else r
    else resolveFallthrough(nextPc)

  override def panic(pc: ProgramCounter): Int =
    _programCounter = pc
    _programCounterValid = true
    _nextProgramCounter = None
    _nextProgramCounterChanged = true
    _interrupt = InterruptKind.Panic
    Step.Interrupt

  override def outOfGas(pc: ProgramCounter): Int =
    _programCounter = pc
    _programCounterValid = true
    _interrupt = InterruptKind.OutOfGas
    Step.Interrupt

  override def ecalli(pc: ProgramCounter, nextPc: ProgramCounter, hostId: UInt): Int =
    _programCounter = pc
    _programCounterValid = true
    _nextProgramCounter = Some(nextPc)
    _nextProgramCounterChanged = true
    _interrupt = InterruptKind.Ecalli(hostId)
    Step.Interrupt

  override def finished(): Int =
    _programCounterValid = true
    _nextProgramCounter = None
    _nextProgramCounterChanged = false
    _interrupt = InterruptKind.Finished
    Step.Interrupt

  override def segfault(pc: ProgramCounter, pageAddress: UInt): Int =
    if pageAddress.toLong < PvmConstants.MinValidAddress.toLong then panic(pc)
    else
      _programCounter = pc
      _programCounterValid = true
      _interrupt =
        InterruptKind.Segfault(SegfaultInfo(pageAddress, UInt(GuestRam.PageSize)))
      Step.Interrupt

  // ============================================================================
  // ExecutionContext
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

  // ============================================================================
  // Run loop (mirrors InterpretedInstance.runImpl, always gas-metered)
  // ============================================================================

  private def runImpl(): InterruptKind =
    if _nextProgramCounterChanged then
      _nextProgramCounter match
        case None =>
          throw new IllegalStateException(
            "Failed to run: next program counter is not set"
          )
        case Some(pc) =>
          _programCounter = pc
          _nextProgramCounter = None
          val resolved = resolveArbitraryJump(pc)
          _compiledOffsetInt = resolved.getOrElse(UInt(0)).signed
          _nextProgramCounterChanged = false

    var offset = _compiledOffsetInt
    val instructions = compiledInstructions
    var instructionsSize = instructions.size

    while true do
      if offset >= instructionsSize then
        _interrupt = InterruptKind.Panic
        _compiledOffsetInt = offset
        return _interrupt

      val compiled = instructions(offset)

      _gas -= 1
      if _gas < 0 then
        outOfGas(compiled.pc)
        _compiledOffsetInt = offset
        return _interrupt

      _compiledOffsetInt = offset
      _programCounter = compiled.pc
      _programCounterValid = true

      val next = InstructionExecutor.execute(
        compiled.opcodeValue,
        compiled.instruction,
        this,
        compiled.pc,
        compiled.nextPc
      )

      if next < 0 then return _interrupt
      else
        offset = next
        if next >= instructionsSize then instructionsSize = instructions.size

    _interrupt

  def resolveArbitraryJump(pc: ProgramCounter): Option[UInt] =
    val packed = targetAt(pc.value)
    if packed != TargetAbsent then Some(UInt(packed & 0x7fffffff))
    else
      val blockStart = findStartOfBasicBlock(pc)
      blockStart.flatMap { start =>
        compileBlock(start)
        val p = targetAt(pc.value)
        if p != TargetAbsent then Some(UInt(p & 0x7fffffff)) else None
      }

  private def isJumpTargetValid(pc: ProgramCounter): Boolean =
    Program.isJumpTargetValid(module.blob.code, module.blob.bitmask, pc.toInt)

  private def findStartOfBasicBlock(pc: ProgramCounter): Option[ProgramCounter] =
    Program
      .findStartOfBasicBlock(module.blob.code, module.blob.bitmask, pc.toInt)
      .map(offset => ProgramCounter(offset))

  private def compileBlock(pc: ProgramCounter): Int =
    if pc.value > module.codeLen then return Step.Interrupt

    module.beginSharedMutation()
    try
      val origin = UInt(compiledInstructions.size)
      var isJumpTargetValidFlag = this.isJumpTargetValid(pc)
      var currentPc = pc
      var done = false

      while !done && currentPc.value <= module.codeLen do
        val packedTarget =
          packTarget(compiledInstructions.size, isJumpTargetValidFlag)
        val insertIdx = currentPc.value.signed
        if insertIdx >= 0 && insertIdx < compiledOffsetForBlock.length then
          compiledOffsetForBlock(insertIdx) = packedTarget
        isJumpTargetValidFlag = false

        val (instruction, nextPc) = parseInstructionAt(currentPc)
        compiledInstructions += CompiledInstruction(
          instruction,
          currentPc,
          nextPc,
          instruction.opcode.value
        )

        if instruction.opcode.startsNewBasicBlock then done = true
        else currentPc = nextPc

      if compiledInstructions.size == origin.signed then Step.Interrupt
      else origin.signed
    finally module.endSharedMutation()

  private def parseInstructionAt(pc: ProgramCounter): (Instruction, ProgramCounter) =
    val code = module.blob.code
    val bitmask = module.blob.bitmask
    val offset = pc.toInt

    if offset >= code.length then
      return (Instruction.Panic, ProgramCounter(offset + 1))

    val (instruction, skip) = InstructionDecoder.decode(code, bitmask, offset)
    (instruction, ProgramCounter(offset + skip))

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
      _programCounter = ProgramCounter.MaxValue,
      _programCounterValid = false,
      _nextProgramCounter = None,
      _nextProgramCounterChanged = true,
      _gas = gas,
      compiledOffsetForBlock = sharedOffsetMap,
      compiledInstructions = sharedInstructions,
      _interrupt = InterruptKind.Finished
    )
    instance.setNextProgramCounter(entryPc)
    instance
