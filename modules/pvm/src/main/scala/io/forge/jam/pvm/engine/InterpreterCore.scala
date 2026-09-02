package io.forge.jam.pvm.engine

import scala.collection.mutable.ArrayBuffer
import spire.math.UInt
import io.forge.jam.pvm.{Abi, Instruction, InterruptKind, SegfaultInfo}
import io.forge.jam.pvm.program.{InstructionDecoder, Program}
import io.forge.jam.pvm.types.*

/** Shared interpreter core: register file, gas, program-counter/interrupt
  * state, basic-block compilation against the module's shared compiled state,
  * jump/branch resolution, and the hot run loop. [[InterpretedInstance]]
  * (host-side, BasicMemory) and [[GuestInstance]] (inner-PVM, GuestRam)
  * differ only in the memory operations, `sbrk`, the segfault page size, and
  * the run-loop knobs — everything else lives here so a PVM semantics fix
  * lands exactly once.
  */
abstract class InterpreterCore protected (
    val module: InterpretedModule,
    val regs: Array[Long],
    protected var _programCounter: ProgramCounter,
    protected var _programCounterValid: Boolean,
    protected var _nextProgramCounter: Option[ProgramCounter],
    protected var _nextProgramCounterChanged: Boolean,
    protected var _gas: Long,
    protected val compiledOffsetForBlock: Array[Int],
    protected val compiledInstructions: ArrayBuffer[CompiledInstruction],
    protected var _interrupt: InterruptKind
) extends ExecutionContext:

  protected val _is64Bit: Boolean = module.is64Bit
  protected var _compiledOffsetInt: Int = 0
  private val TargetOutOfRange: UInt = UInt(0)

  // ==========================================================================
  // Variation points
  // ==========================================================================

  /** Page size reported in segfault interrupts. */
  protected def segfaultPageSize: UInt

  /** Invoked at the top of every run() (e.g. memory dirty-tracking). */
  protected def onRunStart(): Unit = ()

  /** Whether the run loop decrements gas per instruction. Read once per
    * run(), never inside the hot loop.
    */
  protected def gasMeteringEnabled: Boolean

  /** Whether the run loop yields [[InterruptKind.Step]] after each
    * instruction. Read once per run().
    */
  protected def stepTracingEnabled: Boolean
  protected inline val TargetAbsent: -1 = -1
  protected inline def packTarget(offset: Int, isJumpTargetValid: Boolean): Int =
    val base = offset & 0x7fffffff
    if isJumpTargetValid then base | 0x80000000 else base
  // Reads the slot for a pc.value, returning ABSENT (-1) when out of range.
  protected inline def targetAt(pcValue: UInt): Int =
    val idx = pcValue.signed
    if idx >= 0 && idx < compiledOffsetForBlock.length then compiledOffsetForBlock(idx)
    else TargetAbsent

  // ==========================================================================
  // Public API — registers, gas, program counter
  // ==========================================================================

  /** Get register value, applying 32-bit mask if in 32-bit mode. */
  final def reg(regIdx: Int): Long =
    var value = regs(regIdx)
    if !_is64Bit then value = value & 0xffffffffL
    value

  /** Set register value, applying appropriate masking based on mode. */
  final def setReg(regIdx: Int, value: Long): Unit =
    regs(regIdx) = if !_is64Bit then
      (value & 0xffffffffL).toInt.toLong
    else
      value

  final def gas: Long = _gas
  final def setGas(value: Long): Unit = _gas = value

  final def programCounter: Option[ProgramCounter] =
    if _programCounterValid then Some(_programCounter) else None

  final def nextProgramCounter: Option[ProgramCounter] = _nextProgramCounter

  final def setNextProgramCounter(pc: ProgramCounter): Unit =
    _programCounterValid = false
    _nextProgramCounter = Some(pc)
    _nextProgramCounterChanged = true

  final def run(): Either[String, InterruptKind] =
    try Right(runImpl())
    catch case e: Exception => Left(e.getMessage)

  // ==========================================================================
  // ExecutionContext — registers
  // ==========================================================================

  override inline def getReg(idx: Int): Long = reg(idx)

  /** Set 32-bit register value with sign extension. */
  override def setReg32(idx: Int, value: UInt): Unit =
    // Extract signed Int and sign-extend to Long directly
    setReg(idx, value.signed.toLong)

  /** Set 32-bit register from primitive Int without UInt wrapping.
    * Int.toLong automatically sign-extends negative values.
    */
  override inline def setReg32Int(idx: Int, value: Int): Unit =
    setReg(idx, value.toLong)

  override inline def setReg64(idx: Int, value: Long): Unit =
    setReg(idx, value)

  // ==========================================================================
  // ExecutionContext — control flow
  // ==========================================================================

  /** Advance using primitive Int offset. */
  override inline def advance(): Int =
    _compiledOffsetInt + 1

  override def resolveJump(pc: ProgramCounter): Int =
    val packed = targetAt(pc.value)
    if packed != TargetAbsent then
      val isValid = (packed >>> 31) == 1
      val offset = packed & 0x7fffffff
      if isValid then offset else panic(pc)
    else
      if !isJumpTargetValid(pc) then panic(pc)
      else compileBlock(pc)

  override def resolveFallthrough(pc: ProgramCounter): Int =
    val packed = targetAt(pc.value)
    if packed != TargetAbsent then
      packed & 0x7fffffff
    else
      compileBlock(pc)

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
        case None => panic(pc)

  override def branch(condition: Boolean, pc: ProgramCounter, target: Int, nextPc: ProgramCounter): Int =
    if condition then
      val targetPc = ProgramCounter(target)
      val r = resolveJump(targetPc)
      if r < 0 then panic(pc) else r
    else
      resolveFallthrough(nextPc)

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
    if pageAddress.toLong < io.forge.jam.pvm.PvmConstants.MinValidAddress.toLong then panic(pc)
    else
      _programCounter = pc
      _programCounterValid = true
      _interrupt = InterruptKind.Segfault(SegfaultInfo(pageAddress, segfaultPageSize))
      Step.Interrupt

  // ==========================================================================
  // Run loop
  // ==========================================================================

  private def runImpl(): InterruptKind =
    onRunStart()

    if _nextProgramCounterChanged then
      _nextProgramCounter match
        case None =>
          throw new IllegalStateException("Failed to run: next program counter is not set")
        case Some(pc) =>
          _programCounter = pc
          _nextProgramCounter = None
          val resolved = resolveArbitraryJump(pc)
          _compiledOffsetInt = resolved.getOrElse(TargetOutOfRange).signed
          _nextProgramCounterChanged = false

    // Cache values locally for faster access in hot loop
    var offset = _compiledOffsetInt
    val instructions = compiledInstructions
    var instructionsSize = instructions.size
    val isGasMetered = gasMeteringEnabled
    val isStepTracing = stepTracingEnabled

    // Main execution loop - optimized for JIT
    while true do
      // Bounds check - use primitive comparison
      if offset >= instructionsSize then
        _interrupt = InterruptKind.Panic
        _compiledOffsetInt = offset
        return _interrupt

      // Get compiled instruction - ArrayBuffer.apply is O(1)
      val compiled = instructions(offset)

      // Gas metering check
      if isGasMetered then
        _gas -= 1
        if _gas < 0 then
          outOfGas(compiled.pc)
          _compiledOffsetInt = offset
          return _interrupt

      // Update state for instruction execution.
      _compiledOffsetInt = offset
      _programCounter = compiled.pc
      _programCounterValid = true

      // Execute instruction - returns next compiled offset, or a negative
      // sentinel (Step.Interrupt) meaning "interrupt occurred, read _interrupt"
      val next = InstructionExecutor.execute(compiled.opcodeValue, compiled.instruction, this, compiled.pc, compiled.nextPc)

      if next < 0 then
        // Interrupt occurred - exit loop
        return _interrupt
      else
        offset = next
        if next >= instructionsSize then
          instructionsSize = instructions.size
        if isStepTracing then
          _compiledOffsetInt = offset
          _interrupt = InterruptKind.Step
          return _interrupt

    // This should never be reached, but required for type checking
    _interrupt

  final def resolveArbitraryJump(pc: ProgramCounter): Option[UInt] =
    val packed = targetAt(pc.value)
    if packed != TargetAbsent then
      Some(UInt(packed & 0x7fffffff))
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
    Program.findStartOfBasicBlock(module.blob.code, module.blob.bitmask, pc.toInt)
      .map(offset => ProgramCounter(offset))

  // ==========================================================================
  // Block Compilation
  // ==========================================================================

  private def compileBlock(pc: ProgramCounter): Int =
    if pc.value > module.codeLen then return Step.Interrupt
    module.beginSharedMutation()
    try
      val origin = UInt(compiledInstructions.size)
      var isJumpTargetValid = this.isJumpTargetValid(pc)
      var currentPc = pc
      var done = false

      while !done && currentPc.value <= module.codeLen do
        val packedTarget = packTarget(compiledInstructions.size, isJumpTargetValid)
        val insertIdx = currentPc.value.signed
        if insertIdx >= 0 && insertIdx < compiledOffsetForBlock.length then
          compiledOffsetForBlock(insertIdx) = packedTarget
        isJumpTargetValid = false

        val (instruction, nextPc) = parseInstructionAt(currentPc)
        compiledInstructions += CompiledInstruction(instruction, currentPc, nextPc, instruction.opcode.value)

        if instruction.opcode.startsNewBasicBlock then
          done = true
        else
          currentPc = nextPc

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
