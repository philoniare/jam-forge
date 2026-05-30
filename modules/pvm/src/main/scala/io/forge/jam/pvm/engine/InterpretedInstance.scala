package io.forge.jam.pvm.engine

import scala.collection.mutable.ArrayBuffer
import spire.math.{UInt, UByte, UShort, ULong}
import io.forge.jam.pvm.types.*
import io.forge.jam.pvm.{InterruptKind, SegfaultInfo, Abi, Instruction, MemoryResult}
import io.forge.jam.pvm.memory.BasicMemory
import io.forge.jam.pvm.program.{Program, InstructionDecoder}
import java.io.{FileWriter, PrintWriter}
import com.typesafe.scalalogging.StrictLogging

/**
 * Trace writer for PVM execution - writes standardized trace format.
 */
object PvmTraceWriter extends StrictLogging:
  @volatile private var writer: Option[PrintWriter] = None
  @volatile private var enabled: Boolean = false
  @volatile private var targetService: Long = 0
  @volatile private var currentService: Long = 0

  def enable(filePath: String, serviceId: Long = 0): Unit =
    synchronized {
      writer.foreach(_.close())
      writer = Some(new PrintWriter(new FileWriter(filePath, false)))
      enabled = true
      targetService = serviceId
    }

  def disable(): Unit =
    synchronized {
      writer.foreach(_.close())
      writer = None
      enabled = false
    }

  def setCurrentService(serviceId: Long): Unit =
    currentService = serviceId

  def isEnabled: Boolean = enabled && (targetService == 0 || targetService == currentService)

  def debug(msg: => String): Unit = logger.debug(msg)

  def trace(ic: Long, pc: Int, gas: Long, opcode: String, regs: Array[Long]): Unit =
    if isEnabled then
      writer.foreach { w =>
        // Format: ic=<ic> pc=<pc> gas=<gas> op=<opcode> regs=[r0,r1,...,r12]
        val regsStr = regs.take(13).map(r => f"$r%016x").mkString(",")
        w.println(s"ic=$ic pc=$pc gas=$gas op=$opcode regs=[$regsStr]")
        w.flush()
      }

  def traceHostCall(callIndex: Int, gasBefore: Long, gasAfter: Long, regs: Array[Long]): Unit =
    if isEnabled then
      writer.foreach { w =>
        val regsStr = regs.take(13).map(r => f"$r%016x").mkString(",")
        w.println(s"HOST call=$callIndex gasBefore=$gasBefore gasAfter=$gasAfter regs=[$regsStr]")
        w.flush()
      }

/**
 * Packed target value combining jump validity and handler offset.
 *
 * The high bit indicates whether this is a valid jump target,
 * and the lower 31 bits contain the compiled instruction index.
 */
final class PackedTarget private (val value: UInt) extends AnyRef:
  override def toString: String = s"PackedTarget($value)"

object PackedTarget:
  def pack(index: UInt, isJumpTargetValid: Boolean): PackedTarget =
    val base = index & UInt(0x7fffffff)
    val packed = if isJumpTargetValid then base | UInt(0x80000000) else base
    new PackedTarget(packed)

  def unpack(packed: PackedTarget): (Boolean, UInt) =
    val isValid = (packed.value.signed >>> 31) == 1
    val offset = packed.value & UInt(0x7fffffff)
    (isValid, offset)

/**
 * Compiled instruction representation.
 *
 * Stores the parsed instruction along with its program counters
 * to avoid re-parsing during execution.
 */
final case class CompiledInstruction(
  instruction: Instruction,
  pc: ProgramCounter,
  nextPc: ProgramCounter
)

/**
 * Interpreted VM instance using pattern-matching execution model.
 *
 * This implementation uses a more idiomatic Scala approach:
 * - Instructions are pattern-matched rather than using function handlers
 * - The Instruction ADT carries operands directly (no Args indirection)
 * - ExecutionContext trait abstracts VM state access
 */
final class InterpretedInstance private (
  val module: InterpretedModule,
  val basicMemory: BasicMemory,
  val regs: Array[Long],
  private var _programCounter: ProgramCounter,
  private var _programCounterValid: Boolean,
  private var _nextProgramCounter: Option[ProgramCounter],
  private var _nextProgramCounterChanged: Boolean,
  private var _gas: Long,
  private val compiledOffsetForBlock: FlatMap[PackedTarget],
  private val compiledInstructions: ArrayBuffer[CompiledInstruction],
  private var compiledOffset: UInt,
  private var _interrupt: InterruptKind,
  val stepTracing: Boolean,
  val gasMetering: Boolean
) extends ExecutionContext:

  val pageSize: UInt = module.memoryMap.pageSize
  private val TargetOutOfRange: UInt = UInt(0)
  private val _is64Bit: Boolean = module.is64Bit
  private var _compiledOffsetInt: Int = 0

  // ============================================================================
  // Public API - Register Operations
  // ============================================================================

  /**
   * Get register value, applying 32-bit mask if in 32-bit mode.
   * For 64-bit mode (common case), use getReg64Raw() for better performance.
   */
  inline def reg(regIdx: Int): Long =
    var value = regs(regIdx)
    if !_is64Bit then value = value & 0xffffffffL
    value

  /**
   * Get raw 64-bit register value without mode check.
   * Use when module.is64Bit is known to be true (majority of cases).
   */
  inline def getReg64Raw(regIdx: Int): Long = regs(regIdx)

  /**
   * Set register value, applying appropriate masking based on mode.
   */
  def setReg(regIdx: Int, value: Long): Unit =
    regs(regIdx) = if !_is64Bit then
      (value & 0xffffffffL).toInt.toLong
    else
      value

  /**
   * Set raw 64-bit register value without mode check.
   * Use when module.is64Bit is known to be true (majority of cases).
   */
  inline def setReg64Raw(regIdx: Int, value: Long): Unit =
    regs(regIdx) = value

  def gas: Long = _gas
  def setGas(value: Long): Unit = _gas = value
  def consumeGas(amount: Long): Unit = _gas -= amount

  def programCounter: Option[ProgramCounter] =
    if _programCounterValid then Some(_programCounter) else None

  def nextProgramCounter: Option[ProgramCounter] = _nextProgramCounter

  def setNextProgramCounter(pc: ProgramCounter): Unit =
    _programCounterValid = false
    _nextProgramCounter = Some(pc)
    _nextProgramCounterChanged = true

  def heapSize: UInt = basicMemory.heapSize

  def run(): Either[String, InterruptKind] =
    try Right(runImpl())
    catch case e: Exception => Left(e.getMessage)

  // ============================================================================
  // ExecutionContext Implementation
  // ============================================================================

  override inline def getReg(idx: Int): Long = reg(idx)

  /**
   * Set 32-bit register value with sign extension.
   */
  override def setReg32(idx: Int, value: UInt): Unit =
    // Extract signed Int and sign-extend to Long directly
    val signExtended = value.signed.toLong
    setReg(idx, signExtended)

  /**
   * set 32-bit register from primitive Int without UInt wrapping.
   * Int.toLong automatically sign-extends negative values.
   */
  override inline def setReg32Int(idx: Int, value: Int): Unit =
    setReg(idx, value.toLong)

  override inline def setReg64(idx: Int, value: Long): Unit =
    setReg(idx, value)

  /**
   * advance using primitive Int offset.
   */
  override inline def advance(): Int =
    _compiledOffsetInt + 1

  override def resolveJump(pc: ProgramCounter): Int =
    if pc.value.signed == Abi.VmAddrReturnToHost.signed then
      finished()
    else
      val packed = compiledOffsetForBlock.getOrNull(pc.value)
      if packed != null then
        val (isValid, offset) = PackedTarget.unpack(packed)
        if isValid then offset.signed else Step.Interrupt
      else
        if !isJumpTargetValid(pc) then Step.Interrupt
        else compileBlock(pc)

  override def resolveFallthrough(pc: ProgramCounter): Int =
    val packed = compiledOffsetForBlock.getOrNull(pc.value)
    if packed != null then
      PackedTarget.unpack(packed)._2.signed
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
    _programCounter = pc
    _programCounterValid = true
    _interrupt = InterruptKind.Segfault(SegfaultInfo(pageAddress, pageSize))
    Step.Interrupt

  // ============================================================================
  // Memory Operations (UInt address versions for API compatibility)
  // ============================================================================

  override def loadU8(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadU8Int(pc, dst, address.signed)

  override def loadI8(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadI8Int(pc, dst, address.signed)

  override def loadU16(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadU16Int(pc, dst, address.signed)

  override def loadI16(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadI16Int(pc, dst, address.signed)

  override def loadU32(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadU32Int(pc, dst, address.signed)

  override def loadI32(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadI32Int(pc, dst, address.signed)

  override def loadU64(pc: ProgramCounter, dst: Int, address: UInt): Int =
    loadU64Int(pc, dst, address.signed)

  override def storeU8(pc: ProgramCounter, src: Int, address: UInt): Int =
    storeU8Int(pc, src, address.signed)

  override def storeU16(pc: ProgramCounter, src: Int, address: UInt): Int =
    storeU16Int(pc, src, address.signed)

  override def storeU32(pc: ProgramCounter, src: Int, address: UInt): Int =
    storeU32Int(pc, src, address.signed)

  override def storeU64(pc: ProgramCounter, src: Int, address: UInt): Int =
    storeU64Int(pc, src, address.signed)

  override def storeImmU8(pc: ProgramCounter, address: UInt, value: Byte): Int =
    storeImmU8Int(pc, address.signed, value)

  override def storeImmU16(pc: ProgramCounter, address: UInt, value: Short): Int =
    storeImmU16Int(pc, address.signed, value)

  override def storeImmU32(pc: ProgramCounter, address: UInt, value: Int): Int =
    storeImmU32Int(pc, address.signed, value)

  override def storeImmU64(pc: ProgramCounter, address: UInt, value: Long): Int =
    storeImmU64Int(pc, address.signed, value)

  // ============================================================================
  // Memory Operations
  // ============================================================================

  override def loadU8Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadU8(UInt(address)) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.toLong & 0xffL)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def loadI8Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadI8(UInt(address)) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.toLong)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def loadU16Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadU16(UInt(address)) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.toLong & 0xffffL)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def loadI16Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadI16(UInt(address)) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.toLong)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def loadU32Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadU32(UInt(address)) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.toLong)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def loadI32Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadI32(UInt(address)) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.toLong)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def loadU64Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadU64(UInt(address)) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.signed)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeU8Int(pc: ProgramCounter, src: Int, address: Int): Int =
    basicMemory.storeU8(UInt(address), UByte(getReg(src).toByte)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeU16Int(pc: ProgramCounter, src: Int, address: Int): Int =
    basicMemory.storeU16(UInt(address), UShort(getReg(src).toShort)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeU32Int(pc: ProgramCounter, src: Int, address: Int): Int =
    basicMemory.storeU32(UInt(address), UInt(getReg(src).toInt)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeU64Int(pc: ProgramCounter, src: Int, address: Int): Int =
    basicMemory.storeU64(UInt(address), ULong(getReg(src))) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeImmU8Int(pc: ProgramCounter, address: Int, value: Byte): Int =
    basicMemory.storeU8(UInt(address), UByte(value)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeImmU16Int(pc: ProgramCounter, address: Int, value: Short): Int =
    basicMemory.storeU16(UInt(address), UShort(value)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeImmU32Int(pc: ProgramCounter, address: Int, value: Int): Int =
    basicMemory.storeU32(UInt(address), UInt(value)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeImmU64Int(pc: ProgramCounter, address: Int, value: Long): Int =
    basicMemory.storeU64(UInt(address), ULong(value)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def sbrk(dst: Int, size: UInt): Int =
    basicMemory.sbrk(size) match
      case Some(prevHeap) =>
        setReg32(dst, prevHeap)
        advance()
      case None =>
        panic(_programCounter)

  // ============================================================================
  // Internal Implementation
  // ============================================================================

  private var _instructionCounter: Long = 0
  def instructionCounter: Long = _instructionCounter
  def resetInstructionCounter(): Unit = _instructionCounter = 0

  private def runImpl(): InterruptKind =
    basicMemory.markDirty()

    if _nextProgramCounterChanged then
      _nextProgramCounter match
        case None =>
          throw new IllegalStateException("Failed to run: next program counter is not set")
        case Some(pc) =>
          _programCounter = pc
          _nextProgramCounter = None
          val resolved = resolveArbitraryJump(pc)
          compiledOffset = resolved.getOrElse(TargetOutOfRange)
          _compiledOffsetInt = compiledOffset.signed
          _nextProgramCounterChanged = false

    // Cache values locally for faster access in hot loop
    var offset = _compiledOffsetInt
    val instructions = compiledInstructions
    var instructionsSize = instructions.size
    val isGasMetered = gasMetering
    val isStepTracing = stepTracing

    // Main execution loop - optimized for JIT
    while true do
      _instructionCounter += 1

      // Bounds check - use primitive comparison
      if offset >= instructionsSize then
        _interrupt = InterruptKind.Panic
        _compiledOffsetInt = offset
        compiledOffset = UInt(offset)
        return _interrupt

      // Get compiled instruction - ArrayBuffer.apply is O(1)
      val compiled = instructions(offset)

      // Gas metering check
      if isGasMetered then
        _gas -= 1
        if _gas < 0 then
          outOfGas(compiled.pc)
          _compiledOffsetInt = offset
          compiledOffset = UInt(offset)
          return _interrupt

      // Update state for instruction execution
      _compiledOffsetInt = offset
      compiledOffset = UInt(offset)
      _programCounter = compiled.pc
      _programCounterValid = true

      // Execute instruction - returns next compiled offset, or a negative
      // sentinel (Step.Interrupt) meaning "interrupt occurred, read _interrupt"
      val next = InstructionExecutor.execute(compiled.instruction, this, compiled.pc, compiled.nextPc)

      if next < 0 then
        // Interrupt occurred - exit loop
        return _interrupt
      else
        offset = next
        // Update instructionsSize in case compilation added new instructions
        instructionsSize = instructions.size
        if isStepTracing then
          _compiledOffsetInt = offset
          compiledOffset = UInt(offset)
          _interrupt = InterruptKind.Step
          return _interrupt

    // This should never be reached, but required for type checking
    _interrupt

  def resolveArbitraryJump(pc: ProgramCounter): Option[UInt] =
    compiledOffsetForBlock.get(pc.value) match
      case Some(packed) =>
        val (_, offset) = PackedTarget.unpack(packed)
        Some(offset)
      case None =>
        val blockStart = findStartOfBasicBlock(pc)
        blockStart.flatMap { start =>
          compileBlock(start)
          compiledOffsetForBlock.get(pc.value).map(packed => PackedTarget.unpack(packed)._2)
        }

  private def isJumpTargetValid(pc: ProgramCounter): Boolean =
    Program.isJumpTargetValid(module.blob.code, module.blob.bitmask, pc.toInt)

  private def findStartOfBasicBlock(pc: ProgramCounter): Option[ProgramCounter] =
    Program.findStartOfBasicBlock(module.blob.code, module.blob.bitmask, pc.toInt)
      .map(offset => ProgramCounter(offset))

  // ============================================================================
  // Block Compilation
  // ============================================================================

  private def compileBlock(pc: ProgramCounter): Int =
    if pc.value > module.codeLen then return Step.Interrupt

    val origin = UInt(compiledInstructions.size)
    var isJumpTargetValid = this.isJumpTargetValid(pc)
    var currentPc = pc
    var done = false

    while !done && currentPc.value <= module.codeLen do
      val packedTarget = PackedTarget.pack(UInt(compiledInstructions.size), isJumpTargetValid)
      compiledOffsetForBlock.insert(currentPc.value, packedTarget)
      isJumpTargetValid = false

      val (instruction, nextPc) = parseInstructionAt(currentPc)
      compiledInstructions += CompiledInstruction(instruction, currentPc, nextPc)

      if instruction.opcode.startsNewBasicBlock then
        done = true
      else
        currentPc = nextPc

    if compiledInstructions.size == origin.signed then Step.Interrupt
    else origin.signed

  private def parseInstructionAt(pc: ProgramCounter): (Instruction, ProgramCounter) =
    val code = module.blob.code
    val bitmask = module.blob.bitmask
    val offset = pc.toInt

    if offset >= code.length then
      return (Instruction.Panic, ProgramCounter(offset + 1))

    val (instruction, skip) = InstructionDecoder.decode(code, bitmask, offset)
    (instruction, ProgramCounter(offset + skip))

  private def compileOutOfRangeStub(): Unit =
    // Add a panic instruction at index 0 for out-of-range jumps
    compiledInstructions += CompiledInstruction(
      Instruction.Panic,
      ProgramCounter(0),
      ProgramCounter(1)
    )

object InterpretedInstance:
  /**
   * Creates an instance from a module with specific argument data.
   * This allows reusing a cached module with different input data per execution.
   */
  def fromModule(
    module: InterpretedModule,
    argumentData: Array[Byte] = Array.empty,
    forceStepTracing: Boolean = false
  ): InterpretedInstance =
    val pageSize = module.memoryMap.pageSize.toLong
    val actualRwDataLen =
      if module.blob.originalRwDataLen >= 0 then module.blob.originalRwDataLen else module.rwData.length
    val pageAlignedRwDataLen = ((actualRwDataLen + pageSize - 1) / pageSize * pageSize).toInt
    val heapEmptyPagesSize = module.heapEmptyPages.toLong * pageSize
    val initialHeapSize = UInt((pageAlignedRwDataLen + heapEmptyPagesSize).toInt)

    val instance = new InterpretedInstance(
      module = module,
      basicMemory = BasicMemory.create(module.memoryMap, module.roData, module.rwData, initialHeapSize, argumentData),
      regs = new Array[Long](Reg.Count),
      _programCounter = ProgramCounter.MaxValue,
      _programCounterValid = false,
      _nextProgramCounter = None,
      _nextProgramCounterChanged = true,
      _gas = 0L,
      compiledOffsetForBlock = FlatMap.create[PackedTarget](module.codeLen + UInt(1)),
      compiledInstructions = ArrayBuffer.empty,
      compiledOffset = UInt(0),
      _interrupt = InterruptKind.Finished,
      stepTracing = forceStepTracing,
      gasMetering = module.gasMetering
    )
    instance.compileOutOfRangeStub()
    instance
