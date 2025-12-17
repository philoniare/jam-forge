package io.forge.jam.pvm.engine

import scala.collection.mutable.ArrayBuffer
import spire.math.{UInt, UByte, UShort, ULong}
import io.forge.jam.pvm.types.*
import io.forge.jam.pvm.{InterruptKind, SegfaultInfo, Abi, Opcode, Instruction, MemoryResult}
import io.forge.jam.pvm.memory.{BasicMemory, DynamicMemory}
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
  val dynamicMemory: Option[DynamicMemory],
  val regs: Array[Long],
  private var _programCounter: ProgramCounter,
  private var _programCounterValid: Boolean,
  private var _nextProgramCounter: Option[ProgramCounter],
  private var _nextProgramCounterChanged: Boolean,
  private var cycleCounter: Long,
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

  // ============================================================================
  // Public API
  // ============================================================================

  def reg(regIdx: Int): Long =
    var value = regs(regIdx)
    if !module.is64Bit then value = value & 0xffffffffL
    value

  def setReg(regIdx: Int, value: Long): Unit =
    regs(regIdx) = if !module.is64Bit then
      (value & 0xffffffffL).toInt.toLong
    else
      value

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

  override def getReg(idx: Int): Long = reg(idx)

  override def setReg32(idx: Int, value: UInt): Unit =
    val signExtended = value.signed.toLong
    setReg(idx, signExtended)

  override def setReg64(idx: Int, value: Long): Unit =
    setReg(idx, value)

  override def advance(): Option[UInt] =
    Some(compiledOffset + UInt(1))

  override def resolveJump(pc: ProgramCounter): Option[UInt] =
    if pc.value.signed == Abi.VmAddrReturnToHost.signed then
      finished()
    else
      compiledOffsetForBlock.get(pc.value) match
        case Some(packed) =>
          val (isValid, offset) = PackedTarget.unpack(packed)
          if isValid then Some(offset) else None
        case None =>
          if !isJumpTargetValid(pc) then None
          else compileBlock(pc)

  override def resolveFallthrough(pc: ProgramCounter): Option[UInt] =
    compiledOffsetForBlock.get(pc.value) match
      case Some(packed) =>
        val (_, offset) = PackedTarget.unpack(packed)
        Some(offset)
      case None =>
        compileBlock(pc)

  override def jumpIndirect(pc: ProgramCounter, address: UInt): Option[UInt] =
    if address == Abi.VmAddrReturnToHost then
      _programCounter = pc
      _programCounterValid = true
      finished()
    else
      module.blob.jumpTable.getByAddress(address.signed) match
        case Some(targetInt) => resolveJump(ProgramCounter(targetInt))
        case None => panic(pc)

  override def branch(condition: Boolean, pc: ProgramCounter, target: Int, nextPc: ProgramCounter): Option[UInt] =
    if condition then
      val targetPc = ProgramCounter(target)
      resolveJump(targetPc).orElse(panic(pc))
    else
      resolveFallthrough(nextPc)

  override def panic(pc: ProgramCounter): Option[UInt] =
    val stackTrace = Thread.currentThread().getStackTrace.take(10).map(_.toString).mkString("\n  ")
    _programCounter = pc
    _programCounterValid = true
    _nextProgramCounter = None
    _nextProgramCounterChanged = true
    _interrupt = InterruptKind.Panic
    None

  override def outOfGas(pc: ProgramCounter): Option[UInt] =
    _programCounter = pc
    _programCounterValid = true
    _interrupt = InterruptKind.OutOfGas
    None

  override def ecalli(pc: ProgramCounter, nextPc: ProgramCounter, hostId: UInt): Option[UInt] =
    _programCounter = pc
    _programCounterValid = true
    _nextProgramCounter = Some(nextPc)
    _nextProgramCounterChanged = true
    _interrupt = InterruptKind.Ecalli(hostId)
    None

  override def finished(): Option[UInt] =
    _programCounterValid = true
    _nextProgramCounter = None
    _nextProgramCounterChanged = false
    _interrupt = InterruptKind.Finished
    None

  override def segfault(pc: ProgramCounter, pageAddress: UInt): Option[UInt] =
    _programCounter = pc
    _programCounterValid = true
    _interrupt = InterruptKind.Segfault(SegfaultInfo(pageAddress, pageSize))
    None

  // ============================================================================
  // Memory Operations
  // ============================================================================

  override def loadU8(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt] =
    basicMemory.loadU8(address) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.toLong & 0xffL)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def loadI8(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt] =
    basicMemory.loadI8(address) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.toLong)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def loadU16(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt] =
    basicMemory.loadU16(address) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.toLong & 0xffffL)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def loadI16(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt] =
    basicMemory.loadI16(address) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.toLong)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def loadU32(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt] =
    basicMemory.loadU32(address) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.toLong)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def loadI32(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt] =
    basicMemory.loadI32(address) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.toLong)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def loadU64(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt] =
    basicMemory.loadU64(address) match
      case MemoryResult.Success(v) =>
        setReg64(dst, v.signed)
        advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeU8(pc: ProgramCounter, src: Int, address: UInt): Option[UInt] =
    basicMemory.storeU8(address, UByte(getReg(src).toByte)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeU16(pc: ProgramCounter, src: Int, address: UInt): Option[UInt] =
    basicMemory.storeU16(address, UShort(getReg(src).toShort)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeU32(pc: ProgramCounter, src: Int, address: UInt): Option[UInt] =
    basicMemory.storeU32(address, UInt(getReg(src).toInt)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) =>
        panic(pc)

  override def storeU64(pc: ProgramCounter, src: Int, address: UInt): Option[UInt] =
    basicMemory.storeU64(address, ULong(getReg(src))) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeImmU8(pc: ProgramCounter, address: UInt, value: Byte): Option[UInt] =
    basicMemory.storeU8(address, UByte(value)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeImmU16(pc: ProgramCounter, address: UInt, value: Short): Option[UInt] =
    basicMemory.storeU16(address, UShort(value)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeImmU32(pc: ProgramCounter, address: UInt, value: Int): Option[UInt] =
    basicMemory.storeU32(address, UInt(value)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def storeImmU64(pc: ProgramCounter, address: UInt, value: Long): Option[UInt] =
    basicMemory.storeU64(address, ULong(value)) match
      case MemoryResult.Success(_) => advance()
      case MemoryResult.Segfault(_, pageAddr) => segfault(pc, pageAddr)
      case MemoryResult.OutOfBounds(_) => panic(pc)

  override def sbrk(dst: Int, size: UInt): Option[UInt] =
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
          compiledOffset = resolveArbitraryJump(pc).getOrElse(TargetOutOfRange)
          _nextProgramCounterChanged = false

    // Main execution loop using pattern matching
    var offset = compiledOffset
    var continue = true
    while continue do
      cycleCounter += 1
      _instructionCounter += 1

      if offset.signed >= compiledInstructions.size then
        // Out of range - panic
        _interrupt = InterruptKind.Panic
        continue = false
      else
        val compiled = compiledInstructions(offset.signed)
        // Charge gas if enabled
        if gasMetering then
          _gas -= 1
          if _gas < 0 then
            outOfGas(compiled.pc)
            continue = false

        if continue then
          // Execute the instruction using pattern matching
          compiledOffset = offset
          _programCounter = compiled.pc
          _programCounterValid = true

          InstructionExecutor.execute(compiled.instruction, this, compiled.pc, compiled.nextPc) match
            case None =>
              continue = false
            case Some(nextOffset) =>
              offset = nextOffset
              if stepTracing then
                compiledOffset = nextOffset
                _interrupt = InterruptKind.Step
                continue = false

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

  private def compileBlock(pc: ProgramCounter): Option[UInt] =
    if pc.value > module.codeLen then return None

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

    if compiledInstructions.size == origin.signed then None
    else Some(origin)

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
      dynamicMemory = None,
      regs = new Array[Long](Reg.Count),
      _programCounter = ProgramCounter.MaxValue,
      _programCounterValid = false,
      _nextProgramCounter = None,
      _nextProgramCounterChanged = true,
      cycleCounter = 0L,
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
