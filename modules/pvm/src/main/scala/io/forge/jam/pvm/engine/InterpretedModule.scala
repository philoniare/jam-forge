package io.forge.jam.pvm.engine

import java.lang.invoke.{MethodHandles, VarHandle}
import spire.math.UInt
import io.forge.jam.pvm.{MemoryMap, Abi, Instruction}
import io.forge.jam.pvm.types.ProgramCounter
import io.forge.jam.pvm.program.{InstructionDecoder, Program, ProgramBlob}

/**
 * Holds the static module data for an interpreted PVM instance.
 *
 * The InterpretedModule contains the read-only program data that can be
 * shared across multiple instances. This includes:
 * - Read-only data section (roData)
 * - Read-write data section template (rwData)
 * - Number of empty heap pages to allocate
 * - Program blob for instruction decoding
 *
 * @param roData Read-only data section, sized to match memory map
 * @param rwData Read-write data section template (copied per instance)
 * @param heapEmptyPages Number of heap pages to pre-allocate
 * @param blob The parsed program blob containing code and metadata
 * @param memoryMap The memory layout configuration
 * @param codeLen Length of the code section
 * @param is64Bit Whether this is a 64-bit program
 */
final class InterpretedModule private (
  val roData: Array[Byte],
  val rwData: Array[Byte],
  val heapEmptyPages: UInt,
  val blob: ProgramBlob,
  val memoryMap: MemoryMap,
  val codeLen: UInt,
  val is64Bit: Boolean
):

  /**
   * Checks if gas metering is enabled for this module.
   * For now, gas metering is always enabled.
   */
  def gasMetering: Boolean = true

  /**
   * Checks if dynamic paging mode is enabled.
   * For now, we use basic memory mode.
   */
  def isDynamicPaging: Boolean = false

  lazy val isBlobStructurallyValid: Boolean = Program.isValidBlob(blob.code, blob.bitmask)

  // Fallback cache for block costs charged away from a compiled block's first instruction (rare: entry mid-block).
  private val blockGasCosts = new java.util.concurrent.ConcurrentHashMap[Integer, java.lang.Long]()

  def blockGasCostAt(chargePc: Int): Long =
    val cached = blockGasCosts.get(chargePc)
    if cached != null then cached.longValue
    else
      val cost = computeBlockGasCost(chargePc)
      blockGasCosts.put(chargePc, cost)
      cost

  private def computeBlockGasCost(chargePc: Int): Long =
    val start = Program
      .findStartOfBasicBlock(blob.code, blob.bitmask, chargePc)
      .getOrElse(chargePc)
    BlockGasModel.gasCostForBlock(blob.code, blob.bitmask, start)

  // Racy but benign: each slot only ever goes from 0 (unknown) to the same computed value.
  private val jumpTargetValidity = new Array[Byte](blob.code.length + 1)

  def isJumpTargetValid(offset: Int): Boolean =
    if offset < 0 || offset >= jumpTargetValidity.length then
      Program.isJumpTargetValid(blob.code, blob.bitmask, offset)
    else
      jumpTargetValidity(offset) match
        case 1 => true
        case 2 => false
        case _ =>
          val valid = Program.isJumpTargetValid(blob.code, blob.bitmask, offset)
          jumpTargetValidity(offset) = if valid then 1 else 2
          valid

  // ==========================================================================
  // Compiled blocks, shared by every instance of this module
  // ==========================================================================
  //
  // Instances of one module may run on several threads at once (accumulation runs services in parallel, and the
  // module cache is process-wide). Blocks are compiled under `compileLock` and published so that a reader never sees
  // a compiled offset before the instructions it points to:
  //   1. the new instructions are written past the published count, then a new CompiledCode is published through a
  //      volatile field;
  //   2. only then are the pc -> offset slots written, with release semantics.
  // A reader that obtains an offset from `targetAt` (acquire) and then reads `compiled` (volatile) therefore sees an
  // array that contains it. Published entries are never modified.

  @volatile private var _compiled: CompiledCode =
    val first = CompiledInstruction(Instruction.Panic, ProgramCounter(0), ProgramCounter(1), Instruction.Panic.opcode.value)
    val arr = new Array[CompiledInstruction](64)
    arr(0) = first
    CompiledCode(arr, 1)

  /** pc -> packed compiled offset (bit 31 = valid jump target), -1 when not compiled. Accessed through `Slots`. */
  private val offsetForPc: Array[Int] =
    val map = new Array[Int](codeLen.signed + 1)
    java.util.Arrays.fill(map, InterpretedModule.Absent)
    map

  private val compileLock = new Object

  /** The currently published compiled instructions. */
  def compiled: CompiledCode = _compiled

  /** Packed compiled offset for `pc`, or -1 when it is out of range or not compiled yet. */
  def targetAt(pc: Int): Int =
    if pc >= 0 && pc < offsetForPc.length then InterpretedModule.Slots.getAcquire(offsetForPc, pc)
    else InterpretedModule.Absent

  /** Compiles the basic block starting at `pc` and returns the compiled offset of its first instruction, or -1 when
    * `pc` is past the code. If another thread compiled `pc` first, returns that offset.
    */
  private[engine] def compileBlock(pc: Int): Int =
    if (pc.toLong & 0xffffffffL) > codeLen.toLong then return -1
    compileLock.synchronized {
      val existing = targetAt(pc)
      if existing != InterpretedModule.Absent then return existing & 0x7fffffff

      val published = _compiled
      var arr = published.instructions
      var n = published.count
      val origin = n
      val slots = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
      var isTarget = isJumpTargetValid(pc)
      var currentPc = pc
      var done = false
      val code = blob.code

      while !done && (currentPc.toLong & 0xffffffffL) <= codeLen.toLong do
        slots += ((currentPc, InterpretedModule.packTarget(n, isTarget)))
        val (instruction, skip) =
          if currentPc >= code.length then (Instruction.Panic, 1)
          else InstructionDecoder.decode(code, blob.bitmask, currentPc)
        val nextPc = currentPc + skip
        val opcode = instruction.opcode
        // GP 0.8.0 §A.4 (branch): both the target and the fall-through must be block starts, taken or not.
        val branchValid = InterpretedModule.branchTarget(instruction) match
          case Some(target) => isJumpTargetValid(target) && isJumpTargetValid(nextPc)
          case None => true
        // GP 0.8.0 §A.4: a block's cost (§A.9) is charged on entry; it is static for the block's first instruction.
        val blockCost = if n == origin then blockGasCostAt(currentPc) else -1L
        if n == arr.length then arr = java.util.Arrays.copyOf(arr, arr.length * 2)
        arr(n) = CompiledInstruction(
          instruction,
          ProgramCounter(currentPc),
          ProgramCounter(nextPc),
          opcode.value,
          blockCost,
          branchValid
        )
        n += 1
        isTarget = false
        if opcode.startsNewBasicBlock then done = true
        else currentPc = nextPc

      if n == origin then -1
      else
        _compiled = CompiledCode(arr, n)
        slots.foreach((slotPc, packed) =>
          if slotPc >= 0 && slotPc < offsetForPc.length then InterpretedModule.Slots.setRelease(offsetForPc, slotPc, packed)
        )
        origin
    }

  override def equals(other: Any): Boolean = other match
    case that: InterpretedModule =>
      java.util.Arrays.equals(roData, that.roData) &&
      java.util.Arrays.equals(rwData, that.rwData) &&
      heapEmptyPages == that.heapEmptyPages &&
      codeLen == that.codeLen &&
      is64Bit == that.is64Bit
    case _ => false

  override def hashCode(): Int =
    var result = java.util.Arrays.hashCode(roData)
    result = 31 * result + java.util.Arrays.hashCode(rwData)
    result = 31 * result + heapEmptyPages.hashCode()
    result = 31 * result + codeLen.hashCode()
    result = 31 * result + is64Bit.hashCode()
    result

  override def toString: String =
    s"InterpretedModule(roData.size=${roData.length}, rwData.size=${rwData.length}, heapEmptyPages=$heapEmptyPages, codeLen=$codeLen, is64Bit=$is64Bit)"

/** A published prefix of a module's compiled instructions: `instructions(0 until count)` never change. */
final class CompiledCode(val instructions: Array[CompiledInstruction], val count: Int)

object InterpretedModule:
  private[engine] final val Absent = -1
  private[engine] val Slots: VarHandle = MethodHandles.arrayElementVarHandle(classOf[Array[Int]])

  private[engine] def packTarget(offset: Int, isJumpTargetValid: Boolean): Int =
    val base = offset & 0x7fffffff
    if isJumpTargetValid then base | 0x80000000 else base

  /** The static target of a conditional branch, if `instruction` is one. */
  private def branchTarget(instruction: Instruction): Option[Int] = instruction match
    case i: Instruction.BranchEq => Some(i.offset.toInt)
    case i: Instruction.BranchNotEq => Some(i.offset.toInt)
    case i: Instruction.BranchLessUnsigned => Some(i.offset.toInt)
    case i: Instruction.BranchLessSigned => Some(i.offset.toInt)
    case i: Instruction.BranchGreaterOrEqualUnsigned => Some(i.offset.toInt)
    case i: Instruction.BranchGreaterOrEqualSigned => Some(i.offset.toInt)
    case i: Instruction.BranchEqImm => Some(i.offset.toInt)
    case i: Instruction.BranchNotEqImm => Some(i.offset.toInt)
    case i: Instruction.BranchLessUnsignedImm => Some(i.offset.toInt)
    case i: Instruction.BranchLessSignedImm => Some(i.offset.toInt)
    case i: Instruction.BranchGreaterOrEqualUnsignedImm => Some(i.offset.toInt)
    case i: Instruction.BranchGreaterOrEqualSignedImm => Some(i.offset.toInt)
    case i: Instruction.BranchLessOrEqualSignedImm => Some(i.offset.toInt)
    case i: Instruction.BranchLessOrEqualUnsignedImm => Some(i.offset.toInt)
    case i: Instruction.BranchGreaterSignedImm => Some(i.offset.toInt)
    case i: Instruction.BranchGreaterUnsignedImm => Some(i.offset.toInt)
    case _ => None

  /**
   * Creates a new InterpretedModule from a parsed program blob.
   *
   * @param blob The parsed program blob
   * @param heapPages Number of heap pages to pre-allocate (default 0)
   * @param auxDataSize Size of auxiliary data region (default 16MB for GP stack and arguments)
   * @return Either an error message or the created module
   */
  def create(
    blob: ProgramBlob,
    heapPages: UInt = UInt(0),
    auxDataSize: UInt = UInt(16908288)
  ): Either[String, InterpretedModule] =
    // Build memory map from program blob
    val pageSize = Abi.VmMinPageSize.toLong
    val actualRwDataLen = if blob.originalRwDataLen >= 0 then blob.originalRwDataLen else blob.rwData.length
    val pageAlignedRwDataLen = ((actualRwDataLen + pageSize - 1) / pageSize * pageSize).toInt
    val effectiveHeapPages = if blob.heapPages > 0 then blob.heapPages else heapPages.signed
    val heapEmptyPagesSize = effectiveHeapPages * pageSize.toInt
    val rwDataSize = pageAlignedRwDataLen + heapEmptyPagesSize

    MemoryMap.builder(Abi.VmMinPageSize)
      .withRoDataSize(UInt(blob.roData.length))
      .withRwDataSize(UInt(rwDataSize))
      .withStackSize(UInt(blob.stackSize))
      .withAuxDataSize(auxDataSize)
      .build() match
        case Left(err) => Left(err)
        case Right(memoryMap) =>
          // Create roData sized to match memory map
          val roData = new Array[Byte](memoryMap.roDataSize.signed)
          System.arraycopy(blob.roData, 0, roData, 0, math.min(blob.roData.length, roData.length))

          // Use heapPages from blob if available, otherwise use parameter
          val effectiveHeapPagesUInt = if blob.heapPages > 0 then UInt(blob.heapPages) else heapPages
          Right(new InterpretedModule(
            roData = roData,
            rwData = blob.rwData.clone(),
            heapEmptyPages = effectiveHeapPagesUInt,
            blob = blob,
            memoryMap = memoryMap,
            codeLen = UInt(blob.code.length),
            is64Bit = blob.is64Bit
          ))

  /**
   * Creates a simple module for testing with minimal code.
   *
   * @param code The code bytes
   * @param bitmask The instruction boundary bitmask
   * @param is64Bit Whether this is a 64-bit program
   * @return The created module
   */
  def createForTest(
    code: Array[Byte],
    bitmask: Array[Byte],
    is64Bit: Boolean = false
  ): InterpretedModule =
    import io.forge.jam.pvm.program.JumpTable

    val blob = ProgramBlob(
      code = code,
      bitmask = bitmask,
      jumpTable = JumpTable(Array.empty, 0),
      is64Bit = is64Bit,
      roData = Array.empty,
      rwData = Array.empty,
      stackSize = 4096
    )

    create(blob) match
      case Right(module) => module
      case Left(err) => throw new RuntimeException(s"Failed to create test module: $err")
