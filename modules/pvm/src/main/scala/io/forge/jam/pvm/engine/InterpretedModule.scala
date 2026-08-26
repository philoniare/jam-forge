package io.forge.jam.pvm.engine

import scala.collection.mutable.ArrayBuffer
import spire.math.UInt
import io.forge.jam.pvm.{MemoryMap, Abi, Instruction}
import io.forge.jam.pvm.types.ProgramCounter
import io.forge.jam.pvm.program.ProgramBlob

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

  private var _compiledInstructions: ArrayBuffer[CompiledInstruction] = null
  private var _compiledOffsetForBlock: Array[Int] = null

  private val debugConcurrency: Boolean =
    sys.props.get("jam.pvm.debugConcurrency").exists(_.toBoolean)
  private val _mutationGuard = new java.util.concurrent.atomic.AtomicBoolean(false)

  private[engine] def beginSharedMutation(): Unit =
    if debugConcurrency && !_mutationGuard.compareAndSet(false, true) then
      throw new IllegalStateException(
        "InterpretedModule shared compiled-block buffers mutated concurrently — " +
          "the strictly-sequential-execution precondition (see compiledState docs) is violated"
      )

  private[engine] def endSharedMutation(): Unit =
    if debugConcurrency then _mutationGuard.set(false)

  /**
   * Returns the shared compiled-block buffers for this module
   */
  def compiledState(): (ArrayBuffer[CompiledInstruction], Array[Int]) =
    val ci = _compiledInstructions
    if ci != null then (ci, _compiledOffsetForBlock)
    else
      synchronized {
        if _compiledInstructions == null then
          val map = new Array[Int](codeLen.signed + 1)
          java.util.Arrays.fill(map, -1) // -1 == ABSENT (not yet compiled)
          val instructions = ArrayBuffer.empty[CompiledInstruction]
          instructions += CompiledInstruction(
            Instruction.Panic,
            ProgramCounter(0),
            ProgramCounter(1),
            Instruction.Panic.opcode.value
          )
          _compiledOffsetForBlock = map
          _compiledInstructions = instructions
        (_compiledInstructions, _compiledOffsetForBlock)
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

object InterpretedModule:

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
