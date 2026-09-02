package io.forge.jam.pvm.engine

import scala.collection.mutable.ArrayBuffer
import spire.math.UInt
import io.forge.jam.pvm.types.*
import io.forge.jam.pvm.{Instruction, InterruptKind}
import io.forge.jam.pvm.memory.BasicMemory
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
 * Compiled instruction representation.
 *
 * Stores the parsed instruction along with its program counters
 * to avoid re-parsing during execution.
 */
final case class CompiledInstruction(
  instruction: Instruction,
  pc: ProgramCounter,
  nextPc: ProgramCounter,
  opcodeValue: Int
)

/**
 * Interpreted VM instance for host-side execution over [[BasicMemory]].
 *
 * All control-flow, block-compilation and run-loop logic lives in
 * [[InterpreterCore]]; this class contributes the region-based memory
 * operations (BasicMemory fast paths), `sbrk` heap growth, and the
 * step-tracing / optional-gas-metering run-loop knobs.
 */
final class InterpretedInstance private (
  module: InterpretedModule,
  val basicMemory: BasicMemory,
  regs: Array[Long],
  programCounter0: ProgramCounter,
  programCounterValid0: Boolean,
  nextProgramCounter0: Option[ProgramCounter],
  nextProgramCounterChanged0: Boolean,
  gas0: Long,
  compiledOffsetForBlock0: Array[Int],
  compiledInstructions0: ArrayBuffer[CompiledInstruction],
  interrupt0: InterruptKind,
  val stepTracing: Boolean,
  val gasMetering: Boolean
) extends InterpreterCore(
  module,
  regs,
  programCounter0,
  programCounterValid0,
  nextProgramCounter0,
  nextProgramCounterChanged0,
  gas0,
  compiledOffsetForBlock0,
  compiledInstructions0,
  interrupt0
):

  val pageSize: UInt = module.memoryMap.pageSize

  override protected def segfaultPageSize: UInt = pageSize
  override protected def onRunStart(): Unit = basicMemory.markDirty()
  override protected def gasMeteringEnabled: Boolean = gasMetering
  override protected def stepTracingEnabled: Boolean = stepTracing

  // ============================================================================
  // Host-side extras
  // ============================================================================

  /**
   * Get raw 64-bit register value without mode check.
   * Use when module.is64Bit is known to be true (majority of cases).
   */
  inline def getReg64Raw(regIdx: Int): Long = regs(regIdx)

  /**
   * Set raw 64-bit register value without mode check.
   * Use when module.is64Bit is known to be true (majority of cases).
   */
  inline def setReg64Raw(regIdx: Int, value: Long): Unit =
    regs(regIdx) = value

  def consumeGas(amount: Long): Unit = _gas -= amount

  def heapSize: UInt = basicMemory.heapSize

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
    basicMemory.loadUnsignedFast(address, 1) match
      case BasicMemory.FastOk =>
        setReg64(dst, basicMemory.fastValue & 0xffL)
        advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def loadI8Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadUnsignedFast(address, 1) match
      case BasicMemory.FastOk =>
        setReg64(dst, basicMemory.fastValue.toByte.toLong)
        advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def loadU16Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadUnsignedFast(address, 2) match
      case BasicMemory.FastOk =>
        setReg64(dst, basicMemory.fastValue & 0xffffL)
        advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def loadI16Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadUnsignedFast(address, 2) match
      case BasicMemory.FastOk =>
        setReg64(dst, basicMemory.fastValue.toShort.toLong)
        advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def loadU32Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadUnsignedFast(address, 4) match
      case BasicMemory.FastOk =>
        setReg64(dst, basicMemory.fastValue & 0xffffffffL)
        advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def loadI32Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadUnsignedFast(address, 4) match
      case BasicMemory.FastOk =>
        setReg64(dst, basicMemory.fastValue.toInt.toLong)
        advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def loadU64Int(pc: ProgramCounter, dst: Int, address: Int): Int =
    basicMemory.loadUnsignedFast(address, 8) match
      case BasicMemory.FastOk =>
        setReg64(dst, basicMemory.fastValue)
        advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def storeU8Int(pc: ProgramCounter, src: Int, address: Int): Int =
    basicMemory.storeFast(address, getReg(src), 1) match
      case BasicMemory.FastOk => advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def storeU16Int(pc: ProgramCounter, src: Int, address: Int): Int =
    basicMemory.storeFast(address, getReg(src), 2) match
      case BasicMemory.FastOk => advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def storeU32Int(pc: ProgramCounter, src: Int, address: Int): Int =
    basicMemory.storeFast(address, getReg(src), 4) match
      case BasicMemory.FastOk => advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def storeU64Int(pc: ProgramCounter, src: Int, address: Int): Int =
    basicMemory.storeFast(address, getReg(src), 8) match
      case BasicMemory.FastOk => advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def storeImmU8Int(pc: ProgramCounter, address: Int, value: Byte): Int =
    basicMemory.storeFast(address, value.toLong, 1) match
      case BasicMemory.FastOk => advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def storeImmU16Int(pc: ProgramCounter, address: Int, value: Short): Int =
    basicMemory.storeFast(address, value.toLong, 2) match
      case BasicMemory.FastOk => advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def storeImmU32Int(pc: ProgramCounter, address: Int, value: Int): Int =
    basicMemory.storeFast(address, value.toLong, 4) match
      case BasicMemory.FastOk => advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def storeImmU64Int(pc: ProgramCounter, address: Int, value: Long): Int =
    basicMemory.storeFast(address, value, 8) match
      case BasicMemory.FastOk => advance()
      case BasicMemory.FastSegfault => segfault(pc, basicMemory.fastFaultPage)
      case _ => panic(pc)

  override def sbrk(dst: Int, size: UInt): Int =
    basicMemory.sbrk(size) match
      case Some(prevHeap) =>
        setReg32(dst, prevHeap)
        advance()
      case None =>
        panic(_programCounter)

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
    val (sharedInstructions, sharedOffsetMap) = module.compiledState()

    new InterpretedInstance(
      module = module,
      basicMemory = BasicMemory.create(module.memoryMap, module.roData, module.rwData, initialHeapSize, argumentData),
      regs = new Array[Long](Reg.Count),
      programCounter0 = ProgramCounter.MaxValue,
      programCounterValid0 = false,
      nextProgramCounter0 = None,
      nextProgramCounterChanged0 = true,
      gas0 = 0L,
      compiledOffsetForBlock0 = sharedOffsetMap,
      compiledInstructions0 = sharedInstructions,
      interrupt0 = InterruptKind.Finished,
      stepTracing = forceStepTracing,
      gasMetering = module.gasMetering
    )
