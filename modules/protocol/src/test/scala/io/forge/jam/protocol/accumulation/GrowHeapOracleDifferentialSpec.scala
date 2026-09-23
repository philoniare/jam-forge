package io.forge.jam.protocol.accumulation

import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.engine.{InterpretedInstance, InterpretedModule}
import io.forge.jam.pvm.native_.PvmRecompiler
import io.forge.jam.pvm.program.{JumpTable, ProgramBlob}
import io.forge.jam.pvm.types.ProgramCounter
import io.forge.jam.protocol.refine.HostCallDispatcher

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.util.Random

class GrowHeapOracleDifferentialSpec extends AnyFlatSpec with Matchers:

  private def libPath: Option[Path] =
    Option(System.getProperty("jam.pvm.recompiler.lib")).map(Path.of(_)).filter(Files.exists(_))

  private def canRunNative: Boolean =
    val arch = System.getProperty("os.arch", "").toLowerCase
    (arch == "aarch64" || arch == "arm64") && libPath.isDefined

  private val RW_DATA_LEN = 64
  private val PAGE = 4096
  private object GrowHeapDispatcher extends HostCallDispatcher:
    def getGasCost(hostCallId: Int, instance: PvmInstance): Long = 0L
    def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
      hostCallId match
        case HostCall.GROW_HEAP => GrowHeapHostCall.handle(instance)
        case _ => ()

  // ---- abstract program model ----------------------------------------------

  private sealed trait AInstr
  private case class LoadImm64(reg: Int, imm: Long) extends AInstr
  private case object GrowHeap extends AInstr // Ecalli(HostCall.GROW_HEAP)
  private case class LoadInd(dst: Int, base: Int, offset: Int, width: Int, signed: Boolean) extends AInstr
  private case class StoreInd(src: Int, base: Int, offset: Int, width: Int) extends AInstr
  private case object Trap extends AInstr

  private def loadIndOpcode(width: Int, signed: Boolean): Int = (width, signed) match
    case (1, false) => 124; case (1, true) => 125
    case (2, false) => 126; case (2, true) => 127
    case (4, false) => 128; case (4, true) => 129
    case (8, _)     => 130

  private def storeIndOpcode(width: Int): Int = width match
    case 1 => 120; case 2 => 121; case 4 => 122; case _ => 123

  private def sizeOf(a: AInstr): Int = a match
    case _: LoadImm64             => 10
    case GrowHeap                 => 2 // opcode byte + one-byte host id
    case _: LoadInd | _: StoreInd => 6
    case Trap                     => 1

  private def longLE(v: Long): Array[Byte] = Array.tabulate(8)(i => ((v >>> (i * 8)) & 0xff).toByte)
  private def intLE(v: Int): Array[Byte] = Array.tabulate(4)(i => ((v >>> (i * 8)) & 0xff).toByte)
  private def regByte(a: Int, b: Int): Byte = ((a & 0xf) | ((b & 0xf) << 4)).toByte

  private def encodeInstr(a: AInstr): Array[Byte] = a match
    case LoadImm64(reg, imm)         => Array[Byte](20.toByte, reg.toByte) ++ longLE(imm)
    case GrowHeap                    => Array[Byte](10.toByte, HostCall.GROW_HEAP.toByte)
    case LoadInd(dst, base, o, w, s) => Array[Byte](loadIndOpcode(w, s).toByte, regByte(dst, base)) ++ intLE(o)
    case StoreInd(src, base, o, w)   => Array[Byte](storeIndOpcode(w).toByte, regByte(src, base)) ++ intLE(o)
    case Trap                        => Array[Byte](0)

  private def encodeProgram(prog: Seq[AInstr]): (Array[Byte], Array[Byte]) =
    val offsets = prog.map(sizeOf).scanLeft(0)(_ + _)
    val bytes = prog.flatMap(encodeInstr).toArray
    val bitmask = new Array[Byte]((bytes.length + 7) / 8)
    prog.indices.foreach { i => val s = offsets(i); bitmask(s >> 3) = (bitmask(s >> 3) | (1 << (s & 7))).toByte }
    (bytes, bitmask)

  // ---- module/instance construction (real MemoryMap, real heapBase/maxHeapSize) ----

  private def buildInstance(rwLen: Int, code: Array[Byte], bitmask: Array[Byte]): InterpretedInstance =
    val blob = ProgramBlob(
      code = code, bitmask = bitmask, jumpTable = JumpTable(Array.empty, 0),
      is64Bit = true, roData = Array.empty, rwData = new Array[Byte](rwLen), stackSize = 4096
    )
    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(e)  => fail(s"module create failed: $e")
    InterpretedInstance.fromModule(module, forceStepTracing = false)

  private final case class RunResult(exit: Int, gas: Long, regs: Array[Long], pc: Long, heapSize: Int)

  /** The interpreted oracle: PvmRunner's executor loop, restricted to what these
    * programs can produce. */
  private def runInterpreter(inst: InterpretedInstance, initRegs: Array[Long], gas: Long): RunResult =
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(0))
    initRegs.zipWithIndex.foreach { case (v, i) => inst.setReg(i, v) }
    val wrapper = new InterpretedInstanceWrapper(inst)

    var exit = -1
    var running = true
    while running do
      inst.run() match
        case Right(InterruptKind.Panic)       => exit = PvmRecompiler.EXIT_PANIC; running = false
        case Right(InterruptKind.OutOfGas)    => exit = PvmRecompiler.EXIT_OOG; running = false
        case Right(InterruptKind.Finished)    => exit = PvmRecompiler.EXIT_HALT; running = false
        case Right(InterruptKind.Segfault(_)) => exit = PvmRecompiler.EXIT_FAULT; running = false
        case Right(InterruptKind.Step)        => ()
        case Right(InterruptKind.Ecalli(hostId)) =>
          val gasCost = GrowHeapDispatcher.getGasCost(hostId.signed, wrapper)
          inst.setGas(inst.gas - gasCost)
          if inst.gas < 0 then { exit = PvmRecompiler.EXIT_OOG; running = false }
          else GrowHeapDispatcher.dispatch(hostId.signed, wrapper)
        case Left(err) => fail(s"interpreter error: $err")

    RunResult(
      exit, inst.gas, Array.tabulate(13)(i => inst.getReg(i)),
      inst.programCounter.map(_.toInt.toLong & 0xFFFFFFFFL).getOrElse(fail("no programCounter set on exit")),
      inst.basicMemory.heapSize.signed
    )

  private def runNative(inst: InterpretedInstance, initRegs: Array[Long], gas: Long): RunResult =
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(0))
    initRegs.zipWithIndex.foreach { case (v, i) => inst.setReg(i, v) }

    NativeRunner.run(inst, entryPc = 0, io.forge.jam.pvm.ExecutionMode.Recompiled, GrowHeapDispatcher, None) match
      case None => fail("NativeRunner deopted — grow_heap must run natively through the ecalli upcall")
      case Some(o) =>
        val exit = o match
          case NativeRunner.RunOutcome.Halt => PvmRecompiler.EXIT_HALT
          case NativeRunner.RunOutcome.Panic => PvmRecompiler.EXIT_PANIC
          case NativeRunner.RunOutcome.OutOfGas => PvmRecompiler.EXIT_OOG
          case NativeRunner.RunOutcome.PageFault(_) => PvmRecompiler.EXIT_FAULT
        RunResult(
          exit, inst.gas, Array.tabulate(13)(inst.reg),
          inst.nextProgramCounter.map(_.toInt.toLong & 0xFFFFFFFFL).getOrElse(fail("no nextProgramCounter set on exit")),
          inst.basicMemory.heapSize.signed
        )

  /** Every byte the heap currently spans, for post-run memory comparison. */
  private def readHeapSlice(inst: InterpretedInstance): Array[Byte] =
    val mm = inst.module.memoryMap
    val len = (inst.basicMemory.heapEnd.toLong - mm.rwDataAddress.toLong).toInt
    if len <= 0 then Array.emptyByteArray
    else
      inst.basicMemory.getMemorySlice(mm.rwDataAddress, len) match
        case io.forge.jam.pvm.MemoryResult.Success(bytes) => bytes
        case _ => Array.emptyByteArray

  /** `h`, the current heap-end page, for a fresh instance. */
  private def heapPageBounds(inst: InterpretedInstance): (Long, Long) = inst.growHeapPageBounds

  private def compare(prog: Seq[AInstr], initRegs: Array[Long], gas: Long, clue: String): Unit =
    val (code, bitmask) = encodeProgram(prog)
    val interpInst = buildInstance(RW_DATA_LEN, code, bitmask)
    val nativeInst = buildInstance(RW_DATA_LEN, code, bitmask)
    val interp = runInterpreter(interpInst, initRegs.clone(), gas)
    val native = runNative(nativeInst, initRegs.clone(), gas)
    withClue(s"$clue\n interp(exit=${interp.exit} gas=${interp.gas} pc=${interp.pc} heap=${interp.heapSize})\n" +
      s"native(exit=${native.exit} gas=${native.gas} pc=${native.pc} heap=${native.heapSize})\n") {
      native.exit shouldBe interp.exit
      native.gas shouldBe interp.gas
      native.pc shouldBe interp.pc
      native.regs.toSeq shouldBe interp.regs.toSeq
      native.heapSize shouldBe interp.heapSize
      readHeapSlice(nativeInst).toSeq shouldBe readHeapSlice(interpInst).toSeq
    }

  /** `h` for a module built by `buildInstance` — the same for every program here
    * (heapBase and the initial heap size do not depend on the code). */
  private lazy val initialH: Long =
    val (code, bitmask) = encodeProgram(Seq(Trap))
    heapPageBounds(buildInstance(RW_DATA_LEN, code, bitmask))._1

  /** `b`, the highest page the heap may reach. */
  private lazy val initialB: Long =
    val (code, bitmask) = encodeProgram(Seq(Trap))
    heapPageBounds(buildInstance(RW_DATA_LEN, code, bitmask))._2

  private val GenerousGas = 100_000L

  // ==========================================================================
  // The differential
  // ==========================================================================

  "the recompiler's grow_heap upcall" should
    "match the interpreter across >=350 no-op grow_heap programs (requested <= h)" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x19110000L)
      val n = 350
      var i = 0
      while i < n do
        val requested = math.max(0L, initialH - rng.nextInt(8))
        val prog = Seq(LoadImm64(7, requested), GrowHeap, Trap)
        compare(prog, Array.fill(13)(0L), GenerousGas, s"requested=$requested h=$initialH")
        i += 1
      info(s"grow_heap oracle differential (no-op, requested <= h): $n programs matched the interpreter")
  }

  it should "match the interpreter across >=350 out-of-range grow_heap programs (requested > b)" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x19110001L)
      val n = 350
      var i = 0
      while i < n do
        val requested = initialB + 1 + rng.nextInt(1024)
        val prog = Seq(LoadImm64(7, requested), GrowHeap, Trap)
        compare(prog, Array.fill(13)(0L), GenerousGas, s"requested=$requested b=$initialB")
        i += 1
      info(s"grow_heap oracle differential (out of range, requested > b): $n programs matched the interpreter")
  }

  it should "match the interpreter across >=350 single-page growth programs that store/load the new page" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x19110002L)
      val n = 350
      var i = 0
      while i < n do
        val target = initialH + 1
        val offset = rng.nextInt(PAGE - 8)
        val value = rng.nextLong()
        // r5 = the first freshly-mapped byte (h * pageSize), then store and read back.
        val prog = Seq(
          LoadImm64(7, target),
          GrowHeap,
          LoadImm64(5, initialH * PAGE),
          LoadImm64(4, value),
          StoreInd(4, 5, offset, 8),
          LoadInd(6, 5, offset, 8, signed = false),
          Trap
        )
        compare(prog, Array.fill(13)(0L), GenerousGas, s"target=$target offset=$offset")
        i += 1
      info(s"grow_heap oracle differential (single-page growth + store/load): $n programs matched the interpreter")
  }

  it should "match the interpreter across >=350 multi-page growth programs" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x19110003L)
      val n = 350
      var i = 0
      while i < n do
        val pages = 1 + rng.nextInt(8)
        val target = initialH + pages
        // Write into the LAST page grown, so the region really has to cover it all.
        val offset = (pages - 1) * PAGE + rng.nextInt(PAGE - 8)
        val value = rng.nextLong()
        val prog = Seq(
          LoadImm64(7, target),
          GrowHeap,
          LoadImm64(5, initialH * PAGE),
          LoadImm64(4, value),
          StoreInd(4, 5, offset, 8),
          LoadInd(6, 5, offset, 8, signed = false),
          Trap
        )
        compare(prog, Array.fill(13)(0L), GenerousGas, s"pages=$pages target=$target offset=$offset")
        i += 1
      info(s"grow_heap oracle differential (multi-page growth): $n programs matched the interpreter")
  }

  it should "match the interpreter across >=250 chained grow_heap programs" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x19110004L)
      val n = 250
      var i = 0
      while i < n do
        val steps = 2 + rng.nextInt(4)
        val body = (1 to steps).flatMap { k =>
          Seq(LoadImm64(7, initialH + k.toLong), GrowHeap)
        }
        val prog = (body :+ Trap).toSeq
        compare(prog, Array.fill(13)(0L), GenerousGas, s"steps=$steps")
        i += 1
      info(s"grow_heap oracle differential (chained growth): $n programs matched the interpreter")
  }

  it should "match the interpreter when grow_heap cannot afford its own cost" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x19110005L)
      val n = 300
      var i = 0
      while i < n do
        val pages = 1 + rng.nextInt(64)
        val prog = Seq(LoadImm64(7, initialH + pages), GrowHeap, Trap)
        val callCost = GrowHeapHostCall.CgasGeminiConst + pages * GrowHeapHostCall.CgasGeminiLinear
        val blockCost =
          val (code, bitmask) = encodeProgram(prog)
          io.forge.jam.pvm.engine.BlockGasModel.gasCostForBlock(code, bitmask, 0)
        val gas = blockCost + callCost - 1 - rng.nextInt(10)
        compare(prog, Array.fill(13)(0L), gas, s"pages=$pages gas=$gas callCost=$callCost blockCost=$blockCost")
        i += 1
      info(s"grow_heap oracle differential: $n programs matched the interpreter")
  }
