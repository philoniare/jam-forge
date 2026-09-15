package io.forge.jam.protocol.accumulation

import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.engine.{InterpretedInstance, InterpretedModule}
import io.forge.jam.pvm.native_.PvmRecompiler
import io.forge.jam.pvm.program.{InstructionDecoder, JumpTable, ProgramBlob}
import io.forge.jam.pvm.types.ProgramCounter
import io.forge.jam.protocol.refine.HostCallDispatcher

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.util.Random

class SbrkOracleDifferentialSpec extends AnyFlatSpec with Matchers:

  private def libPath: Option[Path] =
    Option(System.getProperty("jam.pvm.recompiler.lib")).map(Path.of(_)).filter(Files.exists(_))

  private val RW_DATA_LEN = 64

  private object ScriptedDispatcher extends HostCallDispatcher:
    def getGasCost(hostCallId: Int, instance: PvmInstance): Long = 1L
    def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
      instance.setReg(7, instance.reg(7) + instance.reg(6))

  // ---- abstract program model ----------------------------------------------

  private sealed trait AInstr
  private case class LoadImm64(reg: Int, imm: Long) extends AInstr
  private case class Sbrk(dst: Int, src: Int) extends AInstr
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
    case _: Sbrk                  => 2 // opcode byte + one regs-nibble byte (regs2 family)
    case _: LoadInd | _: StoreInd => 6
    case Trap                     => 1

  private def longLE(v: Long): Array[Byte] = Array.tabulate(8)(i => ((v >>> (i * 8)) & 0xff).toByte)
  private def intLE(v: Int): Array[Byte] = Array.tabulate(4)(i => ((v >>> (i * 8)) & 0xff).toByte)
  private def regByte(a: Int, b: Int): Byte = ((a & 0xf) | ((b & 0xf) << 4)).toByte

  private def encodeInstrAt(a: AInstr, off: Int, tOff: Int): Array[Byte] = a match
    case LoadImm64(reg, imm)         => Array[Byte](20.toByte, reg.toByte) ++ longLE(imm)
    case Sbrk(dst, src)              => Array[Byte](101.toByte, regByte(dst, src))
    case LoadInd(dst, base, o, w, s) => Array[Byte](loadIndOpcode(w, s).toByte, regByte(dst, base)) ++ intLE(o)
    case StoreInd(src, base, o, w)   => Array[Byte](storeIndOpcode(w).toByte, regByte(src, base)) ++ intLE(o)
    case Trap                        => Array[Byte](0)

  private def encodeProgram(prog: Seq[AInstr]): (Array[Byte], Array[Byte]) =
    val offsets = prog.map(sizeOf).scanLeft(0)(_ + _)
    val code = scala.collection.mutable.ArrayBuffer.empty[Byte]
    prog.zipWithIndex.foreach { case (a, i) => code ++= encodeInstrAt(a, offsets(i), 0) }
    val bytes = code.toArray
    val bitmask = new Array[Byte]((bytes.length + 7) / 8)
    prog.indices.foreach { i => val s = offsets(i); bitmask(s >> 3) = (bitmask(s >> 3) | (1 << (s & 7))).toByte }
    (bytes, bitmask)

  // ---- self-check: the encoder round-trips Sbrk through the real decoder ----

  "the PVM encoder" should "round-trip Sbrk through the real decoder with the correct dst/src" in {
    val prog = Seq(LoadImm64(3, 42L), Sbrk(5, 3), Trap)
    val (code, bitmask) = encodeProgram(prog)
    var off = 0
    val decoded = scala.collection.mutable.ArrayBuffer.empty[io.forge.jam.pvm.Instruction]
    while off < code.length do
      val (instr, skip) = InstructionDecoder.decode(code, bitmask, off)
      decoded += instr
      off += skip
    decoded(1) shouldBe io.forge.jam.pvm.Instruction.Sbrk(5, 3)
  }

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

  // ---- interpreter run: Sbrk is a plain instruction, no InterruptKind.Ecalli case needed ----

  private final case class InterpResult(exit: Int, gas: Long, regs: Array[Long], pc: Long)

  private def runInterpreter(inst: InterpretedInstance, initRegs: Array[Long], gas: Long): InterpResult =
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(0))
    initRegs.zipWithIndex.foreach { case (v, i) => inst.setReg(i, v) }

    var exit = -1
    var running = true
    while running do
      inst.run() match
        case Right(InterruptKind.Panic)       => exit = PvmRecompiler.EXIT_PANIC; running = false
        case Right(InterruptKind.OutOfGas)    => exit = PvmRecompiler.EXIT_OOG; running = false
        case Right(InterruptKind.Finished)    => exit = PvmRecompiler.EXIT_HALT; running = false
        case Right(InterruptKind.Segfault(_)) => exit = PvmRecompiler.EXIT_PANIC; running = false
        case Right(InterruptKind.Step)        => ()
        case Right(InterruptKind.Ecalli(hostId)) =>
          // Executor-loop order (mirrors PvmRunner.run) — only reachable if a
          // test mixes in a real Ecalli; ScriptedDispatcher.getGasCost==1 always.
          val gasCost = ScriptedDispatcher.getGasCost(hostId.signed, new InterpretedInstanceWrapper(inst))
          inst.setGas(inst.gas - gasCost)
          if inst.gas < 0 then { exit = PvmRecompiler.EXIT_OOG; running = false }
          else ScriptedDispatcher.dispatch(hostId.signed, new InterpretedInstanceWrapper(inst))
        case Left(err) => fail(s"interpreter error: $err")

    val regs = Array.tabulate(13)(i => inst.getReg(i))
    val pc = inst.programCounter.map(_.toInt.toLong & 0xFFFFFFFFL).getOrElse(fail("no programCounter set on exit"))
    InterpResult(exit, inst.gas, regs, pc)

  // ---- native run: NativeRunner's dispatcher-aware path (Sbrk goes through the new upcall) ----

  private final case class NativeResult(exit: Int, gas: Long, regs: Array[Long], pc: Long)

  private def runNative(inst: InterpretedInstance, initRegs: Array[Long], gas: Long): NativeResult =
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(0))
    initRegs.zipWithIndex.foreach { case (v, i) => inst.setReg(i, v) }

    val outcome = NativeRunner.run(inst, entryPc = 0, io.forge.jam.pvm.ExecutionMode.Recompiled, ScriptedDispatcher, None)
    outcome match
      case None => fail("NativeRunner deopted — expected a native run (Sbrk is supported with a dispatcher, Task 18/H2)")
      case Some(o) =>
        val exit = o match
          case NativeRunner.RunOutcome.Halt => PvmRecompiler.EXIT_HALT
          case NativeRunner.RunOutcome.Panic => PvmRecompiler.EXIT_PANIC
          case NativeRunner.RunOutcome.OutOfGas => PvmRecompiler.EXIT_OOG
          case NativeRunner.RunOutcome.PageFault(_) => PvmRecompiler.EXIT_FAULT
        val regs = Array.tabulate(13)(inst.reg)
        val pc = inst.nextProgramCounter.map(_.toInt.toLong & 0xFFFFFFFFL).getOrElse(fail("no nextProgramCounter set on exit"))
        NativeResult(exit, inst.gas, regs, pc)

  private def readHeapSlice(inst: InterpretedInstance): Array[Byte] =
    val mm = inst.module.memoryMap
    val len = (inst.basicMemory.heapEnd.toLong - mm.rwDataAddress.toLong).toInt
    if len <= 0 then Array.emptyByteArray
    else
      inst.basicMemory.getMemorySlice(mm.rwDataAddress, len) match
        case io.forge.jam.pvm.MemoryResult.Success(bytes) => bytes
        case _ => Array.emptyByteArray

  private def canRunNative: Boolean =
    val arch = System.getProperty("os.arch", "").toLowerCase
    (arch == "aarch64" || arch == "arm64") && libPath.isDefined

  // ==========================================================================
  // Generators
  // ==========================================================================

  /** sbrk(0): read the current heap end without growing. */
  private def genSbrkZero(rng: Random): Seq[AInstr] =
    Seq(LoadImm64(5, 0L), Sbrk(3, 5), Trap)

  /** Grow by `growBytes`, then store/load in the newly-covered page(s). */
  private def genGrowThenStoreLoad(rng: Random, growBytes: Int): Seq[AInstr] =
    val storeVal = rng.nextLong()
    Seq(
      LoadImm64(5, growBytes.toLong),
      Sbrk(3, 5),               // r3 = old heap end (the first freshly-grown byte)
      LoadImm64(4, storeVal),
      StoreInd(4, 3, 0, 8),     // store 8 bytes at [r3 + 0]
      LoadInd(6, 3, 0, 8, false), // load them back into r6
      Trap
    )

  /** Growth crossing multiple pages (a single large sbrk call). */
  private def genMultiPageGrow(rng: Random): Seq[AInstr] =
    val pages = 1 + rng.nextInt(4)
    genGrowThenStoreLoad(rng, pages * 4096 + rng.nextInt(4096))

  private def genSbrkOverflowFailure(rng: Random): Seq[AInstr] =
    Seq(LoadImm64(5, 0xFFFFFFFFL), Sbrk(3, 5), Trap)

  /** dst==src aliasing: the size register is overwritten with the sbrk result. */
  private def genDstEqualsSrcAliasing(rng: Random, growBytes: Int): Seq[AInstr] =
    Seq(LoadImm64(5, growBytes.toLong), Sbrk(5, 5), Trap)

  /** Multiple sbrks in one program, each expected to return the immediately
    * preceding heap end (chained growth). */
  private def genChainedSbrks(rng: Random, n: Int, stepBytes: Int): Seq[AInstr] =
    val body = (0 until n).flatMap { i =>
      Seq(LoadImm64(5, stepBytes.toLong), Sbrk(2 + (i % 8), 5))
    }
    body :+ Trap

  // ==========================================================================
  // The differential
  // ==========================================================================

  "the recompiler's native Sbrk" should "match the interpreter across >=350 sbrk(0) programs" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x18110000L)
      val n = 350
      var i = 0
      while i < n do
        val prog = genSbrkZero(rng)
        val (code, bitmask) = encodeProgram(prog)
        val initRegs = Array.fill(13)(0L)
        val gas = 100L

        val interpInst = buildInstance(RW_DATA_LEN, code, bitmask)
        val nativeInst = buildInstance(RW_DATA_LEN, code, bitmask)
        val interp = runInterpreter(interpInst, initRegs, gas)
        val native = runNative(nativeInst, initRegs, gas)

        withClue(s"program=$prog gas=$gas\n") {
          native.exit shouldBe interp.exit
          native.gas shouldBe interp.gas
          native.pc shouldBe interp.pc
          native.regs.toSeq shouldBe interp.regs.toSeq
        }
        i += 1
      info(s"Sbrk oracle differential (sbrk(0)): $n programs matched the interpreter")
  }

  it should "match the interpreter across >=350 grow-then-store/load programs (single page)" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x18110001L)
      val n = 350
      var i = 0
      while i < n do
        val growBytes = 8 + rng.nextInt(4089) // within one page, always > 0
        val prog = genGrowThenStoreLoad(rng, growBytes)
        val (code, bitmask) = encodeProgram(prog)
        val initRegs = Array.fill(13)(0L)
        val gas = 200L

        val interpInst = buildInstance(RW_DATA_LEN, code, bitmask)
        val nativeInst = buildInstance(RW_DATA_LEN, code, bitmask)
        val interp = runInterpreter(interpInst, initRegs, gas)
        val native = runNative(nativeInst, initRegs, gas)

        withClue(s"program=$prog growBytes=$growBytes gas=$gas\n") {
          native.exit shouldBe interp.exit
          native.exit shouldBe PvmRecompiler.EXIT_PANIC // Trap after the grow/store/load
          native.gas shouldBe interp.gas
          native.pc shouldBe interp.pc
          native.regs.toSeq shouldBe interp.regs.toSeq
          readHeapSlice(nativeInst).toSeq shouldBe readHeapSlice(interpInst).toSeq
        }
        i += 1
      info(s"Sbrk oracle differential (grow-then-store/load, single page): $n programs matched the interpreter")
  }

  it should "match the interpreter across >=350 multi-page growth programs" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x18110002L)
      val n = 350
      var i = 0
      while i < n do
        val prog = genMultiPageGrow(rng)
        val (code, bitmask) = encodeProgram(prog)
        val initRegs = Array.fill(13)(0L)
        val gas = 200L

        val interpInst = buildInstance(RW_DATA_LEN, code, bitmask)
        val nativeInst = buildInstance(RW_DATA_LEN, code, bitmask)
        val interp = runInterpreter(interpInst, initRegs, gas)
        val native = runNative(nativeInst, initRegs, gas)

        withClue(s"program=$prog gas=$gas\n") {
          native.exit shouldBe interp.exit
          native.gas shouldBe interp.gas
          native.pc shouldBe interp.pc
          native.regs.toSeq shouldBe interp.regs.toSeq
          readHeapSlice(nativeInst).toSeq shouldBe readHeapSlice(interpInst).toSeq
        }
        i += 1
      info(s"Sbrk oracle differential (multi-page growth): $n programs matched the interpreter")
  }

  it should "match the interpreter on sbrk failure (u32 overflow) across >=300 programs — panic parity" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x18110003L)
      val n = 300
      var i = 0
      while i < n do
        val prog = genSbrkOverflowFailure(rng)
        val (code, bitmask) = encodeProgram(prog)
        val initRegs = Array.fill(13)(0L)
        val gas = 100L

        val interpInst = buildInstance(RW_DATA_LEN, code, bitmask)
        val nativeInst = buildInstance(RW_DATA_LEN, code, bitmask)
        val interp = runInterpreter(interpInst, initRegs, gas)
        val native = runNative(nativeInst, initRegs, gas)

        withClue(s"program=$prog gas=$gas\n") {
          interp.exit shouldBe PvmRecompiler.EXIT_PANIC
          native.exit shouldBe interp.exit
          native.gas shouldBe interp.gas
          native.pc shouldBe interp.pc
          native.regs.toSeq shouldBe interp.regs.toSeq
        }
        i += 1
      info(s"Sbrk oracle differential (u32-overflow failure -> panic parity): $n programs matched the interpreter")
  }

  it should "match the interpreter on dst==src aliasing (sbrk result overwrites the size register) across >=300 programs" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x18110004L)
      val n = 300
      var i = 0
      while i < n do
        val growBytes = 8 + rng.nextInt(4089)
        val prog = genDstEqualsSrcAliasing(rng, growBytes)
        val (code, bitmask) = encodeProgram(prog)
        val initRegs = Array.fill(13)(0L)
        val gas = 100L

        val interpInst = buildInstance(RW_DATA_LEN, code, bitmask)
        val nativeInst = buildInstance(RW_DATA_LEN, code, bitmask)
        val interp = runInterpreter(interpInst, initRegs, gas)
        val native = runNative(nativeInst, initRegs, gas)

        withClue(s"program=$prog growBytes=$growBytes gas=$gas\n") {
          native.exit shouldBe interp.exit
          native.gas shouldBe interp.gas
          native.pc shouldBe interp.pc
          native.regs.toSeq shouldBe interp.regs.toSeq
        }
        i += 1
      info(s"Sbrk oracle differential (dst==src aliasing): $n programs matched the interpreter")
  }

  it should "match the interpreter across >=250 chained-sbrk programs (multiple sbrks in one execution)" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x18110005L)
      val n = 250
      var i = 0
      while i < n do
        val steps = 2 + rng.nextInt(4)
        val stepBytes = 8 + rng.nextInt(2048)
        val prog = genChainedSbrks(rng, steps, stepBytes)
        val (code, bitmask) = encodeProgram(prog)
        val initRegs = Array.fill(13)(0L)
        val gas = 300L

        val interpInst = buildInstance(RW_DATA_LEN, code, bitmask)
        val nativeInst = buildInstance(RW_DATA_LEN, code, bitmask)
        val interp = runInterpreter(interpInst, initRegs, gas)
        val native = runNative(nativeInst, initRegs, gas)

        withClue(s"program=$prog steps=$steps stepBytes=$stepBytes gas=$gas\n") {
          native.exit shouldBe interp.exit
          native.gas shouldBe interp.gas
          native.pc shouldBe interp.pc
          native.regs.toSeq shouldBe interp.regs.toSeq
        }
        i += 1
      info(s"Sbrk oracle differential (chained sbrks): $n programs matched the interpreter")
  }
