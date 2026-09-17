package io.forge.jam.protocol.accumulation

import io.forge.jam.pvm.{Instruction, InterruptKind}
import io.forge.jam.pvm.engine.{InterpretedInstance, InterpretedModule}
import io.forge.jam.pvm.native_.PvmRecompiler
import io.forge.jam.pvm.program.{InstructionDecoder, JumpTable, ProgramBlob}
import io.forge.jam.pvm.types.ProgramCounter
import io.forge.jam.protocol.refine.HostCallDispatcher

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.util.Random

class OracleDifferentialSpec extends AnyFlatSpec with Matchers:

  private def libPath: Option[Path] =
    Option(System.getProperty("jam.pvm.recompiler.lib")).map(Path.of(_)).filter(Files.exists(_))

  private val RW_BASE = 0x20000
  private val RW_LEN = 4096

  private object ScriptedException extends RuntimeException("scripted host call 2: intentional throw")

  private object ScriptedDispatcher extends HostCallDispatcher:
    def getGasCost(hostCallId: Int, instance: PvmInstance): Long =
      hostCallId match
        case 3 => 1_000_000L // "big gasCost" — engineered to exceed any test's gas budget
        case _ => 3L
    def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
      hostCallId match
        case 0 =>
          instance.setReg(7, instance.reg(7) + instance.reg(6))
        case 4 =>
          val addr = instance.reg(5).toInt
          val len = instance.reg(8).toInt
          if len > 0 && len <= 256 then
            val byte = instance.reg(6).toByte
            instance.writeBytes(addr, Array.fill(len)(byte))
          ()
        case 2 =>
          throw ScriptedException
        case _ =>
          () // no-op for any other id (defensive; the generator never emits one)

  // ---- a call-counting wrapper proving OOG-without-dispatch (host id 3) ------

  private final class CountingDispatcher extends HostCallDispatcher:
    var dispatchCalls: Int = 0
    def getGasCost(hostCallId: Int, instance: PvmInstance): Long = ScriptedDispatcher.getGasCost(hostCallId, instance)
    def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
      dispatchCalls += 1
      ScriptedDispatcher.dispatch(hostCallId, instance)

  // ---- abstract program model (mirrors the pvm module's OracleDifferentialSpec) ----

  private sealed trait AInstr
  private case class LoadImm64(reg: Int, imm: Long) extends AInstr
  private case class AddImm64(dst: Int, src: Int, imm32: Int) extends AInstr
  private case class Add64(d: Int, s1: Int, s2: Int) extends AInstr
  private case class Sub64(d: Int, s1: Int, s2: Int) extends AInstr
  private case class Mul64(d: Int, s1: Int, s2: Int) extends AInstr
  private case class Jump(targetIdx: Int) extends AInstr
  private case class BranchEq(r1: Int, r2: Int, targetIdx: Int) extends AInstr
  private case class BranchNe(r1: Int, r2: Int, targetIdx: Int) extends AInstr
  private case class LoadInd(dst: Int, base: Int, offset: Int, width: Int, signed: Boolean) extends AInstr
  private case class StoreInd(src: Int, base: Int, offset: Int, width: Int) extends AInstr
  private case class Ecalli(hostId: Int) extends AInstr
  private case object Trap extends AInstr

  private def loadIndOpcode(width: Int, signed: Boolean): Int = (width, signed) match
    case (1, false) => 124; case (1, true) => 125
    case (2, false) => 126; case (2, true) => 127
    case (4, false) => 128; case (4, true) => 129
    case (8, _)     => 130

  private def storeIndOpcode(width: Int): Int = width match
    case 1 => 120; case 2 => 121; case 4 => 122; case _ => 123

  private def sizeOf(a: AInstr): Int = a match
    case _: LoadImm64                   => 10
    case _: AddImm64                    => 6
    case _: Add64 | _: Sub64 | _: Mul64 => 3
    case _: Jump                        => 5
    case _: BranchEq | _: BranchNe      => 6
    case _: LoadInd | _: StoreInd       => 6
    case _: Ecalli                      => 5 // [op] ++ 4-byte hostId LE
    case Trap                           => 1

  private def longLE(v: Long): Array[Byte] = Array.tabulate(8)(i => ((v >>> (i * 8)) & 0xff).toByte)
  private def intLE(v: Int): Array[Byte] = Array.tabulate(4)(i => ((v >>> (i * 8)) & 0xff).toByte)
  private def regByte(a: Int, b: Int): Byte = ((a & 0xf) | ((b & 0xf) << 4)).toByte

  private def encodeInstrAt(a: AInstr, off: Int, tOff: Int): Array[Byte] = a match
    case LoadImm64(reg, imm)         => Array[Byte](20.toByte, reg.toByte) ++ longLE(imm)
    case AddImm64(dst, src, imm)     => Array[Byte](149.toByte, regByte(dst, src)) ++ intLE(imm)
    case Add64(d, s1, s2)            => Array[Byte](200.toByte, regByte(s1, s2), d.toByte)
    case Sub64(d, s1, s2)            => Array[Byte](201.toByte, regByte(s1, s2), d.toByte)
    case Mul64(d, s1, s2)            => Array[Byte](202.toByte, regByte(s1, s2), d.toByte)
    case Jump(_)                     => Array[Byte](40.toByte) ++ intLE(tOff - off)
    case BranchEq(r1, r2, _)         => Array[Byte](170.toByte, regByte(r1, r2)) ++ intLE(tOff - off)
    case BranchNe(r1, r2, _)         => Array[Byte](171.toByte, regByte(r1, r2)) ++ intLE(tOff - off)
    case LoadInd(dst, base, o, w, s) => Array[Byte](loadIndOpcode(w, s).toByte, regByte(dst, base)) ++ intLE(o)
    case StoreInd(src, base, o, w)   => Array[Byte](storeIndOpcode(w).toByte, regByte(src, base)) ++ intLE(o)
    case Ecalli(hostId)              => Array[Byte](10.toByte) ++ intLE(hostId)
    case Trap                        => Array[Byte](0)

  private def targetIdxOf(a: AInstr): Option[Int] = a match
    case Jump(t) => Some(t)
    case BranchEq(_, _, t) => Some(t)
    case BranchNe(_, _, t) => Some(t)
    case _ => None

  private def encodeProgram(prog: Seq[AInstr]): (Array[Byte], Array[Byte]) =
    val offsets = prog.map(sizeOf).scanLeft(0)(_ + _)
    val code = scala.collection.mutable.ArrayBuffer.empty[Byte]
    prog.zipWithIndex.foreach { case (a, i) =>
      val tOff = targetIdxOf(a).map(offsets).getOrElse(0)
      code ++= encodeInstrAt(a, offsets(i), tOff)
    }
    val bytes = code.toArray
    val bitmask = new Array[Byte]((bytes.length + 7) / 8)
    prog.indices.foreach { i => val s = offsets(i); bitmask(s >> 3) = (bitmask(s >> 3) | (1 << (s & 7))).toByte }
    (bytes, bitmask)

  // ---- self-check: the encoder round-trips Ecalli through the real decoder ----

  "the PVM encoder" should "round-trip Ecalli through the real decoder with the correct hostId" in {
    val prog = Seq(LoadImm64(3, 42L), Ecalli(7), Ecalli(-1), Trap)
    val (code, bitmask) = encodeProgram(prog)
    var off = 0
    val decoded = scala.collection.mutable.ArrayBuffer.empty[Instruction]
    while off < code.length do
      val (instr, skip) = InstructionDecoder.decode(code, bitmask, off)
      decoded += instr
      off += skip
    decoded(1) shouldBe Instruction.Ecalli(7L)
    decoded(2) shouldBe Instruction.Ecalli(-1L) // sign-extended 4-byte hostId
  }

  // ---- interpreter oracle: the executor-loop Ecalli order, against a real InterpretedInstance ----

  private final case class InterpResult(exit: Int, gas: Long, regs: Array[Long], pc: Long, dispatchCount: Int)

  private def runInterpreter(prog: Seq[AInstr], initRegs: Array[Long], gas: Long, rwData: Array[Byte], dispatcher: HostCallDispatcher)
      : (InterpResult, Array[Byte]) =
    val (code, bitmask) = encodeProgram(prog)
    val blob = ProgramBlob(
      code = code, bitmask = bitmask, jumpTable = JumpTable(Array.empty, 0),
      is64Bit = true, roData = Array.empty, rwData = rwData, stackSize = 4096
    )
    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(e)  => fail(s"module create failed: $e")
    val inst = InterpretedInstance.fromModule(module, forceStepTracing = false)
    val wrapper = new InterpretedInstanceWrapper(inst)
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(0))
    initRegs.zipWithIndex.foreach { case (v, i) => inst.setReg(i, v) }

    var exit = -1
    var dispatchCount = 0
    var running = true
    while running do
      inst.run() match
        case Right(InterruptKind.Panic)    => exit = PvmRecompiler.EXIT_PANIC; running = false
        case Right(InterruptKind.OutOfGas) => exit = PvmRecompiler.EXIT_OOG; running = false
        case Right(InterruptKind.Finished) => exit = PvmRecompiler.EXIT_HALT; running = false
        case Right(InterruptKind.Segfault(_)) => exit = PvmRecompiler.EXIT_PANIC; running = false
        case Right(InterruptKind.Step)     => ()
        case Right(InterruptKind.Ecalli(hostId)) =>
          // Executor-loop order (PvmRunner.run, "Facts" in the plan): the 1-gas
          // per-instruction charge already happened inside inst.run() before
          // this interrupt was returned. gasCost = getGasCost(...); gas -=
          // gasCost; if gas < 0 -> OOG WITHOUT dispatching; else dispatch,
          // catching RuntimeException -> PANIC.
          val gasCost = dispatcher.getGasCost(hostId.signed, wrapper)
          inst.setGas(inst.gas - gasCost)
          if inst.gas < 0 then { exit = PvmRecompiler.EXIT_OOG; running = false }
          else
            try
              dispatchCount += 1
              dispatcher.dispatch(hostId.signed, wrapper)
            catch
              case _: RuntimeException =>
                exit = PvmRecompiler.EXIT_PANIC
                running = false
        case Left(err) => fail(s"interpreter error: $err")

    val regs = Array.tabulate(13)(i => inst.getReg(i))
    val pc = inst.programCounter.map(_.toInt.toLong & 0xFFFFFFFFL).getOrElse(fail("no programCounter set on exit"))
    val rwAfter = inst.basicMemory.getMemorySlice(spire.math.UInt(RW_BASE), RW_LEN) match
      case io.forge.jam.pvm.MemoryResult.Success(bytes) => bytes
      case _ => rwData
    (InterpResult(exit, inst.gas, regs, pc, dispatchCount), rwAfter)

  // ---- native run: NativeRunner's dispatcher-aware path ----------------------

  private final case class NativeResult(exit: Int, gas: Long, regs: Array[Long], pc: Long)

  private def runNative(
      prog: Seq[AInstr],
      initRegs: Array[Long],
      gas: Long,
      rwData: Array[Byte],
      dispatcher: HostCallDispatcher,
      preDispatch: Option[() => Unit] = None
  ): (NativeResult, Array[Byte]) =
    val (code, bitmask) = encodeProgram(prog)
    val blob = ProgramBlob(
      code = code, bitmask = bitmask, jumpTable = JumpTable(Array.empty, 0),
      is64Bit = true, roData = Array.empty, rwData = rwData, stackSize = 4096
    )
    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(e)  => fail(s"module create failed: $e")
    val inst = InterpretedInstance.fromModule(module, forceStepTracing = false)
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(0))
    initRegs.zipWithIndex.foreach { case (v, i) => inst.setReg(i, v) }

    val outcome = NativeRunner.run(inst, entryPc = 0, io.forge.jam.pvm.ExecutionMode.Recompiled, dispatcher, preDispatch)
    outcome match
      case None => fail("NativeRunner deopted — expected a native run (Ecalli is supported with a dispatcher)")
      case Some(o) =>
        val exit = o match
          case NativeRunner.RunOutcome.Halt => PvmRecompiler.EXIT_HALT
          case NativeRunner.RunOutcome.Panic => PvmRecompiler.EXIT_PANIC
          case NativeRunner.RunOutcome.OutOfGas => PvmRecompiler.EXIT_OOG
          case NativeRunner.RunOutcome.PageFault(_) => PvmRecompiler.EXIT_FAULT
        val regs = Array.tabulate(13)(inst.reg)
        val pc = inst.nextProgramCounter.map(_.toInt.toLong & 0xFFFFFFFFL).getOrElse(fail("no nextProgramCounter set on exit"))
        val rwAfter = inst.basicMemory.getMemorySlice(spire.math.UInt(RW_BASE), RW_LEN) match
          case io.forge.jam.pvm.MemoryResult.Success(bytes) => bytes
          case _ => rwData
        (NativeResult(exit, inst.gas, regs, pc), rwAfter)

  // ---- generators --------------------------------------------------------------

  private def canRunNative: Boolean =
    val arch = System.getProperty("os.arch", "").toLowerCase
    (arch == "aarch64" || arch == "arm64") && libPath.isDefined

  private def randArith(rng: Random): AInstr = rng.nextInt(5) match
    case 0 => LoadImm64(1 + rng.nextInt(12), rng.nextLong())
    case 1 => AddImm64(1 + rng.nextInt(12), rng.nextInt(13), rng.nextInt())
    case 2 => Add64(1 + rng.nextInt(12), rng.nextInt(13), rng.nextInt(13))
    case 3 => Sub64(1 + rng.nextInt(12), rng.nextInt(13), rng.nextInt(13))
    case _ => Mul64(1 + rng.nextInt(12), rng.nextInt(13), rng.nextInt(13))

  private def randMem(rng: Random): AInstr =
    val (w, s) = rng.nextInt(7) match
      case 0 => (1, false); case 1 => (1, true); case 2 => (2, false); case 3 => (2, true)
      case 4 => (4, false); case 5 => (4, true); case _ => (8, false)
    val offset = rng.nextInt(RW_LEN - 16)
    if rng.nextBoolean() then LoadInd(1 + rng.nextInt(12), 0, offset, w, s)
    else StoreInd(rng.nextInt(13), 0, offset, w)

  private def genMixedProgram(rng: Random): Seq[AInstr] =
    val k = 2 + rng.nextInt(4)
    val bodyLens = Array.fill(k)(1 + rng.nextInt(3))
    val starts = bodyLens.map(_ + 1).scanLeft(0)(_ + _)
    val out = scala.collection.mutable.ArrayBuffer.empty[AInstr]
    for b <- 0 until k do
      for _ <- 0 until bodyLens(b) do
        out += (rng.nextInt(4) match
          case 0 => Ecalli(rng.nextInt(2) * 4) // 0 or 4 (id 1 is grow_heap in gp-0.8: deopts)
          case 1 => randMem(rng)
          case _ => randArith(rng))
      if b == k - 1 then out += Trap
      else
        val tb = b + 1 + rng.nextInt(k - b - 1)
        val tgt = starts(tb)
        out += (rng.nextInt(3) match
          case 0 => Jump(tgt)
          case 1 => BranchEq(rng.nextInt(13), rng.nextInt(13), tgt)
          case _ => BranchNe(rng.nextInt(13), rng.nextInt(13), tgt))
    out.toSeq

  // ---- the differential ---------------------------------------------------------
  "the recompiler's inline Ecalli upcall" should "match the interpreter executor-loop across >=10000 mixed programs (host ids 0/4, CONTINUE-only)" ignore {
    if !canRunNative then
      cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x17110000L)
      val n = 12000
      var i = 0
      while i < n do
        val prog = genMixedProgram(rng)
        val initRegs = Array.fill(13)(rng.nextLong())
        initRegs(0) = RW_BASE.toLong // r0: LoadInd/StoreInd base register
        initRegs(5) = RW_BASE.toLong // r5: host-id-1 write target base
        val gas = (prog.length * 10).toLong + rng.nextInt(200) // generous: 1/instr + up to 3/ecalli, times headroom
        val rwData = new Array[Byte](RW_LEN)
        rng.nextBytes(rwData)

        val (interp, interpRw) = runInterpreter(prog, initRegs, gas, rwData.clone(), ScriptedDispatcher)
        val (native, nativeRw) = runNative(prog, initRegs, gas, rwData.clone(), ScriptedDispatcher)

        withClue(s"program=$prog gas=$gas initRegs=${initRegs.toSeq}\n" +
          s"interp(exit=${interp.exit} gas=${interp.gas} pc=${interp.pc})\n" +
          s"native(exit=${native.exit} gas=${native.gas} pc=${native.pc})\n") {
          native.exit shouldBe interp.exit
          native.gas shouldBe interp.gas
          native.pc shouldBe interp.pc
          native.regs.toSeq shouldBe interp.regs.toSeq
          nativeRw.toSeq shouldBe interpRw.toSeq
        }
        i += 1
      info(s"oracle differential (Task 17/H1, CONTINUE-only mixed): $n programs matched the interpreter")
  }

  it should "match the interpreter on host id 2 (throw -> PANIC) mixed into the program" ignore {
    if !canRunNative then
      cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x17110001L)
      var i = 0
      val n = 2000
      while i < n do
        val prefixLen = rng.nextInt(5)
        val prefix = (0 until prefixLen).map(_ => randArith(rng))
        val prog = (prefix :+ Ecalli(2)) ++ Seq(randArith(rng), Trap)
        val initRegs = Array.fill(13)(rng.nextLong())
        initRegs(5) = RW_BASE.toLong
        val gas = (prog.length * 10).toLong + 100
        val rwData = new Array[Byte](RW_LEN)

        val (interp, _) = runInterpreter(prog, initRegs, gas, rwData.clone(), ScriptedDispatcher)
        val (native, _) = runNative(prog, initRegs, gas, rwData.clone(), ScriptedDispatcher)

        withClue(s"program=$prog gas=$gas\n") {
          interp.exit shouldBe PvmRecompiler.EXIT_PANIC
          native.exit shouldBe interp.exit
          native.gas shouldBe interp.gas
          native.pc shouldBe interp.pc
          native.regs.toSeq shouldBe interp.regs.toSeq
        }
        i += 1
      info(s"oracle differential (Task 17/H1, throw->PANIC): $n programs matched the interpreter")
  }

  it should "match the interpreter on host id 4 (memory write) mixed into the program" ignore {
    if !canRunNative then
      cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val rng = new Random(0x17110002L)
      var i = 0
      val n = 2000
      while i < n do
        val len = 1 + rng.nextInt(64)
        val byteVal = rng.nextInt(256)
        val prog = Seq(
          LoadImm64(5, RW_BASE.toLong + rng.nextInt(RW_LEN - 256)),
          LoadImm64(6, byteVal.toLong),
          LoadImm64(8, len.toLong),
          Ecalli(4),
          Trap
        )
        val initRegs = Array.fill(13)(0L)
        val gas = 200L
        val rwData = new Array[Byte](RW_LEN)
        rng.nextBytes(rwData)

        val (interp, interpRw) = runInterpreter(prog, initRegs, gas, rwData.clone(), ScriptedDispatcher)
        val (native, nativeRw) = runNative(prog, initRegs, gas, rwData.clone(), ScriptedDispatcher)

        withClue(s"program=$prog len=$len byteVal=$byteVal\n") {
          native.exit shouldBe interp.exit
          native.exit shouldBe PvmRecompiler.EXIT_PANIC // Trap after the write
          native.gas shouldBe interp.gas
          nativeRw.toSeq shouldBe interpRw.toSeq
        }
        i += 1
      info(s"oracle differential (Task 17/H1, memory-write host call): $n programs matched the interpreter")
  }

  // ---- gas-edge cases: exactly 0 (dispatch proceeds) and exactly -1 (OOG, no dispatch) ----
  it should "dispatch when gasCost drives gas to exactly 0" ignore {
    if !canRunNative then
      cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val prog = Seq(Ecalli(0), Trap)
      val initRegs = Array.fill(13)(0L)
      initRegs(6) = 5L
      initRegs(7) = 10L
      val gas = 4L // 1 (ecalli charge) + 3 (gasCost) = 4, remaining exactly 0 after dispatch

      val interpDispatcher = new CountingDispatcher
      val nativeDispatcher = new CountingDispatcher
      val (interp, _) = runInterpreter(prog, initRegs, gas, new Array[Byte](RW_LEN), interpDispatcher)
      val (native, _) = runNative(prog, initRegs, gas, new Array[Byte](RW_LEN), nativeDispatcher)

      interp.exit shouldBe PvmRecompiler.EXIT_OOG // Trap's own charge against the already-0 gas
      interp.gas shouldBe -1L
      interpDispatcher.dispatchCalls shouldBe 1 // dispatch proceeded at exactly 0 (the assertion this test exists for)
      native.exit shouldBe interp.exit
      native.gas shouldBe interp.gas
      nativeDispatcher.dispatchCalls shouldBe 1
      native.regs(7) shouldBe interp.regs(7)
      native.regs(7) shouldBe 15L // r7 += r6 (host id 0), 10+5 — proves dispatch's mutation happened before the later OOG
  }

  it should "NOT dispatch when gasCost drives gas to exactly -1 (OOG)" ignore {
    if !canRunNative then
      cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      // Same shape, but ONE LESS starting gas: 1 (ecalli charge) + 3
      // (gasCost) would need gas=4 to land at 0; gas=3 lands at exactly -1.
      val prog = Seq(Ecalli(0), Trap)
      val initRegs = Array.fill(13)(0L)
      initRegs(6) = 5L
      initRegs(7) = 10L
      val gas = 3L

      val interpDispatcher = new CountingDispatcher
      val nativeDispatcher = new CountingDispatcher
      val (interp, _) = runInterpreter(prog, initRegs, gas, new Array[Byte](RW_LEN), interpDispatcher)
      val (native, _) = runNative(prog, initRegs, gas, new Array[Byte](RW_LEN), nativeDispatcher)

      interp.exit shouldBe PvmRecompiler.EXIT_OOG
      interp.gas shouldBe -1L
      interpDispatcher.dispatchCalls shouldBe 0 // OOG-without-dispatch — scripted dispatcher NOT called

      native.exit shouldBe interp.exit
      native.gas shouldBe interp.gas
      nativeDispatcher.dispatchCalls shouldBe 0 // the assertion this whole test exists for
      native.regs(7) shouldBe interp.regs(7)
      native.regs(7) shouldBe 10L // unchanged — dispatch never ran, r7 keeps its seeded value
  }

  it should "NOT dispatch when host id 3's big gasCost drives gas deeply negative (OOG)" ignore {
    if !canRunNative then
      cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val prog = Seq(Ecalli(3), Trap)
      val initRegs = Array.fill(13)(0L)
      val gas = 1000L

      val interpDispatcher = new CountingDispatcher
      val nativeDispatcher = new CountingDispatcher
      val (interp, _) = runInterpreter(prog, initRegs, gas, new Array[Byte](RW_LEN), interpDispatcher)
      val (native, _) = runNative(prog, initRegs, gas, new Array[Byte](RW_LEN), nativeDispatcher)

      interp.exit shouldBe PvmRecompiler.EXIT_OOG
      interpDispatcher.dispatchCalls shouldBe 0
      native.exit shouldBe interp.exit
      native.gas shouldBe interp.gas
      nativeDispatcher.dispatchCalls shouldBe 0
  }

  // ---- preDispatch hook (accumulation's checkpoint-capture seam) -------------

  it should "invoke preDispatch exactly once per successful dispatch, BEFORE dispatch runs" in {
    if !canRunNative then
      cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val prog = Seq(Ecalli(0), Ecalli(4), Trap)
      val initRegs = Array.fill(13)(0L)
      initRegs(6) = 5L
      initRegs(7) = 100L
      val gas = 100L

      var preDispatchCalls = 0
      val dispatcher = new CountingDispatcher
      val preDispatch: () => Unit = () => preDispatchCalls += 1

      val (native, _) = runNative(prog, initRegs, gas, new Array[Byte](RW_LEN), dispatcher, Some(preDispatch))

      native.exit shouldBe PvmRecompiler.EXIT_PANIC // Trap, after two successful Ecallis
      preDispatchCalls shouldBe 2 // once per Ecalli — both succeeded (host ids 0 and 4 never throw/OOG here)
      dispatcher.dispatchCalls shouldBe 2
  }

  it should "NOT invoke preDispatch when gasCost drives an Ecalli to OOG (no dispatch)" in {
    if !canRunNative then
      cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      val prog = Seq(Ecalli(3), Trap) // host id 3: gasCost=1_000_000, always OOG
      val initRegs = Array.fill(13)(0L)
      val gas = 1000L

      var preDispatchCalls = 0
      val dispatcher = new CountingDispatcher
      val preDispatch: () => Unit = () => preDispatchCalls += 1

      val (native, _) = runNative(prog, initRegs, gas, new Array[Byte](RW_LEN), dispatcher, Some(preDispatch))

      native.exit shouldBe PvmRecompiler.EXIT_OOG
      preDispatchCalls shouldBe 0 // OOG-without-dispatch: the hook must not fire either
      dispatcher.dispatchCalls shouldBe 0
  }
