package io.forge.jam.pvm.recompiler

import io.forge.jam.pvm.native_.PvmRecompiler
import io.forge.jam.pvm.program.{ProgramBlob, JumpTable, InstructionDecoder}
import io.forge.jam.pvm.engine.{InterpretedModule, InterpretedInstance}
import io.forge.jam.pvm.{InterruptKind, Instruction}
import io.forge.jam.pvm.types.ProgramCounter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.util.Random

/**
 * The oracle differential
 */
class OracleDifferentialSpec extends AnyFlatSpec with Matchers:

  // ---- abstract program model -------------------------------------------------
  private sealed trait AInstr
  private case class LoadImm64(reg: Int, imm: Long) extends AInstr
  private case class AddImm64(dst: Int, src: Int, imm32: Int) extends AInstr
  private case class Add64(d: Int, s1: Int, s2: Int) extends AInstr
  private case class Sub64(d: Int, s1: Int, s2: Int) extends AInstr
  private case class Mul64(d: Int, s1: Int, s2: Int) extends AInstr
  private case class Jump(targetIdx: Int) extends AInstr
  private case class BranchEq(r1: Int, r2: Int, targetIdx: Int) extends AInstr
  private case class BranchNe(r1: Int, r2: Int, targetIdx: Int) extends AInstr
  private case object Trap extends AInstr
  private def sizeOf(a: AInstr): Int = a match
    case _: LoadImm64                 => 10
    case _: AddImm64                  => 6
    case _: Add64 | _: Sub64 | _: Mul64 => 3
    case _: Jump                      => 5
    case _: BranchEq | _: BranchNe    => 6
    case Trap                         => 1

  // ---- PVM encoder (abstract -> code bytes + bitmask) -------------------------
  private def longLE(v: Long): Array[Byte] =
    Array.tabulate(8)(i => ((v >>> (i * 8)) & 0xff).toByte)
  private def intLE(v: Int): Array[Byte] =
    Array.tabulate(4)(i => ((v >>> (i * 8)) & 0xff).toByte)
  private def regByte(a: Int, b: Int): Byte = ((a & 0xf) | ((b & 0xf) << 4)).toByte

  /** Encode one instruction at byte offset `off`, resolving a control-flow
    * target byte offset `tOff` to the encoded relative displacement. */
  private def encodeInstrAt(a: AInstr, off: Int, tOff: Int): Array[Byte] = a match
    case LoadImm64(reg, imm)     => Array[Byte](20.toByte, reg.toByte) ++ longLE(imm)
    case AddImm64(dst, src, imm) => Array[Byte](149.toByte, regByte(dst, src)) ++ intLE(imm)
    case Add64(d, s1, s2)        => Array[Byte](200.toByte, regByte(s1, s2), d.toByte)
    case Sub64(d, s1, s2)        => Array[Byte](201.toByte, regByte(s1, s2), d.toByte)
    case Mul64(d, s1, s2)        => Array[Byte](202.toByte, regByte(s1, s2), d.toByte)
    case Jump(_)                 => Array[Byte](40.toByte) ++ intLE(tOff - off)
    case BranchEq(r1, r2, _)     => Array[Byte](170.toByte, regByte(r1, r2)) ++ intLE(tOff - off)
    case BranchNe(r1, r2, _)     => Array[Byte](171.toByte, regByte(r1, r2)) ++ intLE(tOff - off)
    case Trap                    => Array[Byte](0)

  private def targetIdxOf(a: AInstr): Option[Int] = a match
    case Jump(t) => Some(t); case BranchEq(_, _, t) => Some(t); case BranchNe(_, _, t) => Some(t)
    case _ => None

  /** Encode a program to (code, bitmask). Bitmask marks each instruction's first
    * byte (how the decoder finds boundaries / skip). */
  private def encodeProgram(prog: Seq[AInstr]): (Array[Byte], Array[Byte]) =
    val offsets = prog.map(sizeOf).scanLeft(0)(_ + _) // offsets(i) = byte offset of instr i
    val code = scala.collection.mutable.ArrayBuffer.empty[Byte]
    prog.zipWithIndex.foreach { case (a, i) =>
      val tOff = targetIdxOf(a).map(offsets).getOrElse(0)
      code ++= encodeInstrAt(a, offsets(i), tOff)
    }
    val bytes = code.toArray
    val bitmask = new Array[Byte]((bytes.length + 7) / 8)
    prog.indices.foreach { i => val s = offsets(i); bitmask(s >> 3) = (bitmask(s >> 3) | (1 << (s & 7))).toByte }
    (bytes, bitmask)

  // ---- abstract -> recompiler RawInstr columns --------------------------------
  private def toRawColumns(prog: Seq[AInstr]): (Array[Int], Array[Int], Array[Int], Array[Int], Array[Long]) =
    val n = prog.length
    val op = new Array[Int](n); val dst = new Array[Int](n)
    val src = new Array[Int](n); val src2 = new Array[Int](n); val imm = new Array[Long](n)
    prog.zipWithIndex.foreach { case (a, i) =>
      a match
        case LoadImm64(reg, v)     => op(i) = PvmRecompiler.OP_LOAD_IMM64; dst(i) = reg; imm(i) = v
        case AddImm64(d, s, v)     => op(i) = PvmRecompiler.OP_ADD_IMM64; dst(i) = d; src(i) = s; imm(i) = v.toLong // sign-extends
        case Add64(d, s1, s2)      => op(i) = PvmRecompiler.OP_ADD; dst(i) = d; src(i) = s1; src2(i) = s2
        case Sub64(d, s1, s2)      => op(i) = PvmRecompiler.OP_SUB; dst(i) = d; src(i) = s1; src2(i) = s2
        case Mul64(d, s1, s2)      => op(i) = PvmRecompiler.OP_MUL; dst(i) = d; src(i) = s1; src2(i) = s2
        case Jump(t)               => op(i) = PvmRecompiler.OP_JUMP; imm(i) = t.toLong
        case BranchEq(r1, r2, t)   => op(i) = PvmRecompiler.OP_BRANCH_EQ; src(i) = r1; src2(i) = r2; imm(i) = t.toLong
        case BranchNe(r1, r2, t)   => op(i) = PvmRecompiler.OP_BRANCH_NE; src(i) = r1; src2(i) = r2; imm(i) = t.toLong
        case Trap                  => op(i) = PvmRecompiler.OP_TRAP
    }
    (op, dst, src, src2, imm)

  // ---- interpreter run (the oracle) ------------------------------------------
  private def runInterpreter(prog: Seq[AInstr], initRegs: Array[Long], gas: Long): (Int, Long, Array[Long]) =
    val (code, bitmask) = encodeProgram(prog)
    val blob = ProgramBlob(
      code = code, bitmask = bitmask, jumpTable = JumpTable(Array.empty, 0),
      is64Bit = true, roData = Array.empty, rwData = new Array[Byte](4096), stackSize = 4096
    )
    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(e)  => fail(s"module create failed: $e")
    val inst = InterpretedInstance.fromModule(module, forceStepTracing = false)
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(0))
    initRegs.zipWithIndex.foreach { case (v, i) => inst.setReg(i, v) }
    var exit = -1
    var running = true
    while running do
      inst.run() match
        case Right(InterruptKind.Panic)    => exit = PvmRecompiler.EXIT_PANIC; running = false
        case Right(InterruptKind.OutOfGas) => exit = PvmRecompiler.EXIT_OOG; running = false
        case Right(InterruptKind.Finished) => exit = PvmRecompiler.EXIT_HALT; running = false
        case Right(InterruptKind.Segfault(_)) => exit = PvmRecompiler.EXIT_FAULT; running = false
        case Right(InterruptKind.Step)     => () // keep going
        case Right(InterruptKind.Ecalli(_)) => fail("unexpected ecalli")
        case Left(err)                     => fail(s"interpreter error: $err")
    val regs = Array.tabulate(13)(i => inst.getReg(i))
    (exit, inst.gas, regs)

  // ---- self-check: my encoder round-trips through the real decoder ------------
  "the PVM encoder" should "round-trip every subset opcode through the real decoder" in {
    val prog = Seq(
      LoadImm64(3, 0x1122334455667788L),
      AddImm64(4, 3, -5),
      Add64(5, 3, 4), Sub64(6, 5, 3), Mul64(7, 4, 3), Trap
    )
    val (code, bitmask) = encodeProgram(prog)
    var off = 0
    val decoded = scala.collection.mutable.ArrayBuffer.empty[Instruction]
    while off < code.length do
      val (instr, skip) = InstructionDecoder.decode(code, bitmask, off)
      decoded += instr
      off += skip
    decoded.head shouldBe a[Instruction.LoadImm64]
    decoded(2) shouldBe Instruction.Add64(5, 3, 4)
    decoded(3) shouldBe Instruction.Sub64(6, 5, 3)
    decoded(4) shouldBe Instruction.Mul64(7, 4, 3)
  }

  // ---- generators -------------------------------------------------------------
  private def randArith(rng: Random): AInstr = rng.nextInt(5) match
    case 0 => LoadImm64(rng.nextInt(13), rng.nextLong())
    case 1 => AddImm64(rng.nextInt(13), rng.nextInt(13), rng.nextInt())
    case 2 => Add64(rng.nextInt(13), rng.nextInt(13), rng.nextInt(13))
    case 3 => Sub64(rng.nextInt(13), rng.nextInt(13), rng.nextInt(13))
    case _ => Mul64(rng.nextInt(13), rng.nextInt(13), rng.nextInt(13))

  private def genControlFlowProgram(rng: Random): Seq[AInstr] =
    val k = 2 + rng.nextInt(5) // blocks
    // pre-plan each block's arithmetic length and terminator kind
    val arithLens = Array.fill(k)(rng.nextInt(3))
    // block start instruction indices (each block = arithLen + 1 terminator)
    val starts = arithLens.map(_ + 1).scanLeft(0)(_ + _)
    val out = scala.collection.mutable.ArrayBuffer.empty[AInstr]
    for b <- 0 until k do
      for _ <- 0 until arithLens(b) do out += randArith(rng)
      if b == k - 1 then out += Trap
      else
        val tb = b + 1 + rng.nextInt(k - b - 1) // a strictly later block
        val tgt = starts(tb)
        out += (rng.nextInt(3) match
          case 0 => Jump(tgt)
          case 1 => BranchEq(rng.nextInt(13), rng.nextInt(13), tgt)
          case _ => BranchNe(rng.nextInt(13), rng.nextInt(13), tgt))
    out.toSeq

  // ---- the differential -------------------------------------------------------
  private def libPath: Option[Path] =
    Option(System.getProperty("jam.pvm.recompiler.lib")).map(Path.of(_)).filter(Files.exists(_))

  private def genLoopProgram(rng: Random): Seq[AInstr] =
    val setup = (0 until rng.nextInt(4)).map(_ => randArith(rng))
    val body = (0 until (1 + rng.nextInt(4))).map(_ => randArith(rng))
    val loopStart = setup.length + 1 // index of block1's first instruction
    (setup :+ Jump(loopStart)) ++ (body :+ Jump(loopStart))

  private def compareRun(rc: PvmRecompiler, prog: Seq[AInstr], rng: Random): Unit =
    compareRunGas(rc, prog, prog.length.toLong + rng.nextInt(50), rng) // sufficient

  private def compareRunGas(rc: PvmRecompiler, prog: Seq[AInstr], gas: Long, rng: Random): Unit =
    val initRegs = Array.fill(13)(rng.nextLong())
    val (iExit, iGas, iRegs) = runInterpreter(prog, initRegs.clone(), gas)
    val (op, dst, src, src2, imm) = toRawColumns(prog)
    val blk = rc.compile(op, dst, src, src2, imm)
    blk.isValid shouldBe true
    val nRegs = initRegs.clone()
    val out = rc.execute(blk, nRegs, gas)
    blk.close()
    withClue(s"program=$prog gas=$gas\n interp(exit=$iExit gas=$iGas)\n native(exit=${out(0)} gas=${out(1)})\n") {
      out(0).toInt shouldBe iExit
      out(1) shouldBe iGas
      nRegs.toSeq shouldBe iRegs.toSeq
    }

  "the native recompiler" should "match the production interpreter on arithmetic programs" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xC0FFEEL)
          for _ <- 0 until 20000 do
            val n = 1 + rng.nextInt(12)
            val prog = (0 until n).map(_ => randArith(rng)) :+ Trap
            compareRun(rc, prog, rng)
          info("oracle differential (arithmetic): 20000 programs matched the interpreter")
        finally rc.close()
  }

  it should "match the production interpreter on control-flow programs (forward jumps/branches)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xBEEF01L)
          for _ <- 0 until 20000 do
            compareRun(rc, genControlFlowProgram(rng), rng)
          info("oracle differential (control flow): 20000 programs matched the interpreter")
        finally rc.close()
  }

  it should "match the production interpreter on out-of-gas (partial-execution) semantics" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xDEAD01L)
          // Tight gas so OOG strikes mid-program; per-instruction gas must freeze
          // registers exactly where the interpreter does.
          for _ <- 0 until 20000 do
            val prog = (0 until (1 + rng.nextInt(12))).map(_ => randArith(rng)) :+ Trap
            compareRunGas(rc, prog, rng.nextInt(prog.length + 1).toLong, rng)
          info("oracle differential (arith OOG): 20000 programs matched the interpreter")
        finally rc.close()
  }

  it should "match the production interpreter on backward loops (OOG mid-loop)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0x100F00L)
          // Infinite loops that only end via OOG — the hardest OOG case, now that
          // per-instruction gas matches the interpreter's partial-block semantics.
          for _ <- 0 until 20000 do
            val prog = genLoopProgram(rng)
            compareRunGas(rc, prog, (5 + rng.nextInt(60)).toLong, rng)
          info("oracle differential (backward loop OOG): 20000 programs matched the interpreter")
        finally rc.close()
  }
