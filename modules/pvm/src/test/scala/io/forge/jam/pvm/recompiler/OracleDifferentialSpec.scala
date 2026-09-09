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
import spire.math.UInt

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
  // reg[dst] = load `width` bytes at reg[base] + offset (zero/sign-extended).
  private case class LoadInd(dst: Int, base: Int, offset: Int, width: Int, signed: Boolean) extends AInstr
  // store the low `width` bytes of reg[src] at reg[base] + offset.
  private case class StoreInd(src: Int, base: Int, offset: Int, width: Int) extends AInstr
  private case object Trap extends AInstr
  private case object Fallthrough extends AInstr
  private case class LoadImm(reg: Int, imm: Int) extends AInstr
  // absolute address (no base register), matching Opcode.LoadU64/StoreU64.
  private case class LoadAbs64(reg: Int, address: Int) extends AInstr
  private case class StoreAbs64(reg: Int, address: Int) extends AInstr
  private case class BranchEqImm(reg: Int, imm: Int, targetIdx: Int) extends AInstr
  private case class BranchNeImm(reg: Int, imm: Int, targetIdx: Int) extends AInstr
  private case class MoveReg(dst: Int, src: Int) extends AInstr
  private case class AddImm32(dst: Int, src: Int, imm32: Int) extends AInstr
  private case class Shl64Imm(dst: Int, src: Int, imm: Int) extends AInstr
  private case class And3(d: Int, s1: Int, s2: Int) extends AInstr
  private case class Or3(d: Int, s1: Int, s2: Int) extends AInstr
  private case class CmovNz(d: Int, s1: Int, s2: Int) extends AInstr
  private case class Djump(src: Int, imm: Int) extends AInstr
  private case class BranchCmpImm(op: Int, reg: Int, imm: Int, targetIdx: Int) extends AInstr
  private case class BranchCmpReg(op: Int, r1: Int, r2: Int, targetIdx: Int) extends AInstr
  private case class SetCmpImm(op: Int, dst: Int, src: Int, imm: Int) extends AInstr
  private case class SetCmpReg(op: Int, d: Int, s1: Int, s2: Int) extends AInstr
  private case class ShiftRotImm(op: Int, dst: Int, src: Int, imm: Int) extends AInstr
  private case class ShiftRotImmAlt(op: Int, dst: Int, src: Int, imm: Int) extends AInstr
  private case class ShiftRotReg(op: Int, d: Int, s1: Int, s2: Int) extends AInstr

  // Indirect-store opcode by width — decodes as regs2Imm.
  private def storeIndOpcode(width: Int): Int = width match
    case 1 => 120; case 2 => 121; case 4 => 122; case _ => 123

  // Indirect-load opcode by (width, signed) — decodes as regs2Imm.
  private def loadIndOpcode(width: Int, signed: Boolean): Int = (width, signed) match
    case (1, false) => 124; case (1, true) => 125
    case (2, false) => 126; case (2, true) => 127
    case (4, false) => 128; case (4, true) => 129
    case (8, _)     => 130

  // Fixed instruction sizes (fixed-width immediates) so byte offsets are known
  // in one pass and control-flow targets resolve without size iteration.
  private def sizeOf(a: AInstr): Int = a match
    case _: LoadImm64                 => 10
    case _: AddImm64                  => 6
    case _: Add64 | _: Sub64 | _: Mul64 => 3
    case _: Jump                      => 5
    case _: BranchEq | _: BranchNe    => 6
    case _: LoadInd                   => 6
    case _: StoreInd                  => 6
    case Trap                         => 1
    case Fallthrough                  => 1
    case _: LoadImm                   => 6  // [op, reg] ++ 4-byte imm
    case _: LoadAbs64 | _: StoreAbs64 => 6  // [op, reg] ++ 4-byte absolute address
    case _: BranchEqImm | _: BranchNeImm => 10 // [op, reg|len] ++ 4-byte imm ++ 4-byte disp
    case _: MoveReg                   => 2  // [op, regByte]
    case _: AddImm32                  => 6  // [op, regByte] ++ 4-byte imm
    case _: Shl64Imm                  => 6  // [op, regByte] ++ 4-byte imm
    case _: And3 | _: Or3 | _: CmovNz => 3  // [op, regByte(s1,s2), d]
    case _: Djump                     => 6  // [op, reg] ++ 4-byte imm offset
    case _: BranchCmpImm              => 10 // [op, reg|lenNibble] ++ 4-byte imm ++ 4-byte disp
    case _: BranchCmpReg              => 6  // [op, regByte(r1,r2)] ++ 4-byte disp
    case _: SetCmpImm                 => 6  // [op, regByte(dst,src)] ++ 4-byte imm
    case _: SetCmpReg                 => 3  // [op, regByte(s1,s2), d]
    case _: ShiftRotImm               => 6  // [op, regByte(dst,src)] ++ 4-byte imm
    case _: ShiftRotImmAlt            => 6  // [op, regByte(dst,src)] ++ 4-byte imm
    case _: ShiftRotReg               => 3  // [op, regByte(s1,s2), d]

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
    case LoadInd(dst, base, o, w, s) => Array[Byte](loadIndOpcode(w, s).toByte, regByte(dst, base)) ++ intLE(o)
    case StoreInd(src, base, o, w)   => Array[Byte](storeIndOpcode(w).toByte, regByte(src, base)) ++ intLE(o)
    case Trap                    => Array[Byte](0)
    case Fallthrough             => Array[Byte](1)
    case LoadImm(reg, imm)       => Array[Byte](51.toByte, reg.toByte) ++ intLE(imm)
    case LoadAbs64(reg, address) => Array[Byte](58.toByte, reg.toByte) ++ intLE(address)
    case StoreAbs64(reg, address) => Array[Byte](62.toByte, reg.toByte) ++ intLE(address)
    case BranchEqImm(reg, imm, _) =>
      Array[Byte](81.toByte, ((reg & 0xF) | (4 << 4)).toByte) ++ intLE(imm) ++ intLE(tOff - off)
    case BranchNeImm(reg, imm, _) =>
      Array[Byte](82.toByte, ((reg & 0xF) | (4 << 4)).toByte) ++ intLE(imm) ++ intLE(tOff - off)
    case MoveReg(dst, src)       => Array[Byte](100.toByte, regByte(dst, src))
    case AddImm32(dst, src, imm) => Array[Byte](131.toByte, regByte(dst, src)) ++ intLE(imm)
    case Shl64Imm(dst, src, imm) => Array[Byte](151.toByte, regByte(dst, src)) ++ intLE(imm)
    case And3(d, s1, s2)         => Array[Byte](210.toByte, regByte(s1, s2), d.toByte)
    case Or3(d, s1, s2)          => Array[Byte](212.toByte, regByte(s1, s2), d.toByte)
    case CmovNz(d, s1, s2)       => Array[Byte](219.toByte, regByte(s1, s2), d.toByte)
    case Djump(src, imm)         => Array[Byte](50.toByte, src.toByte) ++ intLE(imm)
    case BranchCmpImm(op, reg, imm, _) =>
      Array[Byte](op.toByte, ((reg & 0xF) | (4 << 4)).toByte) ++ intLE(imm) ++ intLE(tOff - off)
    case BranchCmpReg(op, r1, r2, _) => Array[Byte](op.toByte, regByte(r1, r2)) ++ intLE(tOff - off)
    case SetCmpImm(op, dst, src, imm) => Array[Byte](op.toByte, regByte(dst, src)) ++ intLE(imm)
    case SetCmpReg(op, d, s1, s2) => Array[Byte](op.toByte, regByte(s1, s2), d.toByte)
    case ShiftRotImm(op, dst, src, imm) => Array[Byte](op.toByte, regByte(dst, src)) ++ intLE(imm)
    case ShiftRotImmAlt(op, dst, src, imm) => Array[Byte](op.toByte, regByte(dst, src)) ++ intLE(imm)
    // Three-register shift/rotate: regs3 shape.
    case ShiftRotReg(op, d, s1, s2) => Array[Byte](op.toByte, regByte(s1, s2), d.toByte)

  private def targetIdxOf(a: AInstr): Option[Int] = a match
    case Jump(t) => Some(t); case BranchEq(_, _, t) => Some(t); case BranchNe(_, _, t) => Some(t)
    case BranchEqImm(_, _, t) => Some(t); case BranchNeImm(_, _, t) => Some(t)
    case BranchCmpImm(_, _, _, t) => Some(t)
    case BranchCmpReg(_, _, _, t) => Some(t)
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
  private def toRawColumns(prog: Seq[AInstr]): RecompilerAbi.PreparedProgram =
    val (code, bitmask) = encodeProgram(prog)
    RecompilerAbi.prepareProgram(code, bitmask, JumpTable.Empty)

  private def instrByteOffsets(prog: Seq[AInstr]): Seq[Int] =
    prog.map(sizeOf).scanLeft(0)(_ + _).init

  private def encodeJumpTable(prog: Seq[AInstr], targetIndices: Seq[Int]): JumpTable =
    val offsets = instrByteOffsets(prog)
    val blob = targetIndices.flatMap(idx => intLE(offsets(idx))).toArray
    JumpTable(blob, entrySize = 4)

  // With roData empty and a page-sized RW region, the RW region occupies exactly
  // [RW_BASE, RW_BASE + RW_LEN) = [0x20000, 0x21000). See Abi.build: rwDataAddr =
  // 2*ZZ (ZZ = 0x10000) when roData is empty.
  private val RW_BASE = 0x20000
  private val RW_LEN = 4096 // one page: recompiler's byte bounds == interpreter's page bounds
  private val PAGE_SHIFT = 12 // log2(4096) — the fixed TINY/test page size throughout this suite

  // ---- interpreter run (the oracle) ------------------------------------------
  private final case class InterpResult(
    exit: Int,
    gas: Long,
    regs: Array[Long],
    pc: Long,
    faultPage: Long
  )

  private def runInterpreter(prog: Seq[AInstr], initRegs: Array[Long], gas: Long): InterpResult =
    runInterpreterMem(prog, initRegs, gas, new Array[Byte](RW_LEN))._1

  /** Interpreter run with an explicit RW-region image; also returns the RW bytes
    * after execution (for store verification). */
  private def runInterpreterMem(prog: Seq[AInstr], initRegs: Array[Long], gas: Long, rwData: Array[Byte])
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
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(0))
    initRegs.zipWithIndex.foreach { case (v, i) => inst.setReg(i, v) }
    val (exit, faultPage) = runToTerminal(inst)
    val regs = Array.tabulate(13)(i => inst.getReg(i))
    val pc = inst.programCounter.map(_.toInt.toLong & 0xFFFFFFFFL).getOrElse(fail("no programCounter set on exit"))
    val rwAfter = inst.basicMemory.getMemorySlice(spire.math.UInt(RW_BASE), RW_LEN) match
      case io.forge.jam.pvm.MemoryResult.Success(bytes) => bytes
      case _ => rwData // unchanged if unreadable (e.g. faulted before any store)
    (InterpResult(exit, inst.gas, regs, pc, faultPage), rwAfter)

  /** Drives `inst.run()` to a terminal interrupt (skipping Step); returns
    * (exitCode, faultPage). Shared by the flat-buffer runner above and the
    * region-table runner below (`runInterpreterOn`). */
  private def runToTerminal(inst: InterpretedInstance): (Int, Long) =
    var exit = -1
    var faultPage = 0L
    var running = true
    while running do
      inst.run() match
        case Right(InterruptKind.Panic)    => exit = PvmRecompiler.EXIT_PANIC; running = false
        case Right(InterruptKind.OutOfGas) => exit = PvmRecompiler.EXIT_OOG; running = false
        case Right(InterruptKind.Finished) => exit = PvmRecompiler.EXIT_HALT; running = false
        case Right(InterruptKind.Segfault(info)) =>
          exit = PvmRecompiler.EXIT_FAULT
          faultPage = info.pageAddress.toLong & 0xFFFFFFFFL
          running = false
        case Right(InterruptKind.Step)     => () // keep going
        case Right(InterruptKind.Ecalli(_)) => fail("unexpected ecalli")
        case Left(err)                     => fail(s"interpreter error: $err")
    (exit, faultPage)

  // ---- self-check: my encoder round-trips through the real decoder ------------
  "the PVM encoder" should "round-trip every subset opcode through the real decoder" in {
    val prog = Seq(
      LoadImm64(3, 0x1122334455667788L),
      AddImm64(4, 3, -5),
      Add64(5, 3, 4), Sub64(6, 5, 3), Mul64(7, 4, 3),
      LoadInd(2, 0, 24, 2, signed = true), StoreInd(9, 1, 40, 4), Trap
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
    decoded(5) shouldBe Instruction.LoadIndirectI16(2, 0, 24) // dst, base, offset
    decoded(6) shouldBe Instruction.StoreIndirectU32(9, 1, 40) // src, base, offset
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

  private def genLoadProgram(rng: Random): Seq[AInstr] =
    val n = 1 + rng.nextInt(10)
    val body: Seq[AInstr] = (0 until n).map { _ =>
      if rng.nextInt(3) == 0 then
        // arithmetic that never writes r0 (keeps the memory base intact)
        val d = 1 + rng.nextInt(12)
        rng.nextInt(5) match
          case 0 => LoadImm64(d, rng.nextLong())
          case 1 => AddImm64(d, rng.nextInt(13), rng.nextInt())
          case 2 => Add64(d, rng.nextInt(13), rng.nextInt(13))
          case 3 => Sub64(d, rng.nextInt(13), rng.nextInt(13))
          case _ => Mul64(d, rng.nextInt(13), rng.nextInt(13))
      else
        val (w, s) = rng.nextInt(7) match
          case 0 => (1, false); case 1 => (1, true); case 2 => (2, false); case 3 => (2, true)
          case 4 => (4, false); case 5 => (4, true); case _ => (8, false)
        LoadInd(1 + rng.nextInt(12), 0, rng.nextInt(RW_LEN + 64), w, s) // base r0
    }
    body :+ Trap

  /** A straight-line program of indirect loads AND stores (base = r0 = RW_BASE)
    * plus non-r0 arithmetic, then trap. Stores let register state flow into
    * memory; the writeback is compared after the run. */
  private def genMemProgram(rng: Random): Seq[AInstr] =
    val n = 1 + rng.nextInt(12)
    val body: Seq[AInstr] = (0 until n).map { _ =>
      rng.nextInt(3) match
        case 0 => // non-r0 arithmetic
          val d = 1 + rng.nextInt(12)
          rng.nextInt(5) match
            case 0 => LoadImm64(d, rng.nextLong())
            case 1 => AddImm64(d, rng.nextInt(13), rng.nextInt())
            case 2 => Add64(d, rng.nextInt(13), rng.nextInt(13))
            case 3 => Sub64(d, rng.nextInt(13), rng.nextInt(13))
            case _ => Mul64(d, rng.nextInt(13), rng.nextInt(13))
        case 1 => // load
          val (w, s) = randWidthSigned(rng)
          LoadInd(1 + rng.nextInt(12), 0, rng.nextInt(RW_LEN + 64), w, s)
        case _ => // store
          val w = randWidth(rng)
          StoreInd(rng.nextInt(13), 0, rng.nextInt(RW_LEN + 64), w)
    }
    body :+ Trap

  private def randWidth(rng: Random): Int = Array(1, 2, 4, 8)(rng.nextInt(4))
  private def randWidthSigned(rng: Random): (Int, Boolean) = rng.nextInt(7) match
    case 0 => (1, false); case 1 => (1, true); case 2 => (2, false); case 3 => (2, true)
    case 4 => (4, false); case 5 => (4, true); case _ => (8, false)

  private def singleRwRegion(base: Long, data: Array[Byte]): Array[PvmRecompiler.Region] =
    Array(new PvmRecompiler.Region(base, data.length.toLong, 0L, true))

  /** Compare a load/store program against the interpreter with aligned RW images,
    * including the RW-region contents after execution, AND the final PC. */
  private def compareRunMem(rc: PvmRecompiler, prog: Seq[AInstr], rng: Random): Unit =
    val rwData = new Array[Byte](RW_LEN); rng.nextBytes(rwData)
    val initRegs = Array.fill(13)(rng.nextLong())
    initRegs(0) = RW_BASE.toLong // r0 = memory base (both engines)
    val gas = prog.length.toLong + rng.nextInt(50)

    val (interp, iRwAfter) = runInterpreterMem(prog, initRegs.clone(), gas, rwData.clone())

    val pp = toRawColumns(prog)
    val blk = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
    blk.isValid shouldBe true
    val nRegs = initRegs.clone()
    val backing = rwData.clone()
    val regions = singleRwRegion(RW_BASE.toLong, backing)
    val out = rc.execute(blk, nRegs, gas, regions, backing, PAGE_SHIFT, 0)
    blk.close()
    withClue(s"program=$prog gas=$gas\n interp(exit=${interp.exit} gas=${interp.gas} pc=${interp.pc})\n" +
      s"native(exit=${out.exit} gas=${out.gasRemaining} pc=${out.pc})\n") {
      out.exit shouldBe interp.exit
      out.gasRemaining shouldBe interp.gas
      out.pc shouldBe interp.pc
      if out.exit == PvmRecompiler.EXIT_FAULT then out.faultPage shouldBe interp.faultPage
      nRegs.toSeq shouldBe interp.regs.toSeq
      backing.toSeq shouldBe iRwAfter.toSeq
    }

  private def compareRunLoads(rc: PvmRecompiler, prog: Seq[AInstr], rng: Random): Unit =
    compareRunMem(rc, prog, rng)

  private def compareRun(rc: PvmRecompiler, prog: Seq[AInstr], rng: Random): Unit =
    compareRunGas(rc, prog, prog.length.toLong + rng.nextInt(50), rng) // sufficient

  private def compareRunGas(rc: PvmRecompiler, prog: Seq[AInstr], gas: Long, rng: Random): Unit =
    val initRegs = Array.fill(13)(rng.nextLong())
    val interp = runInterpreter(prog, initRegs.clone(), gas)
    val pp = toRawColumns(prog)
    val blk = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
    blk.isValid shouldBe true
    val nRegs = initRegs.clone()
    val out = rc.execute(blk, nRegs, gas)
    blk.close()
    withClue(s"program=$prog gas=$gas\n interp(exit=${interp.exit} gas=${interp.gas} pc=${interp.pc})\n" +
      s"native(exit=${out.exit} gas=${out.gasRemaining} pc=${out.pc})\n") {
      out.exit shouldBe interp.exit
      out.gasRemaining shouldBe interp.gas
      out.pc shouldBe interp.pc
      nRegs.toSeq shouldBe interp.regs.toSeq
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

  it should "match the production interpreter on indirect memory loads (all widths + faults)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0x10ADEDL)
          for _ <- 0 until 10000 do
            compareRunLoads(rc, genLoadProgram(rng), rng)
          info("oracle differential (memory loads): 10000 programs matched the interpreter")
        finally rc.close()
  }

  it should "match the production interpreter on indirect memory loads AND stores (writeback)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0x570E5L)
          // Interleaved loads + stores at all widths; compares the RW region
          // contents after execution as well as registers/gas/exit/pc.
          for _ <- 0 until 10000 do
            compareRunMem(rc, genMemProgram(rng), rng)
          info("oracle differential (memory load+store): 10000 programs matched the interpreter")
        finally rc.close()
  }

  private def setupRegionRun(
    prog: Seq[AInstr],
    initRegs: Array[Long],
    gas: Long,
    roData: Array[Byte],
    rwData: Array[Byte],
    initialPcByteOffset: Int
  ): (InterpResult, RecompilerMemory.Described) =
    val (code, bitmask) = encodeProgram(prog)
    val blob = ProgramBlob(
      code = code, bitmask = bitmask, jumpTable = JumpTable(Array.empty, 0),
      is64Bit = true, roData = roData, rwData = rwData, stackSize = 4096
    )
    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(e)  => fail(s"module create failed: $e")
    val inst = InterpretedInstance.fromModule(module, forceStepTracing = false)
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(initialPcByteOffset))
    initRegs.zipWithIndex.foreach { case (v, i) => inst.setReg(i, v) }

    // Snapshot the region table + backing bytes BEFORE running (this is what
    // a real caller would hand the recompiler prior to execution).
    val described = RecompilerMemory.describe(inst)

    val (exit, faultPage) = runToTerminal(inst)
    val regsAfter = Array.tabulate(13)(i => inst.getReg(i))
    val pc = inst.programCounter.map(_.toInt.toLong & 0xFFFFFFFFL).getOrElse(fail("no programCounter set on exit"))
    (InterpResult(exit, inst.gas, regsAfter, pc, faultPage), described)

  private def compareRegionRun(
    rc: PvmRecompiler,
    prog: Seq[AInstr],
    initRegs: Array[Long],
    gas: Long,
    described: RecompilerMemory.Described,
    interp: InterpResult,
    entryIndex: Int = 0
  ): Unit =
    val pp = toRawColumns(prog)
    val blk = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
    blk.isValid shouldBe true
    val nRegs = initRegs.clone()
    val backing = described.backing.clone()
    val regions = described.regions.map(r => new PvmRecompiler.Region(r.base, r.len, r.bufOffset, r.writable))
    val out = rc.execute(blk, nRegs, gas, regions, backing, described.pageShift, entryIndex)
    blk.close()
    withClue(s"program=$prog gas=$gas entryIndex=$entryIndex\n" +
      s"interp(exit=${interp.exit} gas=${interp.gas} pc=${interp.pc} faultPage=${interp.faultPage})\n" +
      s"native(exit=${out.exit} gas=${out.gasRemaining} pc=${out.pc} faultPage=${out.faultPage})\n") {
      out.exit shouldBe interp.exit
      out.gasRemaining shouldBe interp.gas
      out.pc shouldBe interp.pc
      if interp.exit == PvmRecompiler.EXIT_FAULT then out.faultPage shouldBe interp.faultPage
      nRegs.toSeq shouldBe interp.regs.toSeq
    }

  private def runBoth(
    rc: PvmRecompiler,
    prog: Seq[AInstr],
    initRegs: Array[Long],
    gas: Long,
    roData: Array[Byte] = Array.emptyByteArray,
    rwData: Array[Byte] = new Array[Byte](RW_LEN),
    initialPcByteOffset: Int = 0,
    entryIndex: Int = 0
  ): InterpResult =
    val (interp, described) = setupRegionRun(prog, initRegs, gas, roData, rwData, initialPcByteOffset)
    compareRegionRun(rc, prog, initRegs, gas, described, interp, entryIndex)
    interp

  it should "match the production interpreter: loads from an RO region succeed, stores fault" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0x120000L)
          var faultCount = 0
          var loadOkCount = 0
          for _ <- 0 until 4000 do
            val roData = Array.fill(4096)(rng.nextInt(256).toByte)
            val rwData = new Array[Byte](4096)
            val roBase = 0x10000 // memoryMap.roDataAddress with a nonzero roData size
            val isStore = rng.nextBoolean()
            val offset = rng.nextInt(4096)
            val prog =
              if isStore then Seq(LoadImm64(1, 0xAAL), StoreInd(1, 0, offset, randWidth(rng)), Trap)
              else Seq(LoadInd(1, 0, offset, randWidth(rng), signed = false), Trap)
            val initRegs = Array.fill(13)(0L)
            initRegs(0) = roBase.toLong
            val interp = runBoth(rc, prog, initRegs, 100L, roData = roData, rwData = rwData)
            if isStore then
              interp.exit shouldBe PvmRecompiler.EXIT_FAULT
              interp.faultPage shouldBe roBase.toLong
              faultCount += 1
            else
              interp.exit should not be PvmRecompiler.EXIT_FAULT
              loadOkCount += 1
          faultCount should be > 0
          loadOkCount should be > 0
          info(s"oracle differential (RO region): 4000 programs matched; $faultCount RO-stores faulted, $loadOkCount RO-loads succeeded")
        finally rc.close()
  }

  // ---- unmapped-gap accesses and page-boundary-spanning accesses -------------
  //
  // Real interpreter layout guarantees a gap between the RW-data region and
  // the stack (both far apart in address space by construction — see
  // Abi/MemoryMap.build). r0 sweeps across that gap and across the RW
  // region's own upper page boundary, driving both an unmapped-gap fault and
  // an in-region-then-off-the-end spanning fault against the SAME real
  // MemoryMap the interpreter built.
  it should "match the production interpreter on unmapped-gap and page-spanning accesses" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0x9A9A9AL)
          var gapFaults = 0
          var spanningFaults = 0
          var oks = 0
          for _ <- 0 until 4000 do
            val rwData = Array.fill(4096)(rng.nextInt(256).toByte)
            // Base RW region address with empty roData: 2*ZZ = 0x20000 (see the
            // RW_BASE comment above) — offsets near its upper edge span into
            // the unmapped gap after it; large positive offsets land well past
            // the gap, deep in genuinely unmapped space.
            val mode = rng.nextInt(3)
            val offset = mode match
              case 0 => 4096 - 4 + rng.nextInt(8) // spans the region's own top page boundary
              case 1 => rng.nextInt(4096 - 8)     // fully in-bounds
              case _ => 0x100000 + rng.nextInt(0x100000) // deep unmapped gap
            val prog = Seq(LoadInd(1, 0, offset, 8, signed = false), Trap)
            val initRegs = Array.fill(13)(0L)
            initRegs(0) = RW_BASE.toLong
            val interp = runBoth(rc, prog, initRegs, 100L, rwData = rwData)
            mode match
              case 0 => if interp.exit == PvmRecompiler.EXIT_FAULT then spanningFaults += 1 else oks += 1
              case 1 => interp.exit should not be PvmRecompiler.EXIT_FAULT; oks += 1
              case _ => interp.exit shouldBe PvmRecompiler.EXIT_FAULT; gapFaults += 1
          gapFaults should be > 0
          spanningFaults should be > 0
          oks should be > 0
          info(s"oracle differential (unmapped gap / spanning): 4000 programs matched; " +
            s"$gapFaults gap faults, $spanningFaults spanning faults, $oks in-bounds")
        finally rc.close()
  }

  // ---- sub-0x10000 panic escalation -------------------------------------------
  it should "match the production interpreter: sub-0x10000 faults escalate to panic" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0x00FEEDL)
          var escalations = 0
          for _ <- 0 until 3000 do
            val rwData = new Array[Byte](4096)
            val offset = rng.nextInt(0x8000) // well below MinValidAddress=0x10000
            val prog = Seq(LoadInd(1, 0, offset, randWidth(rng), signed = false), Trap)
            val initRegs = Array.fill(13)(0L) // r0 = 0
            val interp = runBoth(rc, prog, initRegs, 100L, rwData = rwData)
            interp.exit shouldBe PvmRecompiler.EXIT_PANIC
            escalations += 1
          escalations shouldBe 3000
          info(s"oracle differential (sub-0x10000 escalation): $escalations programs matched (all escalated to panic)")
        finally rc.close()
  }

  // ---- entry-index: nonzero initial PC (mid-program start) -------------------
  it should "match the production interpreter when starting mid-program (nonzero entry index)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xE7719DL)
          for _ <- 0 until 3000 do
            // instruction 0 (skipped): a LoadImm64 that would corrupt r1 if run.
            // instruction 1 (entry): arithmetic. instruction 2: Trap.
            val skipped = LoadImm64(1, 0xDEADL)
            val body = randArith(rng)
            val prog = Seq(skipped, body, Trap)
            val entryByteOffset = sizeOf(skipped) // byte offset of instruction 1
            val pp = toRawColumns(prog)
            val entryIndex = pp.byteOffsetToIndex.getOrElse(entryByteOffset, fail("entry offset not a decoded leader"))
            entryIndex shouldBe 1
            val initRegs = Array.fill(13)(rng.nextLong())
            val interp = runBoth(rc, prog, initRegs, 100L, initialPcByteOffset = entryByteOffset, entryIndex = entryIndex)
            // instruction 0 never ran: r1 must NOT be 0xDEAD unless body also
            // happens to target r1 with that exact value (astronomically
            // unlikely with random operands) — assert exit matches at minimum
            // (compareRegionRun already asserted full register equality).
            interp.exit shouldBe PvmRecompiler.EXIT_PANIC // always runs to the trap
          info("oracle differential (entry index / mid-program start): 3000 programs matched the interpreter")
        finally rc.close()
  }

  // ---- PC assertions across ALL exit kinds (dedicated smoke coverage) --------
  it should "report the correct PC on halt, panic, OOG, and fault exits" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          // halt: JumpIndirect to the sentinel address 0xFFFF0000 is not
          // representable via this suite's abstract encoder (no djump case),
          // so halt coverage lives in the Rust unit tests
          // (djump_to_sentinel_halts) — this suite covers the remaining three.

          // panic (trap)
          val panicProg = Seq(LoadImm64(1, 1L), Trap)
          val panicInterp = runBoth(rc, panicProg, Array.fill(13)(0L), 100L)
          panicInterp.exit shouldBe PvmRecompiler.EXIT_PANIC

          // OOG mid-program
          val oogProg = Seq(LoadImm64(1, 1L), LoadImm64(2, 2L), LoadImm64(3, 3L), Trap)
          val oogInterp = runBoth(rc, oogProg, Array.fill(13)(0L), 2L) // charges for 2 instrs then OOGs
          oogInterp.exit shouldBe PvmRecompiler.EXIT_OOG

          // fault (RO store)
          val roData = Array.fill(4096)(0.toByte)
          val faultProg = Seq(LoadImm64(1, 1L), StoreInd(1, 0, 0, 8), Trap)
          val faultInitRegs = Array.fill(13)(0L); faultInitRegs(0) = 0x10000L
          val faultInterp = runBoth(rc, faultProg, faultInitRegs, 100L, roData = roData)
          faultInterp.exit shouldBe PvmRecompiler.EXIT_FAULT
          info("oracle differential (PC per exit kind): panic/OOG/fault PCs all matched the interpreter")
        finally rc.close()
  }

  // ---- sbrk-grown heap: RecompilerMemory.describe region fidelity ------------
  it should "extend the RW region to cover an sbrk-grown heap (region fidelity)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0x5B4B5B4BL)
          val pageSize = 4096
          val rwData = new Array[Byte](pageSize) // memoryMap.rwDataSize == 1 page pre-sbrk

          // A load/store program targeting an address in the page sbrk is
          // about to add — unreachable under the pre-fix (static rwDataSize
          // only) region table, since it's beyond the initial one-page RW
          // region entirely.
          val grownOffset = pageSize + rng.nextInt(pageSize - 8) // inside the newly-mapped 2nd page
          val prog = Seq(
            LoadImm64(1, 0x0102030405060708L),
            StoreInd(1, 0, grownOffset, 8),
            LoadInd(2, 0, grownOffset, 8, signed = false),
            Trap
          )
          val (code, bitmask) = encodeProgram(prog)
          val blob = ProgramBlob(
            code = code, bitmask = bitmask, jumpTable = JumpTable(Array.empty, 0),
            is64Bit = true, roData = Array.empty, rwData = rwData, stackSize = 4096
          )
          val module = InterpretedModule.create(blob) match
            case Right(m) => m
            case Left(e)  => fail(s"module create failed: $e")
          val inst = InterpretedInstance.fromModule(module, forceStepTracing = false)

          val rwBase = module.memoryMap.rwDataAddress
          val preRwSize = module.memoryMap.rwDataSize.signed
          preRwSize shouldBe pageSize // sanity: exactly one page before growth

          // Grow the heap by one extra page via sbrk — the public entrypoint
          // BasicMemory exposes for tests that don't want to drive it through
          // a real PVM `sbrk` instruction/host call.
          val grown = inst.basicMemory.sbrk(UInt(pageSize))
          grown shouldBe defined
          inst.basicMemory.heapSize.signed shouldBe (preRwSize + pageSize)
          inst.basicMemory.heapEnd shouldBe (rwBase + UInt(preRwSize + pageSize))

          // (a) describe()'s RW region must cover the grown page too — the
          // finding under test: pre-fix this only reported `preRwSize` bytes.
          val described = RecompilerMemory.describe(inst)
          val rwRegion = described.regions.find(r => r.base == (rwBase.toLong & 0xFFFFFFFFL))
            .getOrElse(fail("no RW region found in described regions"))
          rwRegion.len should be >= (preRwSize + pageSize).toLong
          rwRegion.writable shouldBe true

          // (b) the load/store at `grownOffset` (in the grown page) must
          // succeed on the interpreter post-sbrk, and the recompiler — handed
          // this same post-sbrk region table — must match it exactly.
          val initRegs = Array.fill(13)(0L)
          initRegs(0) = rwBase.toLong
          inst.setGas(100L)
          inst.setNextProgramCounter(ProgramCounter(0))
          initRegs.zipWithIndex.foreach { case (v, i) => inst.setReg(i, v) }
          val (exit, faultPage) = runToTerminal(inst)
          exit shouldBe PvmRecompiler.EXIT_PANIC // Trap after the load/store
          val regsAfter = Array.tabulate(13)(i => inst.getReg(i))
          val pcAfter = inst.programCounter.map(_.toInt.toLong & 0xFFFFFFFFL).getOrElse(fail("no PC on exit"))
          val interp = InterpResult(exit, inst.gas, regsAfter, pcAfter, faultPage)
          // Sanity: the store/load round-tripped through the grown page (r2
          // must equal what was stored) — i.e. this test actually exercises
          // the grown extent, not a fault that short-circuited the checks.
          regsAfter(2) shouldBe 0x0102030405060708L

          compareRegionRun(rc, prog, initRegs, 100L, described, interp)

          info(s"oracle differential (sbrk-grown heap): RW region extended from $preRwSize to " +
            s"${rwRegion.len} bytes; load/store at grown offset $grownOffset matched the interpreter")
        finally rc.close()
  }

  private def randBatchAArith(rng: Random): AInstr = rng.nextInt(11) match
    case 0 => LoadImm64(rng.nextInt(13), rng.nextLong())
    case 1 => AddImm64(rng.nextInt(13), rng.nextInt(13), rng.nextInt())
    case 2 => Add64(rng.nextInt(13), rng.nextInt(13), rng.nextInt(13))
    case 3 => LoadImm(rng.nextInt(13), rng.nextInt())
    case 4 => MoveReg(rng.nextInt(13), rng.nextInt(13))
    case 5 => AddImm32(rng.nextInt(13), rng.nextInt(13), rng.nextInt()) // exercises sign-extension edges via random i32
    case 6 => Shl64Imm(rng.nextInt(13), rng.nextInt(13), rng.nextInt(200) - 100) // includes shift amounts > 63 and negative-imm edge
    case 7 => And3(rng.nextInt(13), rng.nextInt(13), rng.nextInt(13))
    case 8 => Or3(rng.nextInt(13), rng.nextInt(13), rng.nextInt(13))
    case 9 => CmovNz(rng.nextInt(13), rng.nextInt(13), rng.nextInt(13))
    case _ => Fallthrough

  private def genBatchAControlFlowProgram(rng: Random): Seq[AInstr] =
    val k = 2 + rng.nextInt(5)
    val arithLens = Array.fill(k)(rng.nextInt(3))
    val starts = arithLens.map(_ + 1).scanLeft(0)(_ + _)
    val out = scala.collection.mutable.ArrayBuffer.empty[AInstr]
    for b <- 0 until k do
      for _ <- 0 until arithLens(b) do out += randBatchAArith(rng)
      if b == k - 1 then out += Trap
      else
        val tb = b + 1 + rng.nextInt(k - b - 1)
        val tgt = starts(tb)
        out += (rng.nextInt(5) match
          case 0 => Jump(tgt)
          case 1 => BranchEq(rng.nextInt(13), rng.nextInt(13), tgt)
          case 2 => BranchNe(rng.nextInt(13), rng.nextInt(13), tgt)
          case 3 => BranchEqImm(rng.nextInt(13), rng.nextInt(400) - 200, tgt) // negative imm edge
          case _ => BranchNeImm(rng.nextInt(13), rng.nextInt(400) - 200, tgt))
    out.toSeq

  it should "match the production interpreter on batch-A straight-line arithmetic (Fallthrough/LoadImm/MoveReg/AddImm32/Shl64Imm/And/Or/Cmov)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xBA7C4AL)
          for _ <- 0 until 20000 do
            val n = 1 + rng.nextInt(12)
            val prog = (0 until n).map(_ => randBatchAArith(rng)) :+ Trap
            compareRun(rc, prog, rng)
          info("oracle differential (batch-A arithmetic): 20000 programs matched the interpreter")
        finally rc.close()
  }

  it should "match the production interpreter on batch-A control flow (BranchEqImm/BranchNeImm with negative immediates)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xBA7C4BL)
          for _ <- 0 until 20000 do
            compareRun(rc, genBatchAControlFlowProgram(rng), rng)
          info("oracle differential (batch-A control flow): 20000 programs matched the interpreter")
        finally rc.close()
  }

  it should "match the production interpreter on absolute LoadU64/StoreU64 in and out of mapped regions" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xA65011L)
          var faultCount = 0
          var okCount = 0
          for _ <- 0 until 10000 do
            val isStore = rng.nextBoolean()
            // Sweep addresses in and just past the RW region [RW_BASE, RW_BASE+RW_LEN).
            val address = RW_BASE + rng.nextInt(RW_LEN + 64) - 32
            val prog =
              if isStore then Seq(LoadImm64(1, rng.nextLong()), StoreAbs64(1, address), Trap)
              else Seq(LoadAbs64(1, address), Trap)
            val initRegs = Array.fill(13)(rng.nextLong())
            val gas = prog.length.toLong + 10
            val interp = runInterpreter(prog, initRegs.clone(), gas)
            val pp = toRawColumns(prog)
            val blk = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
            blk.isValid shouldBe true
            val nRegs = initRegs.clone()
            val backing = new Array[Byte](RW_LEN)
            val regions = singleRwRegion(RW_BASE.toLong, backing)
            val out = rc.execute(blk, nRegs, gas, regions, backing, PAGE_SHIFT, 0)
            blk.close()
            withClue(s"program=$prog address=$address\n") {
              out.exit shouldBe interp.exit
              out.gasRemaining shouldBe interp.gas
              out.pc shouldBe interp.pc
              nRegs.toSeq shouldBe interp.regs.toSeq
            }
            if interp.exit == PvmRecompiler.EXIT_FAULT then faultCount += 1 else okCount += 1
          faultCount should be > 0
          okCount should be > 0
          info(s"oracle differential (absolute LoadU64/StoreU64): 10000 programs matched; $faultCount faulted, $okCount succeeded")
        finally rc.close()
  }

  private def compareDjumpRun(rc: PvmRecompiler, prog: Seq[AInstr], jumpTargetIndices: Seq[Int], initRegs: Array[Long], gas: Long): InterpResult =
    val (code, bitmask) = encodeProgram(prog)
    val jumpTable = encodeJumpTable(prog, jumpTargetIndices)
    val blob = ProgramBlob(
      code = code, bitmask = bitmask, jumpTable = jumpTable,
      is64Bit = true, roData = Array.empty, rwData = new Array[Byte](RW_LEN), stackSize = 4096
    )
    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(e)  => fail(s"module create failed: $e")
    val inst = InterpretedInstance.fromModule(module, forceStepTracing = false)
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(0))
    initRegs.zipWithIndex.foreach { case (v, i) => inst.setReg(i, v) }
    val (exit, faultPage) = runToTerminal(inst)
    val regs = Array.tabulate(13)(i => inst.getReg(i))
    val pc = inst.programCounter.map(_.toInt.toLong & 0xFFFFFFFFL).getOrElse(fail("no programCounter set on exit"))
    val interp = InterpResult(exit, inst.gas, regs, pc, faultPage)

    val pp = RecompilerAbi.prepareProgram(code, bitmask, jumpTable)
    val blk = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
    blk.isValid shouldBe true
    val nRegs = initRegs.clone()
    val out = rc.execute(blk, nRegs, gas)
    blk.close()
    withClue(s"program=$prog jumpTargetIndices=$jumpTargetIndices gas=$gas\n" +
      s"interp(exit=${interp.exit} gas=${interp.gas} pc=${interp.pc})\n" +
      s"native(exit=${out.exit} gas=${out.gasRemaining} pc=${out.pc})\n") {
      out.exit shouldBe interp.exit
      out.gasRemaining shouldBe interp.gas
      out.pc shouldBe interp.pc
      nRegs.toSeq shouldBe interp.regs.toSeq
    }
    interp

  private def genDjumpProgram(rng: Random, mode: Int): (Seq[AInstr], Seq[Int], Int) =
    val tableSize = 1 + rng.nextInt(4)
    val blocks = scala.collection.mutable.ArrayBuffer.empty[AInstr]
    val leaderIdx = scala.collection.mutable.ArrayBuffer.empty[Int]
    for i <- 0 until tableSize do
      leaderIdx += blocks.length
      blocks += LoadImm64(2, 1000L + i)
      blocks += Trap
    val djumpSiteIdx = blocks.length
    // addrReg placeholder LoadImm64 patched below (depends on mode).
    blocks += LoadImm64(1, 0L) // [djumpSiteIdx]
    blocks += Djump(1, 0)      // [djumpSiteIdx+1]
    blocks += Trap             // [djumpSiteIdx+2] (only reached on a resolution bug)
    val prog = blocks.toSeq

    val targetIndices: Seq[Int] = mode match
      case 4 => Seq(djumpSiteIdx) // slot 0 targets the djump SITE itself: a
      case _ => leaderIdx.toSeq

    val nonLeaderMidBlockIdx = leaderIdx(0) + 1 // the Trap right after block 0's LoadImm64 — decoded, not a leader
    val effectiveTargetIndices = if mode == 4 then Seq(nonLeaderMidBlockIdx) else targetIndices

    val addr = mode match
      case 0 => // valid: pick a random in-range slot
        val slot = rng.nextInt(tableSize)
        (slot + 1) * 2
      case 1 => // misaligned: odd address
        val slot = rng.nextInt(tableSize)
        (slot + 1) * 2 + 1
      case 2 => 0 // zero address
      case 3 => (tableSize + 1 + rng.nextInt(5)) * 2 // out of range (idx >= tableSize)
      case 4 => 2 // slot 0, which now holds the non-leader sentinel target
      case _ => 0

    val patchedProg = prog.updated(djumpSiteIdx, LoadImm64(1, addr.toLong))
    (patchedProg, effectiveTargetIndices, mode)

  it should "match the production interpreter on djump: valid table resolution through a REAL encoded jump table" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xD31170L)
          var validCount = 0
          for _ <- 0 until 12000 do
            val (prog, targets, _) = genDjumpProgram(rng, mode = 0)
            val initRegs = Array.fill(13)(0L)
            val interp = compareDjumpRun(rc, prog, targets, initRegs, 100L)
            interp.exit shouldBe PvmRecompiler.EXIT_PANIC // always ends at a Trap (valid jump lands on a block ending in Trap)
            validCount += 1
          info(s"oracle differential (djump valid): $validCount programs matched the interpreter")
        finally rc.close()
  }

  it should "match the production interpreter on djump: misaligned address panics" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xD31171L)
          var panicCount = 0
          for _ <- 0 until 8000 do
            val (prog, targets, _) = genDjumpProgram(rng, mode = 1)
            val initRegs = Array.fill(13)(0L)
            val interp = compareDjumpRun(rc, prog, targets, initRegs, 100L)
            interp.exit shouldBe PvmRecompiler.EXIT_PANIC
            panicCount += 1
          info(s"oracle differential (djump misaligned): $panicCount programs matched the interpreter (all panicked)")
        finally rc.close()
  }

  it should "match the production interpreter on djump: zero address panics" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xD31172L)
          var panicCount = 0
          for _ <- 0 until 4000 do
            val (prog, targets, _) = genDjumpProgram(rng, mode = 2)
            val initRegs = Array.fill(13)(0L)
            val interp = compareDjumpRun(rc, prog, targets, initRegs, 100L)
            interp.exit shouldBe PvmRecompiler.EXIT_PANIC
            panicCount += 1
          info(s"oracle differential (djump zero address): $panicCount programs matched the interpreter (all panicked)")
        finally rc.close()
  }

  it should "match the production interpreter on djump: out-of-range table index panics" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xD31173L)
          var panicCount = 0
          for _ <- 0 until 8000 do
            val (prog, targets, _) = genDjumpProgram(rng, mode = 3)
            val initRegs = Array.fill(13)(0L)
            val interp = compareDjumpRun(rc, prog, targets, initRegs, 100L)
            interp.exit shouldBe PvmRecompiler.EXIT_PANIC
            panicCount += 1
          info(s"oracle differential (djump out-of-range index): $panicCount programs matched the interpreter (all panicked)")
        finally rc.close()
  }

  it should "match the production interpreter on djump: a non-leader table entry panics" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xD31174L)
          var panicCount = 0
          for _ <- 0 until 4000 do
            val (prog, targets, _) = genDjumpProgram(rng, mode = 4)
            val initRegs = Array.fill(13)(0L)
            val interp = compareDjumpRun(rc, prog, targets, initRegs, 100L)
            interp.exit shouldBe PvmRecompiler.EXIT_PANIC
            panicCount += 1
          info(s"oracle differential (djump non-leader entry): $panicCount programs matched the interpreter (all panicked)")
        finally rc.close()
  }

  private val branchCmpImmOpcodes: Array[Int] = Array(83, 84, 85, 86, 87, 88, 89, 90)
  private val branchCmpRegOpcodes: Array[Int] = Array(172, 173, 174, 175)
  private val setCmpImmOpcodes: Array[Int] = Array(136, 137, 142, 143)
  private val setCmpRegOpcodes: Array[Int] = Array(216, 217)
  // Plain Imm32/Imm64 shift/rotate opcodes (138-140, 151-153, 158, 160).
  private val shiftRotImmOpcodes: Array[Int] = Array(138, 139, 140, 151, 152, 153, 158, 160)
  // Alt Imm32/Imm64 forms (144-146, 155-157, 159, 161) — operand roles swapped.
  private val shiftRotImmAltOpcodes: Array[Int] = Array(144, 145, 146, 155, 156, 157, 159, 161)
  // Three-register shift/rotate (197-199, 207-209, 220-223).
  private val shiftRotRegOpcodes: Array[Int] = Array(197, 198, 199, 207, 208, 209, 220, 221, 222, 223)

  private def randBatchBArith(rng: Random): AInstr = rng.nextInt(6) match
    case 0 => LoadImm64(rng.nextInt(13), rng.nextLong())
    case 1 => SetCmpImm(setCmpImmOpcodes(rng.nextInt(setCmpImmOpcodes.length)), rng.nextInt(13), rng.nextInt(13), rng.nextInt())
    case 2 => SetCmpReg(setCmpRegOpcodes(rng.nextInt(setCmpRegOpcodes.length)), rng.nextInt(13), rng.nextInt(13), rng.nextInt(13))
    case 3 =>
      // shift amounts exercised across the full width AND beyond (masking edge):
      // negative, 0, small, exactly-width, and > width.
      val amt = rng.nextInt(9) match
        case 0 => -1 - rng.nextInt(200)
        case 1 => 0
        case 2 => 31; case 3 => 32; case 4 => 63; case 5 => 64
        case _ => rng.nextInt(200) - 100
      ShiftRotImm(shiftRotImmOpcodes(rng.nextInt(shiftRotImmOpcodes.length)), rng.nextInt(13), rng.nextInt(13), amt)
    case 4 =>
      val amt = rng.nextInt(9) match
        case 0 => -1 - rng.nextInt(200)
        case 1 => 0
        case 2 => 31; case 3 => 32; case 4 => 63; case 5 => 64
        case _ => rng.nextInt(200) - 100
      ShiftRotImmAlt(shiftRotImmAltOpcodes(rng.nextInt(shiftRotImmAltOpcodes.length)), rng.nextInt(13), rng.nextInt(13), amt)
    case _ => ShiftRotReg(shiftRotRegOpcodes(rng.nextInt(shiftRotRegOpcodes.length)), rng.nextInt(13), rng.nextInt(13), rng.nextInt(13))

  it should "match the production interpreter on batch-B straight-line arithmetic (SetCmp*/ShiftRotate*)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xB47C40L)
          for _ <- 0 until 20000 do
            val n = 1 + rng.nextInt(12)
            val prog = (0 until n).map(_ => randBatchBArith(rng)) :+ Trap
            compareRun(rc, prog, rng)
          info("oracle differential (batch-B arithmetic): 20000 programs matched the interpreter")
        finally rc.close()
  }

  private def edgeRegs(rng: Random): Array[Long] =
    Array.tabulate(13) { _ =>
      rng.nextInt(6) match
        case 0 => -1L
        case 1 => 0L
        case 2 => Long.MinValue
        case 3 => Long.MaxValue
        case 4 => 0x80000000L // i32::MIN as a positive 64-bit value (unsigned-looking, 32-bit-negative)
        case _ => rng.nextLong()
    }

  it should "match the production interpreter on batch-B compares/shifts with extreme register values" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xB47C41L)
          for _ <- 0 until 20000 do
            val n = 1 + rng.nextInt(8)
            val prog = (0 until n).map(_ => randBatchBArith(rng)) :+ Trap
            val initRegs = edgeRegs(rng)
            val gas = prog.length.toLong + rng.nextInt(50)
            val interp = runInterpreter(prog, initRegs.clone(), gas)
            val pp = toRawColumns(prog)
            val blk = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
            blk.isValid shouldBe true
            val nRegs = initRegs.clone()
            val out = rc.execute(blk, nRegs, gas)
            blk.close()
            withClue(s"program=$prog initRegs=${initRegs.toSeq}\n") {
              out.exit shouldBe interp.exit
              out.gasRemaining shouldBe interp.gas
              out.pc shouldBe interp.pc
              nRegs.toSeq shouldBe interp.regs.toSeq
            }
          info("oracle differential (batch-B extreme-value regs): 20000 programs matched the interpreter")
        finally rc.close()
  }

  private def genBatchBControlFlowProgram(rng: Random): Seq[AInstr] =
    val k = 2 + rng.nextInt(6)
    val arithLens = Array.fill(k)(rng.nextInt(3))
    val starts = arithLens.map(_ + 1).scanLeft(0)(_ + _)
    val out = scala.collection.mutable.ArrayBuffer.empty[AInstr]
    for b <- 0 until k do
      for _ <- 0 until arithLens(b) do
        out += (if rng.nextBoolean() then randBatchAArith(rng) else randBatchBArith(rng))
      if b == k - 1 then out += Trap
      else
        val tb = b + 1 + rng.nextInt(k - b - 1)
        val tgt = starts(tb)
        out += (rng.nextInt(9) match
          case 0 => Jump(tgt)
          case 1 => BranchEq(rng.nextInt(13), rng.nextInt(13), tgt)
          case 2 => BranchNe(rng.nextInt(13), rng.nextInt(13), tgt)
          case 3 => BranchEqImm(rng.nextInt(13), rng.nextInt(400) - 200, tgt)
          case 4 => BranchNeImm(rng.nextInt(13), rng.nextInt(400) - 200, tgt)
          case 5 => BranchCmpImm(branchCmpImmOpcodes(rng.nextInt(branchCmpImmOpcodes.length)), rng.nextInt(13), rng.nextInt(400) - 200, tgt)
          case 6 => BranchCmpImm(branchCmpImmOpcodes(rng.nextInt(branchCmpImmOpcodes.length)), rng.nextInt(13), -1 - rng.nextInt(50), tgt) // negative-imm edge
          case 7 => BranchCmpReg(branchCmpRegOpcodes(rng.nextInt(branchCmpRegOpcodes.length)), rng.nextInt(13), rng.nextInt(13), tgt)
          case _ => BranchCmpReg(branchCmpRegOpcodes(rng.nextInt(branchCmpRegOpcodes.length)), rng.nextInt(13), rng.nextInt(13), tgt))
    out.toSeq

  it should "match the production interpreter on batch-B control flow (Branch*Imm + reg-reg compare branches, signed/unsigned/negative-imm mixes)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xB47C42L)
          for _ <- 0 until 20000 do
            val prog = genBatchBControlFlowProgram(rng)
            val initRegs = if rng.nextBoolean() then edgeRegs(rng) else Array.fill(13)(rng.nextLong())
            val gas = prog.length.toLong + rng.nextInt(50)
            val interp = runInterpreter(prog, initRegs.clone(), gas)
            val pp = toRawColumns(prog)
            val blk = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
            blk.isValid shouldBe true
            val nRegs = initRegs.clone()
            val out = rc.execute(blk, nRegs, gas)
            blk.close()
            withClue(s"program=$prog gas=$gas initRegs=${initRegs.toSeq}\n" +
              s"interp(exit=${interp.exit} gas=${interp.gas} pc=${interp.pc})\n" +
              s"native(exit=${out.exit} gas=${out.gasRemaining} pc=${out.pc})\n") {
              out.exit shouldBe interp.exit
              out.gasRemaining shouldBe interp.gas
              out.pc shouldBe interp.pc
              nRegs.toSeq shouldBe interp.regs.toSeq
            }
          info("oracle differential (batch-B control flow): 20000 programs matched the interpreter")
        finally rc.close()
  }
