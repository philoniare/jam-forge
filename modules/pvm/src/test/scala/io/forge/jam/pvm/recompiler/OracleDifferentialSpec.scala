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
  private case object Trap extends AInstr

  // ---- PVM encoder (abstract -> code bytes + bitmask) -------------------------
  private def longLE(v: Long): Array[Byte] =
    Array.tabulate(8)(i => ((v >>> (i * 8)) & 0xff).toByte)
  private def intLE(v: Int): Array[Byte] =
    Array.tabulate(4)(i => ((v >>> (i * 8)) & 0xff).toByte)

  private def regByte(a: Int, b: Int): Byte = ((a & 0xf) | ((b & 0xf) << 4)).toByte
  private def encodeInstr(a: AInstr): Array[Byte] = a match
    case LoadImm64(reg, imm)      => Array[Byte](20.toByte, reg.toByte) ++ longLE(imm) // skip 10
    case AddImm64(dst, src, imm)  => Array[Byte](149.toByte, regByte(dst, src)) ++ intLE(imm) // skip 6
    case Add64(d, s1, s2)         => Array[Byte](200.toByte, regByte(s1, s2), d.toByte) // skip 3
    case Sub64(d, s1, s2)         => Array[Byte](201.toByte, regByte(s1, s2), d.toByte)
    case Mul64(d, s1, s2)         => Array[Byte](202.toByte, regByte(s1, s2), d.toByte)
    case Trap                     => Array[Byte](0) // skip 1

  /** Encode a program to (code, bitmask), setting a bitmask bit at every
    * instruction's first byte (which is how the decoder finds instruction
    * boundaries / skip). */
  private def encodeProgram(prog: Seq[AInstr]): (Array[Byte], Array[Byte]) =
    val code = scala.collection.mutable.ArrayBuffer.empty[Byte]
    val starts = scala.collection.mutable.ArrayBuffer.empty[Int]
    for a <- prog do
      starts += code.length
      code ++= encodeInstr(a)
    val bytes = code.toArray
    val bitmask = new Array[Byte]((bytes.length + 7) / 8)
    for s <- starts do bitmask(s >> 3) = (bitmask(s >> 3) | (1 << (s & 7))).toByte
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

  // ---- the differential -------------------------------------------------------
  private def libPath: Option[Path] =
    Option(System.getProperty("jam.pvm.recompiler.lib")).map(Path.of(_)).filter(Files.exists(_))

  "the native recompiler" should "match the production interpreter on arithmetic programs" in {
    libPath match
      case None =>
        cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val rng = new Random(0xC0FFEEL)
          val iters = 20000
          var checked = 0
          for _ <- 0 until iters do
            val n = 1 + rng.nextInt(12)
            val body: Seq[AInstr] = (0 until n).map { _ =>
              rng.nextInt(5) match
                case 0 => LoadImm64(rng.nextInt(13), rng.nextLong())
                case 1 => AddImm64(rng.nextInt(13), rng.nextInt(13), rng.nextInt())
                case 2 => Add64(rng.nextInt(13), rng.nextInt(13), rng.nextInt(13))
                case 3 => Sub64(rng.nextInt(13), rng.nextInt(13), rng.nextInt(13))
                case _ => Mul64(rng.nextInt(13), rng.nextInt(13), rng.nextInt(13))
            }
            val prog = body :+ Trap
            val initRegs = Array.fill(13)(rng.nextLong())
            val gas = prog.length.toLong + rng.nextInt(50) // always sufficient

            val (iExit, iGas, iRegs) = runInterpreter(prog, initRegs.clone(), gas)

            val (op, dst, src, src2, imm) = toRawColumns(prog)
            val blk = rc.compile(op, dst, src, src2, imm)
            blk.isValid shouldBe true
            val nRegs = initRegs.clone()
            val out = rc.execute(blk, nRegs, gas)
            blk.close()
            val nExit = out(0).toInt
            val nGas = out(1)

            withClue(s"program=$prog gas=$gas\n interp(exit=$iExit gas=$iGas)\n native(exit=$nExit gas=$nGas)\n") {
              nExit shouldBe iExit
              nGas shouldBe iGas
              nRegs.toSeq shouldBe iRegs.toSeq
            }
            checked += 1
          info(s"oracle differential: $checked programs matched the interpreter")
        finally rc.close()
  }
