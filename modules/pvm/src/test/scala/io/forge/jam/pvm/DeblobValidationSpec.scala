package io.forge.jam.pvm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.{Program, ProgramBlob, JumpTable}
import io.forge.jam.pvm.types.ProgramCounter

class DeblobValidationSpec extends AnyFlatSpec with Matchers:

  private def instanceFor(code: Array[Byte], bitmask: Array[Byte]): InterpretedInstance =
    val blob = ProgramBlob(
      code = code,
      bitmask = bitmask,
      jumpTable = JumpTable(Array.empty, 0),
      is64Bit = false,
      roData = Array.empty,
      rwData = new Array[Byte](4096),
      stackSize = 4096
    )
    val module = InterpretedModule.create(blob) match
      case Right(m)   => m
      case Left(err)  => throw new RuntimeException(s"Failed to create module: $err")
    InterpretedInstance.fromModule(module)

  "deblob" should "panic immediately, executing zero instructions, when the program's last instruction is not a basic-block terminator" in {
    val code = Array[Byte](51, 0, 42) // LoadImm reg0, imm=42
    val bitmask = Array[Byte](0x01) // single instruction boundary at offset 0
    val instance = instanceFor(code, bitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(100L)

    instance.run() match
      case Right(interrupt) => interrupt shouldBe InterruptKind.Panic
      case Left(err)         => fail(s"Unexpected error: $err")

    // Zero instructions executed: LoadImm never ran, so reg0 is untouched...
    instance.reg(0) shouldBe 0L
    // ...and no gas was charged (deblob fails BEFORE Psi_1 / gas metering runs).
    instance.gas shouldBe 100L
    // Panic is reported at the entry imath itself.
    instance.programCounter.map(_.value.toInt) shouldBe Some(0)
  }

  it should "panic immediately when the entry imath lands mid-instruction (not an instruction boundary)" in {
    // Well-formed 2-instruction program (LoadImm; Panic) -- v_blob(c,k,0) holds.
    val code = Array[Byte](51, 0, 42, 0) // LoadImm reg0=42; Panic
    val bitmask = Array[Byte](0x09) // bits 0 and 3 set
    val instance = instanceFor(code, bitmask)
    // Entry at offset 1: k[1] = 0, mid-instruction -- v_inst(imath) fails.
    instance.setNextProgramCounter(ProgramCounter(1))
    instance.setGas(100L)

    instance.run() match
      case Right(interrupt) => interrupt shouldBe InterruptKind.Panic
      case Left(err)         => fail(s"Unexpected error: $err")

    instance.reg(0) shouldBe 0L
    instance.gas shouldBe 100L
    instance.programCounter.map(_.value.toInt) shouldBe Some(1)
  }

  it should "panic immediately when the entry imath is >= the instruction-boundary bitmask length" in {
    val code = Array[Byte](51, 0, 42, 0) // LoadImm reg0=42; Panic
    val bitmask = Array[Byte](0x09)
    val instance = instanceFor(code, bitmask)
    // Entry way past the end of the program -- v_inst(imath) fails (imath >= |k|).
    instance.setNextProgramCounter(ProgramCounter(100))
    instance.setGas(100L)

    instance.run() match
      case Right(interrupt) => interrupt shouldBe InterruptKind.Panic
      case Left(err)         => fail(s"Unexpected error: $err")

    instance.reg(0) shouldBe 0L
    instance.gas shouldBe 100L
    instance.programCounter.map(_.value.toInt) shouldBe Some(100)
  }

  it should "execute a structurally-valid program normally (regression guard)" in {
    val code = Array[Byte](51, 0, 42, 0) // LoadImm reg0=42; Panic
    val bitmask = Array[Byte](0x09)
    val instance = instanceFor(code, bitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(100L)

    instance.run() match
      case Right(interrupt) => interrupt shouldBe InterruptKind.Panic
      case Left(err)         => fail(s"Unexpected error: $err")

    // Both instructions actually ran this time.
    instance.reg(0) shouldBe 42L
    instance.gas should be < 100L
  }

  "Program.isValidBlob" should "accept a program ending in a terminator and reject one that doesn't" in {
    Program.isValidBlob(Array[Byte](0), Array[Byte](0x01)) shouldBe true // Panic (trap) alone
    Program.isValidBlob(Array[Byte](51, 0, 42), Array[Byte](0x01)) shouldBe false // LoadImm alone, not a terminator
  }

  "Program.isValidInstructionBoundary" should "hold only at a set bitmask bit with a defined opcode" in {
    val code = Array[Byte](51, 0, 42, 0)
    val bitmask = Array[Byte](0x09)
    Program.isValidInstructionBoundary(code, bitmask, 0) shouldBe true
    Program.isValidInstructionBoundary(code, bitmask, 3) shouldBe true
    Program.isValidInstructionBoundary(code, bitmask, 1) shouldBe false // mid-instruction
    Program.isValidInstructionBoundary(code, bitmask, 100) shouldBe false // out of range
  }

  // ==========================================================================
  // Part B: branch(b, C) both-target validation + sjump
  // ==========================================================================

  it should "panic on a conditional branch whose TAKEN target is invalid, even though it is NOT taken" in {
    val code = Array[Byte](81.toByte, 0x10, 5, 2, 0) // branch_eq_imm; Panic (fallthrough target, offset 4)
    val bitmask = Array[Byte](0x11) // bits 0 and 4 set (NOT bit 2 -- invalid taken target)
    val instance = instanceFor(code, bitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(100L)
    instance.reg(0) shouldBe 0L

    instance.run() match
      case Right(interrupt) => interrupt shouldBe InterruptKind.Panic
      case Left(err)         => fail(s"Unexpected error: $err")

    // Panic at the branch instruction's own pc; the fallthrough Panic at
    // offset 4 never ran.
    instance.programCounter.map(_.value.toInt) shouldBe Some(0)
  }

  it should "panic on a conditional branch whose FALLTHROUGH target is invalid, even though it IS taken" in {
    val code = Array[Byte](81.toByte, 0x10, 5, 0)
    val bitmask = Array[Byte](0x01) // single instruction boundary at offset 0
    Program.isValidBlob(code, bitmask) shouldBe true // sanity: deblob passes

    val instance = instanceFor(code, bitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(100L)
    instance.setReg(0, 5L)

    instance.run() match
      case Right(interrupt) => interrupt shouldBe InterruptKind.Panic
      case Left(err)         => fail(s"Unexpected error: $err")

    // Pre-fix this would have jumped back to offset 0 and kept looping
    // (condition true -> resolveJump(0) succeeds, fallthrough never checked).
    instance.programCounter.map(_.value.toInt) shouldBe Some(0)
  }

  it should "panic on a standalone fallthrough instruction whose next address is not a block start (sjump)" in {
    val code = Array[Byte](1) // fallthrough
    val bitmask = Array[Byte](0x01)
    Program.isValidBlob(code, bitmask) shouldBe true // sanity: deblob passes

    val instance = instanceFor(code, bitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(100L)

    instance.run() match
      case Right(interrupt) => interrupt shouldBe InterruptKind.Panic
      case Left(err)         => fail(s"Unexpected error: $err")

    instance.programCounter.map(_.value.toInt) shouldBe Some(0)
    instance.gas shouldBe 98L
  }

  // ==========================================================================
  // Part C: blob size limits, c ∈ B[:Z_C] and j ∈ ⟦N_R⟧[:Z_J]
  // ==========================================================================

  private val MaxCodeSize = 1 << 25 // Z_C
  private val MaxJumpTableEntries = 1 << 24 // Z_J

  /** GP natural-number encoding E(x); the values used here stay below 2^56. */
  private def natural(x: Long): Array[Byte] =
    val l = (0 to 7).find(l => x < (1L << (7 * (l + 1)))).get
    val prefix = 256 - (1 << (8 - l)) + (x >>> (8 * l)).toInt
    Array(prefix.toByte) ++ Array.tabulate(l)(i => (x >>> (8 * i)).toByte)

  private val hugeNatural = Array.fill[Byte](9)(0xff.toByte) // E(2^64 - 1)

  /** E(|j|) ++ E_1(z) ++ E(|c|) ++ E_z(j) ++ E(c) ++ E(k), with zeroed j, c and k. */
  private def jamBlob(jumpEntries: Long, entrySize: Int, codeLen: Int): Array[Byte] =
    natural(jumpEntries) ++ Array(entrySize.toByte) ++ natural(codeLen) ++
      new Array[Byte]((jumpEntries * entrySize).toInt + codeLen + (codeLen + 7) / 8)

  /** Parsed code length, so a failing assertion never prints a multi-MiB blob. */
  private def parsedCodeLen(data: Array[Byte]): Option[Int] =
    ProgramBlob.fromCodeAndJumpTable(data).map(_.code.length)

  "ProgramBlob.fromCodeAndJumpTable" should "accept code of exactly Z_C = 2^25 octets" in {
    parsedCodeLen(jamBlob(0, 0, MaxCodeSize)) shouldBe Some(MaxCodeSize)
  }

  it should "reject code longer than Z_C" in {
    parsedCodeLen(jamBlob(0, 0, MaxCodeSize + 1)) shouldBe None
  }

  it should "accept exactly Z_J = 2^24 jump-table entries" in {
    parsedCodeLen(jamBlob(MaxJumpTableEntries, 0, 1)) shouldBe Some(1)
  }

  it should "reject more than Z_J jump-table entries, even when z = 0 makes them occupy no octets" in {
    parsedCodeLen(jamBlob(MaxJumpTableEntries + 1, 0, 1)) shouldBe None
  }

  it should "reject, not throw on, length prefixes of 2^63 or more" in {
    val hugeCount = hugeNatural ++ Array[Byte](0) ++ natural(1) ++ new Array[Byte](2)
    val hugeCode = natural(0) ++ Array[Byte](0) ++ hugeNatural ++ new Array[Byte](2)
    parsedCodeLen(hugeCount) shouldBe None
    parsedCodeLen(hugeCode) shouldBe None
  }

  it should "reject, not throw on, a truncated jump table whose octet length exceeds Int.MaxValue" in {
    // 2^24 entries of 255 octets each, but no jump-table octets actually present.
    val truncated = natural(MaxJumpTableEntries) ++ Array(255.toByte) ++ natural(1) ++ new Array[Byte](2)
    parsedCodeLen(truncated) shouldBe None
  }
