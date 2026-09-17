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
