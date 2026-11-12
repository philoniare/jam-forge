package io.forge.jam.pvm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.UInt
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.types.*
import io.forge.jam.pvm.program.{ProgramBlob, JumpTable}

/**
 * Tests for the PVM execution engine.
 *
 * These tests verify:
 * 1. Register file read/write (13 registers)
 * 2. Program counter and gas accessors
 * 3. load_imm_jump register write and panic pc on an invalid jump
 */
class ExecutionEngineSpec extends AnyFlatSpec with Matchers:

  "InterpretedInstance" should "read/write all 13 registers, with 32-bit truncation and sign extension" in {
    val instance = createTestInstance()

    // Verify we have 13 registers
    instance.regs.length shouldBe 13

    // Test writing and reading each register
    for i <- 0 until 13 do
      instance.setReg(i, 0x100L + i)
      instance.reg(i) shouldBe (0x100L + i)

    // Test 32-bit truncation in 32-bit mode (default)
    instance.setReg(0, 0xFFFFFFFF_12345678L)
    // In 32-bit mode, only lower 32 bits are kept, sign-extended
    val value = instance.reg(0)
    (value & 0xFFFFFFFFL) shouldBe 0x12345678L

    // Test negative values (sign extension)
    instance.setReg(1, -42L)
    instance.reg(1) shouldBe (-42L & 0xFFFFFFFFL)

    // setReg32 sign-extends: UInt(0x80000000) is -2^31 as signed 32-bit, so the
    // raw 64-bit register holds 0xFFFFFFFF_80000000L.
    instance.setReg32(0, UInt(0x80000000))
    instance.regs(0) shouldBe -2147483648L
    // In 32-bit mode, reg() masks to lower 32 bits
    instance.reg(0) shouldBe 0x80000000L

    // setReg32 with a positive value; getReg returns the full 64-bit value
    instance.setReg32(1, UInt(0x12345678))
    instance.reg(1) shouldBe 0x12345678L
    instance.getReg(1) shouldBe 0x12345678L

    // setReg64 in 32-bit mode: masked to 32 bits (0x9ABCDEF0) then sign-extended
    instance.setReg64(2, 0x123456789ABCDEF0L)
    instance.regs(2) shouldBe 0xFFFFFFFF9ABCDEF0L
  }

  it should "keep programCounter unset until run, and store nextProgramCounter and gas as given" in {
    val instance = createTestInstance()

    instance.programCounter shouldBe None
    instance.setNextProgramCounter(ProgramCounter(100))
    instance.nextProgramCounter shouldBe Some(ProgramCounter(100))
    instance.programCounter shouldBe None
    instance.setNextProgramCounter(ProgramCounter(200))
    instance.nextProgramCounter shouldBe Some(ProgramCounter(200))

    instance.setGas(1000L)
    instance.gas shouldBe 1000L
    instance.setGas(-10L)
    instance.gas shouldBe -10L
  }

  it should "write load_imm_jump's register even when the jump panics, and panic at the jump's pc (PVM-3)" in {
    // load_imm_jump: reg7 = 1234 (0x04D2), target = 99 (not a valid block start).
    // Encoding mirrors inst_load_imm_and_jump.json: opcode 80, operand byte 39
    // (reg_A=7, imm length 2), imm bytes 0xD2 0x04, then the 1-byte jump target.
    val code = Array[Byte](80.toByte, 39.toByte, 210.toByte, 4.toByte, 99.toByte)
    val bitmask = Array[Byte](0x01.toByte) // single instruction boundary at offset 0
    val instance = createTestInstanceWithCode(code, bitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(10000L)

    instance.run() match
      case Right(interrupt) => interrupt shouldBe InterruptKind.Panic
      case Left(err)        => fail(s"Unexpected error: $err")

    // The destination register MUST hold immX despite the panicking jump.
    instance.reg(7) shouldBe 1234L
    // The panic pc is the jump instruction (0), not the invalid target (99).
    instance.programCounter.map(_.value.toInt) shouldBe Some(0)
  }

  // Helper: Create a minimal test instance
  private def createTestInstance(): InterpretedInstance =
    val code = Array[Byte](0) // Single panic instruction
    val bitmask = Array[Byte](1) // Instruction at offset 0
    createTestInstanceWithCode(code, bitmask)

  // Helper: Create a test instance with specific code
  private def createTestInstanceWithCode(code: Array[Byte], bitmask: Array[Byte]): InterpretedInstance =
    val blob = ProgramBlob(
      code = code,
      bitmask = bitmask,
      jumpTable = JumpTable(Array.empty, 0),
      is64Bit = false,
      roData = Array.empty,
      rwData = new Array[Byte](4096), // Some RW data for memory
      stackSize = 4096
    )

    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(err) => throw new RuntimeException(s"Failed to create module: $err")

    InterpretedInstance.fromModule(module)
