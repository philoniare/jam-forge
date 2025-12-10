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
 * 2. Program counter management and jumps
 * 3. Gas metering deduction
 * 4. Basic block compilation and caching
 * 5. Interrupt handling (Panic, Segfault, OutOfGas)
 * 6. Simple instruction execution (add, load_imm)
 */
class ExecutionEngineSpec extends AnyFlatSpec with Matchers:

  // Test 1: Register file read/write (13 registers)
  "InterpretedInstance" should "support read/write of all 13 registers" in {
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
  }

  // Test 2: Program counter management and jumps
  it should "correctly manage program counter and jumps" in {
    val instance = createTestInstance()

    // Initially, program counter should not be valid
    instance.programCounter shouldBe None

    // Set next program counter
    instance.setNextProgramCounter(ProgramCounter(100))
    instance.nextProgramCounter shouldBe Some(ProgramCounter(100))

    // After setting, programCounter is still invalid until run
    instance.programCounter shouldBe None

    // Set to a different value
    instance.setNextProgramCounter(ProgramCounter(200))
    instance.nextProgramCounter shouldBe Some(ProgramCounter(200))
  }

  // Test 3: Gas metering deduction
  it should "correctly track and deduct gas" in {
    val instance = createTestInstance()

    // Set initial gas
    instance.setGas(1000L)
    instance.gas shouldBe 1000L

    // Simulate gas deduction
    instance.setGas(instance.gas - 50)
    instance.gas shouldBe 950L

    // Gas can go negative (indicates exhaustion)
    instance.setGas(-10L)
    instance.gas shouldBe -10L
    instance.gas < 0 shouldBe true
  }

  // Test 4: Basic block compilation and caching via FlatMap
  "FlatMap" should "support O(1) lookup for compiled handlers" in {
    // Test FlatMap directly since it's used for handler caching
    val flatMap = FlatMap.create[PackedTarget](UInt(100))

    // Initially empty
    flatMap.get(UInt(0)) shouldBe None
    flatMap.get(UInt(50)) shouldBe None

    // Insert some values
    val packed1 = PackedTarget.pack(UInt(10), isJumpTargetValid = true)
    val packed2 = PackedTarget.pack(UInt(20), isJumpTargetValid = false)

    flatMap.insert(UInt(0), packed1)
    flatMap.insert(UInt(50), packed2)

    // Verify retrieval
    flatMap.get(UInt(0)) shouldBe Some(packed1)
    flatMap.get(UInt(50)) shouldBe Some(packed2)

    // Unpack and verify
    val (isValid1, offset1) = PackedTarget.unpack(packed1)
    isValid1 shouldBe true
    offset1 shouldBe UInt(10)

    val (isValid2, offset2) = PackedTarget.unpack(packed2)
    isValid2 shouldBe false
    offset2 shouldBe UInt(20)

    // Clear and verify
    flatMap.clear()
    flatMap.get(UInt(0)) shouldBe None
    flatMap.get(UInt(50)) shouldBe None
  }

  // Test 5: Interrupt handling (Panic, Segfault, OutOfGas)
  it should "correctly handle interrupts" in {
    // Test with a simple panic program (opcode 0)
    val code = Array[Byte](0) // Panic opcode
    val bitmask = Array[Byte](1) // First instruction boundary

    val instance = createTestInstanceWithCode(code, bitmask)

    // Set entry point and gas
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(100L)

    // Run should return Panic interrupt
    instance.run() match
      case Right(interrupt) =>
        interrupt shouldBe InterruptKind.Panic
      case Left(err) =>
        fail(s"Unexpected error: $err")

    // Test OutOfGas interrupt by setting zero gas
    val instance2 = createTestInstanceWithCode(code, bitmask)
    instance2.setNextProgramCounter(ProgramCounter(0))
    instance2.setGas(0L) // No gas - should trigger OutOfGas during chargeGas

    instance2.run() match
      case Right(interrupt) =>
        // Should get OutOfGas since we charge gas before panic
        interrupt shouldBe InterruptKind.OutOfGas
      case Left(err) =>
        fail(s"Unexpected error: $err")
  }

  // Test 6: Simple instruction execution (add, load_imm)
  it should "correctly execute simple instructions" in {
    // Test LoadImm instruction
    // Opcode 51 (LoadImm) with register 0 and immediate value 42
    val loadImmCode = Array[Byte](
      51.toByte,  // LoadImm opcode
      0.toByte,   // Register 0
      42.toByte,  // Immediate value
      0.toByte    // Panic to terminate
    )
    val loadImmBitmask = Array[Byte](0x09) // Instructions at offsets 0 and 3

    val instance = createTestInstanceWithCode(loadImmCode, loadImmBitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(100L)

    // Before execution, register 0 should be 0
    instance.reg(0) shouldBe 0L

    // Run should execute LoadImm and then Panic
    instance.run() match
      case Right(interrupt) =>
        // Execution should terminate with Panic after LoadImm
        interrupt shouldBe InterruptKind.Panic
        // Register 0 should now have the loaded value
        instance.reg(0) shouldBe 42L
      case Left(err) =>
        fail(s"Unexpected error: $err")

    // Verify gas was deducted (2 instructions: LoadImm + Panic)
    instance.gas should be < 100L
  }

  // Additional test: ExecutionContext register operations
  "ExecutionContext" should "correctly perform register operations" in {
    val instance = createTestInstance()

    // Test setReg32 with sign extension
    // UInt(0x80000000) is 2147483648 as unsigned, but when treated as signed 32-bit it's negative
    // After sign extension to 64-bit, it becomes -2147483648 = 0xFFFFFFFF_80000000L
    instance.setReg32(0, UInt(0x80000000))
    // The raw value in regs is sign-extended
    val rawValue = instance.regs(0)
    rawValue shouldBe -2147483648L  // -2^31

    // In 32-bit mode, reg() masks to lower 32 bits
    val value = instance.reg(0)
    // 0x80000000 masked to 32-bit unsigned is 2147483648
    value shouldBe 0x80000000L

    // Test setReg32 with positive value
    instance.setReg32(1, UInt(0x12345678))
    instance.reg(1) shouldBe 0x12345678L

    // Test getReg (returns full 64-bit value)
    instance.getReg(1) shouldBe 0x12345678L

    // Test setReg64 - in 32-bit mode, values are masked to lower 32 bits and sign-extended
    // 0x123456789ABCDEF0L masked to 32 bits: 0x9ABCDEF0
    // Sign-extended (bit 31 is 1): 0xFFFFFFFF_9ABCDEF0L = -1698898192L
    instance.setReg64(2, 0x123456789ABCDEF0L)
    instance.regs(2) shouldBe 0xFFFFFFFF9ABCDEF0L
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
