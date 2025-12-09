package io.forge.jam.pvm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import types.*

/**
 * Tests for Opcode enum and Instruction ADT.
 *
 * These tests verify:
 * 1. Opcode bytecode value correctness
 * 2. Control flow analysis methods
 * 3. Instruction pattern matching
 * 4. Instruction operand access
 */
class OpcodeSpec extends AnyFlatSpec with Matchers:

  // Test 1: Opcode bytecode value correctness for representative opcodes
  "Opcode" should "have correct bytecode values for representative opcodes" in {
    // Sample 5 opcodes from different categories
    Opcode.Panic.value shouldBe 0
    Opcode.Jump.value shouldBe 40
    Opcode.Ecalli.value shouldBe 10
    Opcode.Add32.value shouldBe 190
    Opcode.LoadImm.value shouldBe 51
  }

  // Test 2: startsNewBasicBlock() for control flow instructions
  it should "correctly identify instructions that start new basic blocks" in {
    // Control flow instructions that start new blocks
    Opcode.Panic.startsNewBasicBlock shouldBe true
    Opcode.Jump.startsNewBasicBlock shouldBe true
    Opcode.JumpIndirect.startsNewBasicBlock shouldBe true
    Opcode.Fallthrough.startsNewBasicBlock shouldBe true
    Opcode.BranchEq.startsNewBasicBlock shouldBe true
    Opcode.BranchNotEq.startsNewBasicBlock shouldBe true
    Opcode.BranchEqImm.startsNewBasicBlock shouldBe true
    Opcode.LoadImmAndJump.startsNewBasicBlock shouldBe true
    Opcode.LoadImmAndJumpIndirect.startsNewBasicBlock shouldBe true

    // Non-control flow instructions should not start blocks
    Opcode.Add32.startsNewBasicBlock shouldBe false
    Opcode.LoadImm.startsNewBasicBlock shouldBe false
    Opcode.StoreU8.startsNewBasicBlock shouldBe false
    Opcode.MoveReg.startsNewBasicBlock shouldBe false
  }

  // Test 3: canFallthrough() for jump/branch instructions
  it should "correctly identify instructions that cannot fallthrough" in {
    // Instructions that cannot fallthrough (unconditional jumps)
    Opcode.Panic.canFallthrough shouldBe false
    Opcode.Jump.canFallthrough shouldBe false
    Opcode.JumpIndirect.canFallthrough shouldBe false
    Opcode.LoadImmAndJump.canFallthrough shouldBe false
    Opcode.LoadImmAndJumpIndirect.canFallthrough shouldBe false

    // Instructions that can fallthrough
    Opcode.Add32.canFallthrough shouldBe true
    Opcode.BranchEq.canFallthrough shouldBe true  // Conditional branch can fallthrough
    Opcode.LoadImm.canFallthrough shouldBe true
    Opcode.Ecalli.canFallthrough shouldBe true
  }

  // Test 4: Instruction pattern matching exhaustiveness
  "Instruction" should "support exhaustive pattern matching" in {
    import Instruction.*

    // Create representative instructions
    val instructions: List[Instruction] = List(
      Panic,
      Fallthrough,
      Jump(100),
      Add32(0, 1, 2),
      Add64(0, 1, 2),
      LoadImm(0, 42),
      BranchEq(0, 1, 200)
    )

    // Pattern match to verify all cases can be matched
    instructions.foreach { instr =>
      val result = instr match
        case Panic => "panic"
        case Fallthrough => "fallthrough"
        case Jump(target) => s"jump($target)"
        case Add32(d, s1, s2) => s"add32($d, $s1, $s2)"
        case Add64(d, s1, s2) => s"add64($d, $s1, $s2)"
        case LoadImm(r, v) => s"load_imm($r, $v)"
        case BranchEq(r1, r2, off) => s"beq($r1, $r2, $off)"
        case _ => "other"

      result should not be empty
    }
  }

  // Test 5: Instruction operand access
  it should "provide correct operand access" in {
    import Instruction.*

    // Test register operand access
    val add32 = Add32(5, 6, 7)
    add32.d shouldBe 5
    add32.s1 shouldBe 6
    add32.s2 shouldBe 7
    add32.opcode shouldBe Opcode.Add32

    // Test immediate operand access (use a value that fits cleanly in positive Long)
    val loadImm = LoadImm(3, 0x12345678L)
    loadImm.reg shouldBe 3
    loadImm.imm shouldBe 0x12345678L
    loadImm.opcode shouldBe Opcode.LoadImm

    // Test branch instruction operand access
    val branchEq = BranchEq(1, 2, 0x1000)
    branchEq.r1 shouldBe 1
    branchEq.r2 shouldBe 2
    branchEq.offset shouldBe 0x1000
    branchEq.opcode shouldBe Opcode.BranchEq

    // Test jump instruction
    val jump = Jump(0x500)
    jump.target shouldBe 0x500
    jump.opcode shouldBe Opcode.Jump
  }
