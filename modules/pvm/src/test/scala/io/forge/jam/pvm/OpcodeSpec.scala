package io.forge.jam.pvm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.forge.jam.pvm.Opcode

/**
 * Tests for the Opcode enum.
 *
 * These tests verify the control flow analysis methods
 * (startsNewBasicBlock / canFallthrough).
 */
class OpcodeSpec extends AnyFlatSpec with Matchers:

  // startsNewBasicBlock() for control flow instructions
  "Opcode" should "correctly identify instructions that start new basic blocks" in {
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
    // Unlikely (byte 2) is an argless hint, not a terminator: Panic and
    // Fallthrough remain the only no-arg basic-block terminators.
    Opcode.Unlikely.startsNewBasicBlock shouldBe false
  }

  // canFallthrough() for jump/branch instructions
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
    Opcode.Unlikely.canFallthrough shouldBe true
  }
