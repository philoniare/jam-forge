package io.forge.jam.pvm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.forge.jam.pvm.program.InstructionDecoder
import io.forge.jam.pvm.types.ProgramCounter

class OpcodeRenumberSpec extends AnyFlatSpec with Matchers:

  // ---- 1. byte -> Opcode mapping (Opcode.fromByte) --------------------------

  "Opcode.fromByte" should "map the no-arg family to trap/fallthrough/unlikely (0,1,2)" in {
    Opcode.fromByte(0) shouldBe Some(Opcode.Panic) // trap
    Opcode.fromByte(1) shouldBe Some(Opcode.Fallthrough)
    Opcode.fromByte(2) shouldBe Some(Opcode.Unlikely)
  }

  it should "map byte 101 to CountSetBits64, NOT Sbrk (sbrk instruction removed)" in {
    Opcode.fromByte(101) shouldBe Some(Opcode.CountSetBits64)
  }

  it should "map the full shifted two-register family 100-110" in {
    Opcode.fromByte(100) shouldBe Some(Opcode.MoveReg)
    Opcode.fromByte(101) shouldBe Some(Opcode.CountSetBits64)
    Opcode.fromByte(102) shouldBe Some(Opcode.CountSetBits32)
    Opcode.fromByte(103) shouldBe Some(Opcode.CountLeadingZeroBits64)
    Opcode.fromByte(104) shouldBe Some(Opcode.CountLeadingZeroBits32)
    Opcode.fromByte(105) shouldBe Some(Opcode.CountTrailingZeroBits64)
    Opcode.fromByte(106) shouldBe Some(Opcode.CountTrailingZeroBits32)
    Opcode.fromByte(107) shouldBe Some(Opcode.SignExtend8)
    Opcode.fromByte(108) shouldBe Some(Opcode.SignExtend16)
    Opcode.fromByte(109) shouldBe Some(Opcode.ZeroExtend16)
    Opcode.fromByte(110) shouldBe Some(Opcode.ReverseByte)
  }

  it should "leave byte 111 unmapped (the family now ends at 110)" in {
    Opcode.fromByte(111) shouldBe None
  }

  it should "have no opcode value 101 pointing at a removed Sbrk case" in {
    // There must be no `Opcode` case named Sbrk at all post-removal.
    Opcode.values.map(_.toString) should not contain "Sbrk"
  }

  // ---- 2. instruction decoder wiring ----------------------------------------

  "InstructionDecoder.decode" should "decode byte 2 as Instruction.Unlikely (argless, 1-byte)" in {
    val code = Array[Byte](2, 0, 0, 0)
    val bitmask = Array[Byte](0x02) // next instruction boundary bit set at offset 1 (bit index 1)
    val (instr, skip) = InstructionDecoder.decode(code, bitmask, 0)
    instr shouldBe Instruction.Unlikely
    skip shouldBe 1
  }

  it should "decode byte 101 (regs2 family) as Instruction.CountSetBits64" in {
    val code = Array[Byte](101, ((3 & 0xf) | ((5 & 0xf) << 4)).toByte, 0)
    val bitmask = Array[Byte](0x04) // next instruction boundary at offset 2 (bit 2)
    val (instr, skip) = InstructionDecoder.decode(code, bitmask, 0)
    instr shouldBe Instruction.CountSetBits64(3, 5)
    skip shouldBe 2
  }

  // ---- 3. basic-block classification -----------------------------------------

  "Opcode.Unlikely" should "NOT start a new basic block (non-terminator, falls through)" in {
    Opcode.Unlikely.startsNewBasicBlock shouldBe false
  }

  it should "be able to fall through to the next instruction" in {
    Opcode.Unlikely.canFallthrough shouldBe true
  }

  "Opcode.Panic and Opcode.Fallthrough" should "remain the only no-arg basic-block terminators" in {
    Opcode.Panic.startsNewBasicBlock shouldBe true
    Opcode.Fallthrough.startsNewBasicBlock shouldBe true
    Opcode.Unlikely.startsNewBasicBlock shouldBe false
  }

  // ---- 4. executor: Unlikely is a no-op that just advances -------------------

  "InstructionExecutor" should "execute Unlikely as a no-op: registers unchanged, pc advances" in {
    import io.forge.jam.pvm.engine.{InterpretedInstance, InterpretedModule}
    import io.forge.jam.pvm.program.{JumpTable, ProgramBlob}

    // Program: Unlikely (opcode 2, 1 byte) ++ JumpIndirect(reg=0, offset=0) (halt via RA_INIT sentinel)
    val code = Array[Byte](2, 50, 0)
    val bitmask = Array[Byte]((1 | (1 << 1)).toByte) // boundaries at offset 0 and offset 1

    val blob = ProgramBlob(
      code = code,
      bitmask = bitmask,
      jumpTable = JumpTable(Array.empty, 0),
      is64Bit = true,
      roData = Array.empty,
      rwData = Array.empty,
      stackSize = 4096
    )
    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(e)  => fail(s"module create failed: $e")

    val inst = InterpretedInstance.fromModule(module, forceStepTracing = false)
    inst.setGas(1000L)
    inst.setNextProgramCounter(ProgramCounter(0))
    val preRegs = Array.tabulate(13)(i => if i == 0 then 0xffff0000L else (i * 7).toLong)
    preRegs.zipWithIndex.foreach { case (v, i) => inst.setReg(i, v) }

    // Step once: should execute Unlikely as a no-op and land on the JumpIndirect
    // at offset 1 (not terminate/panic, not change any register).
    inst.run()

    val postRegs = Array.tabulate(13)(inst.getReg)
    postRegs.toSeq shouldBe preRegs.toSeq
    inst.programCounter shouldBe Some(ProgramCounter(1))
  }
