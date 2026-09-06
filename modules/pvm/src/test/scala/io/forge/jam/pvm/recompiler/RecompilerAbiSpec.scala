package io.forge.jam.pvm.recompiler

import io.forge.jam.pvm.Instruction
import io.forge.jam.pvm.program.JumpTable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Unit coverage for `RecompilerAbi`
 */
class RecompilerAbiSpec extends AnyFlatSpec with Matchers:

  import RecompilerAbi.RawInstr

  // ---- per-family instruction mapping -----------------------------------------

  "RecompilerAbi.mapInstruction" should "map regs3 (Add32) as a=d, b=s1, c=s2" in {
    RecompilerAbi.mapInstruction(Instruction.Add32(5, 1, 2)) shouldBe
      RawInstr(Instruction.Add32(5, 1, 2).opcode.value, 5, 1, 2, 0L, 0L)
  }

  it should "map regs2+imm (AddImm32) as a=dst, b=src, imm=imm" in {
    RecompilerAbi.mapInstruction(Instruction.AddImm32(3, 4, 99L)) shouldBe
      RawInstr(Instruction.AddImm32(3, 4, 99L).opcode.value, 3, 4, 0, 99L, 0L)
  }

  it should "map indirect load (LoadIndirectU8) as a=dst, b=base, imm=offset" in {
    RecompilerAbi.mapInstruction(Instruction.LoadIndirectU8(2, 6, 40L)) shouldBe
      RawInstr(Instruction.LoadIndirectU8(2, 6, 40L).opcode.value, 2, 6, 0, 40L, 0L)
  }

  it should "map indirect store (StoreIndirectU8) as a=src, b=base, imm=offset" in {
    RecompilerAbi.mapInstruction(Instruction.StoreIndirectU8(9, 1, 40L)) shouldBe
      RawInstr(Instruction.StoreIndirectU8(9, 1, 40L).opcode.value, 9, 1, 0, 40L, 0L)
  }

  it should "map regs2+offset branch (BranchEq) as a=r1, b=r2, imm=translated target index" in {
    // byte offset 120 resolves (via the injected resolver) to instruction index 7
    RecompilerAbi.mapInstruction(Instruction.BranchEq(1, 2, 120L), _ => 7L) shouldBe
      RawInstr(Instruction.BranchEq(1, 2, 120L).opcode.value, 1, 2, 0, 7L, 0L)
  }

  it should "map reg+imm+offset branch (BranchEqImm) as a=reg, imm=compare-imm, imm2=translated target index" in {
    RecompilerAbi.mapInstruction(Instruction.BranchEqImm(3, 42L, 200L), _ => 11L) shouldBe
      RawInstr(Instruction.BranchEqImm(3, 42L, 200L).opcode.value, 3, 0, 0, 42L, 11L)
  }

  it should "map reg+imm (LoadImm) as a=reg, imm=imm" in {
    RecompilerAbi.mapInstruction(Instruction.LoadImm(6, -7L)) shouldBe
      RawInstr(Instruction.LoadImm(6, -7L).opcode.value, 6, 0, 0, -7L, 0L)
  }

  it should "map reg+imm (LoadU32) as a=reg, imm=address" in {
    RecompilerAbi.mapInstruction(Instruction.LoadU32(6, 0x1000L)) shouldBe
      RawInstr(Instruction.LoadU32(6, 0x1000L).opcode.value, 6, 0, 0, 0x1000L, 0L)
  }

  it should "map reg+imm (JumpIndirect) as a=reg, imm=offset (a runtime target, not translated)" in {
    RecompilerAbi.mapInstruction(Instruction.JumpIndirect(4, 16L)) shouldBe
      RawInstr(Instruction.JumpIndirect(4, 16L).opcode.value, 4, 0, 0, 16L, 0L)
  }

  it should "map imm+imm (StoreImmU8) as imm=address, imm2=value" in {
    RecompilerAbi.mapInstruction(Instruction.StoreImmU8(0x2000L, 0xAB)) shouldBe
      RawInstr(Instruction.StoreImmU8(0x2000L, 0xAB).opcode.value, 0, 0, 0, 0x2000L, 0xABL)
  }

  it should "map imm (Jump) as imm=translated target index" in {
    RecompilerAbi.mapInstruction(Instruction.Jump(80L), _ => 3L) shouldBe
      RawInstr(Instruction.Jump(80L).opcode.value, 0, 0, 0, 3L, 0L)
  }

  it should "map imm (Ecalli) as imm=hostId" in {
    RecompilerAbi.mapInstruction(Instruction.Ecalli(12345L)) shouldBe
      RawInstr(Instruction.Ecalli(12345L).opcode.value, 0, 0, 0, 12345L, 0L)
  }

  it should "map reg+imm+imm (LoadImmAndJump) as a=reg, imm=imm, imm2=translated target index" in {
    RecompilerAbi.mapInstruction(Instruction.LoadImmAndJump(5, 77L, 240L), _ => 9L) shouldBe
      RawInstr(Instruction.LoadImmAndJump(5, 77L, 240L).opcode.value, 5, 0, 0, 77L, 9L)
  }

  it should "map regs2+imm+imm (LoadImmAndJumpIndirect) as a=dst, b=base, imm=imm, imm2=offset (untranslated)" in {
    RecompilerAbi.mapInstruction(Instruction.LoadImmAndJumpIndirect(2, 3, 55L, 16L)) shouldBe
      RawInstr(Instruction.LoadImmAndJumpIndirect(2, 3, 55L, 16L).opcode.value, 2, 3, 0, 55L, 16L)
  }

  it should "map regs2 (MoveReg) as a=dst, b=src" in {
    RecompilerAbi.mapInstruction(Instruction.MoveReg(7, 8)) shouldBe
      RawInstr(Instruction.MoveReg(7, 8).opcode.value, 7, 8, 0, 0L, 0L)
  }

  it should "map argless (Panic, Fallthrough) as opcode-only" in {
    RecompilerAbi.mapInstruction(Instruction.Panic) shouldBe RawInstr(Instruction.Panic.opcode.value, 0, 0, 0, 0L, 0L)
    RecompilerAbi.mapInstruction(Instruction.Fallthrough) shouldBe RawInstr(Instruction.Fallthrough.opcode.value, 0, 0, 0, 0L, 0L)
  }

  it should "always take opcode from instr.opcode.value, never a private table" in {
    // Sanity: the real PVM value for Add64 is 200 (io.forge.jam.pvm.Opcode.Add64).
    RecompilerAbi.mapInstruction(Instruction.Add64(1, 2, 3)).opcode shouldBe 200
    // ...and for BranchEq it's 170.
    RecompilerAbi.mapInstruction(Instruction.BranchEq(0, 1, 0L)).opcode shouldBe 170
  }

  // ---- program preparation: target translation --------------------------------

  // Minimal hand-encoded blobs (opcode byte, regs-nibble byte, 4-byte LE
  // relative displacement) — same shapes InstructionDecoder expects.
  private def regByte(a: Int, b: Int): Byte = ((a & 0xf) | ((b & 0xf) << 4)).toByte
  private def intLE(v: Int): Array[Byte] = Array.tabulate(4)(i => ((v >>> (i * 8)) & 0xff).toByte)
  private def bitmaskFor(offsets: Seq[Int], totalLen: Int): Array[Byte] =
    val bm = new Array[Byte]((totalLen + 7) / 8)
    offsets.foreach(s => bm(s >> 3) = (bm(s >> 3) | (1 << (s & 7))).toByte)
    bm

  "RecompilerAbi.prepareProgram" should "translate a forward Jump's byte-offset target to an instruction index" in {
    // instr0: Jump at offset 0 (size 5, opcode=40) encodes relative
    // displacement (tOff - off) = 5 - 0 = 5 -> absolute target byte offset 5.
    // instr1: Panic at offset 5 (target of the jump, instruction index 1).
    val code = Array[Byte](40.toByte) ++ intLE(5) ++ Array[Byte](0)
    val bitmask = bitmaskFor(Seq(0, 5), code.length)
    val pp = RecompilerAbi.prepareProgram(code, bitmask, JumpTable.Empty)
    pp.opcodes.toSeq shouldBe Seq(Instruction.Jump(0).opcode.value, Instruction.Panic.opcode.value)
    pp.imm(0) shouldBe 1L // instruction index of the Panic at offset 5
  }

  it should "replace an instruction with an invalid static target with Panic" in {
    // Jump's relative displacement resolves to byte offset 2, which is NOT a
    // decoded instruction's leader (mid-instruction, inside the Jump itself).
    val code = Array[Byte](40.toByte) ++ intLE(2) ++ Array[Byte](0)
    val bitmask = bitmaskFor(Seq(0, 5), code.length)
    val pp = RecompilerAbi.prepareProgram(code, bitmask, JumpTable.Empty)
    pp.opcodes(0) shouldBe Instruction.Panic.opcode.value
    pp.imm(0) shouldBe 0L
  }

  it should "translate jump-table byte-offset entries to instruction indices, dropping invalid entries" in {
    // instr0: Panic (offset 0), instr1: Panic (offset 1). Jump table entries:
    // {1 (valid -> index 1), 4 (invalid, not a leader)}.
    val code = Array[Byte](0, 0)
    val bitmask = bitmaskFor(Seq(0, 1), code.length)
    val jt = JumpTable(Array[Byte](1, 0, 0, 0, 4, 0, 0, 0), entrySize = 4)
    val pp = RecompilerAbi.prepareProgram(code, bitmask, jt)
    pp.jumpTable.toSeq shouldBe Seq(1)
  }

  it should "leave a program with no control-flow untouched (identity target translation)" in {
    // Add64(2,0,1) at offset 0 (opcode=200, regs byte, dst byte -> size 3), then Panic.
    val code = Array[Byte](200.toByte, regByte(0, 1), 2.toByte, 0)
    val bitmask = bitmaskFor(Seq(0, 3), code.length)
    val pp = RecompilerAbi.prepareProgram(code, bitmask, JumpTable.Empty)
    pp.opcodes.toSeq shouldBe Seq(Instruction.Add64(0, 0, 0).opcode.value, Instruction.Panic.opcode.value)
    pp.a(0) shouldBe 2; pp.b(0) shouldBe 0; pp.c(0) shouldBe 1
  }
  private val branchEqImmOp: Int = Instruction.BranchEqImm(0, 0L, 0L).opcode.value
  private val loadImmAndJumpOp: Int = Instruction.LoadImmAndJump(0, 0L, 0L).opcode.value

  private def encodeRegImmOffset(op: Int, reg: Int, imm: Long, immLen: Int, disp: Array[Byte]): Array[Byte] =
    val header = ((reg & 0xF) | ((immLen & 0x7) << 4)).toByte
    val immBytes = Array.tabulate(immLen)(i => ((imm >> (i * 8)) & 0xff).toByte)
    Array(op.toByte, header) ++ immBytes ++ disp

  it should "map BranchEqImm with compare-immediate -1 and a VALID target normally in prepareProgram (not Panic)" in {
    val instr0 = encodeRegImmOffset(branchEqImmOp, reg = 0, imm = -1L, immLen = 1, disp = Array[Byte](4))
    val code = instr0 ++ Array[Byte](0)
    val bitmask = bitmaskFor(Seq(0, instr0.length), code.length)
    val pp = RecompilerAbi.prepareProgram(code, bitmask, JumpTable.Empty)

    pp.opcodes(0) should not be Instruction.Panic.opcode.value
    pp.opcodes(0) shouldBe branchEqImmOp
    pp.imm(0) shouldBe -1L // compare-immediate preserved, not treated as the InvalidTarget sentinel
    pp.imm2(0) shouldBe 1L // translated target: instruction index of the Panic leader at offset 4
  }

  it should "map LoadImmAndJump with load-immediate -1 and a VALID target normally in prepareProgram (not Panic)" in {
    // Same shape as above but for LoadImmAndJump.
    val instr0 = encodeRegImmOffset(loadImmAndJumpOp, reg = 2, imm = -1L, immLen = 1, disp = Array[Byte](4))
    val code = instr0 ++ Array[Byte](0)
    val bitmask = bitmaskFor(Seq(0, instr0.length), code.length)
    val pp = RecompilerAbi.prepareProgram(code, bitmask, JumpTable.Empty)

    pp.opcodes(0) should not be Instruction.Panic.opcode.value
    pp.opcodes(0) shouldBe loadImmAndJumpOp
    pp.imm(0) shouldBe -1L // load-immediate preserved, not treated as the InvalidTarget sentinel
    pp.imm2(0) shouldBe 1L // translated target: instruction index of the Panic leader
  }

  it should "still replace BranchEqImm with Panic in prepareProgram when its target offset is INVALID" in {
    val instr0 = encodeRegImmOffset(branchEqImmOp, reg = 0, imm = -1L, immLen = 1, disp = Array[Byte](1))
    val code = instr0 ++ Array[Byte](0)
    val bitmask = bitmaskFor(Seq(0, instr0.length), code.length)
    val pp = RecompilerAbi.prepareProgram(code, bitmask, JumpTable.Empty)

    pp.opcodes(0) shouldBe Instruction.Panic.opcode.value
  }
