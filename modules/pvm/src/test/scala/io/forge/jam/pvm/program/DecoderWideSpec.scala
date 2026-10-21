package io.forge.jam.pvm.program

import org.scalatest.funsuite.AnyFunSuite
import io.forge.jam.pvm.Instruction

/** Decoder encodings with 9 or more argument bytes, padding up to ℓ = 24, and register nibbles (GP 0.8.0 §A.5). */
class DecoderWideSpec extends AnyFunSuite:

  private def b(xs: Int*): Array[Byte] = xs.map(_.toByte).toArray

  /** Bitmask with the given instruction starts set, sized for `codeLen` bytes. */
  private def mask(codeLen: Int, starts: Int*): Array[Byte] =
    val m = new Array[Byte]((codeLen + 7) / 8)
    starts.foreach(s => m(s >> 3) = (m(s >> 3) | (1 << (s & 7))).toByte)
    m

  /** Decode the instruction at offset 0 of `code`, followed by a trap at `code.length`. */
  private def decodeOne(code: Array[Byte]): (Instruction, Int) =
    val full = code :+ 0.toByte
    InstructionDecoder.decode(full, mask(full.length, 0, code.length), 0)

  test("GP: store_imm_u32 with 4-byte address and 4-byte value (ℓ = 9)"):
    val (insn, skip) = decodeOne(b(32, 0x04, 0x00, 0x00, 0x02, 0x00, 0xDD, 0xCC, 0xBB, 0xAA))
    assert(skip == 10)
    assert(insn == Instruction.StoreImmU32(0x00020000L, 0xFFFFFFFFAABBCCDDL))

  test("GP: store_imm_u64 with 4-byte address and 4-byte value sign-extends the value"):
    val (insn, _) = decodeOne(b(33, 0x04, 0x00, 0x00, 0x02, 0x00, 0x01, 0x00, 0x00, 0x80))
    assert(insn == Instruction.StoreImmU64(0x00020000L, 0xFFFFFFFF80000001L))

  test("GP: store_imm_ind_u32 with 4-byte offset and 4-byte value (ℓ = 9)"):
    val (insn, skip) = decodeOne(b(72, 0x41, 0x00, 0x00, 0x10, 0x00, 0xDD, 0xCC, 0xBB, 0xAA))
    assert(skip == 10)
    assert(insn == Instruction.StoreImmIndirectU32(1, 0x00100000L, 0xFFFFFFFFAABBCCDDL))

  test("GP: store_imm_ind_u64 with 4-byte offset and 4-byte value"):
    val (insn, _) = decodeOne(b(73, 0x4C, 0xF0, 0xFF, 0xFF, 0xFF, 0x78, 0x56, 0x34, 0x12))
    assert(insn == Instruction.StoreImmIndirectU64(12, -16L, 0x12345678L))

  test("GP: branch_eq_imm with 4-byte immediate and 4-byte offset"):
    val (insn, _) = decodeOne(b(81, 0x44, 0x78, 0x56, 0x34, 0x12, 0x20, 0x00, 0x00, 0x01))
    assert(insn == Instruction.BranchEqImm(4, 0x12345678L, 0x01000020L))

  test("GP: branch_gt_s_imm with 4-byte immediate and a negative 4-byte offset"):
    val code = new Array[Byte](0x40) ++ b(90, 0x45, 0xFF, 0xFF, 0xFF, 0xFF, 0xF0, 0xFF, 0xFF, 0xFF, 0)
    val (insn, skip) = InstructionDecoder.decode(code, mask(code.length, 0x40, 0x4A), 0x40)
    assert(skip == 10)
    assert(insn == Instruction.BranchGreaterSignedImm(5, -1L, 0x30L))

  test("GP: load_imm_jump with 4-byte immediate and 4-byte offset"):
    val (insn, _) = decodeOne(b(80, 0x42, 0xEF, 0xBE, 0xAD, 0xDE, 0x0A, 0x00, 0x00, 0x01))
    assert(insn == Instruction.LoadImmAndJump(2, 0xFFFFFFFFDEADBEEFL, 0x0100000AL))

  test("GP: load_imm_jump_ind with two 4-byte immediates (10 argument bytes)"):
    val (insn, skip) = decodeOne(b(180, 0x21, 0x04, 0x78, 0x56, 0x34, 0x12, 0x02, 0x00, 0x01, 0x00))
    assert(skip == 11)
    assert(insn == Instruction.LoadImmAndJumpIndirect(1, 2, 0x12345678L, 0x00010002L))

  test("GP: padded add_imm_64 (ℓ = 12) ignores the bytes past its immediate"):
    val (insn, skip) = decodeOne(b(149, 0x57, 0x01, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF))
    assert(skip == 13)
    assert(insn == Instruction.AddImm64(7, 5, 1L))

  test("GP: padded ecalli and jump (ℓ = 24) read only their 4-byte field"):
    val pad = Seq.fill(20)(0xAB)
    val (ecalli, skip) = decodeOne(b(10 +: 0x05 +: 0x00 +: 0x00 +: 0x00 +: pad*))
    assert(skip == 25)
    assert(ecalli == Instruction.Ecalli(5L))
    val (jump, _) = decodeOne(b(40 +: 0x08 +: 0x00 +: 0x00 +: 0x00 +: pad*))
    assert(jump == Instruction.Jump(8L))

  test("GP: padded store_imm_ind_u8 (ℓ = 16) reads l_X from the nibble and l_Y ≤ 4"):
    val (insn, _) =
      decodeOne(b(70, 0x33, 0x10, 0x20, 0x30, 0x7F, 0x00, 0x00, 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88))
    assert(insn == Instruction.StoreImmIndirectU8(3, 0x00302010L, 0x7FL))

  test("GP: load_imm_64 register byte is taken mod 16"):
    val (insn, _) = decodeOne(b(20, 0x21, 1, 0, 0, 0, 0, 0, 0, 0x80))
    assert(insn == Instruction.LoadImm64(1, 0x8000000000000001L))

  test("GP: operands past the end of the code read as zero"):
    val code = b(72, 0x41, 0x10)
    val (insn, skip) = InstructionDecoder.decode(code, mask(code.length, 0), 0)
    assert(skip == 3)
    assert(insn == Instruction.StoreImmIndirectU32(1, 0x10L, 0L))

  test("GP: register bytes are read from ζ even when ℓ is shorter"):
    // move_reg with ℓ = 0: r_D and r_A come from ζ_{ı+1}, the next opcode byte (0x21 = store_imm_u64).
    val code = b(100, 0x21, 0x00)
    val (insn, skip) = InstructionDecoder.decode(code, mask(code.length, 0, 1), 0)
    assert(skip == 1)
    assert(insn == Instruction.MoveReg(1, 2))
