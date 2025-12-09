package io.forge.jam.pvm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.forge.jam.pvm.program.*
import spire.math.{UByte, UInt, ULong}

/**
 * Tests for Program Binary Parsing (Task Group 3).
 *
 * These tests verify:
 * 1. Varint decoding for small and large values
 * 2. PVM blob header parsing
 * 3. Jump table extraction
 * 4. Bitmask parsing for basic block boundaries
 * 5. Instruction boundary detection
 */
class ProgramParsingSpec extends AnyFlatSpec with Matchers:

  // Test 1: Varint decoding for small and large values
  "Varint" should "decode small values correctly" in {
    // Zero-length varint returns 0
    Varint.readSimpleVarint(0x000000ffL, 0) shouldBe 0L
    Varint.readSimpleVarint(0x555555ffL, 0) shouldBe 0L
    Varint.readSimpleVarint(0xaaaaaaffL, 0) shouldBe 0L
    Varint.readSimpleVarint(0xffffffffL, 0) shouldBe 0L

    // Length 1 with high bit set -> sign extends to all 1s
    Varint.readSimpleVarint(0x000000ffL, 1) shouldBe 0xffffffffL
    Varint.readSimpleVarint(0x555555ffL, 1) shouldBe 0xffffffffL
    Varint.readSimpleVarint(0xaaaaaaffL, 1) shouldBe 0xffffffffL
    Varint.readSimpleVarint(0xffffffffL, 1) shouldBe 0xffffffffL

    // Small positive value (no sign extension)
    Varint.readSimpleVarint(0x00000042L, 1) shouldBe 0x42L
  }

  it should "decode large values correctly" in {
    // Length 2: 16-bit value sign extended
    Varint.readSimpleVarint(0x00001234L, 2) shouldBe 0x1234L
    Varint.readSimpleVarint(0x0000ff00L, 2) shouldBe 0xffffff00L // Negative when sign extended

    // Length 3: 24-bit value sign extended
    Varint.readSimpleVarint(0x00123456L, 3) shouldBe 0x123456L

    // Length 4: full 32-bit value (no extension needed)
    Varint.readSimpleVarint(0x12345678L, 4) shouldBe 0x12345678L
    Varint.readSimpleVarint(0x80000000L, 4) shouldBe 0x80000000L
  }

  it should "determine bytes required for value" in {
    Varint.getBytesRequired(0L) shouldBe 0
    Varint.getBytesRequired(1L) shouldBe 1
    Varint.getBytesRequired(0x7fL) shouldBe 1
    Varint.getBytesRequired(0x80L) shouldBe 2
    Varint.getBytesRequired(0x7fffL) shouldBe 2
    Varint.getBytesRequired(0x8000L) shouldBe 3
    Varint.getBytesRequired(0x7fffffL) shouldBe 3
    Varint.getBytesRequired(0x800000L) shouldBe 4
    Varint.getBytesRequired(0xffffffffL) shouldBe 1 // All 1s needs only 1 byte
  }

  // Test 2: PVM blob header parsing
  "ProgramBlob" should "parse valid header correctly" in {
    // PVM blob format:
    // - version (1 byte): 0 = 32-bit, 1 = 64-bit
    // - ro_data_size (varint)
    // - rw_data_size (varint)
    // - stack_size (varint)
    // - code_size (varint)
    // - jump_table_entry_count (varint)
    // - [jump_table_entry_size (varint) if entry_count > 0]
    // - code section
    // - jump table
    // - bitmask
    // - ro_data
    // - rw_data
    val minimalBlob = Array[Byte](
      0,  // version = 0 (32-bit)
      0,  // ro_data_size = 0
      0,  // rw_data_size = 0
      0,  // stack_size = 0
      0,  // code_size = 0
      0,  // jump_table_entry_count = 0 (no jump table entries, so no entry_size varint)
    )

    val result = ProgramBlob.parse(minimalBlob)
    result.isDefined shouldBe true
    result.foreach { blob =>
      blob.is64Bit shouldBe false
      blob.code.length shouldBe 0
    }
  }

  it should "reject invalid blobs" in {
    // Empty blob
    ProgramBlob.parse(Array.emptyByteArray) shouldBe None

    // Blob too short (missing required fields)
    ProgramBlob.parse(Array[Byte](0)) shouldBe None
    ProgramBlob.parse(Array[Byte](0, 0)) shouldBe None
    ProgramBlob.parse(Array[Byte](0, 0, 0)) shouldBe None
    ProgramBlob.parse(Array[Byte](0, 0, 0, 0)) shouldBe None
    ProgramBlob.parse(Array[Byte](0, 0, 0, 0, 0)) shouldBe None
  }

  it should "parse 64-bit program header" in {
    val blob64 = Array[Byte](
      1,  // version = 1 (64-bit)
      0,  // ro_data_size = 0
      0,  // rw_data_size = 0
      0,  // stack_size = 0
      0,  // code_size = 0
      0,  // jump_table_entry_count = 0
    )

    val result = ProgramBlob.parse(blob64)
    result.isDefined shouldBe true
    result.foreach { blob =>
      blob.is64Bit shouldBe true
    }
  }

  // Test 3: Jump table extraction
  "JumpTable" should "extract entries correctly" in {
    // Jump table with 2-byte entries
    val jumpTableData = Array[Byte](
      0x10, 0x00,  // Entry 0: 0x0010
      0x20, 0x00,  // Entry 1: 0x0020
      0x30, 0x00   // Entry 2: 0x0030
    )

    val jumpTable = JumpTable(jumpTableData, 2)
    jumpTable.len shouldBe 3
    jumpTable.getByIndex(0) shouldBe Some(0x0010)
    jumpTable.getByIndex(1) shouldBe Some(0x0020)
    jumpTable.getByIndex(2) shouldBe Some(0x0030)
    jumpTable.getByIndex(3) shouldBe None
  }

  it should "handle different entry sizes" in {
    // 1-byte entries
    val oneByteTable = JumpTable(Array[Byte](0x01, 0x02, 0x03), 1)
    oneByteTable.len shouldBe 3
    oneByteTable.getByIndex(0) shouldBe Some(0x01)

    // 3-byte entries
    val threeByteData = Array[Byte](
      0x01, 0x02, 0x03,
      0x04, 0x05, 0x06
    )
    val threeByteTable = JumpTable(threeByteData, 3)
    threeByteTable.len shouldBe 2
    threeByteTable.getByIndex(0) shouldBe Some(0x030201)
    threeByteTable.getByIndex(1) shouldBe Some(0x060504)
  }

  it should "handle empty jump table" in {
    val emptyTable = JumpTable(Array.emptyByteArray, 0)
    emptyTable.len shouldBe 0
    emptyTable.isEmpty shouldBe true
    emptyTable.getByIndex(0) shouldBe None
  }

  // Test 4: Bitmask parsing for basic block boundaries
  "Program.getBitForOffset" should "check bit correctly" in {
    // Bitmask: 0b10101010 0b11001100
    val bitmask = Array[Byte](0xAA.toByte, 0xCC.toByte)
    val codeLen = 16

    // Byte 0 = 0b10101010: bits 1,3,5,7 are set
    Program.getBitForOffset(bitmask, codeLen, 0) shouldBe false
    Program.getBitForOffset(bitmask, codeLen, 1) shouldBe true
    Program.getBitForOffset(bitmask, codeLen, 2) shouldBe false
    Program.getBitForOffset(bitmask, codeLen, 3) shouldBe true

    // Byte 1 = 0b11001100: bits 2,3,6,7 are set (offset 8+)
    Program.getBitForOffset(bitmask, codeLen, 8) shouldBe false
    Program.getBitForOffset(bitmask, codeLen, 10) shouldBe true
    Program.getBitForOffset(bitmask, codeLen, 11) shouldBe true

    // Out of bounds
    Program.getBitForOffset(bitmask, codeLen, 20) shouldBe false
  }

  "Program.parseBitmaskFast" should "find next instruction boundary" in {
    // Bitmask where instruction starts at offset 0, next at offset 3
    // 0b00001001 = bits 0 and 3 set
    val bitmask = Array[Byte](0x09, 0x00, 0x00, 0x00)

    val skip = Program.parseBitmaskFast(bitmask, 0)
    skip shouldBe Some(2) // Skip 2 bytes to get to offset 3 (next instruction)
  }

  "Program.parseBitmaskSlow" should "handle edge cases" in {
    val bitmask = Array[Byte](0x09, 0x00)
    val codeLength = 8

    val (skip, isNextInvalid) = Program.parseBitmaskSlow(bitmask, codeLength, 0)
    skip shouldBe 2 // Skip to next instruction at offset 3
  }

  // Test 5: Instruction boundary detection
  "Program.isJumpTargetValid" should "validate jump targets at basic block boundaries" in {
    // Create code with instructions that start new basic blocks
    // Let's say we have: [Jump(0), ?, ?, LoadImm(3), ...]
    // Offset 0: Jump (starts new block)
    // Offset 3: LoadImm (can be jump target only if preceded by block-ending instruction)

    // Code with Jump at offset 0 (opcode 40), then padding, then LoadImm
    val code = Array[Byte](
      40.toByte,   // offset 0: Jump opcode
      0, 0,        // offset 1-2: instruction args
      51.toByte,   // offset 3: LoadImm opcode (valid jump target after Jump)
      0, 0         // offset 4-5: args
    )

    // Bitmask: instructions at offsets 0 and 3
    // 0b00001001 = bits 0 and 3 set
    val bitmask = Array[Byte](0x09)

    // Offset 0 is always valid
    Program.isJumpTargetValid(code, bitmask, 0) shouldBe true

    // Offset 3 is valid because it follows a Jump (which starts new block)
    Program.isJumpTargetValid(code, bitmask, 3) shouldBe true

    // Offset 1 is not a valid instruction boundary
    Program.isJumpTargetValid(code, bitmask, 1) shouldBe false
  }

  "Program.findStartOfBasicBlock" should "trace back to block start" in {
    // Code: Add32, Add32, Jump
    // Block starts at offset 0, ends at Jump
    val code = Array[Byte](
      190.toByte,  // offset 0: Add32
      0, 0, 0,     // args
      190.toByte,  // offset 4: Add32
      0, 0, 0,     // args
      40.toByte    // offset 8: Jump (ends block)
    )

    // Instructions at offsets 0, 4, 8
    // 0b00010001 (byte 0 = offsets 0,4) and 0b00000001 (byte 1 = offset 8)
    val bitmask = Array[Byte](0x11, 0x01)

    // From offset 4, should trace back to 0 (start of basic block)
    Program.findStartOfBasicBlock(code, bitmask, 4) shouldBe Some(0)

    // From offset 0, should return 0
    Program.findStartOfBasicBlock(code, bitmask, 0) shouldBe Some(0)
  }

  "Program.getPreviousInstructionSkip" should "find previous instruction offset" in {
    // Instructions at offsets 0, 3, 7
    // Byte 0: 0b00001001 (bits 0, 3)
    // Byte 1: 0b10000000 (bit 7)
    val bitmask = Array[Byte](0x09.toByte, 0x80.toByte, 0, 0)

    // From offset 3, previous instruction was at offset 0, skip = 2
    Program.getPreviousInstructionSkip(bitmask, 3) shouldBe Some(2)

    // From offset 7, previous instruction was at offset 3, skip = 3
    Program.getPreviousInstructionSkip(bitmask, 7) shouldBe Some(3)
  }
