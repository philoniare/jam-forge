package io.forge.jam.pvm.program

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** deblob: exact encoding E(|j|) ⌢ E₁(z) ⌢ E(|c|) ⌢ E_z(j) ⌢ E(c) ⌢ E(k), v_blob and v_inst. */
class DeblobSpec extends AnyFunSuite with Matchers:

  private def b(xs: Int*): Array[Byte] = xs.map(_.toByte).toArray

  // load_imm r7, 42 ; trap — instructions at 0 and 3.
  private val twoInstr = b(0, 0, 4, 51, 7, 42, 0, 0x09)

  test("a well-formed program decodes at every instruction start and nowhere else") {
    Deblob.deblob(twoInstr, 0L) shouldBe defined
    Deblob.deblob(twoInstr, 3L) shouldBe defined
    for i <- Seq(1L, 2L, 4L, 1L << 31, 1L << 32, -1L) do
      withClue(s"i=$i: ")(Deblob.deblob(twoInstr, i) shouldBe None)
  }

  test("exact encoding rejects trailing octets, padding bits and non-shortest varints") {
    Deblob.isExactEncoding(twoInstr) shouldBe true
    Deblob.isExactEncoding(twoInstr :+ 0.toByte) shouldBe false
    Deblob.isExactEncoding(twoInstr.init :+ 0x19.toByte) shouldBe false // bit 4 is padding (|c| = 4)
    Deblob.isExactEncoding(twoInstr.init) shouldBe false // truncated
    // |j| = 0 as [0x80, 0x00] instead of [0x00].
    Deblob.isExactEncoding(b(0x80, 0) ++ twoInstr.drop(1)) shouldBe false
    // |c| = 4 as [0xff, 4, 0, 0, 0, 0, 0, 0, 0] (only values >= 2^56 use the 0xff prefix).
    Deblob.isExactEncoding(b(0, 0, 0xff, 4, 0, 0, 0, 0, 0, 0, 0) ++ twoInstr.drop(3)) shouldBe false
  }

  test("a full bitmask octet has no padding bits") {
    // 8 × trap, bitmask 0xff.
    Deblob.isExactEncoding(b(0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0xff)) shouldBe true
  }

  test("jump table: any z is framed, but entries must be register values (< 2^64)") {
    // |j| = 1, z = 1, j = [0], code = [trap].
    Deblob.isExactEncoding(b(1, 1, 1, 0, 0, 1)) shouldBe true
    // z = 0: |j| entries of zero octets each.
    Deblob.isExactEncoding(b(5, 0, 1, 0, 1)) shouldBe true
    // z = 9 with the ninth octet zero is fine; non-zero makes j_0 >= 2^64.
    Deblob.isExactEncoding(b(1, 9, 1) ++ b(1, 0, 0, 0, 0, 0, 0, 0, 0) ++ b(0, 1)) shouldBe true
    Deblob.isExactEncoding(b(1, 9, 1) ++ b(0, 0, 0, 0, 0, 0, 0, 0, 1) ++ b(0, 1)) shouldBe false
  }

  test("v_blob: the last instruction must be in T and every opcode in U") {
    Deblob.deblob(b(0, 0, 3, 51, 7, 42, 1), 0L) shouldBe None // ends in load_imm
    Deblob.deblob(b(0, 0, 2, 3, 0, 3), 0L) shouldBe None // opcode 3 unknown
    Deblob.deblob(b(0, 0, 0), 0L) shouldBe None // empty code
    Deblob.deblob(b(0, 0, 1, 0, 1), 0L) shouldBe defined // [trap]
  }
