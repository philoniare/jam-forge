package io.forge.jam.pvm.program

/** The `deblob` validity rules.
  *
  * deblob(p, ı) is ∇ unless:
  *   - p = E(|j|) ⌢ E₁(z) ⌢ E(|c|) ⌢ E_z(j) ⌢ E(c) ⌢ E(k) for exactly one (c, k, j) ([[isExactEncoding]]);
  *   - v_blob(c, k, 0): every instruction is a known opcode and the last one is in T ([[isValidCode]]);
  *   - v_inst(c, k, ı): ı is the start of an instruction with a known opcode ([[isValidEntry]]).
  *
  * The `machine` host call (Ω_M) returns HUH when deblob(p, ı) = ∇.
  */
object Deblob:

  /** True iff `data` is exactly the encoding E(|j|) ⌢ E₁(z) ⌢ E(|c|) ⌢ E_z(j) ⌢ E(c) ⌢ E(k):
    *   - |j| and |c| use the canonical (shortest) general natural encoding E;
    *   - no octets follow the bitmask;
    *   - the padding bits of the last bitmask octet are zero (bits are packed LSB-first, data.md "Bit Sequence
    *     Encoding");
    *   - each jump-table entry is a register value (< 2^64), which only constrains z > 8.
    */
  def isExactEncoding(data: Array[Byte]): Boolean =
    readCanonicalVarint(data, 0) match
      case None => false
      case Some((jumpCount, afterJumpCount)) =>
        if afterJumpCount >= data.length then false
        else
          val z = data(afterJumpCount) & 0xff
          readCanonicalVarint(data, afterJumpCount + 1) match
            case None => false
            case Some((codeLen, afterCodeLen)) =>
              val remaining = (data.length - afterCodeLen).toLong
              // Unsigned bounds first so the products below cannot overflow.
              if java.lang.Long.compareUnsigned(codeLen, remaining) > 0 then false
              else if z != 0 && java.lang.Long.compareUnsigned(jumpCount, remaining) > 0 then false
              else
                val jumpOctets = if z == 0 then 0L else jumpCount * z
                val bitmaskLen = (codeLen + 7) / 8
                if jumpOctets + codeLen + bitmaskLen != remaining then false
                else
                  val jumpStart = afterCodeLen
                  val bitmaskStart = jumpStart + (jumpOctets + codeLen).toInt
                  jumpEntriesFitRegister(data, jumpStart, jumpCount.toInt, z) &&
                  bitmaskPaddingIsZero(data, bitmaskStart, codeLen.toInt)

  /** v_blob(c, k, 0). */
  def isValidCode(blob: ProgramBlob): Boolean =
    Program.isValidBlob(blob.code, blob.bitmask)

  /** v_inst(c, k, ı) for a 64-bit ı (a negative Long is a value ≥ 2^63, never in range). */
  def isValidEntry(blob: ProgramBlob, entry: Long): Boolean =
    entry >= 0 && entry < blob.code.length &&
      Program.isValidInstructionBoundary(blob.code, blob.bitmask, entry.toInt)

  /** deblob(p, ı) ≠ ∇: the parsed program, or None for ∇. */
  def deblob(data: Array[Byte], entry: Long): Option[ProgramBlob] =
    if !isExactEncoding(data) then None
    else
      ProgramBlob
        .fromCodeAndJumpTable(data)
        .filter(blob => isValidCode(blob) && isValidEntry(blob, entry))

  /** E⁻¹ for a general natural, rejecting non-shortest encodings (E is a function, so only its image decodes). */
  private def readCanonicalVarint(data: Array[Byte], offset: Int): Option[(Long, Int)] =
    ProgramBlob.readJamFormatVarint(data, offset).filter { case (value, next) =>
      val l = next - offset - 1
      if l == 0 then true
      else if l < 8 then value >= (1L << (7 * l))
      else java.lang.Long.compareUnsigned(value, 1L << 56) >= 0
    }

  /** For z > 8 every entry's octets beyond the eighth must be zero, else the value is ≥ 2^64. */
  private def jumpEntriesFitRegister(data: Array[Byte], start: Int, count: Int, z: Int): Boolean =
    if z <= 8 then true
    else
      var ok = true
      var e = 0
      while ok && e < count do
        var b = 8
        while ok && b < z do
          if data(start + e * z + b) != 0 then ok = false
          b += 1
        e += 1
      ok

  private def bitmaskPaddingIsZero(data: Array[Byte], bitmaskStart: Int, codeLen: Int): Boolean =
    val used = codeLen & 7
    if used == 0 then true
    else ((data(bitmaskStart + (codeLen >> 3)) & 0xff) >>> used) == 0
