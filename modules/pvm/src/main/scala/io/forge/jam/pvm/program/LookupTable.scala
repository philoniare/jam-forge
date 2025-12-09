package io.forge.jam.pvm.program

/**
 * Lookup table for handling immediate value operations.
 *
 * Provides efficient lookup for immediate value bit patterns during instruction decoding.
 * The table is indexed by (skip, aux) pairs and returns (imm1Bits, imm1Skip, imm2Bits).
 *
 * @param table The underlying array of packed lookup entries
 */
final class LookupTable private (private val table: Array[Int]):

  /**
   * Get the unpacked values for given skip and aux parameters.
   *
   * @param skip Skip value (0-31)
   * @param aux Auxiliary value
   * @return Triple of (imm1Bits, imm1Skip, imm2Bits)
   */
  def get(skip: Int, aux: Long): (Int, Int, Int) =
    val index = getLookupIndex(skip, aux)
    unpack(table(index))

  private def getLookupIndex(skip: Int, aux: Long): Int =
    skip | (((aux.toInt) & 0x7) << 5)

  private def unpack(entry: Int): (Int, Int, Int) =
    val imm1Bits = entry & 0x3F
    val imm1Skip = (entry >> 6) & 0x3F
    val imm2Bits = (entry >> 12) & 0x3F
    (imm1Bits, imm1Skip, imm2Bits)

object LookupTable:

  /** Table with offset 1 (used for most argument parsing) */
  val Table1: LookupTable = build(1)

  /** Table with offset 2 (used for regs2imm2 parsing) */
  val Table2: LookupTable = build(2)

  /**
   * Build a lookup table with the specified offset.
   *
   * @param offset The offset to apply during table construction
   * @return New LookupTable instance
   */
  def build(offset: Int): LookupTable =
    val output = new Array[Int](256)

    var skip = 0
    while skip <= 0x1F do
      var aux = 0
      while aux <= 0x7 do
        val imm1Length = math.min(4, aux)
        val imm2Length = math.max(0, math.min(4, skip - imm1Length - offset))

        val imm1Bits = signExtendCutoffForLength(imm1Length)
        val imm2Bits = signExtendCutoffForLength(imm2Length)
        val imm1Skip = imm1Length * 8

        val index = skip | (aux << 5)
        output(index) = pack(imm1Bits, imm1Skip, imm2Bits)

        aux += 1
      skip += 1

    new LookupTable(output)

  private def pack(imm1Bits: Int, imm1Skip: Int, imm2Bits: Int): Int =
    require(imm1Bits <= 0x3F, s"imm1Bits out of range: $imm1Bits")
    require(imm1Skip <= 0x3F, s"imm1Skip out of range: $imm1Skip")
    require(imm2Bits <= 0x3F, s"imm2Bits out of range: $imm2Bits")
    imm1Bits | (imm1Skip << 6) | (imm2Bits << 12)

  private def signExtendCutoffForLength(length: Int): Int = length match
    case 0 => 32
    case 1 => 24
    case 2 => 16
    case 3 => 8
    case 4 => 0
    case _ => throw new IllegalArgumentException(s"Invalid length: $length")
