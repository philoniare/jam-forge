package io.forge.jam.pvm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.forge.jam.pvm.engine.BlockGasModel

class BlockGasModelSpec extends AnyFlatSpec with Matchers:

  /** Build the instruction-boundary bitmask for the given instruction start offsets. */
  private def bitmaskFor(codeLen: Int, starts: Seq[Int]): Array[Byte] =
    val bm = new Array[Byte](math.max(1, (codeLen + 7) / 8))
    starts.foreach { i => bm(i >> 3) = (bm(i >> 3) | (1 << (i & 7))).toByte }
    bm

  /** Concatenate instruction byte chunks and derive the bitmask. */
  private def program(instrs: Array[Byte]*): (Array[Byte], Array[Byte]) =
    val code = instrs.flatten.toArray
    val starts = instrs.scanLeft(0)(_ + _.length).init
    (code, bitmaskFor(code.length, starts))

  private def cost(instrs: Array[Byte]*): Long =
    val (code, bm) = program(instrs*)
    BlockGasModel.gasCostForBlock(code, bm, 0)

  private val trap = Array[Byte](0)
  private val fallthrough = Array[Byte](1)
  private def add64(d: Int, s1: Int, s2: Int) = Array[Byte](200.toByte, (s1 | (s2 << 4)).toByte, d.toByte)
  private def divU64(d: Int, s1: Int, s2: Int) = Array[Byte](203.toByte, (s1 | (s2 << 4)).toByte, d.toByte)
  private def mul64(d: Int, s1: Int, s2: Int) = Array[Byte](202.toByte, (s1 | (s2 << 4)).toByte, d.toByte)
  private def moveReg(dst: Int, src: Int) = Array[Byte](100.toByte, (dst | (src << 4)).toByte)
  private def loadImm(r: Int, v: Int) = Array[Byte](51, r.toByte, v.toByte)
  private def loadIndU64(dst: Int, base: Int) = Array[Byte](130.toByte, (dst | (base << 4)).toByte)
  private def storeIndU64(src: Int, base: Int) = Array[Byte](123.toByte, (src | (base << 4)).toByte)
  private def unlikely = Array[Byte](2)
  private def ecalli(id: Int) = Array[Byte](10, id.toByte)

  // ---------------------------------------------------------------------
  // Single-terminator blocks
  // ---------------------------------------------------------------------

  "gasCostForBlock" should "cost 2 for a single trap block" in {
    cost(trap) shouldBe 2L
  }

  it should "cost 2 for a single fallthrough block" in {
    cost(fallthrough) shouldBe 2L
  }

  it should "cost 15 for a single jump block" in {
    cost(Array[Byte](40)) shouldBe 15L
  }

  it should "cost 40 for [unlikely; trap]" in {
    cost(unlikely, trap) shouldBe 40L
  }

  // ---------------------------------------------------------------------
  // Simple two-instruction blocks
  // ---------------------------------------------------------------------

  it should "cost 2 for [load_imm; fallthrough]" in {
    cost(loadImm(0, 42), fallthrough) shouldBe 2L
  }

  it should "cost 2 for [move_reg; fallthrough] (frontend-special-cased, minimum-cost block)" in {
    cost(moveReg(1, 2), fallthrough) shouldBe 2L
  }

  // ---------------------------------------------------------------------
  // ALU sequences: dependencies and decode-slot saturation
  // ---------------------------------------------------------------------

  it should "cost 3 for three serially-dependent add_64 + trap" in {
    cost(add64(1, 1, 2), add64(1, 1, 2), add64(1, 1, 2), trap) shouldBe 3L
  }

  it should "cost 6 for eight independent non-overlapping add_64 + trap (decode saturation at 2/cycle)" in {
    val adds = (0 to 7).map(k => add64(k, 11, 12))
    cost(adds :+ trap*) shouldBe 6L
  }

  it should "cost 4 for eight independent OVERLAPPING add_64 + trap (decode saturation at 4/cycle)" in {
    val adds = (0 to 7).map(k => add64(k, k, 12))
    cost(adds :+ trap*) shouldBe 4L
  }

  // ---------------------------------------------------------------------
  // Execution-unit contention
  // ---------------------------------------------------------------------

  it should "cost 120 for two independent div_u_64 + trap (single DIV unit serializes)" in {
    cost(divU64(1, 2, 3), divU64(4, 5, 6), trap) shouldBe 120L
  }

  it should "cost 60 for div_u_64 + mul_64 + trap (DIV and MUL units run in parallel)" in {
    cost(divU64(1, 2, 3), mul64(4, 5, 6), trap) shouldBe 60L
  }

  it should "cost 25 for load_ind_u64 + store_ind_u64 + trap (LOAD/STORE units in parallel)" in {
    cost(loadIndU64(1, 2), storeIndU64(3, 4), trap) shouldBe 25L
  }

  it should "cost 1 for a branch whose TAKEN target byte is trap" in {
    val (code, bm) = program(Array[Byte](81.toByte, 0x10, 5, 4), trap)
    BlockGasModel.gasCostForBlock(code, bm, 0) shouldBe 1L
  }

  it should "cost 1 for a branch whose target byte is unlikely" in {
    val (code, bm) = program(Array[Byte](81.toByte, 0x10, 5, 4), unlikely, trap)
    BlockGasModel.gasCostForBlock(code, bm, 0) shouldBe 1L
  }

  it should "cost 20 for a branch with no unlikely/trap target" in {
    val (code, bm) = program(Array[Byte](81.toByte, 0x10, 5, 4), add64(1, 2, 3), trap)
    BlockGasModel.gasCostForBlock(code, bm, 0) shouldBe 20L
  }

  it should "treat an out-of-bounds branch target as a zero (trap) byte -> b = 1" in {
    val (code, bm) = program(Array[Byte](81.toByte, 0x10, 5, 100), add64(1, 2, 3), trap)
    BlockGasModel.gasCostForBlock(code, bm, 0) shouldBe 1L
  }

  it should "propagate a dependency through move_reg renaming (cost 61)" in {
    cost(divU64(1, 2, 3), moveReg(4, 1), add64(5, 4, 6), trap) shouldBe 61L
  }

  it should "clear a stale clobber through move_reg renaming (cost 60)" in {
    cost(divU64(4, 2, 3), moveReg(4, 9), add64(5, 4, 6), trap) shouldBe 60L
  }

  // ---------------------------------------------------------------------
  // ecalli
  // ---------------------------------------------------------------------

  it should "cost 100 for [ecalli; fallthrough]" in {
    cost(ecalli(1), fallthrough) shouldBe 100L
  }

  // ---------------------------------------------------------------------
  // Block starts other than 0
  // ---------------------------------------------------------------------

  it should "cost a mid-program block from its own start (not offset 0)" in {
    val (code, bm) = program(trap, Array[Byte](40))
    BlockGasModel.gasCostForBlock(code, bm, 0) shouldBe 2L
    BlockGasModel.gasCostForBlock(code, bm, 1) shouldBe 15L
  }
