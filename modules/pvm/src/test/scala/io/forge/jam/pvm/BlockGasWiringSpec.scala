package io.forge.jam.pvm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.{ProgramBlob, JumpTable}
import io.forge.jam.pvm.types.ProgramCounter

class BlockGasWiringSpec extends AnyFlatSpec with Matchers:

  private def instanceFor(code: Array[Byte], bitmask: Array[Byte]): InterpretedInstance =
    val blob = ProgramBlob(
      code = code,
      bitmask = bitmask,
      jumpTable = JumpTable(Array.empty, 0),
      is64Bit = false,
      roData = Array.empty,
      rwData = new Array[Byte](4096),
      stackSize = 4096
    )
    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(err) => throw new RuntimeException(s"Failed to create module: $err")
    InterpretedInstance.fromModule(module)

  private val twoBlockCode = Array[Byte](51, 0, 42, 1, 0)
  private val twoBlockBitmask = Array[Byte](0x19) // bits 0, 3, 4

  "block gas wiring" should "charge each basic block once, on entry" in {
    val instance = instanceFor(twoBlockCode, twoBlockBitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(100L)

    instance.run() match
      case Right(interrupt) => interrupt shouldBe InterruptKind.Panic // trap
      case Left(err) => fail(s"Unexpected error: $err")

    // 2 (block [load_imm; fallthrough]) + 2 (block [trap]) — NOT the flat
    // per-instruction 3 of the 0.7.x model.
    instance.gas shouldBe 96L
    instance.reg(0) shouldBe 42L
  }

  it should "leave the gas counter UNCHANGED on out-of-gas (Psi no longer returns negative gas)" in {
    val instance = instanceFor(twoBlockCode, twoBlockBitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(3L)

    instance.run() match
      case Right(interrupt) => interrupt shouldBe InterruptKind.OutOfGas
      case Left(err) => fail(s"Unexpected error: $err")

    // Block 1 charged (3 - 2 = 1); block 2 costs 2 > 1 -> OOG with the
    // counter untouched at 1 (pvm.tex ~161-165: (oog, gas, bot) case).
    instance.gas shouldBe 1L
    // OOG reports at the block-entry instruction (the trap at offset 4).
    instance.programCounter.map(_.value.toInt) shouldBe Some(4)
    // gaschargedflag' = bot after OOG.
    instance.gasChargedFlag shouldBe false

    // Re-entering without more gas immediately OOGs again, still unchanged.
    instance.setNextProgramCounter(ProgramCounter(4))
    instance.run() shouldBe Right(InterruptKind.OutOfGas)
    instance.gas shouldBe 1L
  }

  it should "OOG before executing ANY instruction of an unaffordable first block" in {
    val instance = instanceFor(twoBlockCode, twoBlockBitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(1L) // first block costs 2

    instance.run() shouldBe Right(InterruptKind.OutOfGas)
    instance.gas shouldBe 1L
    instance.reg(0) shouldBe 0L // load_imm never executed
  }

  it should "re-charge on a back-jump to the start of the (same) block" in {
    val code = Array[Byte](40)
    val bitmask = Array[Byte](0x01)
    val instance = instanceFor(code, bitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(100L)

    instance.run() shouldBe Right(InterruptKind.OutOfGas)
    instance.gas shouldBe 10L
    instance.programCounter.map(_.value.toInt) shouldBe Some(0)
  }

  it should "NOT re-charge when resuming mid-block after a host call (gaschargedflag stays top)" in {
    val code = Array[Byte](10, 1, 1, 0)
    val bitmask = Array[Byte](0x0d) // bits 0, 2, 3
    val instance = instanceFor(code, bitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(150L)

    // First run: the whole [ecalli; fallthrough] block is charged on entry
    // (150 - 100 = 50), then ecalli interrupts with the host id.
    instance.run() shouldBe Right(InterruptKind.Ecalli(spire.math.UInt(1)))
    instance.gas shouldBe 50L
    // ecalli is NOT in T and the outcome is `host` -> flag stays top.
    instance.gasChargedFlag shouldBe true

    // Resume (nextProgramCounter was set to the instruction after ecalli):
    // the fallthrough runs WITHOUT re-charging its block, then the [trap]
    // block is charged (50 - 2 = 48) and traps.
    instance.run() shouldBe Right(InterruptKind.Panic)
    instance.gas shouldBe 48L
  }

  it should "keep the block's charge after a mid-block page fault (PR #497 semantics, no refund)" in {
    // [load_u8 r0 <- 0x40000000 (@0); trap (@6)]: one block, cost 25.
    // The load faults (address unmapped), but the block was already charged.
    val code = Array[Byte](52, 0, 0, 0, 0, 64, 0)
    val bitmask = Array[Byte](0x41) // bits 0, 6
    val instance = instanceFor(code, bitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(100L)

    instance.run() match
      case Right(InterruptKind.Segfault(_)) => // expected
      case other => fail(s"Expected Segfault, got $other")
    instance.gas shouldBe 75L
    // fault keeps the flag top: resuming at the same pc does NOT re-charge.
    instance.gasChargedFlag shouldBe true

    instance.setNextProgramCounter(ProgramCounter(0))
    instance.run() match
      case Right(InterruptKind.Segfault(_)) => // faults again...
      case other => fail(s"Expected Segfault, got $other")
    instance.gas shouldBe 75L // ...with NO additional charge
  }

  it should "OOG on the very next step when a self-metered host call signals forced OOG mid-block" in {
    val code = Array[Byte](10, 1, 1, 0) // [ecalli 1; fallthrough] | [trap]
    val bitmask = Array[Byte](0x0d)
    val instance = instanceFor(code, bitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(150L)

    instance.run() shouldBe Right(InterruptKind.Ecalli(spire.math.UInt(1)))
    instance.gas shouldBe 50L
    instance.forceOutOfGas()

    instance.run() shouldBe Right(InterruptKind.OutOfGas)
    instance.gas shouldBe 50L 
    instance.isForcedOutOfGas shouldBe true
    // OOG fired AT the resume pc (the fallthrough, offset 2): it never ran.
    instance.programCounter.map(_.value.toInt) shouldBe Some(2)
  }

  it should "charge zero gas when deblob prevalidation panics (Psi_1 never runs)" in {
    // Non-terminator-final program: v_blob fails -> panic at entry, before
    // any block is entered or charged (Task 3 interaction).
    val code = Array[Byte](51, 0, 42) // LoadImm alone
    val bitmask = Array[Byte](0x01)
    val instance = instanceFor(code, bitmask)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.setGas(100L)

    instance.run() shouldBe Right(InterruptKind.Panic)
    instance.gas shouldBe 100L
  }
