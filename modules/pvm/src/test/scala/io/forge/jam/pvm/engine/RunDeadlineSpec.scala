package io.forge.jam.pvm.engine

import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.memory.GuestRam
import io.forge.jam.pvm.types.ProgramCounter
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RunDeadlineSpec extends AnyFunSuite with Matchers:

  private val spin = InterpretedModule.createForTest(code = Array[Byte](40, 0), bitmask = Array[Byte](1))

  private def guest(gas: Long): GuestInstance =
    GuestInstance.create(spin, new GuestRam, new Array[Long](13), gas, ProgramCounter(0))

  test("a run started past its deadline throws before its first block, with its gas untouched") {
    val g = guest(50_000_000L)
    g.setDeadlineMillis(0L)
    val thrown = intercept[RunDeadlinePassed](g.run())
    thrown.deadlineMillis shouldBe 0L
    g.gas shouldBe 50_000_000L
  }

  test("a deadline that passes while a loop without host calls runs stops it, long before its gas is gone") {
    val gas = 1_000_000_000_000L // minutes of spinning
    val g = guest(gas)
    val started = System.currentTimeMillis()
    g.setDeadlineMillis(started + 100)
    intercept[RunDeadlinePassed](g.run())
    val elapsed = System.currentTimeMillis() - started
    elapsed should (be >= 100L and be < 5000L)
    g.gas should (be > 0L and be < gas)
  }

  test("with no deadline, or one not reached, a run ends as it would and uses the same gas") {
    val plain = guest(100_000L)
    plain.run() shouldBe Right(InterruptKind.OutOfGas)
    val bounded = guest(100_000L)
    bounded.setDeadlineMillis(System.currentTimeMillis() + 600_000L)
    bounded.run() shouldBe Right(InterruptKind.OutOfGas)
    bounded.gas shouldBe plain.gas
  }
