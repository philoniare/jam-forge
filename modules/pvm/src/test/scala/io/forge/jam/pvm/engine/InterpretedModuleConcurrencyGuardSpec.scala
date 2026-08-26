package io.forge.jam.pvm.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.forge.jam.pvm.program.{ProgramBlob, JumpTable}
import io.forge.jam.pvm.types.*

class InterpretedModuleConcurrencyGuardSpec extends AnyFlatSpec with Matchers:

  // LoadImm r0=42 (offsets 0..2) then Panic (offset 3) — exercises compileBlock.
  private def buildModule(): InterpretedModule =
    val blob = ProgramBlob(
      code = Array[Byte](51.toByte, 0.toByte, 42.toByte, 0.toByte),
      bitmask = Array[Byte](0x09.toByte),
      jumpTable = JumpTable(Array.empty, 0),
      is64Bit = false,
      roData = Array.empty,
      rwData = new Array[Byte](4096),
      stackSize = 4096
    )
    InterpretedModule.create(blob) match
      case Right(m)  => m
      case Left(err) => throw new RuntimeException(s"Failed to create module: $err")

  private def withDebugConcurrency[A](enabled: Boolean)(body: => A): A =
    val key = "jam.pvm.debugConcurrency"
    val old = System.getProperty(key)
    System.setProperty(key, enabled.toString)
    try body
    finally
      if old == null then System.clearProperty(key) else System.setProperty(key, old)

  "InterpretedModule debug guard" should "be a no-op when the flag is off (default)" in {
    withDebugConcurrency(false) {
      val module = buildModule()
      // Disabled: begin/end never throw and never observe each other.
      noException should be thrownBy {
        module.beginSharedMutation()
        module.beginSharedMutation()
        module.endSharedMutation()
      }
    }
  }

  it should "not false-positive on real sequential execution when enabled" in {
    withDebugConcurrency(true) {
      val module = buildModule()
      // Two instances of the same module execute one after another (the production
      // invariant). compileBlock runs under the guard for both with no overlap.
      for _ <- 0 until 2 do
        val instance = InterpretedInstance.fromModule(module)
        instance.setNextProgramCounter(ProgramCounter(0))
        instance.setGas(100L)
        instance.run() match
          case Right(_)  => instance.reg(0) shouldBe 42L
          case Left(err) => fail(s"Unexpected error: $err")
    }
  }

  it should "detect overlapping (concurrent) mutation regions when enabled" in {
    withDebugConcurrency(true) {
      val module = buildModule()
      module.beginSharedMutation()
      // A second begin without an intervening end simulates a concurrent mutator.
      an[IllegalStateException] should be thrownBy module.beginSharedMutation()
      module.endSharedMutation()
      // Once released, a fresh region is accepted again.
      noException should be thrownBy {
        module.beginSharedMutation()
        module.endSharedMutation()
      }
    }
  }
