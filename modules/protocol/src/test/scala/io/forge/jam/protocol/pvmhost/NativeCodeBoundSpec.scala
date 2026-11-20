package io.forge.jam.protocol.pvmhost

import io.forge.jam.pvm.ExecutionMode
import io.forge.jam.pvm.engine.{InterpretedInstance, InterpretedModule}
import io.forge.jam.pvm.program.{JumpTable, ProgramBlob}
import io.forge.jam.pvm.recompiler.RecompilerAbi
import io.forge.jam.pvm.types.ProgramCounter
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class NativeCodeBoundSpec extends AnyFunSuite:

  private def canRunNative: Boolean =
    Option(System.getProperty("jam.pvm.recompiler.lib")).map(Path.of(_)).exists(Files.exists(_))

  private object NoCalls extends HostCallDispatcher:
    def getGasCost(hostCallId: Int, instance: PvmInstance): Long = 10L
    def dispatch(hostCallId: Int, instance: PvmInstance): Unit = ()

  private def moduleOf(code: Array[Byte], bitmask: Array[Byte]): InterpretedModule =
    InterpretedModule
      .create(ProgramBlob(code, bitmask, JumpTable.Empty, true, Array.empty, Array.empty, 4096))
      .fold(e => fail(s"module: $e"), identity)

  private def traps(n: Int): InterpretedModule =
    moduleOf(new Array[Byte](n), Array.fill[Byte]((n + 7) / 8)(0xff.toByte))

  private def moves(n: Int): InterpretedModule =
    val code = new Array[Byte](2 * n + 1)
    val bitmask = new Array[Byte]((code.length + 7) / 8)
    for i <- 0 until n do
      code(2 * i) = 100
      code(2 * i + 1) = 0x21
      bitmask(2 * i / 8) = (bitmask(2 * i / 8) | (1 << (2 * i % 8))).toByte
    bitmask(2 * n / 8) = (bitmask(2 * n / 8) | (1 << (2 * n % 8))).toByte
    moduleOf(code, bitmask)

  private def run(module: InterpretedModule): Option[NativeRunner.RunOutcome] =
    val inst = InterpretedInstance.fromModule(module, forceStepTracing = false)
    inst.setGas(1_000_000L)
    inst.setNextProgramCounter(ProgramCounter(0))
    NativeRunner.run(inst, entryPc = 0, ExecutionMode.Recompiled, NoCalls, preDispatch = None)

  private def refusals(reason: String): Long = NativeRunner.deoptReasons.getOrElse(reason, 0L)

  test("a blob of more instructions than the native code limit can hold runs interpreted and is never prepared"):
    assume(canRunNative, "no recompiler library on this host")
    val module = traps(RecompilerAbi.MaxNativeInstructions + 1)
    val (compiles, refused) = (NativeRunner.compileCount, refusals("too-large"))
    assert(run(module).isEmpty)
    assert(run(module).isEmpty)
    assert(NativeRunner.compileCount == compiles)
    assert(refusals("too-large") == refused + 2)
    assert(module.nativeBytes == 0L)

  test("a program whose blocks cost more to price than the limit runs interpreted"):
    assume(canRunNative, "no recompiler library on this host")
    val n = (RecompilerAbi.MaxPreparedBlockGas / 25).toInt + 1_000
    assert(n < RecompilerAbi.MaxNativeInstructions)
    val code = Array.fill[Byte](n + 1)(10)
    code(n) = 0
    val module = moduleOf(code, Array.fill[Byte]((n + 8) / 8)(0xff.toByte))
    val (compiles, refused) = (NativeRunner.compileCount, refusals("too-costly-to-price"))
    assert(run(module).isEmpty)
    assert(run(module).isEmpty)
    assert(NativeRunner.compileCount == compiles)
    assert(refusals("too-costly-to-price") == refused + 2)

  test("a compiled module weighs its machine code and its offset index"):
    assume(canRunNative, "no recompiler library on this host")
    val module = moves(20_000)
    val before = module.retainedBytes
    assert(run(module).contains(NativeRunner.RunOutcome.Panic))
    assert(module.nativeBytes > 20_001L * 30)
    assert(module.retainedBytes == before + 4L * 40_001)

  test("no program is compiled while the machine code alive in the process is at its budget, and none is refused for good"):
    assume(canRunNative, "no recompiler library on this host")
    val budget = NativeRunner.nativeCode
    val module = moves(1_000)
    val filler = budget.limitBytes
    budget.add(filler)
    try
      val (compiles, refused) = (NativeRunner.compileCount, refusals("native-code-budget"))
      assert(run(module).isEmpty)
      assert(NativeRunner.compileCount == compiles)
      assert(refusals("native-code-budget") == refused + 1)
    finally budget.remove(filler)
    assert(run(module).contains(NativeRunner.RunOutcome.Panic))
    assert(budget.liveBytes > 1_001L * 30)

  test("the budget counts what is added and removed"):
    val budget = NativeCodeBudget(100L)
    assert(budget.hasRoomFor(100L))
    budget.add(60L)
    assert(budget.liveBytes == 60L)
    assert(budget.hasRoomFor(40L))
    assert(!budget.hasRoomFor(41L))
    budget.remove(60L)
    assert(budget.hasRoomFor(100L))
