package io.forge.jam.pvm.engine

import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.types.ProgramCounter
import org.scalatest.funsuite.AnyFunSuite

class BlockPricingBoundSpec extends AnyFunSuite:

  private def calls(n: Int): InterpretedModule =
    val code = Array.fill[Byte](n + 1)(10)
    code(n) = 0
    InterpretedModule.createForTest(code, Array.fill[Byte]((n + 8) / 8)(0xff.toByte), true)

  private def run(module: InterpretedModule, gas: Long): (InterruptKind, InterpretedInstance) =
    val inst = InterpretedInstance.fromModule(module)
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(0))
    (inst.run().fold(e => fail(s"run: $e"), identity), inst)

  private val n = 20_000
  private val cost = BlockGasModel.gasCostForBlock(calls(n).blob.code, calls(n).blob.bitmask, 0)

  test("the block costs far more than the gas the tests bring"):
    assert(cost > 400_000L)

  test("an instance that cannot pay for a block runs out of gas at it, and the block is priced no further than twice its gas"):
    val module = calls(n)
    val before = BlockGasModel.simulatedCycles
    val (exit, inst) = run(module, 1_000L)
    assert(exit == InterruptKind.OutOfGas)
    assert(inst.gas == 1_000L)
    assert(inst.programCounter.contains(ProgramCounter(0)))
    assert(BlockGasModel.simulatedCycles - before <= 2_100L)

  test("asking again with no more gas prices nothing, and with a little more prices from at least double"):
    val module = calls(n)
    run(module, 1_000L)
    val before = BlockGasModel.simulatedCycles
    for g <- Seq(1_000L, 500L, 0L) do assert(run(module, g)._1 == InterruptKind.OutOfGas)
    assert(BlockGasModel.simulatedCycles == before)
    assert(run(module, 1_001L)._1 == InterruptKind.OutOfGas)
    val afterDoubling = BlockGasModel.simulatedCycles
    assert(afterDoubling - before <= 2_100L)
    for g <- 1_002L to 2_000L by 111L do assert(run(module, g)._1 == InterruptKind.OutOfGas)
    assert(BlockGasModel.simulatedCycles == afterDoubling)

  test("an instance that can pay is charged the block's cost, whatever was asked before"):
    val module = calls(n)
    assert(run(module, 1_000L)._1 == InterruptKind.OutOfGas)
    val (exit, inst) = run(module, cost + 7)
    assert(exit.isInstanceOf[InterruptKind.Ecalli])
    assert(inst.gas == 7L)
    val before = BlockGasModel.simulatedCycles
    val (short, shortInst) = run(module, cost - 1)
    assert(short == InterruptKind.OutOfGas)
    assert(shortInst.gas == cost - 1)
    assert(BlockGasModel.simulatedCycles == before)
    val (exact, exactInst) = run(module, cost)
    assert(exact.isInstanceOf[InterruptKind.Ecalli])
    assert(exactInst.gas == 0L)

  test("a block is priced once, wherever it is entered"):
    val module = calls(n)
    val before = BlockGasModel.simulatedCycles
    val partWay = InterpretedInstance.fromModule(module)
    partWay.setGas(cost + 7)
    partWay.setNextProgramCounter(ProgramCounter(5))
    assert(partWay.run().exists(_.isInstanceOf[InterruptKind.Ecalli]))
    assert(partWay.gas == 7L)
    val priced = BlockGasModel.simulatedCycles - before
    assert(priced >= cost && priced <= cost + 10)
    assert(run(module, cost + 7)._2.gas == 7L)
    val elsewhere = InterpretedInstance.fromModule(module)
    elsewhere.setGas(cost)
    elsewhere.setNextProgramCounter(ProgramCounter(9))
    assert(elsewhere.run().exists(_.isInstanceOf[InterruptKind.Ecalli]))
    assert(elsewhere.gas == 0L)
    assert(BlockGasModel.simulatedCycles - before == priced)

  test("an instance that enters a block part-way and cannot pay runs out of gas there"):
    val module = calls(n)
    val inst = InterpretedInstance.fromModule(module)
    inst.setGas(1_000L)
    inst.setNextProgramCounter(ProgramCounter(5))
    assert(inst.run() == Right(InterruptKind.OutOfGas))
    assert(inst.gas == 1_000L)
