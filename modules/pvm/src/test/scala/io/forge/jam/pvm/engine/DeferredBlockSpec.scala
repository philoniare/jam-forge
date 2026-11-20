package io.forge.jam.pvm.engine

import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.types.ProgramCounter
import org.scalatest.funsuite.AnyFunSuite

class DeferredBlockSpec extends AnyFunSuite:

  private val n = 20_000

  private def calls(): InterpretedModule =
    val code = Array.fill[Byte](n + 1)(10)
    code(n) = 0
    InterpretedModule.createForTest(code, Array.fill[Byte]((n + 8) / 8)(0xff.toByte), true)

  private def fallthroughThenCalls(): InterpretedModule =
    val code = Array.fill[Byte](n + 2)(10)
    code(0) = 1
    code(n + 1) = 0
    InterpretedModule.createForTest(code, Array.fill[Byte]((n + 9) / 8)(0xff.toByte), true)

  private val cost: Long =
    val m = calls()
    BlockGasModel.gasCostForBlock(m.blob.code, m.blob.bitmask, 0)

  private type Seen = (Either[String, InterruptKind], Long, Option[ProgramCounter], Seq[Long])

  private def seen(inst: InterpretedInstance): Seen =
    (inst.run().left.map(_.toString), inst.gas, inst.programCounter, (0 until 13).map(inst.reg))

  private def instance(module: InterpretedModule, gas: Long, entry: Int, stepping: Boolean): InterpretedInstance =
    val inst = InterpretedInstance.fromModule(module, forceStepTracing = stepping)
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(entry))
    inst

  private def story(module: InterpretedModule, entry: Int, poor: Long, rich: Long, stepping: Boolean): Seq[Seen] =
    val first = instance(module, poor, entry, stepping)
    val out = Seq.newBuilder[Seen]
    var last = seen(first)
    out += last
    var steps = 0
    while last._1 == Right(InterruptKind.Step) && steps < 4 do
      last = seen(first)
      out += last
      steps += 1
    first.setGas(rich)
    for _ <- 1 to 4 do out += seen(first)
    val second = instance(module, rich, entry, stepping)
    for _ <- 1 to 4 do out += seen(second)
    out.result()

  private def decoded(build: () => InterpretedModule, entry: Int): InterpretedModule =
    val module = build()
    val inst = instance(module, Long.MaxValue / 4, entry, stepping = false)
    for _ <- 1 to 3 do inst.run()
    assert(module.compiled.count > n)
    module

  test("the block costs far more than the gas the poor instances bring"):
    assert(cost > 400_000L)

  test("a block the instance entering it cannot pay for is not decoded"):
    val module = calls()
    val before = module.retainedBytes
    val inst = instance(module, 1_000L, entry = 0, stepping = false)
    assert(inst.run() == Right(InterruptKind.OutOfGas))
    assert(inst.gas == 1_000L)
    assert(inst.programCounter.contains(ProgramCounter(0)))
    assert(module.compiled.count <= 2)
    assert(module.retainedBytes < before + 1_000L)

  test("nor is one entered part-way, or by a jump"):
    val entered = calls()
    assert(instance(entered, 1_000L, entry = 5, stepping = false).run() == Right(InterruptKind.OutOfGas))
    assert(entered.compiled.count <= 2)
    val jumped = fallthroughThenCalls()
    val inst = instance(jumped, 1_000L, entry = 0, stepping = false)
    assert(inst.run() == Right(InterruptKind.OutOfGas))
    assert(inst.programCounter.contains(ProgramCounter(1)))
    assert(inst.gas < 1_000L && inst.gas > 900L)
    assert(jumped.compiled.count <= 3)

  test("the first instance that can pay decodes the block, and it is decoded once"):
    val module = calls()
    assert(instance(module, 1_000L, entry = 0, stepping = false).run() == Right(InterruptKind.OutOfGas))
    val rich = instance(module, cost + 7, entry = 0, stepping = false)
    assert(rich.run().exists(_.isInstanceOf[InterruptKind.Ecalli]))
    assert(rich.gas == 7L)
    val decodedCount = module.compiled.count
    assert(decodedCount > n)
    assert(instance(module, 1_000L, entry = 0, stepping = false).run() == Right(InterruptKind.OutOfGas))
    assert(instance(module, cost, entry = 0, stepping = false).run().exists(_.isInstanceOf[InterruptKind.Ecalli]))
    assert(module.compiled.count == decodedCount)

  test("instances that can and cannot pay, entering at once from many threads, each see what they would alone"):
    import scala.concurrent.duration.*
    import scala.concurrent.{Await, ExecutionContext, Future}
    given ExecutionContext = ExecutionContext.global
    for round <- 1 to 5 do
      val module = calls()
      val outcomes = Await.result(
        Future.sequence((0 until 16).map { i =>
          Future {
            val gas = if i % 2 == 0 then 1_000L + i else cost + i
            val inst = instance(module, gas, entry = if i % 4 < 2 then 0 else 5, stepping = false)
            (i, inst.run(), inst.gas)
          }
        }),
        60.seconds
      )
      for (i, exit, gas) <- outcomes do
        if i % 2 == 0 then assert(exit == Right(InterruptKind.OutOfGas) && gas == 1_000L + i, s"round $round, poor $i")
        else assert(exit.exists(_.isInstanceOf[InterruptKind.Ecalli]) && gas == i.toLong, s"round $round, rich $i")
      assert(module.compiled.count <= n + 1 + 1 + 2)

  for
    (name, build) <- Seq[(String, () => InterpretedModule)]("one block" -> (() => calls()), "a block entered by a jump" -> (() => fallthroughThenCalls()))
    entry <- Seq(0, 5)
    stepping <- Seq(false, true)
    poor <- Seq(0L, 1L, 1_000L, cost - 1)
  do
    test(s"an instance sees the same whether the block was decoded or not: $name, entry $entry, ${if stepping then "stepping" else "running"}, $poor gas"):
      val rich = cost + 1_000L
      val expected = story(decoded(build, entry), entry, poor, rich, stepping)
      val actual = story(build(), entry, poor, rich, stepping)
      assert(actual == expected)
      assert(expected.exists(_._1 == Right(InterruptKind.OutOfGas)))
