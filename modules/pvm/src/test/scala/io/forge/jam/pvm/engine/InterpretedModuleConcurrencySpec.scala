package io.forge.jam.pvm.engine

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import org.scalatest.funsuite.AnyFunSuite
import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.program.{JumpTable, ProgramBlob}
import io.forge.jam.pvm.types.ProgramCounter

/** Instances of one module run on several threads at once and compile its blocks concurrently. */
class InterpretedModuleConcurrencySpec extends AnyFunSuite:

  private val Blocks = 300

  // Block i (7 bytes): add_imm_64 r7, r7, i, then branch_ne_imm r8, 0. After the last block, jump_ind r0 (halt).
  // Every block ends in a conditional branch on r8 = 0 whose taken and fall-through targets are both the next block,
  // so each block is compiled separately and the branch paths are exercised.
  private def buildModule(): InterpretedModule =
    val code = Array.newBuilder[Byte]
    val starts = Array.newBuilder[Int]
    var pc = 0
    def emit(bytes: Int*): Unit =
      starts += pc
      bytes.foreach(b => code += b.toByte)
      pc += bytes.length
    for i <- 0 until Blocks do
      emit(149, 0x77, i & 0xff, (i >> 8) & 0xff) // add_imm_64 r7, r7, i
      emit(82, 0x08, 3) // branch_ne_imm r8, 0, +3: the next block, as is the fall-through
    emit(50, 0) // jump_ind r0 → halt
    val c = code.result()
    val bitmask = new Array[Byte]((c.length + 7) / 8)
    starts.result().foreach(s => bitmask(s >> 3) = (bitmask(s >> 3) | (1 << (s & 7))).toByte)
    val blob = ProgramBlob(c, bitmask, JumpTable.Empty, true, Array.empty, new Array[Byte](4096), 4096)
    InterpretedModule.create(blob).fold(e => fail(e), identity)

  private def runOnce(module: InterpretedModule): (Long, Long, InterruptKind) =
    val inst = InterpretedInstance.fromModule(module)
    inst.setReg(0, 0xffff0000L)
    inst.setGas(10_000_000L)
    inst.setNextProgramCounter(ProgramCounter(0))
    val result = inst.run().fold(e => fail(s"internal error: $e"), identity)
    (inst.reg(7), inst.gas, result)

  test("instances compiling the same module's blocks concurrently all see the same program"):
    val reference = runOnce(buildModule())
    assert(reference._3 == InterruptKind.Finished)
    assert(reference._1 == (0 until Blocks).sum.toLong)

    val threads = 8
    val pool = Executors.newFixedThreadPool(threads)
    given ExecutionContext = ExecutionContext.fromExecutorService(pool)
    try
      for _ <- 0 until 50 do
        val module = buildModule()
        val start = new CountDownLatch(1)
        val results = (0 until threads).map(_ => Future { start.await(); runOnce(module) })
        start.countDown()
        Await.result(Future.sequence(results), 60.seconds).foreach(r => assert(r == reference))
    finally
      pool.shutdown()
      pool.awaitTermination(10, TimeUnit.SECONDS)
