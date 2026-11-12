package io.forge.jam.pvm.engine

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import org.scalatest.funsuite.AnyFunSuite
import spire.math.UInt
import io.forge.jam.pvm.{InterruptKind, MemoryResult}
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

  // --- Shared module, private memory ---------------------------------------------------------------------------
  //
  // Block i: counter = mem64[rw]; counter += i + 1; mem64[rw] = counter; mem8[heap] = i; then the same conditional
  // branch as above. The read-write template starts at RwSeed, so a run that saw another run's memory (a shared
  // template or heap) ends with a different counter. Finishes with r10 = mem64[rw], r11 = mem8[heap].
  private val MemBlocks = 200
  private val RwSeed = 5
  private val HeapPages = 16

  private def buildMemoryModule(): InterpretedModule =
    val rwBase = 0x20000 // 2 * Z_Z, no read-only data
    val heapAddr = rwBase + 4096 + 8 // first declared heap page, after the one page of read-write data
    def le4(v: Int): Seq[Int] = (0 until 4).map(i => (v >>> (8 * i)) & 0xff)
    val code = Array.newBuilder[Byte]
    val starts = Array.newBuilder[Int]
    var pc = 0
    def emit(bytes: Int*): Unit =
      starts += pc
      bytes.foreach(b => code += b.toByte)
      pc += bytes.length
    for i <- 0 until MemBlocks do
      emit((58 +: 0x09 +: le4(rwBase))*) // load_u64 r9, [rw]
      emit(149, 0x99, (i + 1) & 0xff, ((i + 1) >> 8) & 0xff) // add_imm_64 r9, r9, i + 1
      emit((62 +: 0x09 +: le4(rwBase))*) // store_u64 [rw], r9
      emit(((30 +: 4 +: le4(heapAddr)) :+ (i & 0x7f))*) // store_imm_u8 [heap], i
      emit(82, 0x08, 3) // branch_ne_imm r8, 0, +3
    emit((58 +: 0x0a +: le4(rwBase))*) // load_u64 r10, [rw]
    emit((52 +: 0x0b +: le4(heapAddr))*) // load_u8 r11, [heap]
    emit(50, 0) // jump_ind r0 → halt
    val c = code.result()
    val bitmask = new Array[Byte]((c.length + 7) / 8)
    starts.result().foreach(s => bitmask(s >> 3) = (bitmask(s >> 3) | (1 << (s & 7))).toByte)
    val rw = Array[Byte](RwSeed.toByte, 0, 0, 0, 0, 0, 0, 0)
    val blob = ProgramBlob(c, bitmask, JumpTable.Empty, true, Array.empty, rw, 4096, HeapPages, rw.length)
    InterpretedModule.create(blob).fold(e => fail(e), identity)

  /** (r10, r11, gas left, exit, first untouched heap byte) of one run on a new instance. */
  private def runMemoryOnce(module: InterpretedModule): (Long, Long, Long, InterruptKind, Int) =
    val inst = InterpretedInstance.fromModule(module)
    inst.setReg(0, 0xffff0000L)
    inst.setGas(10_000_000L)
    inst.setNextProgramCounter(ProgramCounter(0))
    val result = inst.run().fold(e => fail(s"internal error: $e"), identity)
    val untouched = inst.basicMemory.loadU8(UInt(0x20000 + 4096 + 9)) match
      case MemoryResult.Success(v) => v.toInt
      case other => fail(s"heap not readable: $other")
    val out = (inst.reg(10), inst.reg(11), inst.gas, result, untouched)
    inst.basicMemory.release()
    out

  test("concurrent runs of one module match a sequential run: same registers, gas and memory"):
    val reference = runMemoryOnce(buildMemoryModule())
    assert(reference._4 == InterruptKind.Finished)
    assert(reference._1 == RwSeed + (1 to MemBlocks).sum.toLong)
    assert(reference._2 == ((MemBlocks - 1) & 0x7f).toLong)
    assert(reference._5 == 0)

    val threads = 8
    val pool = Executors.newFixedThreadPool(threads)
    given ExecutionContext = ExecutionContext.fromExecutorService(pool)
    try
      // New module each round: the runs race to compile its blocks. Then many runs on the compiled module.
      for round <- 0 until 60 do
        val module = buildMemoryModule()
        val start = new CountDownLatch(1)
        val runsPerThread = if round == 0 then 400 else 4
        val results = (0 until threads).map(_ =>
          Future { start.await(); (0 until runsPerThread).map(_ => runMemoryOnce(module)) }
        )
        start.countDown()
        Await.result(Future.sequence(results), 120.seconds).flatten.foreach(r => assert(r == reference))
        // The shared read-write template is what the blob supplied, untouched by any run.
        assert(module.rwData.toSeq == Seq[Byte](RwSeed.toByte, 0, 0, 0, 0, 0, 0, 0))
    finally
      pool.shutdown()
      pool.awaitTermination(10, TimeUnit.SECONDS)
