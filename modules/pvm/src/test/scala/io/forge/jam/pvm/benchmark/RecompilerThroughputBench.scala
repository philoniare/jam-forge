package io.forge.jam.pvm.benchmark

import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.native_.PvmRecompiler
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.recompiler.{RecompilerAbi, RecompilerMemory}
import io.forge.jam.pvm.types.ProgramCounter
import io.forge.jam.pvm.PvmTestCase

import io.circe.parser.decode

import java.io.File
import java.nio.file.{Files, Path}
import scala.io.Source

object RecompilerThroughputBench:
  private def requireAarch64Host(): Unit =
    val arch = System.getProperty("os.arch", "").toLowerCase
    if arch != "aarch64" && arch != "arm64" then
      sys.error(s"RecompilerThroughputBench requires an aarch64 host (recompiler backend is " +
        s"AArch64-only through Phase 4); host os.arch=$arch. Refusing to bench nothing silently.")

  private def requireLibPath(): Path =
    Option(System.getProperty("jam.pvm.recompiler.lib")).map(Path.of(_)).filter(Files.exists(_)) match
      case Some(p) => p
      case None => sys.error("RecompilerThroughputBench requires -Djam.pvm.recompiler.lib pointing at the " +
        "built recompiler dylib (wired into pvm/Test/javaOptions — run via `sbt pvm/Test/runMain`, not a bare " +
        "`scala` invocation). Refusing to bench nothing silently.")

  private def intLE(v: Int): Array[Byte] = Array.tabulate(4)(i => ((v >>> (i * 8)) & 0xff).toByte)
  private def regByte(a: Int, b: Int): Byte = ((a & 0xf) | ((b & 0xf) << 4)).toByte
  private def containerize(code: Array[Byte], bitmask: Array[Byte]): Array[Byte] =
    require(code.length < 128, s"code length ${code.length} needs a multi-byte varint (unsupported by this helper)")
    val blob = Array.newBuilder[Byte]
    blob += 0.toByte // jump table entry count
    blob += 0.toByte // jump table entry size
    blob += code.length.toByte // code length
    blob ++= code
    blob ++= bitmask
    blob.result()

  /** Bitmask with one bit set at each instruction-start offset in `starts`. */
  private def bitmaskFor(codeLen: Int, starts: Seq[Int]): Array[Byte] =
    val bitmask = new Array[Byte]((codeLen + 7) / 8)
    starts.foreach(off => bitmask(off >> 3) = (bitmask(off >> 3) | (1 << (off & 7))).toByte)
    bitmask

  // ==========================================================================
  // Workload 1: Pure-ALU hot loop (byte-identical shape to PvmThroughputBench)
  // ==========================================================================

  private val OpAddImm64 = 149
  private val OpJump = 40

  private def buildAluLoop(adds: Int): Array[Byte] =
    val code = Array.newBuilder[Byte]
    for _ <- 0 until adds do code ++= Array[Byte](OpAddImm64.toByte, 0x77, 0x01)
    val jumpOffset = adds * 3
    require(jumpOffset <= 127, "jump offset must fit signed byte")
    code ++= Array[Byte](OpJump.toByte, (-jumpOffset).toByte)
    val c = code.result()
    val bitmask = new Array[Byte]((c.length + 7) / 8)
    var off = 0
    while off < c.length do
      bitmask(off >> 3) = (bitmask(off >> 3) | (1 << (off & 7))).toByte
      off += (if (c(off) & 0xff) == OpJump then 2 else 3)
    containerize(c, bitmask)

  // ==========================================================================
  // Workload 2: Branchy compare loop
  // ==========================================================================
  private val OpSetLessThanUnsignedImm = 136
  private val OpShiftLogicalRightImm64 = 152
  private val OpBranchNotEq = 171
  private val Bound = 1 << 20

  private def buildBranchyLoop(): Array[Byte] =
    // offsets: 0:SetLessThanUnsignedImm(6) 6:ShiftLogicalRightImm64(6) 12:AddImm64(3) 15:BranchNotEq(6) 21:Jump(2)
    val setLt = Array[Byte](OpSetLessThanUnsignedImm.toByte, regByte(9, 7)) ++ intLE(Bound)
    val shr = Array[Byte](OpShiftLogicalRightImm64.toByte, regByte(8, 8)) ++ intLE(1)
    val addImm = Array[Byte](OpAddImm64.toByte, regByte(7, 7), 0x01)
    val branchOff = 15
    val branchTarget = 0 - branchOff // relative disp back to instruction 0
    val branch = Array[Byte](OpBranchNotEq.toByte, regByte(9, 0)) ++ intLE(branchTarget)
    val jumpOff = 21
    val jumpBack = Array[Byte](OpJump.toByte) ++ intLE(0 - jumpOff)
    val code = setLt ++ shr ++ addImm ++ branch ++ jumpBack
    val bitmask = bitmaskFor(code.length, Seq(0, 6, 12, 15, 21))
    containerize(code, bitmask)

  // ==========================================================================
  // Workload 3: Memory-traffic loop (indirect loads/stores, page-wrapped)
  // ==========================================================================
  private val OpStoreIndirectU32 = 122
  private val OpLoadIndirectU32 = 128
  private val OpAndImm = 132

  private def buildMemoryLoop(): Array[Byte] =
    val store = Array[Byte](OpStoreIndirectU32.toByte, regByte(7, 6)) ++ intLE(0)
    val load = Array[Byte](OpLoadIndirectU32.toByte, regByte(8, 6)) ++ intLE(0)
    val addImm = Array[Byte](OpAddImm64.toByte, regByte(7, 7), 0x04)
    val andImm = Array[Byte](OpAndImm.toByte, regByte(7, 7)) ++ intLE(0xFFF)
    val jumpOff = 6 + 6 + 3 + 6
    val jumpBack = Array[Byte](OpJump.toByte) ++ intLE(0 - jumpOff)
    val code = store ++ load ++ addImm ++ andImm ++ jumpBack
    val bitmask = bitmaskFor(code.length, Seq(0, 6, 12, 15, 21))
    containerize(code, bitmask)

  // ==========================================================================
  // Workload 4: Div/mul-upper loop (uop-expensive ops)
  // ==========================================================================
  private val OpDivUnsigned64 = 203
  private val OpRemUnsigned64 = 205
  private val OpMulUpperUnsignedUnsigned = 214

  private def buildDivMulLoop(): Array[Byte] =
    val div = Array[Byte](OpDivUnsigned64.toByte, regByte(7, 8), 9.toByte)
    val rem = Array[Byte](OpRemUnsigned64.toByte, regByte(7, 8), 10.toByte)
    val mulUpper = Array[Byte](OpMulUpperUnsignedUnsigned.toByte, regByte(7, 8), 11.toByte)
    val addImm = Array[Byte](OpAddImm64.toByte, regByte(7, 7), 0x01)
    val jumpOff = 3 + 3 + 3 + 3
    val jumpBack = Array[Byte](OpJump.toByte) ++ intLE(0 - jumpOff)
    val code = div ++ rem ++ mulUpper ++ addImm ++ jumpBack
    val bitmask = bitmaskFor(code.length, Seq(0, 3, 6, 9, 12))
    containerize(code, bitmask)

  // ==========================================================================
  // Engine drivers
  // ==========================================================================

  private final case class InterpOutcome(exitKind: String, gasRemaining: Long, regs: Array[Long], pc: Int, seconds: Double)
  private def runInterpreterTimed(module: InterpretedModule, initRegs: Array[Long], gasBudget: Long): InterpOutcome =
    val instance = InterpretedInstance.fromModule(module, forceStepTracing = false)
    instance.setGas(gasBudget)
    instance.setNextProgramCounter(ProgramCounter(0))
    initRegs.zipWithIndex.foreach { case (v, i) => instance.setReg(i, v) }
    val t0 = System.nanoTime()
    var kind = ""
    var running = true
    while running do
      instance.run() match
        case Right(InterruptKind.OutOfGas) => kind = "OutOfGas"; running = false
        case Right(InterruptKind.Finished) => kind = "Finished"; running = false
        case Right(InterruptKind.Panic) => kind = "Panic"; running = false
        case Right(InterruptKind.Step) => ()
        case Right(other) => sys.error(s"unexpected interrupt: $other")
        case Left(err) => sys.error(s"interpreter execution error: $err")
    val t1 = System.nanoTime()
    val regs = Array.tabulate(13)(instance.reg)
    val pc = instance.programCounter.map(_.toInt).getOrElse(-1)
    InterpOutcome(kind, instance.gas, regs, pc, (t1 - t0) / 1e9)

  private final case class RecompOutcome(exit: Int, gasRemaining: Long, regs: Array[Long], pc: Long, seconds: Double)

  /** Runs a compiled `block` against a fresh region/backing snapshot derived
    * from a FRESH interpreter instance's pre-run memory (same convention as
    * `VectorConformanceSpec.runVector`), timing only `rc.execute`. */
  private def runRecompilerTimed(
    rc: PvmRecompiler, block: PvmRecompiler#Block, module: InterpretedModule,
    initRegs: Array[Long], gasBudget: Long
  ): RecompOutcome =
    val freshInstance = InterpretedInstance.fromModule(module, forceStepTracing = false)
    freshInstance.setGas(gasBudget)
    freshInstance.setNextProgramCounter(ProgramCounter(0))
    initRegs.zipWithIndex.foreach { case (v, i) => freshInstance.setReg(i, v) }
    val described = RecompilerMemory.describe(freshInstance)
    val regs = initRegs.clone()
    val backing = described.backing.clone()
    val regions = described.regions.map(r => new PvmRecompiler.Region(r.base, r.len, r.bufOffset, r.writable))
    val t0 = System.nanoTime()
    val out = rc.execute(block, regs, gasBudget, regions, backing, described.pageShift, 0)
    val t1 = System.nanoTime()
    RecompOutcome(out.exit, out.gasRemaining, regs, out.pc, (t1 - t0) / 1e9)

  private def median(xs: Seq[Double]): Double =
    val sorted = xs.sorted
    val n = sorted.length
    if n % 2 == 1 then sorted(n / 2) else (sorted(n / 2 - 1) + sorted(n / 2)) / 2.0

  private def assertEngineParity(label: String, interp: InterpOutcome, recomp: RecompOutcome): Unit =
    val interpExitAsExit = interp.exitKind match
      case "Finished" | "Halt" => PvmRecompiler.EXIT_HALT
      case "Panic" => PvmRecompiler.EXIT_PANIC
      case "OutOfGas" => PvmRecompiler.EXIT_OOG
      case "Fault" => PvmRecompiler.EXIT_FAULT
      case other => sys.error(s"[$label] unexpected interpreter exit kind: $other")
    if interpExitAsExit != recomp.exit then
      sys.error(s"[$label] engine DIVERGED on exit — interpreter=${interp.exitKind} recompiler=${recomp.exit}")
    if interp.gasRemaining != recomp.gasRemaining then
      sys.error(s"[$label] engine DIVERGED on gas — interpreter=${interp.gasRemaining} recompiler=${recomp.gasRemaining}")
    if (interp.pc.toLong & 0xFFFFFFFFL) != recomp.pc then
      sys.error(s"[$label] engine DIVERGED on pc — interpreter=${interp.pc} recompiler=${recomp.pc}")
    interp.regs.indices.foreach { r =>
      if interp.regs(r) != recomp.regs(r) then
        sys.error(s"[$label] engine DIVERGED on reg $r — interpreter=${interp.regs(r)} recompiler=${recomp.regs(r)}")
    }

  // ==========================================================================
  // Loop-workload driver: build blob, compile once, warm up, time both
  // engines, assert result equality on EVERY accepted timing.
  // ==========================================================================

  private final case class LoopResult(
    name: String, interpMips: Double, recompMips: Double, speedup: Double,
    compileSeconds: Double
  )

  private def runLoopWorkload(
    rc: PvmRecompiler, name: String, code: Array[Byte], initRegs: Array[Long], gasBudget: Long,
    rwDataLen: Int = 0, warmupRuns: Int = 3, timedRuns: Int = 5
  ): LoopResult =
    val blob = ProgramBlob.fromCodeAndJumpTable(
      data = code, rwData = new Array[Byte](rwDataLen), stackSize = 4096, is64Bit = true
    ).getOrElse(sys.error(s"[$name] failed to parse hand-assembled program blob"))
    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(e) => sys.error(s"[$name] module create failed: $e")

    val prepared = RecompilerAbi.prepareProgram(blob)
    val tCompile0 = System.nanoTime()
    val block = rc.compile(prepared.opcodes, prepared.a, prepared.b, prepared.c, prepared.pc,
      prepared.imm, prepared.imm2, prepared.blockGas, prepared.jumpTable, prepared.codeLen)
    val tCompile1 = System.nanoTime()
    val compileSeconds = (tCompile1 - tCompile0) / 1e9
    try
      if !block.isValid then
        sys.error(s"[$name] pvm_compile returned an invalid block (unsupported opcode) — cannot bench")

      // ---- warmup (both engines; discarded) ----
      for _ <- 1 to warmupRuns do
        runInterpreterTimed(module, initRegs, math.min(gasBudget, 5_000_000L))
        runRecompilerTimed(rc, block, module, initRegs, math.min(gasBudget, 5_000_000L))

      // ---- timed runs, asserting result equality BEFORE accepting each timing ----
      val interpSeconds = scala.collection.mutable.ArrayBuffer.empty[Double]
      val recompSeconds = scala.collection.mutable.ArrayBuffer.empty[Double]
      for i <- 1 to timedRuns do
        val interp = runInterpreterTimed(module, initRegs, gasBudget)
        val recomp = runRecompilerTimed(rc, block, module, initRegs, gasBudget)

        assertEngineParity(s"$name run $i", interp, recomp)

        interpSeconds += interp.seconds
        recompSeconds += recomp.seconds

      val interpMedian = median(interpSeconds.toSeq)
      val recompMedian = median(recompSeconds.toSeq)
      val interpMips = gasBudget / interpMedian / 1e6
      val recompMips = gasBudget / recompMedian / 1e6
      println(f"  $name%-24s interp: $interpMedian%.4f s ($interpMips%.1f MIPS)   " +
        f"recompiler: $recompMedian%.4f s ($recompMips%.1f MIPS)   compile: ${compileSeconds * 1000}%.2f ms")
      LoopResult(name, interpMips, recompMips, recompMips / interpMips, compileSeconds)
    finally
      block.close()

  // ==========================================================================
  // Workload 5: vector-corpus replay
  // ==========================================================================

  private def loadTestCase(file: File): PvmTestCase =
    val content = Source.fromFile(file).mkString
    decode[PvmTestCase](content) match
      case Right(tc) => tc
      case Left(err) => sys.error(s"failed to parse ${file.getName}: $err")

  private def buildMemoryRegion(pages: List[io.forge.jam.pvm.PageMapEntry], memory: List[io.forge.jam.pvm.MemoryEntry]): Array[Byte] =
    if pages.isEmpty then Array.empty
    else
      val firstPageAddr = pages.map(_.address).min
      val totalSize = pages.map(p => p.address + p.length - firstPageAddr).max.toInt
      val data = new Array[Byte](totalSize)
      memory.foreach { mem =>
        pages.find(p => mem.address >= p.address && mem.address < p.address + p.length).foreach { _ =>
          val offset = (mem.address - firstPageAddr).toInt
          System.arraycopy(mem.contents, 0, data, offset, mem.contents.length)
        }
      }
      data

  private final case class PreparedVector(
    tc: PvmTestCase, module: InterpretedModule, prepared: RecompilerAbi.PreparedProgram,
    entryIndex: Int, initRegions: RecompilerMemory.Described
  )

  private def prepareVector(tc: PvmTestCase): Option[PreparedVector] =
    val roPages = tc.initialPageMap.filterNot(_.isWritable)
    val rwPages = tc.initialPageMap.filter(_.isWritable)
    val roData = buildMemoryRegion(roPages, tc.initialMemory)
    val rwData = buildMemoryRegion(rwPages, tc.initialMemory)
    val blob = ProgramBlob.fromCodeAndJumpTable(
      data = tc.program, roData = roData, rwData = rwData, stackSize = 4096, is64Bit = true
    ).getOrElse(sys.error(s"[${tc.name}] failed to parse program blob"))
    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(err) => sys.error(s"[${tc.name}] module create failed: $err")
    val prepared = RecompilerAbi.prepareProgram(blob)
    prepared.byteOffsetToIndex.get(tc.initialPc) match
      case None => None // not a decoded instruction boundary — excluded from the replay (mirrors VectorConformanceSpec's Unsupported path)
      case Some(entryIndex) =>
        val seedInstance = InterpretedInstance.fromModule(module, forceStepTracing = false)
        seedInstance.setGas(tc.initialGas)
        seedInstance.setNextProgramCounter(ProgramCounter(tc.initialPc))
        tc.initialRegs.zipWithIndex.foreach { case (v, i) => seedInstance.setReg(i, v) }
        val described = RecompilerMemory.describe(seedInstance)
        Some(PreparedVector(tc, module, prepared, entryIndex, described))

  private final case class VectorReplayResult(
    total: Int, compiledCount: Int, deoptCount: Int,
    interpExecSeconds: Double, recompCompileSeconds: Double, recompExecSeconds: Double,
    interpMips: Double, recompMips: Double
  )

  private def runVectorCorpusReplay(rc: PvmRecompiler, vectors: Seq[PreparedVector]): VectorReplayResult =
    var interpExecNanos = 0L
    var recompCompileNanos = 0L
    var recompExecNanos = 0L
    var interpInstrsExecuted = 0L
    var recompInstrsExecuted = 0L
    var compiledCount = 0
    var deoptCount = 0

    vectors.foreach { pv =>
      val tc = pv.tc

      // ---- interpreter run (timed) ----
      val interpInstance = InterpretedInstance.fromModule(pv.module, forceStepTracing = false)
      interpInstance.setGas(tc.initialGas)
      interpInstance.setNextProgramCounter(ProgramCounter(tc.initialPc))
      tc.initialRegs.zipWithIndex.foreach { case (v, i) => interpInstance.setReg(i, v) }
      val it0 = System.nanoTime()
      var interpExit = ""
      var continue = true
      while continue do
        interpInstance.run() match
          case Right(InterruptKind.Finished) => interpExit = "Halt"; continue = false
          case Right(InterruptKind.Panic) => interpExit = "Panic"; continue = false
          case Right(InterruptKind.OutOfGas) => interpExit = "OutOfGas"; continue = false
          case Right(InterruptKind.Segfault(_)) => interpExit = "Fault"; continue = false
          case Right(InterruptKind.Step) => ()
          case Right(InterruptKind.Ecalli(_)) => sys.error(s"[${tc.name}] unexpected ecalli")
          case Left(err) => sys.error(s"[${tc.name}] interpreter execution error: $err")
      val it1 = System.nanoTime()
      val interpElapsedSeconds = (it1 - it0) / 1e9
      val interpRegsOut = Array.tabulate(13)(interpInstance.reg)
      val interpPcOut = interpInstance.programCounter.map(_.toInt).getOrElse(-1)
      val interpOutcome = InterpOutcome(interpExit, interpInstance.gas, interpRegsOut, interpPcOut, interpElapsedSeconds)

      // ---- recompiler compile (timed separately) + execute (timed) ----
      val ct0 = System.nanoTime()
      val block = rc.compile(pv.prepared.opcodes, pv.prepared.a, pv.prepared.b, pv.prepared.c, pv.prepared.pc,
        pv.prepared.imm, pv.prepared.imm2, pv.prepared.blockGas, pv.prepared.jumpTable, pv.prepared.codeLen)
      val ct1 = System.nanoTime()
      recompCompileNanos += (ct1 - ct0)
      try
        if !block.isValid then
          deoptCount += 1
          interpExecNanos += (it1 - it0)
          interpInstrsExecuted += (tc.initialGas - interpInstance.gas)
        else
          compiledCount += 1
          val regs = tc.initialRegs.clone()
          val backing = pv.initRegions.backing.clone()
          val regions = pv.initRegions.regions.map(r => new PvmRecompiler.Region(r.base, r.len, r.bufOffset, r.writable))
          val et0 = System.nanoTime()
          val out = rc.execute(block, regs, tc.initialGas, regions, backing, pv.initRegions.pageShift, pv.entryIndex)
          val et1 = System.nanoTime()
          val recompElapsedSeconds = (et1 - et0) / 1e9
          val recompOutcome = RecompOutcome(out.exit, out.gasRemaining, regs, out.pc, recompElapsedSeconds)
          assertEngineParity(tc.name, interpOutcome, recompOutcome)

          // Only counted once the vector has passed the strict gate above.
          interpExecNanos += (it1 - it0)
          interpInstrsExecuted += (tc.initialGas - interpInstance.gas)
          recompExecNanos += (et1 - et0)
          recompInstrsExecuted += (tc.initialGas - out.gasRemaining)
      finally
        block.close()
    }

    val interpSeconds = interpExecNanos / 1e9
    val recompCompileSeconds = recompCompileNanos / 1e9
    val recompExecSeconds = recompExecNanos / 1e9
    VectorReplayResult(
      total = vectors.length, compiledCount = compiledCount, deoptCount = deoptCount,
      interpExecSeconds = interpSeconds, recompCompileSeconds = recompCompileSeconds, recompExecSeconds = recompExecSeconds,
      interpMips = interpInstrsExecuted / interpSeconds / 1e6,
      recompMips = if recompExecSeconds > 0 then recompInstrsExecuted / recompExecSeconds / 1e6 else Double.NaN
    )

  private def resolveVectorResourceDir(): File =
    val viaClassloader =
      try
        Option(getClass.getClassLoader.getResource("pvm")).map(u => new File(u.toURI)).filter(_.isDirectory)
      catch case _: IllegalArgumentException => None
    viaClassloader.getOrElse {
      val fallback = new File("src/test/resources/pvm")
      if !fallback.isDirectory then
        sys.error(s"could not locate the pvm vector resource directory via classloader OR fallback path " +
          s"(${fallback.getAbsolutePath}); cwd=${new File(".").getAbsolutePath}")
      fallback
    }

  // ==========================================================================
  // main
  // ==========================================================================

  def main(args: Array[String]): Unit =
    requireAarch64Host()
    val libPath = requireLibPath()
    val rc = new PvmRecompiler(libPath)
    try
      println("=" * 100)
      println("RecompilerThroughputBench — recompiler vs interpreter A/B (Task 14)")
      println("=" * 100)

      val gasBudget = 200_000_000L
      val results = scala.collection.mutable.ArrayBuffer.empty[LoopResult]

      println("\n[1/5] Pure-ALU hot loop (AddImm64 x24 + Jump)")
      results += runLoopWorkload(rc, "pure-alu", buildAluLoop(24), new Array[Long](13), gasBudget)

      println("\n[2/5] Branchy compare loop (SetLessThanUnsignedImm/ShiftLogicalRightImm64/AddImm64/BranchNotEq)")
      results += runLoopWorkload(rc, "branchy", buildBranchyLoop(), new Array[Long](13), gasBudget)

      println("\n[3/5] Memory-traffic loop (indirect u32 load+store, page-wrapped)")
      val memRegs = new Array[Long](13)
      memRegs(6) = 0x20000L // RW region base (matches OracleDifferentialSpec's RW_BASE convention)
      results += runLoopWorkload(rc, "memory", buildMemoryLoop(), memRegs, gasBudget, rwDataLen = 4096)

      println("\n[4/5] Div/mul-upper loop (DivUnsigned64/RemUnsigned64/MulUpperUnsignedUnsigned)")
      val divRegs = new Array[Long](13)
      divRegs(7) = 1L
      divRegs(8) = 7L // constant nonzero divisor
      results += runLoopWorkload(rc, "div-mul", buildDivMulLoop(), divRegs, gasBudget)

      println("\n[5/5] Vector-corpus replay (all PVM test vectors)")
      val testDir = resolveVectorResourceDir()
      val files = testDir.listFiles().filter(_.getName.endsWith(".json")).sortBy(_.getName)
      println(s"  preparing ${files.length} vectors (decode/prepare excluded from timing)...")
      val vectors = files.flatMap(f => prepareVector(loadTestCase(f)))
      println(s"  ${vectors.length}/${files.length} vectors have a valid recompiler entry index; replaying...")
      val vectorResult = runVectorCorpusReplay(rc, vectors.toSeq)
      println(f"  interpreter: ${vectorResult.interpExecSeconds}%.4f s total (${vectorResult.interpMips}%.1f MIPS)")
      println(f"  recompiler:  compile ${vectorResult.recompCompileSeconds * 1000}%.2f ms total, " +
        f"execute ${vectorResult.recompExecSeconds}%.4f s total (${vectorResult.recompMips}%.1f MIPS)")
      println(s"  compiled=${vectorResult.compiledCount} deopt(unsupported)=${vectorResult.deoptCount} of ${vectorResult.total}")

      // ---- final table ----
      val sb = new StringBuilder
      sb ++= "\n" + ("=" * 100) + "\n"
      sb ++= "RESULTS — recompiler vs interpreter throughput (Task 14)\n"
      sb ++= ("=" * 100) + "\n"
      sb ++= f"${"workload"}%-24s ${"interp MIPS"}%14s ${"recomp MIPS"}%14s ${"speedup"}%10s ${"compile"}%12s\n"
      sb ++= ("-" * 100) + "\n"
      results.foreach { r =>
        sb ++= f"${r.name}%-24s ${r.interpMips}%14.1f ${r.recompMips}%14.1f ${r.speedup}%9.2fx ${r.compileSeconds * 1000}%10.2f ms\n"
      }
      sb ++= ("-" * 100) + "\n"
      val vectorSpeedup = vectorResult.recompMips / vectorResult.interpMips
      sb ++= f"${"vector-corpus (309)"}%-24s ${vectorResult.interpMips}%14.1f ${vectorResult.recompMips}%14.1f ${vectorSpeedup}%9.2fx " +
        f"${vectorResult.recompCompileSeconds * 1000}%10.2f ms\n"
      sb ++= ("=" * 100) + "\n"
      sb ++= f"vector-corpus detail: total=${vectorResult.total} compiled=${vectorResult.compiledCount} deopt=${vectorResult.deoptCount}  " +
        f"interp-exec=${vectorResult.interpExecSeconds}%.4fs  recomp-compile=${vectorResult.recompCompileSeconds * 1000}%.2fms  recomp-exec=${vectorResult.recompExecSeconds}%.4fs\n"

      println(sb.toString)
    finally
      rc.close()
