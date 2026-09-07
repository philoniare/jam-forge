package io.forge.jam.pvm.recompiler

import io.forge.jam.pvm.{PvmTestCase, PvmStatus, PageMapEntry, MemoryEntry, InterruptKind, MemoryResult}
import io.forge.jam.pvm.native_.PvmRecompiler
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.engine.{InterpretedModule, InterpretedInstance}
import io.forge.jam.pvm.types.ProgramCounter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.circe.parser.decode
import java.io.File
import java.nio.file.{Files, Path}
import scala.io.Source
import spire.math.UInt

class VectorConformanceSpec extends AnyFlatSpec with Matchers:
  private def libPath: Option[Path] =
    Option(System.getProperty("jam.pvm.recompiler.lib")).map(Path.of(_)).filter(Files.exists(_))
  private val testDir = new File(getClass.getClassLoader.getResource("pvm").toURI)

  private def loadTestCase(file: File): PvmTestCase =
    val content = Source.fromFile(file).mkString
    decode[PvmTestCase](content) match
      case Right(tc) => tc
      case Left(err) => throw new RuntimeException(s"Failed to parse ${file.getName}: $err")

  private def buildMemoryRegion(pages: List[PageMapEntry], memory: List[MemoryEntry]): Array[Byte] =
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

  private final case class InterpRun(
    instance: InterpretedInstance,
    blob: ProgramBlob,
    actualStatus: PvmStatus,
    rawExit: Int,
    finalPc: Int,
    pageFaultAddress: Long
  )

  private def runInterpreter(tc: PvmTestCase): InterpRun =
    val programBytes = tc.program
    val roPages = tc.initialPageMap.filterNot(_.isWritable)
    val rwPages = tc.initialPageMap.filter(_.isWritable)
    val roData = buildMemoryRegion(roPages, tc.initialMemory)
    val rwData = buildMemoryRegion(rwPages, tc.initialMemory)

    val blob = ProgramBlob.fromCodeAndJumpTable(
      data = programBytes, roData = roData, rwData = rwData, stackSize = 4096, is64Bit = true
    ).getOrElse(fail(s"Failed to parse program blob for ${tc.name}"))

    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(err) => fail(s"Failed to create module for ${tc.name}: $err")

    val instance = InterpretedInstance.fromModule(module, forceStepTracing = false)
    instance.setGas(tc.initialGas)
    instance.setNextProgramCounter(ProgramCounter(tc.initialPc))
    tc.initialRegs.zipWithIndex.foreach { case (value, idx) => instance.setReg(idx, value) }

    var finalPc = tc.initialPc
    var pageFaultAddress: Long = 0L
    var actualStatus: PvmStatus = PvmStatus.Panic
    var rawExit: Int = -1
    var continue = true
    while continue do
      instance.run() match
        case Right(InterruptKind.Finished) =>
          actualStatus = PvmStatus.Halt
          rawExit = PvmRecompiler.EXIT_HALT
          finalPc = instance.programCounter.map(_.toInt).getOrElse(finalPc)
          continue = false
        case Right(InterruptKind.Panic) =>
          actualStatus = PvmStatus.Panic
          rawExit = PvmRecompiler.EXIT_PANIC
          finalPc = instance.programCounter.map(_.toInt).getOrElse(finalPc)
          continue = false
        case Right(InterruptKind.Segfault(info)) =>
          pageFaultAddress = info.pageAddress.toLong
          actualStatus = PvmStatus.PageFault
          rawExit = PvmRecompiler.EXIT_FAULT
          finalPc = instance.programCounter.map(_.toInt).getOrElse(finalPc)
          // NOTE: PVM test vectors expect 1 gas consumed on page fault (PvmSpec parity).
          instance.consumeGas(1)
          continue = false
        case Right(InterruptKind.OutOfGas) =>
          // PvmStatus (expected-status comparison): treat as halt, matching PvmSpec.
          actualStatus = PvmStatus.Halt
          // Raw exit (recompiler parity): OOG is its own EXIT_OOG code.
          rawExit = PvmRecompiler.EXIT_OOG
          finalPc = instance.programCounter.map(_.toInt).getOrElse(finalPc)
          continue = false
        case Right(InterruptKind.Ecalli(_)) =>
          fail(s"Unexpected ecalli in test ${tc.name}")
        case Right(InterruptKind.Step) =>
          finalPc = instance.programCounter.map(_.toInt).getOrElse(finalPc)
        case Left(err) =>
          fail(s"Execution error for ${tc.name}: $err")

    if actualStatus != PvmStatus.Halt then
      finalPc = instance.programCounter.map(_.toInt).getOrElse(finalPc)

    InterpRun(instance, blob, actualStatus, rawExit, finalPc, pageFaultAddress)

  // ---- outcome of attempting one vector ----------------------------------------
  private enum Outcome:
    case Pass
    case Unsupported(reason: String)
    case Fail(message: String)

  /** First differing register index + (expected, actual), if any. */
  private def firstRegDiff(expected: Array[Long], actual: Array[Long]): Option[(Int, Long, Long)] =
    expected.indices.find(i => expected(i) != actual(i)).map(i => (i, expected(i), actual(i)))

  /** First differing byte offset (relative to region base) + (expected, actual)
    * across a region, if any. */
  private def firstByteDiff(expected: Array[Byte], actual: Array[Byte]): Option[(Int, Byte, Byte)] =
    val n = math.min(expected.length, actual.length)
    (0 until n).find(i => expected(i) != actual(i)).map(i => (i, expected(i), actual(i)))
      .orElse(if expected.length != actual.length then Some((n, 0.toByte, 0.toByte)) else None)

  private def runVector(rc: PvmRecompiler, tc: PvmTestCase): Outcome =
    val interp = runInterpreter(tc)

    // Recompiler-side entry index translation (byte offset -> instruction index).
    val prepared = RecompilerAbi.prepareProgram(interp.blob)
    val entryIndexOpt = prepared.byteOffsetToIndex.get(tc.initialPc)

    entryIndexOpt match
      case None =>
        if interp.actualStatus == PvmStatus.Panic && interp.finalPc == tc.initialPc then
          Outcome.Unsupported(s"initialPc=${tc.initialPc} is not an instruction boundary (interpreter panics immediately, consistent)")
        else
          Outcome.Fail(s"initialPc=${tc.initialPc} is not a decoded instruction boundary, but interpreter did not " +
            s"panic immediately (status=${interp.actualStatus}, finalPc=${interp.finalPc}) — cannot derive a recompiler entry index")

      case Some(entryIndex) =>
        val blk = rc.compile(prepared.opcodes, prepared.a, prepared.b, prepared.c, prepared.pc, prepared.imm, prepared.imm2, prepared.jumpTable, prepared.codeLen)
        try
          if !blk.isValid then
            Outcome.Unsupported("pvm_compile returned an invalid block (unsupported opcode in program)")
          else
            InterpretedModule.create(interp.blob) match
              case Left(err) => Outcome.Fail(s"Failed to re-create module for recompiler run: $err")
              case Right(freshModule) =>
                val freshInstance = InterpretedInstance.fromModule(freshModule, forceStepTracing = false)
                freshInstance.setGas(tc.initialGas)
                freshInstance.setNextProgramCounter(ProgramCounter(tc.initialPc))
                tc.initialRegs.zipWithIndex.foreach { case (value, idx) => freshInstance.setReg(idx, value) }
                val described = RecompilerMemory.describe(freshInstance)

                val nRegs = tc.initialRegs.clone()
                val backing = described.backing.clone()
                val regions = described.regions.map(r => new PvmRecompiler.Region(r.base, r.len, r.bufOffset, r.writable))
                val out = rc.execute(blk, nRegs, tc.initialGas, regions, backing, described.pageShift, entryIndex)
                evaluateResult(out, nRegs, backing, described, tc, interp)
        finally
          blk.close()

  private def evaluateResult(
    out: PvmRecompiler.ExecResult,
    nRegs: Array[Long],
    backing: Array[Byte],
    described: RecompilerMemory.Described,
    tc: PvmTestCase,
    interp: InterpRun
  ): Outcome =
    // ---- status mapping (recompiler EXIT_* -> PvmStatus for expected-status) ----
    val recompilerMappedStatusOpt: Option[PvmStatus] = out.exit match
      case PvmRecompiler.EXIT_HALT => Some(PvmStatus.Halt)
      case PvmRecompiler.EXIT_PANIC => Some(PvmStatus.Panic)
      case PvmRecompiler.EXIT_OOG => Some(PvmStatus.Halt) // matches interpreter OutOfGas->Halt mapping
      case PvmRecompiler.EXIT_FAULT => Some(PvmStatus.PageFault)
      case _ => None

    recompilerMappedStatusOpt match
      case None => Outcome.Fail(s"unrecognized recompiler exit code ${out.exit}")
      case Some(recompilerMappedStatus) =>
        val recompilerGasForParity =
          if out.exit == PvmRecompiler.EXIT_FAULT then out.gasRemaining - 1 else out.gasRemaining
        val recompilerGasForExpected = out.gasRemaining

        val failures = scala.collection.mutable.ArrayBuffer.empty[String]

        // ---- 1) oracle parity: recompiler vs interpreter --------------------
        if out.exit != interp.rawExit then
          failures += s"[parity] exit: interpreter=${interp.rawExit} recompiler=${out.exit}"
        if recompilerGasForParity != interp.instance.gas then
          failures += s"[parity] gas: interpreter=${interp.instance.gas} recompiler(raw, +1-adjusted)=$recompilerGasForParity (recompiler raw=${out.gasRemaining})"
        if out.pc != (interp.finalPc.toLong & 0xFFFFFFFFL) then
          failures += s"[parity] pc: interpreter=${interp.finalPc} (0x${interp.finalPc.toHexString}) recompiler=${out.pc} (0x${out.pc.toHexString})"
        if out.exit == PvmRecompiler.EXIT_FAULT && out.faultPage != interp.pageFaultAddress then
          failures += s"[parity] faultPage: interpreter=${interp.pageFaultAddress} (0x${interp.pageFaultAddress.toHexString}) recompiler=${out.faultPage} (0x${out.faultPage.toHexString})"
        firstRegDiff(Array.tabulate(13)(i => interp.instance.reg(i)), nRegs) match
          case Some((idx, exp, act)) =>
            failures += s"[parity] reg $idx: interpreter=$exp (0x${exp.toHexString}) recompiler=$act (0x${act.toHexString})"
          case None => ()

        // Memory parity: compare the recompiler's backing buffer against the
        // interpreter's live memory, region by region (via getMemorySlice).
        described.regions.foreach { region =>
          interp.instance.basicMemory.getMemorySlice(UInt(region.base.toInt), region.len.toInt) match
            case MemoryResult.Success(interpBytes) =>
              val recompilerBytes = backing.slice(region.bufOffset.toInt, region.bufOffset.toInt + region.len.toInt)
              firstByteDiff(interpBytes, recompilerBytes) match
                case Some((off, exp, act)) =>
                  failures += f"[parity] memory @0x${region.base}%x+0x$off%x: interpreter=0x$exp%02x recompiler=0x$act%02x"
                case None => ()
            case _ => () // region unreadable on interpreter side post-run (e.g. never mapped there) — skip
        }

        // ---- 2) belt-and-braces: recompiler vs vector's expected-* ----------
        if recompilerMappedStatus != tc.expectedStatus then
          failures += s"[expected] status: expected=${tc.expectedStatus} recompiler=$recompilerMappedStatus"
        if out.pc != (tc.expectedPc.toLong & 0xFFFFFFFFL) then
          failures += s"[expected] pc: expected=${tc.expectedPc} recompiler=${out.pc}"
        if recompilerGasForExpected != tc.expectedGas then
          failures += s"[expected] gas: expected=${tc.expectedGas} recompiler=$recompilerGasForExpected"
        firstRegDiff(tc.expectedRegs, nRegs) match
          case Some((idx, exp, act)) =>
            failures += s"[expected] reg $idx: expected=$exp (0x${exp.toHexString}) recompiler=$act (0x${act.toHexString})"
          case None => ()
        tc.expectedPageFaultAddress.foreach { expected =>
          if expected != 0L && out.exit == PvmRecompiler.EXIT_FAULT && out.faultPage != expected then
            failures += s"[expected] faultPage: expected=$expected recompiler=${out.faultPage}"
        }
        tc.expectedMemory.foreach { mem =>
          described.regions.find(r => mem.address >= r.base && mem.address < r.base + r.len) match
            case Some(r) =>
              val startOff = (mem.address - r.base).toInt
              val recompilerBytes = backing.slice(r.bufOffset.toInt + startOff, r.bufOffset.toInt + startOff + mem.contents.length)
              firstByteDiff(mem.contents, recompilerBytes) match
                case Some((off, exp, act)) =>
                  // Cross-check against the interpreter's own post-run memory to
                  // distinguish a recompiler bug from a fixture/interpreter mismatch.
                  val interpMatches = interp.instance.basicMemory.getMemorySlice(UInt((mem.address + off).toInt), 1) match
                    case MemoryResult.Success(b) => b.headOption.contains(exp)
                    case _ => false
                  if interpMatches then
                    failures += f"[expected-memory] @0x${mem.address + off}%x: expected=0x$exp%02x recompiler=0x$act%02x (interpreter matches expected; recompiler diverges)"
                  else
                    failures += f"[expected-memory] @0x${mem.address + off}%x: expected=0x$exp%02x recompiler=0x$act%02x " +
                      f"(interpreter ALSO disagrees with expected-memory — likely fixture/spec finding, not a recompiler-only bug)"
                case None => ()
            case None =>
              failures += s"[expected-memory] address 0x${mem.address.toHexString} not covered by any described recompiler region"
        }

        if failures.isEmpty then Outcome.Pass
        else Outcome.Fail(failures.mkString("; "))

  // ---- top-level suite ----------------------------------------------------------
  "the native recompiler" should "match the production interpreter and expected-* fields across all PVM test vectors (coverage mode)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val files = testDir.listFiles().filter(_.getName.endsWith(".json")).sortBy(_.getName)
          var passCount = 0
          var unsupportedCount = 0
          val failures = scala.collection.mutable.ArrayBuffer.empty[String]

          files.foreach { file =>
            val tc = loadTestCase(file)
            val outcome =
              try runVector(rc, tc)
              catch case e: Throwable => Outcome.Fail(s"exception: ${e.getClass.getSimpleName}: ${e.getMessage}")
            outcome match
              case Outcome.Pass => passCount += 1
              case Outcome.Unsupported(_) => unsupportedCount += 1
              case Outcome.Fail(msg) => failures += s"${tc.name}: $msg"
          }

          val total = files.length
          val failCount = failures.length
          val coverageLine =
            s"recompiler vector conformance: $passCount/$total pass, $unsupportedCount unsupported (deopt), $failCount fail"
          info(coverageLine)
          println(coverageLine)

          if failures.nonEmpty then
            val detail = failures.take(20).mkString("\n  - ", "\n  - ", "")
            fail(s"$coverageLine\n${failures.length} vector(s) compiled but FAILED parity/expected-* (showing up to 20):$detail")

          total shouldBe 309
        finally rc.close()
  }
