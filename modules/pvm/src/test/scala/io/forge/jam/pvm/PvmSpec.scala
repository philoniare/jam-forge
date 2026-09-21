package io.forge.jam.pvm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.circe.parser.decode
import java.io.File
import scala.io.Source
import spire.math.UInt
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.types.ProgramCounter

/**
 * PVM conformance suite running test vectors from
 * `resources/pvm/`
 */
class PvmSpec extends AnyFlatSpec with Matchers:

  private val testDir = new File(getClass.getClassLoader.getResource("pvm").toURI)
  private val dynamicPagingVectors: Set[String] = Set(
    "multistep_paging_at_the_start_of_block",
    "multistep_paging_in_the_middle_of_block"
  )

  private def loadTestCase(file: File): PvmTestCase =
    val content = Source.fromFile(file).mkString
    decode[PvmTestCase](content) match
      case Right(tc) => tc
      case Left(err) => throw new RuntimeException(s"Failed to parse ${file.getName}: $err")

  private final case class Region(address: Long, length: Long, isWritable: Boolean)

  /**
   * Materialize the contents of one mapped region from the `write` steps that
   * target it.
   */
  private def regionBytes(regions: List[Region], writes: List[PvmStep.Write]): Array[Byte] =
    if regions.isEmpty then Array.empty
    else
      val base = regions.map(_.address).min
      val end = regions.map(r => r.address + r.length).max
      val data = new Array[Byte]((end - base).toInt)
      writes.foreach { w =>
        if regions.exists(r => w.address >= r.address && w.address < r.address + r.length) then
          System.arraycopy(w.contents, 0, data, (w.address - base).toInt, w.contents.length)
      }
      data

  /** Non-zero runs of `bytes`, as spectool's `extract_chunks` produces them. */
  private def extractChunks(base: Long, bytes: Array[Byte]): List[(Long, Array[Byte])] =
    val out = scala.collection.mutable.ListBuffer.empty[(Long, Array[Byte])]
    var i = 0
    while i < bytes.length do
      if bytes(i) != 0 then
        var j = i
        while j < bytes.length && bytes(j) != 0 do j += 1
        out += ((base + i, bytes.slice(i, j)))
        i = j
      else i += 1
    out.toList

  private def runTestCase(tc: PvmTestCase): Unit =
    val firstRun = tc.steps.indexWhere(_ == PvmStep.Run)
    if firstRun < 0 then cancel(s"${tc.name}: no run step")
    val prologue = tc.steps.take(firstRun)

    val maps = prologue.collect { case m: PvmStep.Map => Region(m.address, m.length, m.isWritable) }
    val writes = prologue.collect { case w: PvmStep.Write => w }

    val roRegions = maps.filterNot(_.isWritable)
    val rwRegions = maps.filter(_.isWritable)
    val roData = regionBytes(roRegions, writes)
    val rwData = regionBytes(rwRegions, writes)

    // The vectors' page maps are exactly the GP memory map: read-only data at
    // Z_Z = 0x10000, read-write data at 2*Z_Z + Q(|ro|). No stack or argument
    // region is mapped, so both are sized zero here to keep the accessible
    // address space identical to the vector's page map.
    val blob = ProgramBlob.fromCodeAndJumpTable(
      data = tc.program,
      roData = roData,
      rwData = rwData,
      stackSize = 0,
      is64Bit = true
    ).getOrElse(fail(s"Failed to parse program blob for ${tc.name}"))

    val module = InterpretedModule.create(blob, auxDataSize = UInt(0)) match
      case Right(m) => m
      case Left(err) => fail(s"Failed to create module for ${tc.name}: $err")

    roRegions.headOption.foreach { r =>
      withClue(s"${tc.name}: unexpected read-only region base:") {
        r.address shouldBe module.memoryMap.roDataAddress.toLong
      }
    }
    rwRegions.headOption.foreach { r =>
      withClue(s"${tc.name}: unexpected read-write region base:") {
        r.address shouldBe module.memoryMap.rwDataAddress.toLong
      }
    }

    val instance = InterpretedInstance.fromModule(module, forceStepTracing = false)
    instance.setGas(tc.initialGas)
    instance.setNextProgramCounter(ProgramCounter(tc.initialPc))

    var status: PvmStatus = PvmStatus.Panic
    var pageFaultAddress: Option[Long] = None
    var hostcall: Option[Long] = None
    var liveRegions: List[Region] = maps

    tc.steps.zipWithIndex.foreach { (step, idx) =>
      step match
        case PvmStep.SetReg(reg, value) => instance.setReg(reg, value)

        case PvmStep.Map(_, _, _) =>
          if idx > firstRun then
            cancel(s"${tc.name}: dynamic page mapping is not supported by this engine")

        case PvmStep.Write(address, contents) =>
          if idx > firstRun then
            instance.basicMemory.setMemorySlice(UInt(address.toInt), contents) match
              case MemoryResult.Success(_) => ()
              case other => fail(s"${tc.name}: write to 0x${address.toHexString} failed: $other")

        case PvmStep.Run =>
          instance.run() match
            case Right(InterruptKind.Finished) => status = PvmStatus.Halt
            case Right(InterruptKind.Panic) => status = PvmStatus.Panic
            case Right(InterruptKind.OutOfGas) => status = PvmStatus.OutOfGas
            case Right(InterruptKind.Segfault(info)) =>
              status = PvmStatus.PageFault
              pageFaultAddress = Some(info.pageAddress.toLong & 0xffffffffL)
            case Right(InterruptKind.Ecalli(id)) =>
              status = PvmStatus.Ecalli
              hostcall = Some(id.toLong & 0xffffffffL)
            case Right(InterruptKind.Step) => fail(s"${tc.name}: unexpected step interrupt")
            case Left(err) => fail(s"${tc.name}: execution error: $err")

        case a: PvmStep.Assert =>
          a.status.foreach { expected =>
            withClue(s"Status mismatch for ${tc.name}:") { status shouldBe expected }
          }
          a.pc.foreach { expected =>
            withClue(s"Program counter mismatch for ${tc.name}:") {
              instance.programCounter.map(_.toInt) shouldBe Some(expected)
            }
          }
          a.regs.zipWithIndex.foreach { (expected, i) =>
            expected.foreach { v =>
              withClue(s"Register $i mismatch for ${tc.name}:") { instance.reg(i) shouldBe v }
            }
          }
          a.gas.foreach { expected =>
            withClue(s"Gas mismatch for ${tc.name}:") { instance.gas shouldBe expected }
          }
          a.pageFaultAddress.foreach { expected =>
            withClue(s"Page fault address mismatch for ${tc.name}:") {
              pageFaultAddress shouldBe Some(expected)
            }
          }
          a.hostcall.foreach { expected =>
            withClue(s"Host-call id mismatch for ${tc.name}:") { hostcall shouldBe Some(expected) }
          }
          a.memory.foreach { expected =>
            val actual = liveRegions.flatMap { r =>
              instance.basicMemory.getMemorySlice(UInt(r.address.toInt), r.length.toInt) match
                case MemoryResult.Success(bytes) => extractChunks(r.address, bytes)
                case other => fail(s"${tc.name}: could not read region 0x${r.address.toHexString}: $other")
            }
            withClue(s"Memory mismatch for ${tc.name}:") {
              actual.map((a, b) => (a, b.toList)) shouldBe expected.map(e => (e.address, e.contents.toList))
            }
          }
    }

  private def blockGasCosts(tc: PvmTestCase): Unit =
    if tc.blockGasCosts.nonEmpty then
      val blob = ProgramBlob.fromCodeAndJumpTable(
        data = tc.program,
        is64Bit = true
      ).getOrElse(fail(s"Failed to parse program blob for ${tc.name}"))
      tc.blockGasCosts.foreach { (pc, cost) =>
        withClue(s"Block gas cost at pc=$pc for ${tc.name}:") {
          BlockGasModel.gasCostForBlock(blob.code, blob.bitmask, pc) shouldBe cost
        }
      }

  // Generate tests for each test vector file
  testDir.listFiles().filter(_.getName.endsWith(".json")).sorted.foreach { file =>
    val testName = file.getName.replace(".json", "")

    if dynamicPagingVectors.contains(testName) then
      testName should "pass test vector" ignore {
        runTestCase(loadTestCase(file))
      }
    else
      testName should "pass test vector" in {
        val tc = loadTestCase(file)
        blockGasCosts(tc)
        runTestCase(tc)
      }
  }
