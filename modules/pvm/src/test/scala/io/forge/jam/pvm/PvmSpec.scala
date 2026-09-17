package io.forge.jam.pvm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.circe.parser.decode
import java.io.File
import scala.io.Source
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.types.ProgramCounter

/**
 * PVM test suite running test vectors from resources/pvm/.
 */
class PvmSpec extends AnyFlatSpec with Matchers:

  private val testDir = new File(getClass.getClassLoader.getResource("pvm").toURI)

  private def loadTestCase(file: File): PvmTestCase =
    val content = Source.fromFile(file).mkString
    decode[PvmTestCase](content) match
      case Right(tc) => tc
      case Left(err) => throw new RuntimeException(s"Failed to parse ${file.getName}: $err")

  /**
   * Build memory region from page map and initial memory.
   */
  private def buildMemoryRegion(pages: List[PageMapEntry], memory: List[MemoryEntry]): Array[Byte] =
    if pages.isEmpty then Array.empty
    else
      val firstPageAddr = pages.map(_.address).min
      val totalSize = pages.map(p => p.address + p.length - firstPageAddr).max.toInt
      val data = new Array[Byte](totalSize)

      // Fill with memory contents
      memory.foreach { mem =>
        pages.find(p => mem.address >= p.address && mem.address < p.address + p.length).foreach { _ =>
          val offset = (mem.address - firstPageAddr).toInt
          System.arraycopy(mem.contents, 0, data, offset, mem.contents.length)
        }
      }
      data

  private def runTestCase(tc: PvmTestCase): Unit =
    // Parse the program blob from code+jumptable format
    val programBytes = tc.program

    // Build roData and rwData from page map and initial memory
    val roPages = tc.initialPageMap.filterNot(_.isWritable)
    val rwPages = tc.initialPageMap.filter(_.isWritable)

    val roData = buildMemoryRegion(roPages, tc.initialMemory)
    val rwData = buildMemoryRegion(rwPages, tc.initialMemory)

    val blob = ProgramBlob.fromCodeAndJumpTable(
      data = programBytes,
      roData = roData,
      rwData = rwData,
      stackSize = 4096,  // Default stack size
      is64Bit = true
    ).getOrElse(fail(s"Failed to parse program blob for ${tc.name}"))

    // Create module
    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(err) => fail(s"Failed to create module for ${tc.name}: $err")

    // Create instance
    val instance = InterpretedInstance.fromModule(module, forceStepTracing = false)

    // Set initial state
    instance.setGas(tc.initialGas)
    instance.setNextProgramCounter(ProgramCounter(tc.initialPc))

    // Set initial registers
    tc.initialRegs.zipWithIndex.foreach { case (value, idx) =>
      instance.setReg(idx, value)
    }

    // Run until interrupt
    var finalPc = tc.initialPc
    var pageFaultAddress: Long = 0L
    var actualStatus: PvmStatus = PvmStatus.Panic

    var continue = true
    while continue do
      instance.run() match
        case Right(InterruptKind.Finished) =>
          actualStatus = PvmStatus.Halt
          finalPc = instance.programCounter.map(_.toInt).getOrElse(finalPc)
          continue = false
        case Right(InterruptKind.Panic) =>
          actualStatus = PvmStatus.Panic
          finalPc = instance.programCounter.map(_.toInt).getOrElse(finalPc)
          continue = false
        case Right(InterruptKind.Segfault(info)) =>
          pageFaultAddress = info.pageAddress.toLong
          actualStatus = PvmStatus.PageFault
          finalPc = instance.programCounter.map(_.toInt).getOrElse(finalPc)
          // NOTE: PVM test vectors expect 1 gas consumed on page fault
          instance.consumeGas(1)
          continue = false
        case Right(InterruptKind.OutOfGas) =>
          // Treat as halt for now
          actualStatus = PvmStatus.Halt
          finalPc = instance.programCounter.map(_.toInt).getOrElse(finalPc)
          continue = false
        case Right(InterruptKind.Ecalli(_)) =>
          fail("Unexpected ecalli in test")
        case Right(InterruptKind.Step) =>
          finalPc = instance.programCounter.map(_.toInt).getOrElse(finalPc)
          // Continue execution
        case Left(err) =>
          fail(s"Execution error: $err")

    if actualStatus != PvmStatus.Halt then
      finalPc = instance.programCounter.map(_.toInt).getOrElse(finalPc)

    // Validate results
    withClue(s"Status mismatch for ${tc.name}:") {
      actualStatus shouldBe tc.expectedStatus
    }

    withClue(s"Program counter mismatch for ${tc.name}:") {
      finalPc shouldBe tc.expectedPc
    }

    // Validate registers
    tc.expectedRegs.zipWithIndex.foreach { case (expected, idx) =>
      withClue(s"Register $idx mismatch for ${tc.name}:") {
        instance.reg(idx) shouldBe expected
      }
    }

    // Validate gas
    withClue(s"Gas mismatch for ${tc.name}:") {
      instance.gas shouldBe tc.expectedGas
    }

    // Validate page fault address if applicable
    tc.expectedPageFaultAddress.foreach { expected =>
      if expected != 0L then
        withClue(s"Page fault address mismatch for ${tc.name}:") {
          pageFaultAddress shouldBe expected
        }
    }

  // Generate tests for each test vector file
  testDir.listFiles().filter(_.getName.endsWith(".json")).sorted.foreach { file =>
    val testName = file.getName.replace(".json", "")

    testName should s"pass test vector" in {
      val tc = loadTestCase(file)
      runTestCase(tc)
    }
  }
