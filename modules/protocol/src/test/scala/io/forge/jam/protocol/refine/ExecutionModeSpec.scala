package io.forge.jam.protocol.refine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.pvm.ExecutionMode
import io.forge.jam.pvm.engine.InterpretedModule
import io.forge.jam.pvm.program.{JumpTable, ProgramBlob}
import io.forge.jam.protocol.accumulation.{HostCall, HostCallResult, PvmInstance}
import spire.math.UShort

import java.nio.file.{Files, Path}

class ExecutionModeSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY

  private def libPath: Option[Path] =
    Option(System.getProperty("jam.pvm.recompiler.lib")).map(Path.of(_)).filter(Files.exists(_))

  private def isAarch64Host: Boolean =
    val arch = System.getProperty("os.arch", "").toLowerCase
    arch == "aarch64" || arch == "arm64"

  private def canRunNative: Boolean = isAarch64Host && libPath.isDefined
  private val haltCode = Array[Byte](50, 0)
  private val haltBitmask = Array[Byte](1)
  private val ecalliThenHaltCode = Array[Byte](10, 0, 50, 0)
  // bits 0 and 2 set (instruction boundaries at byte offsets 0 and 2).
  private val ecalliThenHaltBitmask = Array[Byte](0x05)

  private def moduleOf(code: Array[Byte], bitmask: Array[Byte]): InterpretedModule =
    val blob = ProgramBlob(
      code = code,
      bitmask = bitmask,
      jumpTable = JumpTable.Empty,
      is64Bit = true,
      roData = Array.empty,
      rwData = Array.empty,
      stackSize = 4096
    )
    InterpretedModule.create(blob) match
      case Right(m)   => m
      case Left(err)  => fail(s"failed to create module: $err")

  /** A host-call dispatcher that fails the test if it's ever invoked — used
    * by the host-call-free program test, where reaching a dispatch would
    * mean the program didn't actually halt on its first instruction.
    */
  private object NoHostCalls extends HostCallDispatcher:
    def getGasCost(hostCallId: Int, instance: PvmInstance): Long =
      fail(s"unexpected host call $hostCallId in a host-call-free program")
    def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
      fail(s"unexpected host call $hostCallId in a host-call-free program")

  /** A minimal "gas" host-call dispatcher (HostCall.GAS = 0), matching
    * IsAuthorizedHostCalls'/AccumulationHostCalls' own handling, so the
    * interpreter fallback path for the Ecalli test produces the same
    * observable result (r7 = remaining gas) that a native-deopt run would
    * see once it falls through to the interpreter.
    */
  private object GasOnlyHostCalls extends HostCallDispatcher:
    def getGasCost(hostCallId: Int, instance: PvmInstance): Long = 10L
    def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
      hostCallId match
        case HostCall.GAS => instance.setReg(7, instance.gas)
        case _            => instance.setReg(7, HostCallResult.WHAT.signed)

  // ---- 1. host-call-free program: Interpreted vs Recompiled parity ----------

  test("a host-call-free program produces identical (exit, registers, gas, pc) in both modes") {
    if !canRunNative then
      cancel("recompiler unavailable on this host (AArch64 dylib required) — Recompiled mode would only exercise the deopt path here, not native execution")
    else
      val module = moduleOf(haltCode, haltBitmask)

      val (interpExit, interpGas, interpOutput) =
        PvmRunner.run(module, Array.empty, gasLimit = 1000L, entryPc = 0, NoHostCalls, ExecutionMode.Interpreted)
      val (nativeExit, nativeGas, nativeOutput) =
        PvmRunner.run(module, Array.empty, gasLimit = 1000L, entryPc = 0, NoHostCalls, ExecutionMode.Recompiled)

      nativeExit shouldBe interpExit
      nativeExit shouldBe PvmRunner.PvmExit.Halt
      nativeGas shouldBe interpGas
      nativeOutput.toSeq shouldBe interpOutput.toSeq
  }

  // ---- 2. Ecalli deopt: Recompiled falls back to the interpreter -------------

  test("a program containing Ecalli run in Recompiled mode deopts and matches Interpreted mode") {
    val module = moduleOf(ecalliThenHaltCode, ecalliThenHaltBitmask)

    val (interpExit, interpGas, interpOutput) =
      PvmRunner.run(module, Array.empty, gasLimit = 1000L, entryPc = 0, GasOnlyHostCalls, ExecutionMode.Interpreted)
    val (deoptExit, deoptGas, deoptOutput) =
      PvmRunner.run(module, Array.empty, gasLimit = 1000L, entryPc = 0, GasOnlyHostCalls, ExecutionMode.Recompiled)

    deoptExit shouldBe interpExit
    deoptExit shouldBe PvmRunner.PvmExit.Halt
    deoptGas shouldBe interpGas
    deoptOutput.toSeq shouldBe interpOutput.toSeq
  }

  // ---- 3. missing dylib: Recompiled mode is safe, never crashes --------------

  test("Recompiled mode with an unset/missing dylib property falls back to interpreter results without crashing") {
    val original = Option(System.getProperty("jam.pvm.recompiler.lib"))
    try
      System.clearProperty("jam.pvm.recompiler.lib")

      val module = moduleOf(haltCode, haltBitmask)
      val (interpExit, interpGas, interpOutput) =
        PvmRunner.run(module, Array.empty, gasLimit = 1000L, entryPc = 0, NoHostCalls, ExecutionMode.Interpreted)
      val (fallbackExit, fallbackGas, fallbackOutput) =
        PvmRunner.run(module, Array.empty, gasLimit = 1000L, entryPc = 0, NoHostCalls, ExecutionMode.Recompiled)

      fallbackExit shouldBe interpExit
      fallbackGas shouldBe interpGas
      fallbackOutput.toSeq shouldBe interpOutput.toSeq
    finally
      original.foreach(p => System.setProperty("jam.pvm.recompiler.lib", p))
  }

  private val authCodeHash = Hash(Array.fill[Byte](32)(0x21))

  private def preimageOf(code: Array[Byte], bitmask: Array[Byte]): Array[Byte] =
    Array[Byte](0, 0, 0, code.length.toByte) ++ code ++ bitmask

  private class HostLookup(hostService: Long, preimage: Option[Array[Byte]]) extends HistoricalLookupService:
    def serviceExists(id: Long): Boolean = id == hostService
    def historicalLookup(id: Long, t: Long, h: Hash): Option[Array[Byte]] =
      if id == hostService && h == authCodeHash then preimage else None

  private def workPackage(hostService: Long): WorkPackage =
    WorkPackage(
      authCodeHost = ServiceId(hostService.toInt),
      authCodeHash = authCodeHash,
      context = Context(
        anchor = Hash(Array.fill[Byte](32)(1)),
        stateRoot = Hash(Array.fill[Byte](32)(2)),
        beefyRoot = Hash(Array.fill[Byte](32)(3)),
        lookupAnchor = Hash(Array.fill[Byte](32)(4)),
        lookupAnchorSlot = Timeslot(100),
        prerequisites = List.empty
      ),
      authorization = JamBytes(Array[Byte](0x0a, 0x0b, 0x0c)),
      authorizerConfig = JamBytes(Array[Byte](0x0d)),
      items = List(
        WorkItem(
          service = ServiceId(7),
          codeHash = Hash(Array.fill[Byte](32)(5)),
          payload = JamBytes(Array[Byte](1)),
          refineGasLimit = Gas(1000L),
          accumulateGasLimit = Gas(1000L),
          importSegments = List.empty,
          extrinsic = List.empty,
          exportCount = UShort(0)
        )
      )
    )

  test("conformance sanity: forcing Recompiled mode on a real is-authorized invocation matches Interpreted mode") {
    val wp = workPackage(5L)
    val accounts = new HostLookup(5L, Some(preimageOf(haltCode, haltBitmask)))

    val interpResult = new IsAuthorizedExecutor(config).execute(wp, 3, accounts, ExecutionMode.Interpreted)
    val recompiledResult = new IsAuthorizedExecutor(config).execute(wp, 3, accounts, ExecutionMode.Recompiled)

    recompiledResult.result shouldBe interpResult.result
    recompiledResult.gasUsed shouldBe interpResult.gasUsed
    interpResult.result match
      case ExecutionResult.Ok(trace) => trace.toArray shouldBe Array[Byte](3, 0)
      case other                     => fail(s"expected Ok, got $other")
  }
