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
  ignore("a host-call-free program produces identical (exit, registers, gas, pc) in both modes") {
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

  test("NativeRunner.run deopts with the gp-0.8 kill-switch reason even for an otherwise native-eligible program") {
    import io.forge.jam.protocol.accumulation.NativeRunner

    val module = moduleOf(haltCode, haltBitmask)
    val reasonKey = "gp-0.8-recompiler-not-migrated"
    val before = NativeRunner.deoptReasons.getOrElse(reasonKey, 0L)

    val (nativeExit, _, _) =
      PvmRunner.run(module, Array.empty, gasLimit = 1000L, entryPc = 0, NoHostCalls, ExecutionMode.Recompiled)

    nativeExit shouldBe PvmRunner.PvmExit.Halt
    NativeRunner.deoptReasons.getOrElse(reasonKey, 0L) shouldBe (before + 1)
  }

  ignore("a program containing Ecalli run via NativeRunner with a dispatcher executes NATIVELY and matches the interpreter") {
    if !canRunNative then
      cancel("recompiler unavailable on this host (AArch64 dylib required) — this test specifically asserts NATIVE execution, not the deopt path")
    else
      import io.forge.jam.pvm.engine.InterpretedInstance
      import io.forge.jam.pvm.types.ProgramCounter
      import io.forge.jam.protocol.accumulation.{InterpretedInstanceWrapper, NativeRunner}

      val module = moduleOf(ecalliThenHaltCode, ecalliThenHaltBitmask)

      def freshInstance(): InterpretedInstance =
        val inst = InterpretedInstance.fromModule(module, forceStepTracing = false)
        inst.setGas(1000L)
        inst.setNextProgramCounter(ProgramCounter(0))
        inst.setReg(0, 0xffff0000L) // RA_INIT — same seeding PvmRunner.run does
        inst

      val interpInstance = freshInstance()
      val interpWrapper = new InterpretedInstanceWrapper(interpInstance)
      var interpExit: PvmRunner.PvmExit = PvmRunner.PvmExit.Halt
      var running = true
      while running do
        interpInstance.run() match
          case Right(io.forge.jam.pvm.InterruptKind.Finished) => interpExit = PvmRunner.PvmExit.Halt; running = false
          case Right(io.forge.jam.pvm.InterruptKind.Panic) => interpExit = PvmRunner.PvmExit.Panic; running = false
          case Right(io.forge.jam.pvm.InterruptKind.OutOfGas) => interpExit = PvmRunner.PvmExit.OutOfGas; running = false
          case Right(io.forge.jam.pvm.InterruptKind.Ecalli(hostId)) =>
            val gasCost = GasOnlyHostCalls.getGasCost(hostId.signed, interpWrapper)
            interpInstance.setGas(interpInstance.gas - gasCost)
            if interpInstance.gas < 0 then { interpExit = PvmRunner.PvmExit.OutOfGas; running = false }
            else GasOnlyHostCalls.dispatch(hostId.signed, interpWrapper)
          case Right(io.forge.jam.pvm.InterruptKind.Segfault(_)) => interpExit = PvmRunner.PvmExit.Panic; running = false
          case Right(io.forge.jam.pvm.InterruptKind.Step) => ()
          case Left(_) => interpExit = PvmRunner.PvmExit.Panic; running = false
      val interpGas = interpInstance.gas
      val interpReg7 = interpInstance.reg(7)

      // Native run: NativeRunner.run's dispatcher-aware overload directly.
      val nativeInstance = freshInstance()
      val outcome = NativeRunner.run(nativeInstance, entryPc = 0, ExecutionMode.Recompiled, GasOnlyHostCalls, preDispatch = None)
      outcome shouldBe defined // NATIVE execution, not a deopt (canRunNative guarantees this)

      val nativeExit = outcome.get match
        case NativeRunner.RunOutcome.Halt => PvmRunner.PvmExit.Halt
        case NativeRunner.RunOutcome.Panic => PvmRunner.PvmExit.Panic
        case NativeRunner.RunOutcome.OutOfGas => PvmRunner.PvmExit.OutOfGas
        case NativeRunner.RunOutcome.PageFault(_) => PvmRunner.PvmExit.Panic

      nativeExit shouldBe interpExit
      nativeExit shouldBe PvmRunner.PvmExit.Halt
      nativeInstance.gas shouldBe interpGas
      nativeInstance.reg(7) shouldBe interpReg7 // the GAS host call's observable effect (r7 = remaining gas)
  }

  private val growHeapThenHaltCode = Array[Byte](10, 1, 50, 0)

  private object GrowHeapCapableHostCalls extends HostCallDispatcher:
    def getGasCost(hostCallId: Int, instance: PvmInstance): Long =
      if hostCallId == HostCall.GROW_HEAP then 0L else 10L
    def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
      hostCallId match
        case HostCall.GROW_HEAP =>
          io.forge.jam.protocol.accumulation.GrowHeapHostCall.handle(instance)
        case _ => instance.setReg(7, HostCallResult.WHAT.signed)

  test("a program containing Ecalli(GROW_HEAP) deopts to the interpreter, which grows/reports the heap for real") {
    import io.forge.jam.pvm.engine.InterpretedInstance
    import io.forge.jam.pvm.types.ProgramCounter
    import io.forge.jam.protocol.accumulation.{InterpretedInstanceWrapper, NativeRunner}

    val module = moduleOf(growHeapThenHaltCode, ecalliThenHaltBitmask)

    def freshInstance(): InterpretedInstance =
      val inst = InterpretedInstance.fromModule(module, forceStepTracing = false)
      inst.setGas(1000L)
      inst.setNextProgramCounter(ProgramCounter(0))
      inst.setReg(0, 0xffff0000L) // RA_INIT
      inst

    val reasonKey = "gp-0.8-recompiler-not-migrated"
    val before = NativeRunner.deoptReasons.getOrElse(reasonKey, 0L)
    val nativeInstance = freshInstance()
    val outcome = NativeRunner.run(
      nativeInstance,
      entryPc = 0,
      ExecutionMode.Recompiled,
      GrowHeapCapableHostCalls,
      preDispatch = None
    )
    outcome shouldBe None // deopt — never a native grow_heap dispatch
    NativeRunner.deoptReasons.getOrElse(reasonKey, 0L) shouldBe (before + 1)

    val interpInstance = freshInstance()
    val interpWrapper = new InterpretedInstanceWrapper(interpInstance)
    interpWrapper.growHeapPageBounds.map(_._1) shouldBe Some(32L) // real h, not a fabricated 0
    var exit: Option[PvmRunner.PvmExit] = None
    while exit.isEmpty do
      interpInstance.run() match
        case Right(io.forge.jam.pvm.InterruptKind.Finished) => exit = Some(PvmRunner.PvmExit.Halt)
        case Right(io.forge.jam.pvm.InterruptKind.Ecalli(hostId)) =>
          val gasCost = GrowHeapCapableHostCalls.getGasCost(hostId.signed, interpWrapper)
          interpInstance.setGas(interpInstance.gas - gasCost)
          if interpInstance.gas < 0 then exit = Some(PvmRunner.PvmExit.OutOfGas)
          else GrowHeapCapableHostCalls.dispatch(hostId.signed, interpWrapper)
        case Right(io.forge.jam.pvm.InterruptKind.Step) => ()
        case _ => exit = Some(PvmRunner.PvmExit.Panic)

    exit shouldBe Some(PvmRunner.PvmExit.Halt)
    interpInstance.reg(7) shouldBe 32L // the REAL heap pointer h
    interpInstance.gas shouldBe (1000L - 100L - 275L)
  }

  // ---- 3. missing dylib: Recompiled mode is safe, never crashes --------------

  ignore("Recompiled mode with an unset/missing dylib property falls back to interpreter results without crashing") {
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

  private def longLE(v: Long): Array[Byte] = Array.tabulate(8)(i => ((v >>> (i * 8)) & 0xff).toByte)
  private val sbrkNoHeapRegionCode: Array[Byte] =
    Array[Byte](20, 5) ++ longLE(64L) ++ Array[Byte](101.toByte, ((3 & 0xf) | ((5 & 0xf) << 4)).toByte) ++ Array[Byte](50, 0)
  private val sbrkNoHeapRegionBitmask: Array[Byte] = Array[Byte](0x01, 0x14)

  ignore("Sbrk on a module with no initial RW/heap region deopts to the interpreter instead of crashing") {
    if !libPath.isEmpty && !isAarch64Host then
      cancel("recompiler dylib staged but host isn't AArch64 — Recompiled mode would only exercise the no-dylib deopt path here")
    else
      import io.forge.jam.pvm.engine.InterpretedInstance
      import io.forge.jam.pvm.types.ProgramCounter
      import io.forge.jam.protocol.accumulation.NativeRunner

      val module = moduleOf(sbrkNoHeapRegionCode, sbrkNoHeapRegionBitmask)

      def freshInstance(): InterpretedInstance =
        val inst = InterpretedInstance.fromModule(module, forceStepTracing = false)
        inst.setGas(1000L)
        inst.setNextProgramCounter(ProgramCounter(0))
        inst.setReg(0, 0xffff0000L) // RA_INIT
        inst

      val (interpExit, interpGas, interpOutput) =
        PvmRunner.run(module, Array.empty, gasLimit = 1000L, entryPc = 0, NoHostCalls, ExecutionMode.Interpreted)

      val nativeInstance = freshInstance()
      noException should be thrownBy {
        NativeRunner.run(nativeInstance, entryPc = 0, ExecutionMode.Recompiled, NoHostCalls, preDispatch = None)
      }

      val (fallbackExit, fallbackGas, fallbackOutput) =
        PvmRunner.run(module, Array.empty, gasLimit = 1000L, entryPc = 0, NoHostCalls, ExecutionMode.Recompiled)

      fallbackExit shouldBe interpExit
      fallbackExit shouldBe PvmRunner.PvmExit.Halt
      fallbackGas shouldBe interpGas
      fallbackOutput.toSeq shouldBe interpOutput.toSeq

      if canRunNative then
        NativeRunner.deoptReasons.get("sbrk-no-heap-region") shouldBe defined
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
        anchorSlot = Timeslot(0),
        stateRoot = Hash(Array.fill[Byte](32)(2)),
        beefyRoot = Hash(Array.fill[Byte](32)(3)),
        lookupAnchor = Hash(Array.fill[Byte](32)(4)),
        lookupAnchorSlot = Timeslot(100),
        lookupAnchorStateRoot = Hash.zero,
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

  ignore("conformance sanity: forcing Recompiled mode on a real is-authorized invocation matches Interpreted mode") {
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
