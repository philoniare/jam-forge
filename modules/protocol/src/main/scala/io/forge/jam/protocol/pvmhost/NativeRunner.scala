package io.forge.jam.protocol.pvmhost

import io.forge.jam.pvm.{ExecutionMode, Instruction, InterruptKind, PvmSettings, SegfaultInfo}
import io.forge.jam.pvm.engine.{InterpretedInstance, InterpretedModule, RunDeadlinePassed}
import io.forge.jam.pvm.memory.{GuestRam, InstanceMemoryBudget}
import io.forge.jam.pvm.native_.PvmRecompiler
import io.forge.jam.pvm.program.InstructionDecoder
import io.forge.jam.pvm.recompiler.{RecompilerAbi, RecompilerMemory}
import io.forge.jam.pvm.types.ProgramCounter
import io.forge.jam.protocol.HostCallPanic
import io.forge.jam.protocol.pipeline.StageTimers
import com.typesafe.scalalogging.StrictLogging
import spire.math.UInt

import java.nio.file.Files
import java.util.concurrent.atomic.{AtomicLong, LongAdder}
import scala.jdk.CollectionConverters.*

object NativeRunner extends StrictLogging:

  enum RunOutcome:
    case Halt, Panic, OutOfGas
    case PageFault(pageFaultAddress: Long)

  private val nativeRuns = new AtomicLong(0)
  private val deoptRuns = new AtomicLong(0)
  private val deoptByReason = new java.util.concurrent.ConcurrentHashMap[String, LongAdder]()

  private def recordNative(): Unit = nativeRuns.incrementAndGet()
  private def recordDeopt(reason: String): Unit =
    deoptRuns.incrementAndGet()
    deoptByReason.computeIfAbsent(reason, _ => new LongAdder()).increment()

  def nativeCount: Long = nativeRuns.get()
  def deoptCount: Long = deoptRuns.get()
  def deoptReasons: Map[String, Long] =
    deoptByReason.asScala.view.mapValues(_.sum()).toMap

  def logSummary(): Unit =
    logger.info(
      s"NativeRunner: ${nativeCount} native, ${compileCount} compiled, ${nativeCode.liveBytes >> 20} MiB of machine code alive, " +
        s"${deoptCount} deopt (reasons: ${deoptReasons.mkString(", ")})"
    )

  def run(
      instance: InterpretedInstance,
      entryPc: Int,
      mode: ExecutionMode
  ): Option[RunOutcome] =
    mode match
      case ExecutionMode.Interpreted => None
      case ExecutionMode.Recompiled => runRecompiled(instance, entryPc, hostCalls = None)

  def run(
      instance: InterpretedInstance,
      entryPc: Int,
      mode: ExecutionMode,
      dispatcher: HostCallDispatcher,
      preDispatch: Option[() => Unit],
      deadlineMillis: Option[Long] = None
  ): Option[RunOutcome] =
    mode match
      case ExecutionMode.Interpreted => None
      case ExecutionMode.Recompiled =>
        runRecompiled(instance, entryPc, hostCalls = Some((dispatcher, preDispatch)), deadlineMillis)

  @volatile private[pvmhost] var deadlineSliceGas: Long = 1L << 24

  private val slicesResumed = new LongAdder
  private[pvmhost] def resumedSlices: Long = slicesResumed.sum()

  private final class GasSlices(live: PvmRecompiler#LiveExecution, deadlineMillis: Long, indexOfPc: Int => Option[Int]):
    private val cell = live.gasSegment()
    private var held = 0L
    private var slice = math.max(1L, deadlineSliceGas)

    private def gas: Long = cell.get(java.lang.foreign.ValueLayout.JAVA_LONG, 0)
    private def setGas(v: Long): Unit = cell.set(java.lang.foreign.ValueLayout.JAVA_LONG, 0, v)

    def cut(): Unit =
      val all = gas
      if all > slice then
        setGas(slice)
        held = all - slice

    def restore(): Unit =
      if held != 0L then
        setGas(gas + held)
        held = 0L

    def run(): PvmRecompiler.ExecResult =
      cut()
      var out = live.run()
      while out.exit == PvmRecompiler.EXIT_OOG && held > 0L do
        if System.currentTimeMillis() > deadlineMillis then throw new RunDeadlinePassed(deadlineMillis)
        if gas == slice && slice <= (Long.MaxValue >> 1) then slice <<= 1
        restore()
        cut()
        val index = indexOfPc(out.pc.toInt).getOrElse(
          throw new IllegalStateException(s"NativeRunner: out of gas at pc=${out.pc}, which is no instruction")
        )
        slicesResumed.increment()
        out = live.resume(index)
      restore()
      new PvmRecompiler.ExecResult(out.exit, gas, out.pc, out.faultPage)

  private lazy val recompiler: Option[PvmRecompiler] =
    PvmSettings.current.recompilerLib
      .filter(Files.exists(_))
      .flatMap { lib =>
        try Some(new PvmRecompiler(lib))
        catch
          case e: Throwable =>
            logger.debug(s"NativeRunner: failed to open recompiler dylib at $lib: ${e.getMessage}")
            None
      }

  private def runRecompiled(
      instance: InterpretedInstance,
      entryPc: Int,
      hostCalls: Option[(HostCallDispatcher, Option[() => Unit])],
      deadlineMillis: Option[Long] = None
  ): Option[RunOutcome] =
    recompiler match
      case None =>
        logger.debug("NativeRunner: deopt to interpreter — recompiler dylib not available (jam.pvm.recompiler.lib unset or file missing)")
        recordDeopt("no-dylib")
        None
      case Some(_) if currentHeapSize(instance) > MaxDescribedHeapBytes =>
        logger.debug("NativeRunner: deopt to interpreter — the instance's heap is too large to describe")
        recordDeopt("heap-too-large")
        None
      case Some(rc) =>
        val program = StageTimers.time("native/program")(compiledFor(rc, instance.module))
        val unsupported = program.scan
        val heapSlack =
          if unsupported.hasGrowHeapEcalli then Some(StageTimers.time("native/describe")(RecompilerMemory.describeWithHeapSlack(instance)))
          else None

        // Every reason this program cannot run natively, checked before any
        // work is done. `None` means "run it".
        val deoptReason: Option[String] =
          if program.invalidBlob then Some("invalid-blob")
          else if unsupported.hasEcalli && hostCalls.isEmpty then Some("ecalli-no-dispatcher")
          else if heapSlack.exists(_.heapRegionIndex == -1) then Some("grow-heap-no-heap-region")
          else if heapSlack.exists(ht => ht.maxHeapSize - currentHeapSize(instance) > ht.slackBytes) then
            Some("grow-heap-slack-cap")
          else if program.block == null then Some(program.whyNot)
          else None

        deoptReason match
          case Some(reason) =>
            logger.debug(s"NativeRunner: deopt to interpreter — $reason")
            recordDeopt(reason)
            None
          case None =>
            program.entryIndex(entryPc) match
              case None =>
                logger.debug(s"NativeRunner: deopt to interpreter — entryPc=$entryPc is not a decoded instruction boundary")
                recordDeopt("invalid-entry-pc")
                None
              case Some(entryIndex) =>
                val blk = program.block
                locally {
                    val describedWithHeap = heapSlack
                    val described = StageTimers.time("native/describe")(describedWithHeap.map(_.described).getOrElse(RecompilerMemory.describe(instance)))
                    val regs = Array.tabulate(13)(instance.reg)
                    // Guest memory is laid out by guest address (see PvmRecompiler.ExecContext).
                    val regions = described.regions.map(r => new PvmRecompiler.Region(r.base, r.len, r.base, r.writable))
                    val backing = described.backing

                    val (out, finalRegionLens, heapWrittenBack) = StageTimers.time("native/execute")(
                      executeInContext(
                        rc, instance, blk, regs, instance.gas, regions, backing, described.pageShift, entryIndex,
                        hostCalls, describedWithHeap, deadlineMillis, program.entryIndex
                      )
                    )

                    // Write registers back onto the instance (callers read via instance.reg).
                    for i <- 0 until 13 do instance.setReg(i, regs(i))

                    // Write gas back. Mirrors VectorConformanceSpec's parity convention: the
                    // recompiler's out.gasRemaining is the RAW remaining gas, same meaning as
                    // instance.gas after an interpreter run.
                    instance.setGas(out.gasRemaining)

                    // Write PC back so callers that inspect instance.programCounter after a
                    // run (mirroring the interpreter path) see the same final value.
                    instance.setNextProgramCounter(ProgramCounter(out.pc.toInt))

                    // Write only WRITABLE regions' bytes back — RO regions (RO data, aux/args)
                    // are never mutated by a correct program, and BasicMemory.setMemorySlice
                    // rejects writes to read-only ranges outright.
                    StageTimers.time("native/writeback")(described.regions.zip(finalRegionLens).zipWithIndex.foreach { case ((region, finalLen), idx) =>
                      if region.writable && !(heapWrittenBack && describedWithHeap.exists(_.heapRegionIndex == idx)) then
                        val slice = backing.slice(region.bufOffset.toInt, region.bufOffset.toInt + finalLen.toInt)
                        instance.basicMemory.setMemorySlice(UInt(region.base.toInt), slice)
                    })

                    val outcome = out.exit match
                      case PvmRecompiler.EXIT_HALT => RunOutcome.Halt
                      case PvmRecompiler.EXIT_PANIC => RunOutcome.Panic
                      case PvmRecompiler.EXIT_OOG => RunOutcome.OutOfGas
                      case PvmRecompiler.EXIT_FAULT => RunOutcome.PageFault(out.faultPage)
                      case other => throw new IllegalStateException(s"NativeRunner: unrecognized recompiler exit code $other")
                    recordNative()
                    Some(outcome)
                }

  /** A module's native code, built the first time the module runs and kept on the module for every later
    * invocation: decoding, translating and assembling a service is far more work than running it once.
    *
    * `block` is null when the backend rejected the program (it then always runs interpreted). The executable
    * memory is released when the module itself is collected, which cannot happen while an instance of it runs.
    */
  private final class CompiledProgram(
      val scan: UnsupportedOpcodeScan,
      val invalidBlob: Boolean,
      private val byteOffsetToIndex: RecompilerAbi.OffsetIndex,
      val block: PvmRecompiler#Block,
      val whyNot: String = "compile-null",
      blockGas: Array[Long] = Array.emptyLongArray
  ):
    def blockGasAt(index: Int): Long = blockGas(index)

    def waitsForBudget: Boolean = whyNot == OverBudget

    def entryIndex(pc: Int): Option[Int] = byteOffsetToIndex.get(pc)

    @volatile private var directTemplate: DirectTemplate = null

    /** The module's memory template, built on the first direct run. */
    def template(module: InterpretedModule): DirectTemplate =
      var t = directTemplate
      if t == null then
        t = buildTemplate(module, scan)
        directTemplate = t
      t

  /** What a module's guest memory looks like before its arguments are added, worked out once from a real
    * interpreter instance (so the layout rules stay in one place) and reused by every direct run.
    *
    * `contents(i)` are region i's leading bytes, the rest being zero; null is an all-zero region (the stack).
    */
  private final class DirectTemplate(
      val regions: Array[PvmRecompiler.Region],
      val contents: Array[Array[Byte]],
      val pageShift: Int,
      val heap: Option[HeapTemplate],
      val deopt: Option[String],
      val argsBase: Long,
      val argsCapacity: Long
  )

  private final case class HeapTemplate(
      regionIndex: Int,
      heapBase: Long,
      initialHeapSize: Long,
      maxHeapSize: Long,
      pageBytes: Long,
      upperPageBound: Long
  )

  /** `bytes` without its trailing zeros; null when it is all zeros. */
  private def trimZeros(bytes: Array[Byte]): Array[Byte] =
    var n = bytes.length
    while n > 0 && bytes(n - 1) == 0 do n -= 1
    if n == 0 then null else if n == bytes.length then bytes else java.util.Arrays.copyOf(bytes, n)

  private def buildTemplate(module: InterpretedModule, scan: UnsupportedOpcodeScan): DirectTemplate =
    val instance = InterpretedInstance.fromModule(module)
    // Always with the heap: growing it needs no room set aside, only the region's length and pages opened.
    val withHeap = RecompilerMemory.describeWithHeapSlack(instance)
    val described = withHeap.described
    val heapUsable =
      withHeap.heapRegionIndex != -1 && withHeap.maxHeapSize - currentHeapSize(instance) <= withHeap.slackBytes
    val deopt =
      if heapUsable || !scan.hasGrowHeapEcalli then None
      else if withHeap.heapRegionIndex == -1 then Some("grow-heap-no-heap-region")
      else Some("grow-heap-slack-cap")
    val regions = described.regions.map(r => new PvmRecompiler.Region(r.base, r.len, r.base, r.writable))
    val contents = described.regions.map { r =>
      trimZeros(java.util.Arrays.copyOfRange(described.backing, r.bufOffset.toInt, (r.bufOffset + r.len).toInt))
    }
    val heap =
      if !heapUsable then None
      else
        Some(
          HeapTemplate(
            withHeap.heapRegionIndex, withHeap.heapBase, currentHeapSize(instance), withHeap.maxHeapSize,
            1L << described.pageShift, instance.growHeapPageBounds._2
          )
        )
    val map = module.memoryMap
    instance.basicMemory.release()
    new DirectTemplate(
      regions, contents, described.pageShift, heap, deopt,
      map.auxDataAddress.toLong & 0xFFFFFFFFL, map.auxDataSize.toLong & 0xFFFFFFFFL
    )

  /** Heap growth for a direct run: the same arithmetic as `BasicMemory.sbrk`, with no interpreter memory to
    * keep in step. */
  private final class DirectHeapGrowth(h: HeapTemplate, reservation: InstanceMemoryBudget.Reservation)
      extends NativeInstanceWrapper.HeapGrowth:
    private var heapSize = h.initialHeapSize

    override def heapRegionIndex: Int = h.regionIndex

    override def pageBounds(): (Long, Long) = ((h.heapBase + heapSize) / h.pageBytes, h.upperPageBound)

    override def growPages(deltaPages: Long): Long =
      if deltaPages <= 0 then -1L
      else
        val grown = heapSize + deltaPages * h.pageBytes
        if grown > 0xFFFFFFFFL || grown > h.maxHeapSize then
          throw new IllegalStateException(
            s"grow_heap invariant violated: growing the heap to $grown bytes after " +
              "GrowHeapHostCall accepted the request against the page bounds"
          )
        // Counted against the instance memory budget, like the interpreter's heap; may wait for other runs.
        if reservation != null then reservation.grow(deltaPages * h.pageBytes)
        heapSize = grown
        alignUpLong(h.heapBase + grown, h.pageBytes) - h.heapBase

  /** A finished direct run: how it ended, the gas left, and `mem[r7 ..+ r8]` when it halted with that range
    * readable (else empty). */
  final case class DirectResult(outcome: RunOutcome, gasRemaining: Long, output: Array[Byte])

  /** Run `module` natively from its entry point without building an interpreter instance: guest memory is
    * assembled in the thread's execution context straight from the module's template and `args`, and the output
    * is read from it. `regs` are the thirteen initial registers; heap growth is taken from `reservation`.
    *
    * None when this run has to go the long way (no library, a program the backend rejects, arguments larger
    * than the argument region, ...): the caller then builds an instance and uses [[run]], which may in turn fall
    * back to the interpreter.
    */
  def runDirect(
      module: InterpretedModule,
      args: Array[Byte],
      regs: Array[Long],
      gas: Long,
      entryPc: Int,
      dispatcher: HostCallDispatcher,
      preDispatch: Option[() => Unit],
      reservation: InstanceMemoryBudget.Reservation = null,
      deadlineMillis: Option[Long] = None
  ): Option[DirectResult] =
    recompiler.flatMap { rc =>
      val program = compiledFor(rc, module)
      if program.invalidBlob || program.block == null then None
      else
        val template = program.template(module)
        val pageBytes = 1L << template.pageShift
        val argsLen = alignUpLong(args.length.toLong, pageBytes)
        if template.deopt.isDefined || argsLen > template.argsCapacity then None
        else
          program.entryIndex(entryPc).map { entryIndex =>
            val (regions, contents) =
              if args.isEmpty then (template.regions, template.contents)
              else
                (
                  template.regions :+ new PvmRecompiler.Region(template.argsBase, argsLen, template.argsBase, false),
                  template.contents :+ args
                )
            var wrapper: NativeInstanceWrapper = null
            var slices: GasSlices = null
            val handler: PvmRecompiler.HostCallHandler = (hostCallId: Long, _: Int) =>
              if slices != null then slices.restore()
              val gasCost = dispatcher.getGasCost(hostCallId.toInt, wrapper)
              val newGas = wrapper.gas - gasCost
              wrapper.setGas(newGas)
              if newGas < 0 then PvmRecompiler.HOST_OOG
              else
                try
                  preDispatch.foreach(_.apply())
                  if StageTimers.enabled then
                    StageTimers.time(s"pvm/host-$hostCallId")(dispatcher.dispatch(hostCallId.toInt, wrapper))
                  else dispatcher.dispatch(hostCallId.toInt, wrapper)
                  if wrapper.isForcedOutOfGas then PvmRecompiler.HOST_OOG_NEXT
                  else
                    if slices != null then slices.cut()
                    PvmRecompiler.HOST_CONTINUE
                catch
                  case _: HostCallPanic => PvmRecompiler.HOST_PANIC
            val live = StageTimers.time("native/setup") {
              val reused = threadContext(rc).tryBeginDirect(
                program.block, regs, gas, regions, contents, template.pageShift, entryIndex, handler
              )
              if reused != null then reused
              else rc.beginDirectOnce(program.block, regs, gas, regions, contents, template.pageShift, entryIndex, handler)
            }
            try
              wrapper = new NativeInstanceWrapper(live, template.heap.map(new DirectHeapGrowth(_, reservation)))
              slices = deadlineMillis.map(new GasSlices(live, _, program.entryIndex)).orNull
              val out = StageTimers.time("native/run")(if slices != null then slices.run() else live.run())
              val outcome = out.exit match
                case PvmRecompiler.EXIT_HALT => RunOutcome.Halt
                case PvmRecompiler.EXIT_PANIC => RunOutcome.Panic
                case PvmRecompiler.EXIT_OOG => RunOutcome.OutOfGas
                case PvmRecompiler.EXIT_FAULT => RunOutcome.PageFault(out.faultPage)
                case other => throw new IllegalStateException(s"NativeRunner: unrecognized recompiler exit code $other")
              val output =
                if outcome == RunOutcome.Halt then
                  // o = mem[w7 ..+ w8] when readable, else empty; a register >= 2^32 names no readable range.
                  val r7 = wrapper.reg(7)
                  val r8 = wrapper.reg(8)
                  val len = r8.toInt
                  if (r7 >>> 32) == 0 && (r8 >>> 32) == 0 && len >= 0 && wrapper.isMemoryReadable(r7.toInt, len) then
                    wrapper.readBytes(r7.toInt, len).getOrElse(Array.emptyByteArray)
                  else Array.emptyByteArray
                else Array.emptyByteArray
              recordNative()
              DirectResult(outcome, out.gasRemaining, output)
            finally StageTimers.time("native/close")(live.close())
          }
    }

  private[pvmhost] val nativeCode = NativeCodeBudget(NativeCodeBudget.ProcessBytes)

  private final val OverBudget = "native-code-budget"
  private final val TooLarge = "too-large"
  private final val TooCostly = "too-costly-to-price"

  private def settled(artifact: AnyRef): Boolean = artifact match
    case p: CompiledProgram => !p.waitsForBudget || !nativeCode.hasRoomFor(RecompilerAbi.MaxNativeCodeBytes)
    case _ => false

  private val cleaner = java.lang.ref.Cleaner.create()
  private val compiles = new AtomicLong(0)

  /** Programs compiled so far; an invocation that finds its module already compiled does not count. */
  def compileCount: Long = compiles.get()

  private def compiledFor(rc: PvmRecompiler, module: InterpretedModule): CompiledProgram =
    module.nativeArtifact match
      case p: CompiledProgram if settled(p) => p
      case _ =>
        module.synchronized {
          module.nativeArtifact match
            case p: CompiledProgram if settled(p) => p
            case _ =>
              val p = compileProgram(rc, module)
              module.nativeArtifact = p
              p
        }

  private def compileProgram(rc: PvmRecompiler, module: InterpretedModule): CompiledProgram =
    val blob = module.blob
    val scan = scanUnsupportedOpcodes(blob.code, blob.bitmask)
    val none = RecompilerAbi.OffsetIndex.Empty
    if !module.isBlobStructurallyValid then new CompiledProgram(scan, true, none, null)
    else if RecompilerAbi.markedInstructions(blob.code, blob.bitmask) > RecompilerAbi.MaxNativeInstructions then
      logger.debug("NativeRunner: the program has too many instructions to compile; it will run interpreted")
      new CompiledProgram(scan, false, none, null, TooLarge)
    else if !nativeCode.hasRoomFor(RecompilerAbi.MaxNativeCodeBytes) then
      new CompiledProgram(scan, false, none, null, OverBudget)
    else
      RecompilerAbi.prepareWithin(blob.code, blob.bitmask, blob.jumpTable, RecompilerAbi.MaxPreparedBlockGas) match
        case None =>
          logger.debug("NativeRunner: the program's blocks cost too much to price ahead; it will run interpreted")
          new CompiledProgram(scan, false, none, null, TooCostly)
        case Some(prepared) => compilePrepared(rc, module, scan, prepared)

  private def compilePrepared(
      rc: PvmRecompiler,
      module: InterpretedModule,
      scan: UnsupportedOpcodeScan,
      prepared: RecompilerAbi.PreparedProgram
  ): CompiledProgram =
    val none = RecompilerAbi.OffsetIndex.Empty
    val blk = rc.compile(
      prepared.opcodes, prepared.a, prepared.b, prepared.c,
      prepared.pc, prepared.imm, prepared.imm2, prepared.blockGas, prepared.jumpTable, prepared.codeLen
    )
    compiles.incrementAndGet()
    if !blk.isValid then
      logger.debug("NativeRunner: pvm_compile rejected the program (null block); it will run interpreted")
      new CompiledProgram(scan, false, none, null)
    else
      val codeBytes = blk.bytes()
      nativeCode.add(codeBytes)
      module.nativeBytes = codeBytes
      module.nativeHeapBytes = prepared.byteOffsetToIndex.bytes
      cleaner.register(module, () => { blk.close(); nativeCode.remove(codeBytes) })
      new CompiledProgram(scan, false, prepared.byteOffsetToIndex, blk, blockGas = prepared.blockGas)

  /** Each thread keeps one execution context (cells, upcall stub, guest-memory mapping) across its runs. */
  private val contexts = new ThreadLocal[PvmRecompiler#ExecContext]()

  private def threadContext(rc: PvmRecompiler): PvmRecompiler#ExecContext =
    var ctx = contexts.get()
    if ctx == null then
      ctx = rc.newContext()
      contexts.set(ctx)
    ctx

  private def executeInContext(
      rc: PvmRecompiler,
      instance: InterpretedInstance,
      blk: PvmRecompiler#Block,
      regs: Array[Long],
      gas: Long,
      regions: Array[PvmRecompiler.Region],
      backing: Array[Byte],
      pageShift: Int,
      entryIndex: Int,
      hostCalls: Option[(HostCallDispatcher, Option[() => Unit])],
      heapTracking: Option[RecompilerMemory.DescribedWithHeap],
      deadlineMillis: Option[Long],
      indexOfPc: Int => Option[Int]
  ): (PvmRecompiler.ExecResult, Array[Long], Boolean) =
    var wrapperRef: NativeInstanceWrapper = null
    var slices: GasSlices = null
    val handler: PvmRecompiler.HostCallHandler = hostCalls match
      case None => null
      case Some((dispatcher, preDispatch)) =>
        (hostCallId: Long, _: Int) =>
          if slices != null then slices.restore()
          val wrapper = wrapperRef
          val gasCost = dispatcher.getGasCost(hostCallId.toInt, wrapper)
          val newGas = wrapper.gas - gasCost
          wrapper.setGas(newGas)
          if newGas < 0 then PvmRecompiler.HOST_OOG
          else
            try
              preDispatch.foreach(_.apply())
              dispatcher.dispatch(hostCallId.toInt, wrapper)
              if wrapper.isForcedOutOfGas then PvmRecompiler.HOST_OOG_NEXT
              else
                if slices != null then slices.cut()
                PvmRecompiler.HOST_CONTINUE
            catch
              case _: HostCallPanic => PvmRecompiler.HOST_PANIC

    val live = StageTimers.time("native/setup") {
      // The thread's own context, unless it is busy with an outer run on this same thread.
      val reused = threadContext(rc).tryBegin(blk, regs, gas, regions, backing, pageShift, entryIndex, handler)
      if reused != null then reused
      else rc.beginOnce(blk, regs, gas, regions, backing, pageShift, entryIndex, handler)
    }
    wrapperRef = new NativeInstanceWrapper(live, heapTracking.map(new InstanceHeapGrowth(instance, _)))
    try
      slices = deadlineMillis.map(new GasSlices(live, _, indexOfPc)).orNull
      val result = StageTimers.time("native/run")(if slices != null then slices.run() else live.run())
      // Final region lengths, read from the LIVE region-table segment (which a
      // grow_heap host call may have mutated) BEFORE close() releases it.
      val finalLens = Array.tabulate(regions.length) { i =>
        if heapTracking.isDefined && i == heapTracking.get.heapRegionIndex then
          Integer.toUnsignedLong(live.regionsSegment().get(java.lang.foreign.ValueLayout.JAVA_INT, i * 16L + 4))
        else regions(i).len
      }
      val heapWrittenBack = heapTracking.exists { ht =>
        val idx = ht.heapRegionIndex
        val finalLen = finalLens(idx)
        val snapshotLen = regions(idx).len
        if finalLen > snapshotLen then
          StageTimers.time("native/writeback")(
            writeGrownHeapBack(instance, live.backingSegment(), regions(idx).bufOffset, regions(idx).base, snapshotLen, finalLen)
          )
          true
        else false
      }
      (result, finalLens, heapWrittenBack)
    finally StageTimers.time("native/close")(live.close())

  private val HeapCopyPieceBytes = 1 << 20
  private val ZeroPiece = new Array[Byte](HeapCopyPieceBytes)

  private def writeGrownHeapBack(
      instance: InterpretedInstance,
      backing: java.lang.foreign.MemorySegment,
      offset: Long,
      base: Long,
      snapshot: Long,
      length: Long
  ): Unit =
    val piece = new Array[Byte](math.min(length, HeapCopyPieceBytes.toLong).toInt)
    var at = 0L
    while at < length do
      val n = math.min(piece.length.toLong, length - at).toInt
      java.lang.foreign.MemorySegment.copy(backing, java.lang.foreign.ValueLayout.JAVA_BYTE, offset + at, piece, 0, n)
      if at < snapshot || java.util.Arrays.mismatch(piece, 0, n, ZeroPiece, 0, n) >= 0 then
        val data = if n == piece.length then piece else java.util.Arrays.copyOf(piece, n)
        instance.basicMemory.setMemorySlice(UInt((base + at).toInt), data)
      at += n

  private val MaxDescribedHeapBytes = 1L << 30

  private def currentHeapSize(instance: InterpretedInstance): Long =
    instance.basicMemory.heapSize.signed.toLong & 0xFFFFFFFFL

  private final class InstanceHeapGrowth(
      instance: InterpretedInstance,
      ht: RecompilerMemory.DescribedWithHeap
  ) extends NativeInstanceWrapper.HeapGrowth:

    override def heapRegionIndex: Int = ht.heapRegionIndex

    override def pageBounds(): (Long, Long) = instance.growHeapPageBounds

    override def growPages(deltaPages: Long): Long =
      if deltaPages <= 0 then -1L
      else
        val pageBytes = 1L << ht.described.pageShift
        val growBytes = deltaPages * pageBytes
        instance.basicMemory.sbrk(UInt(growBytes.toInt)) match
          case None =>
            throw new IllegalStateException(
              s"grow_heap invariant violated: BasicMemory.sbrk($growBytes) failed after " +
                "GrowHeapHostCall accepted the request against the page bounds"
            )
          case Some(_) =>
            val newHeapEnd = instance.basicMemory.heapEnd.toLong & 0xFFFFFFFFL
            alignUpLong(newHeapEnd, pageBytes) - ht.heapBase

  private def alignUpLong(v: Long, pageSize: Long): Long =
    val rem = v % pageSize
    if rem == 0 then v else v + (pageSize - rem)


  final case class GuestRun(outcome: InterruptKind, regs: Array[Long], gas: Long, pc: Long, gasChargedFlag: Boolean)

  private val guestRuns = new AtomicLong(0)

  def guestNativeCount: Long = guestRuns.get()

  val MaxGuestBlankWritablePages: Long = 1L << 16

  private val guestContexts = new ThreadLocal[PvmRecompiler#ExecContext]()

  private val ZeroPage = java.lang.foreign.MemorySegment.ofArray(new Array[Byte](GuestRam.PageSize))

  def runGuest(
      module: InterpretedModule,
      ram: GuestRam,
      regs: Array[Long],
      gas: Long,
      pc: Long,
      gasChargedFlag: Boolean,
      deadlineMillis: Option[Long]
  ): Option[GuestRun] =
    def deopt(reason: String): Option[GuestRun] =
      recordDeopt(s"guest-$reason")
      None
    deadlineMillis.foreach(d => if System.currentTimeMillis() > d then throw new RunDeadlinePassed(d))
    recompiler match
      case None => deopt("no-dylib")
      case Some(rc) =>
        val program = compiledFor(rc, module)
        if program.invalidBlob then deopt("invalid-blob")
        else if program.block == null then deopt(program.whyNot)
        else
          val entry = if pc >= 0 && pc <= Int.MaxValue then program.entryIndex(pc.toInt) else None
          entry match
            case None => deopt("invalid-entry-pc")
            case Some(entryIndex) =>
              val prepaid = if gasChargedFlag then program.blockGasAt(entryIndex) else 0L
              val runs = ram.accessRuns
              val materialised = ram.materialisedPages
              val writablePages = runs.iterator.filter(_.writable).map(_.count).sum
              if gas > Long.MaxValue - prepaid then deopt("gas-overflow")
              else if writablePages - materialised.length > MaxGuestBlankWritablePages then deopt("memory-too-large")
              else Some(runGuestNatively(rc, program, module, ram, runs, materialised, regs, gas + prepaid, entryIndex, deadlineMillis))

  private def runGuestNatively(
      rc: PvmRecompiler,
      program: CompiledProgram,
      module: InterpretedModule,
      ram: GuestRam,
      runs: IndexedSeq[GuestRam.Run],
      materialised: Array[Long],
      regs0: Array[Long],
      gas: Long,
      entryIndex: Int,
      deadlineMillis: Option[Long]
  ): GuestRun =
    import java.lang.foreign.{MemorySegment, ValueLayout}
    val shift = GuestRam.PageShift
    val regions = runs.map(r => new PvmRecompiler.Region(r.first << shift, r.count << shift, r.first << shift, r.writable)).toArray
    val contents = new Array[Array[Byte]](regions.length) // all zeros: the pages with data are copied in below
    val regs = java.util.Arrays.copyOf(regs0, 13)
    var ecalliId = -1L
    var ecalliPc = -1
    val handler: PvmRecompiler.HostCallHandler = (hostCallId: Long, at: Int) =>
      ecalliId = hostCallId
      ecalliPc = at
      PvmRecompiler.HOST_PANIC
    val ctx =
      var c = guestContexts.get()
      if c == null then
        c = rc.newContext()
        guestContexts.set(c)
      c
    val live =
      val reused = ctx.tryBeginDirect(program.block, regs, gas, regions, contents, shift, entryIndex, handler)
      if reused != null then reused
      else rc.beginDirectOnce(program.block, regs, gas, regions, contents, shift, entryIndex, handler)
    try
      val memory = live.backingSegment()
      materialised.foreach { page =>
        MemorySegment.copy(ram.pageBytes(page), 0, memory, ValueLayout.JAVA_BYTE, page << shift, GuestRam.PageSize)
      }
      val out = deadlineMillis match
        case Some(deadline) => new GasSlices(live, deadline, program.entryIndex).run()
        case None => live.run()
      runs.foreach { r =>
        if r.writable then
          var page = r.first
          while page < r.first + r.count do
            val at = memory.asSlice(page << shift, GuestRam.PageSize.toLong)
            val bytes = ram.pageBytes(page)
            if bytes != null then MemorySegment.copy(at, ValueLayout.JAVA_BYTE, 0L, bytes, 0, GuestRam.PageSize)
            else if at.mismatch(ZeroPage) != -1L then
              val fresh = new Array[Byte](GuestRam.PageSize)
              MemorySegment.copy(at, ValueLayout.JAVA_BYTE, 0L, fresh, 0, GuestRam.PageSize)
              ram.storePage(page, fresh)
            page += 1
      }
      live.close() // copies the registers into `regs`
      guestRuns.incrementAndGet()
      val gasLeft = out.gasRemaining
      out.exit match
        case PvmRecompiler.EXIT_PANIC if ecalliPc >= 0 =>
          val blob = module.blob
          val (_, length) = InstructionDecoder.decode(blob.code, blob.bitmask, ecalliPc)
          GuestRun(InterruptKind.Ecalli(UInt(ecalliId.toInt)), regs, gasLeft, ecalliPc.toLong + math.max(1, length), true)
        case PvmRecompiler.EXIT_HALT => GuestRun(InterruptKind.Finished, regs, gasLeft, 0L, true)
        case PvmRecompiler.EXIT_PANIC => GuestRun(InterruptKind.Panic, regs, gasLeft, 0L, true)
        case PvmRecompiler.EXIT_OOG => GuestRun(InterruptKind.OutOfGas, regs, gasLeft, out.pc, false)
        case PvmRecompiler.EXIT_FAULT =>
          val info = SegfaultInfo(UInt(out.faultPage.toInt), UInt(GuestRam.PageSize))
          GuestRun(InterruptKind.Segfault(info), regs, gasLeft, out.pc, true)
        case other => throw new IllegalStateException(s"NativeRunner: unrecognized recompiler exit code $other")
    finally live.close()

  /** What a program needs from the runner beyond plain instruction emission */
  private final case class UnsupportedOpcodeScan(
      hasEcalli: Boolean,
      hasGrowHeapEcalli: Boolean
  )

  private def scanUnsupportedOpcodes(code: Array[Byte], bitmask: Array[Byte]): UnsupportedOpcodeScan =
    var off = 0
    var hasEcalli = false
    var hasGrowHeapEcalli = false
    while off < code.length && !(hasEcalli && hasGrowHeapEcalli) do
      val (instr, skip) = InstructionDecoder.decode(code, bitmask, off)
      instr match
        case e: Instruction.Ecalli =>
          hasEcalli = true
          if e.hostId == HostCall.GROW_HEAP.toLong then hasGrowHeapEcalli = true
        case _ => ()
      off += math.max(1, skip)
    UnsupportedOpcodeScan(hasEcalli, hasGrowHeapEcalli)
