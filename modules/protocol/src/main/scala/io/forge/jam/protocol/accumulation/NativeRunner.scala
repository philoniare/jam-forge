package io.forge.jam.protocol.accumulation

import io.forge.jam.pvm.{ExecutionMode, Instruction}
import io.forge.jam.pvm.engine.{InterpretedInstance, InterpretedModule}
import io.forge.jam.pvm.memory.InstanceMemoryBudget
import io.forge.jam.pvm.native_.PvmRecompiler
import io.forge.jam.pvm.program.InstructionDecoder
import io.forge.jam.pvm.recompiler.{RecompilerAbi, RecompilerMemory}
import io.forge.jam.pvm.types.ProgramCounter
import io.forge.jam.protocol.HostCallPanic
import io.forge.jam.protocol.pipeline.StageTimers
import io.forge.jam.protocol.refine.HostCallDispatcher
import com.typesafe.scalalogging.StrictLogging
import spire.math.UInt

import java.nio.file.{Files, Path}
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
      s"NativeRunner: ${nativeCount} native, ${compileCount} compiled, ${deoptCount} deopt (reasons: ${deoptReasons.mkString(", ")})"
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
      preDispatch: Option[() => Unit]
  ): Option[RunOutcome] =
    mode match
      case ExecutionMode.Interpreted => None
      case ExecutionMode.Recompiled => runRecompiled(instance, entryPc, hostCalls = Some((dispatcher, preDispatch)))

  private lazy val recompiler: Option[PvmRecompiler] =
    Option(System.getProperty("jam.pvm.recompiler.lib"))
      .map(Path.of(_))
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
      hostCalls: Option[(HostCallDispatcher, Option[() => Unit])]
  ): Option[RunOutcome] =
    recompiler match
      case None =>
        logger.debug("NativeRunner: deopt to interpreter — recompiler dylib not available (jam.pvm.recompiler.lib unset or file missing)")
        recordDeopt("no-dylib")
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
          else if program.block == null then Some("compile-null")
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

                    val (out, finalRegionLens, grownHeapBytes) = StageTimers.time("native/execute")(
                      executeInContext(
                        rc, instance, blk, regs, instance.gas, regions, backing, described.pageShift, entryIndex,
                        hostCalls, describedWithHeap
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
                      if region.writable then
                        val slice =
                          if describedWithHeap.exists(_.heapRegionIndex == idx) && grownHeapBytes.isDefined then
                            grownHeapBytes.get
                          else
                            backing.slice(region.bufOffset.toInt, region.bufOffset.toInt + finalLen.toInt)
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
      private val byteOffsetToIndex: Map[Int, Int],
      val block: PvmRecompiler#Block
  ):
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
      reservation: InstanceMemoryBudget.Reservation = null
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
            val handler: PvmRecompiler.HostCallHandler = (hostCallId: Long, _: Int) =>
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
                  else PvmRecompiler.HOST_CONTINUE
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
              val out = StageTimers.time("native/run")(live.run())
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

  private val cleaner = java.lang.ref.Cleaner.create()
  private val compiles = new AtomicLong(0)

  /** Programs compiled so far; an invocation that finds its module already compiled does not count. */
  def compileCount: Long = compiles.get()

  private def compiledFor(rc: PvmRecompiler, module: InterpretedModule): CompiledProgram =
    module.nativeArtifact match
      case p: CompiledProgram => p
      case _ =>
        module.synchronized {
          module.nativeArtifact match
            case p: CompiledProgram => p
            case _ =>
              val p = compileProgram(rc, module)
              module.nativeArtifact = p
              p
        }

  private def compileProgram(rc: PvmRecompiler, module: InterpretedModule): CompiledProgram =
    val blob = module.blob
    val scan = scanUnsupportedOpcodes(blob.code, blob.bitmask)
    if !module.isBlobStructurallyValid then new CompiledProgram(scan, true, Map.empty, null)
    else
      val prepared = RecompilerAbi.prepareProgram(blob)
      val blk = rc.compile(
        prepared.opcodes, prepared.a, prepared.b, prepared.c,
        prepared.pc, prepared.imm, prepared.imm2, prepared.blockGas, prepared.jumpTable, prepared.codeLen
      )
      compiles.incrementAndGet()
      if !blk.isValid then
        logger.debug("NativeRunner: pvm_compile rejected the program (null block); it will run interpreted")
        new CompiledProgram(scan, false, prepared.byteOffsetToIndex, null)
      else
        // The action must not reach the module, or the module would never be collected.
        cleaner.register(module, () => blk.close())
        new CompiledProgram(scan, false, prepared.byteOffsetToIndex, blk)

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
      heapTracking: Option[RecompilerMemory.DescribedWithHeap]
  ): (PvmRecompiler.ExecResult, Array[Long], Option[Array[Byte]]) =
    var wrapperRef: NativeInstanceWrapper = null
    val handler: PvmRecompiler.HostCallHandler = hostCalls match
      case None => null
      case Some((dispatcher, preDispatch)) =>
        (hostCallId: Long, _: Int) =>
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
              else PvmRecompiler.HOST_CONTINUE
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
      val result = StageTimers.time("native/run")(live.run())
      // Final region lengths, read from the LIVE region-table segment (which a
      // grow_heap host call may have mutated) BEFORE close() releases it.
      val finalLens = Array.tabulate(regions.length) { i =>
        if heapTracking.isDefined && i == heapTracking.get.heapRegionIndex then
          Integer.toUnsignedLong(live.regionsSegment().get(java.lang.foreign.ValueLayout.JAVA_INT, i * 16L + 4))
        else regions(i).len
      }
      val grownHeapBytes: Option[Array[Byte]] = heapTracking.flatMap { ht =>
        val idx = ht.heapRegionIndex
        val finalLen = finalLens(idx)
        val snapshotLen = regions(idx).len
        if finalLen > snapshotLen then
          val nativeOff = regions(idx).bufOffset
          val out = new Array[Byte](finalLen.toInt)
          java.lang.foreign.MemorySegment.copy(
            live.backingSegment(), java.lang.foreign.ValueLayout.JAVA_BYTE, nativeOff,
            out, 0, finalLen.toInt
          )
          Some(out)
        else None
      }
      (result, finalLens, grownHeapBytes)
    finally StageTimers.time("native/close")(live.close())

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
