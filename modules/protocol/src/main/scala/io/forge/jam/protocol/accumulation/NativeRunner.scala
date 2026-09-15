package io.forge.jam.protocol.accumulation

import io.forge.jam.pvm.{ExecutionMode, Instruction}
import io.forge.jam.pvm.engine.InterpretedInstance
import io.forge.jam.pvm.native_.PvmRecompiler
import io.forge.jam.pvm.program.InstructionDecoder
import io.forge.jam.pvm.recompiler.{RecompilerAbi, RecompilerMemory}
import io.forge.jam.pvm.types.ProgramCounter
import io.forge.jam.protocol.refine.HostCallDispatcher
import com.typesafe.scalalogging.StrictLogging
import spire.math.UInt

import java.nio.file.{Files, Path}

object NativeRunner extends StrictLogging:

  enum RunOutcome:
    case Halt, Panic, OutOfGas
    case PageFault(pageFaultAddress: Long)

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
        None
      case Some(rc) =>
        val blob = instance.module.blob

        val unsupported = scanUnsupportedOpcodes(blob.code, blob.bitmask)
        if (unsupported.hasEcalli || unsupported.hasSbrk) && hostCalls.isEmpty then
          logger.debug("NativeRunner: deopt to interpreter — program contains Ecalli/Sbrk and no HostCallDispatcher was supplied")
          None
        else
          val prepared = RecompilerAbi.prepareProgram(blob)
          prepared.byteOffsetToIndex.get(entryPc) match
            case None =>
              logger.debug(s"NativeRunner: deopt to interpreter — entryPc=$entryPc is not a decoded instruction boundary")
              None
            case Some(entryIndex) =>
              val blk = rc.compile(
                prepared.opcodes, prepared.a, prepared.b, prepared.c,
                prepared.pc, prepared.imm, prepared.imm2, prepared.jumpTable, prepared.codeLen
              )
              try
                if !blk.isValid then
                  logger.debug("NativeRunner: deopt to interpreter — pvm_compile returned an invalid block (unsupported opcode)")
                  None
                else
                  val describedWithHeap =
                    if unsupported.hasSbrk then Some(RecompilerMemory.describeWithHeapSlack(instance))
                    else None
                  val described = describedWithHeap.map(_.described).getOrElse(RecompilerMemory.describe(instance))
                  val regs = Array.tabulate(13)(instance.reg)
                  val regions = described.regions.map(r => new PvmRecompiler.Region(r.base, r.len, r.bufOffset, r.writable))
                  val backing = described.backing.clone()

                  val (out, finalRegionLens) = hostCalls match
                    case None =>
                      val r = rc.execute(blk, regs, instance.gas, regions, backing, described.pageShift, entryIndex)
                      (r, described.regions.map(_.len))
                    case Some((dispatcher, preDispatch)) =>
                      executeWithHostCalls(
                        rc, instance, blk, regs, instance.gas, regions, backing, described.pageShift, entryIndex,
                        dispatcher, preDispatch, describedWithHeap
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
                  described.regions.zip(finalRegionLens).foreach { case (region, finalLen) =>
                    if region.writable then
                      val slice = backing.slice(region.bufOffset.toInt, region.bufOffset.toInt + finalLen.toInt)
                      instance.basicMemory.setMemorySlice(UInt(region.base.toInt), slice)
                  }

                  val outcome = out.exit match
                    case PvmRecompiler.EXIT_HALT => RunOutcome.Halt
                    case PvmRecompiler.EXIT_PANIC => RunOutcome.Panic
                    case PvmRecompiler.EXIT_OOG => RunOutcome.OutOfGas
                    case PvmRecompiler.EXIT_FAULT => RunOutcome.PageFault(out.faultPage)
                    case other => throw new IllegalStateException(s"NativeRunner: unrecognized recompiler exit code $other")
                  Some(outcome)
              finally blk.close()

  private def executeWithHostCalls(
      rc: PvmRecompiler,
      instance: InterpretedInstance,
      blk: PvmRecompiler#Block,
      regs: Array[Long],
      gas: Long,
      regions: Array[PvmRecompiler.Region],
      backing: Array[Byte],
      pageShift: Int,
      entryIndex: Int,
      dispatcher: HostCallDispatcher,
      preDispatch: Option[() => Unit],
      heapTracking: Option[RecompilerMemory.DescribedWithHeap]
  ): (PvmRecompiler.ExecResult, Array[Long]) =
    var wrapperRef: NativeInstanceWrapper = null
    val handler: PvmRecompiler.HostCallHandler = (hostCallId: Long, _pc: Int) =>
      val wrapper = wrapperRef
      val gasCost = dispatcher.getGasCost(hostCallId.toInt, wrapper)
      val newGas = wrapper.gas - gasCost
      wrapper.setGas(newGas)
      if newGas < 0 then PvmRecompiler.HOST_OOG
      else
        try
          preDispatch.foreach(_.apply())
          dispatcher.dispatch(hostCallId.toInt, wrapper)
          PvmRecompiler.HOST_CONTINUE
        catch
          case _: RuntimeException => PvmRecompiler.HOST_PANIC

    def exceedsSlackCap(ht: RecompilerMemory.DescribedWithHeap, newHeapEndCandidate: Long): Boolean =
      val pageSize = 1L << ht.described.pageShift
      val newRegionLenAligned = alignUpLong(newHeapEndCandidate, pageSize) - ht.heapBase
      val initialHeapRegionLen = regions(ht.heapRegionIndex).len
      val backingCapacity = initialHeapRegionLen + slackReservedFor(ht)
      newRegionLenAligned > backingCapacity

    val sbrkHandler: Option[PvmRecompiler.SbrkCallHandler] = heapTracking.map { ht =>
      (dst: Int, size: Long, _pc: Int) =>
        val wrapper = wrapperRef
        val sizeU32 = UInt((size & 0xFFFFFFFFL).toInt)
        val curHeapSize = instance.basicMemory.heapSize.signed.toLong & 0xFFFFFFFFL
        val newHeapSizeLong = curHeapSize + (sizeU32.signed.toLong & 0xFFFFFFFFL)
        val slackCapExceeded =
          sizeU32 != UInt(0) &&
            newHeapSizeLong <= 0xFFFFFFFFL &&
            newHeapSizeLong <= ht.maxHeapSize &&
            exceedsSlackCap(ht, ht.heapBase + newHeapSizeLong)

        if slackCapExceeded then
          logger.warn(
            s"NativeRunner: SBRK SLACK-CAP EXCEEDED (divergence guard) — " +
              s"heapBase=0x${ht.heapBase.toHexString} requestedNewHeapSize=$newHeapSizeLong " +
              s"maxHeapSize=${ht.maxHeapSize}. This is a documented native-recompiler " +
              "limitation (256 MiB slack cap), NOT an interpreter-parity failure — see " +
              "docs/superpowers/specs/2026-07-03-recompiler-host-calls-plan.md 'Sbrk memory model'."
          )
          PvmRecompiler.HOST_PANIC
        else
          instance.basicMemory.sbrk(sizeU32) match
            case None =>
              PvmRecompiler.HOST_PANIC
            case Some(oldHeapEnd) =>
              val newHeapEndAuthoritative = instance.basicMemory.heapEnd.toLong & 0xFFFFFFFFL
              val pageSize = 1L << ht.described.pageShift
              val newRegionLenAligned = alignUpLong(newHeapEndAuthoritative, pageSize) - ht.heapBase
              if sizeU32 != UInt(0) then wrapper.growRegion(ht.heapRegionIndex, newRegionLenAligned)
              wrapper.setReg(dst, oldHeapEnd.toLong & 0xFFFFFFFFL)
              PvmRecompiler.HOST_CONTINUE
    }

    val live = sbrkHandler match
      case Some(sh) => rc.executeLive(blk, regs, gas, regions, backing, pageShift, entryIndex, handler, sh)
      case None => rc.executeLive(blk, regs, gas, regions, backing, pageShift, entryIndex, handler)
    wrapperRef = new NativeInstanceWrapper(live)
    try
      val result = live.run()
      // Final region lengths, read from the LIVE region-table segment (which
      // an Sbrk upcall may have mutated) BEFORE close() releases it.
      val finalLens = Array.tabulate(regions.length) { i =>
        if heapTracking.isDefined && i == heapTracking.get.heapRegionIndex then
          Integer.toUnsignedLong(live.regionsSegment().get(java.lang.foreign.ValueLayout.JAVA_INT, i * 16L + 4))
        else regions(i).len
      }
      (result, finalLens)
    finally live.close()

  private def alignUpLong(v: Long, pageSize: Long): Long =
    val rem = v % pageSize
    if rem == 0 then v else v + (pageSize - rem)

  private def slackReservedFor(ht: RecompilerMemory.DescribedWithHeap): Long =
    val room = math.max(0L, ht.maxHeapSize - (ht.initialHeapEnd - ht.heapBase))
    val pageSize = 1L << ht.described.pageShift
    alignUpLong(math.min(room, RecompilerMemory.MaxHeapSlackBytes), pageSize)

  private final case class UnsupportedOpcodeScan(hasEcalli: Boolean, hasSbrk: Boolean)

  private def scanUnsupportedOpcodes(code: Array[Byte], bitmask: Array[Byte]): UnsupportedOpcodeScan =
    var off = 0
    var hasEcalli = false
    var hasSbrk = false
    while off < code.length && !(hasEcalli && hasSbrk) do
      val (instr, skip) = InstructionDecoder.decode(code, bitmask, off)
      instr match
        case _: Instruction.Ecalli => hasEcalli = true
        case _: Instruction.Sbrk => hasSbrk = true
        case _ => ()
      off += math.max(1, skip)
    UnsupportedOpcodeScan(hasEcalli, hasSbrk)
