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
        if unsupported.hasSbrk then
          logger.debug("NativeRunner: deopt to interpreter — program contains Sbrk (no heap-growth support in the recompiler yet)")
          None
        else if unsupported.hasEcalli && hostCalls.isEmpty then
          logger.debug("NativeRunner: deopt to interpreter — program contains Ecalli and no HostCallDispatcher was supplied")
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
                  val described = RecompilerMemory.describe(instance)
                  val regs = Array.tabulate(13)(instance.reg)
                  val regions = described.regions.map(r => new PvmRecompiler.Region(r.base, r.len, r.bufOffset, r.writable))
                  val backing = described.backing.clone()

                  val out = hostCalls match
                    case None =>
                      rc.execute(blk, regs, instance.gas, regions, backing, described.pageShift, entryIndex)
                    case Some((dispatcher, preDispatch)) =>
                      executeWithHostCalls(rc, blk, regs, instance.gas, regions, backing, described.pageShift, entryIndex, dispatcher, preDispatch)

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
                  described.regions.foreach { region =>
                    if region.writable then
                      val slice = backing.slice(region.bufOffset.toInt, region.bufOffset.toInt + region.len.toInt)
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
      blk: PvmRecompiler#Block,
      regs: Array[Long],
      gas: Long,
      regions: Array[PvmRecompiler.Region],
      backing: Array[Byte],
      pageShift: Int,
      entryIndex: Int,
      dispatcher: HostCallDispatcher,
      preDispatch: Option[() => Unit]
  ): PvmRecompiler.ExecResult =
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

    val live = rc.executeLive(blk, regs, gas, regions, backing, pageShift, entryIndex, handler)
    wrapperRef = new NativeInstanceWrapper(live)
    try live.run()
    finally live.close()

  private final case class UnsupportedOpcodeScan(hasEcalli: Boolean, hasSbrk: Boolean)

  /** Scans the decoded program for `Ecalli`/`Sbrk` — the two opcodes the
    * recompiler cannot execute unconditionally (brief H0: "any Ecalli (10)
    * or Sbrk (101)"; H1 narrows this to "Ecalli is fine WITH a dispatcher,
    * Sbrk always deopts" — see `runRecompiled`). */
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
