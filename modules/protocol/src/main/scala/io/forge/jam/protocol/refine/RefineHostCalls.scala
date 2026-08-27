package io.forge.jam.protocol.refine

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.protocol.accumulation.{ConstantsBlob, HostCall, HostCallResult, PvmInstance}
import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.engine.{GuestInstance, InterpretedModule}
import io.forge.jam.pvm.memory.GuestRam
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.types.ProgramCounter
import scodec.Codec
import spire.math.ULong

/** Host-call dispatcher for the refine invocation. 
  * INVOKE (12) is not yet wired to the interpreter and currently reports WHAT;
  * every other refine host call is implemented per the spec.
  */
class RefineHostCalls(
    val context: RefineContext
):
  private val config: ChainConfig = context.config

  /** Cmaxpackageexports: maximum exported segments
    * per work package.
    */
  private val MaxPackageExports: Long = 3072L

  private def getReg(instance: PvmInstance, reg: Int): ULong =
    ULong(instance.reg(reg))

  private def setReg(instance: PvmInstance, reg: Int, value: ULong): Unit =
    instance.setReg(reg, value.signed)

  /** min(register, available) in unsigned-64 arithmetic, narrowed only after
    * the clamp (same helper as the accumulate dispatcher).
    */
  private def argClampedLen(
      instance: PvmInstance,
      reg: Int,
      available: Long
  ): Int =
    val v = getReg(instance, reg)
    val cap = ULong(available)
    (if v < cap then v else cap).toLong.toInt

  private def panic(message: String): Nothing =
    throw new RuntimeException(message)

  private def readMemory(
      instance: PvmInstance,
      address: Int,
      buffer: Array[Byte]
  ): Boolean =
    instance.readInto(address, buffer, 0, buffer.length)

  private def writeMemory(
      instance: PvmInstance,
      address: Int,
      data: Array[Byte]
  ): Boolean =
    if instance.isMemoryWritable(address, data.length) then
      instance.writeBytes(address, data)
    else false

  def getGasCost(hostCallId: Int, instance: PvmInstance): Long =
    hostCallId match
      case _ => 10L

  def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
    hostCallId match
      case HostCall.GAS               => handleGas(instance)
      case HostCall.FETCH             => handleFetch(instance)
      case HostCall.HISTORICAL_LOOKUP => handleHistoricalLookup(instance)
      case HostCall.EXPORT            => handleExport(instance)
      case HostCall.MACHINE           => handleMachine(instance)
      case HostCall.PEEK              => handlePeek(instance)
      case HostCall.POKE              => handlePoke(instance)
      case HostCall.PAGES             => handlePages(instance)
      case HostCall.INVOKE            => handleInvoke(instance)
      case HostCall.EXPUNGE           => handleExpunge(instance)
      case HostCall.LOG               => setReg(instance, 7, HostCallResult.WHAT)
      case _                          => setReg(instance, 7, HostCallResult.WHAT)

  /** gas (0): remaining gas (already net of this call's cost). */
  private def handleGas(instance: PvmInstance): Unit =
    setReg(instance, 7, ULong(instance.gas))

  // ===========================================================================
  // fetch (1) — Omega_Y with the refine parameterisation:
  // p = work package, n = zerohash, r = authorizer trace, i = item index,
  // ī = import segments, x̄ = extrinsic data, operands = none.
  // ===========================================================================

  private lazy val zeroEntropy: Array[Byte] = new Array[Byte](32)

  private def handleFetch(instance: PvmInstance): Unit =
    val selector = getReg(instance, 10)
    val outputAddr = getReg(instance, 7).toInt
    val r11 = getReg(instance, 11)
    val r12 = getReg(instance, 12)

    val data: Option[Array[Byte]] =
      RefineFetch.fetchValue(
        selector,
        r11,
        r12,
        config,
        Some(context.workPackage),
        Some(zeroEntropy),
        Some(context.authorizerTrace),
        Some(context.workItemIndex),
        Some(context.importSegments),
        Some(context.extrinsicData)
      )

    data match
      case None =>
        // Zero-length writable check is trivially true; just report NONE.
        setReg(instance, 7, HostCallResult.NONE)
      case Some(bytes) =>
        val actualOffset = argClampedLen(instance, 8, bytes.length.toLong)
        val actualLength =
          argClampedLen(instance, 9, (bytes.length - actualOffset).toLong)
        val slice = bytes.slice(actualOffset, actualOffset + actualLength)

        if !instance.isMemoryWritable(outputAddr, actualLength) then
          panic(
            s"Fetch PANIC: Output memory not writable at 0x${outputAddr.toHexString} len $actualLength"
          )

        if !writeMemory(instance, outputAddr, slice) then
          setReg(instance, 7, HostCallResult.OOB)
        else setReg(instance, 7, ULong(bytes.length))

  // ===========================================================================
  // historical_lookup (6) — Omega_H
  // ===========================================================================

  private def handleHistoricalLookup(instance: PvmInstance): Unit =
    val r7 = getReg(instance, 7)
    val ownServiceId = context.workItem.service.value.toLong

    val targetService: Option[Long] =
      if r7 == ULong.MaxValue then
        if context.accounts.serviceExists(ownServiceId) then Some(ownServiceId)
        else None
      else
        val id = r7.toLong
        if r7 <= ULong(0xffffffffL) && context.accounts.serviceExists(id) then
          Some(id)
        else None

    val hashAddr = getReg(instance, 8).toInt
    val outputAddr = getReg(instance, 9).toInt

    // v = error when the 32-byte hash is unreadable — checked before the
    // account-missing NONE case per Omega_H.
    val hashBuffer = new Array[Byte](32)
    if !readMemory(instance, hashAddr, hashBuffer) then
      panic(
        s"HistoricalLookup PANIC: Failed to read hash at 0x${hashAddr.toHexString}"
      )

    val preimage: Option[Array[Byte]] = targetService.flatMap { sid =>
      context.accounts.historicalLookup(
        sid,
        context.lookupAnchorTimeslot,
        Hash(hashBuffer)
      )
    }

    val dataSize: Long = preimage.map(_.length.toLong).getOrElse(0L)
    val actualOffset = argClampedLen(instance, 10, dataSize)
    val actualLength = argClampedLen(instance, 11, dataSize - actualOffset)

    if !instance.isMemoryWritable(outputAddr, actualLength) then
      panic(
        s"HistoricalLookup PANIC: Output memory not writable at 0x${outputAddr.toHexString} len $actualLength"
      )

    preimage match
      case None =>
        setReg(instance, 7, HostCallResult.NONE)
      case Some(v) =>
        val slice = v.slice(actualOffset, actualOffset + actualLength)
        if !writeMemory(instance, outputAddr, slice) then
          setReg(instance, 7, HostCallResult.OOB)
        else setReg(instance, 7, ULong(v.length.toLong))

  // ===========================================================================
  // export (7) — Omega_E
  // ===========================================================================

  private def handleExport(instance: PvmInstance): Unit =
    val addr = getReg(instance, 7).toInt
    val z = argClampedLen(instance, 8, Csegmentsize)

    if !instance.isMemoryReadable(addr, z) then
      panic(
        s"Export PANIC: memory not readable at 0x${addr.toHexString} len $z"
      )

    val buf = new Array[Byte](z)
    if !readMemory(instance, addr, buf) then
      panic(
        s"Export PANIC: failed to read memory at 0x${addr.toHexString} len $z"
      )

    if context.exportSegmentOffset + context.exports.size >= MaxPackageExports
    then setReg(instance, 7, HostCallResult.FULL)
    else
      val segment =
        if buf.length == Csegmentsize.toInt then buf
        else java.util.Arrays.copyOf(buf, Csegmentsize.toInt) // zero-pad
      setReg(
        instance,
        7,
        ULong(context.exportSegmentOffset + context.exports.size)
      )
      context.exports += segment

  // ===========================================================================
  // machine (8) — Omega_M
  // ===========================================================================

  private def handleMachine(instance: PvmInstance): Unit =
    val codeAddr = getReg(instance, 7)
    val codeLen = getReg(instance, 8)
    val pc = getReg(instance, 9).toLong

    // A length beyond the 32-bit address space can never be readable → panic.
    if codeLen > ULong(Int.MaxValue) ||
      !instance.isMemoryReadable(codeAddr.toInt, codeLen.toInt)
    then
      panic(
        s"Machine PANIC: code not readable at 0x${codeAddr.toLong.toHexString} len $codeLen"
      )

    val code = new Array[Byte](codeLen.toInt)
    if !readMemory(instance, codeAddr.toInt, code) then
      panic(s"Machine PANIC: failed to read code")

    // deblob validation: the guest blob is the raw jumptable+bitmask+code
    // format (no SPI memory header).
    val parsed = ProgramBlob
      .fromCodeAndJumpTable(
        data = code,
        roData = Array.empty,
        rwData = Array.empty,
        stackSize = 0,
        is64Bit = true
      )
      .flatMap(blob => InterpretedModule.create(blob).toOption)

    parsed match
      case None =>
        setReg(instance, 7, HostCallResult.HUH)
      case Some(module) =>
        val n = context.nextMachineIndex
        context.innerPvms.update(n, new InnerPvm(code, module, new GuestRam, pc))
        setReg(instance, 7, ULong(n))

  // ===========================================================================
  // peek (9) — Omega_P: outer[o..o+z) := guest[s..s+z)
  // ===========================================================================

  private def handlePeek(instance: PvmInstance): Unit =
    val n = getReg(instance, 7)
    val o = getReg(instance, 8)
    val s = getReg(instance, 9)
    val z = getReg(instance, 10)

    // Outer destination must be writable, else panic (checked first).
    if z > ULong(Int.MaxValue) ||
      !instance.isMemoryWritable(o.toInt, z.toInt)
    then panic(s"Peek PANIC: outer memory not writable at 0x${o.toLong.toHexString} len $z")

    context.innerPvms.get(n.toLong) match
      case None =>
        setReg(instance, 7, HostCallResult.WHO)
      case Some(guest) =>
        guest.ram.read(s.toLong, z.toInt) match
          case None =>
            setReg(instance, 7, HostCallResult.OOB)
          case Some(data) =>
            if !writeMemory(instance, o.toInt, data) then
              setReg(instance, 7, HostCallResult.OOB)
            else setReg(instance, 7, HostCallResult.OK)

  // ===========================================================================
  // poke (10) — Omega_O: guest[o..o+z) := outer[s..s+z)
  // ===========================================================================

  private def handlePoke(instance: PvmInstance): Unit =
    val n = getReg(instance, 7)
    val s = getReg(instance, 8)
    val o = getReg(instance, 9)
    val z = getReg(instance, 10)

    if z > ULong(Int.MaxValue) ||
      !instance.isMemoryReadable(s.toInt, z.toInt)
    then panic(s"Poke PANIC: outer memory not readable at 0x${s.toLong.toHexString} len $z")

    context.innerPvms.get(n.toLong) match
      case None =>
        setReg(instance, 7, HostCallResult.WHO)
      case Some(guest) =>
        if !guest.ram.isWritable(o.toLong, z.toLong) then
          setReg(instance, 7, HostCallResult.OOB)
        else
          val buf = new Array[Byte](z.toInt)
          if !readMemory(instance, s.toInt, buf) then
            setReg(instance, 7, HostCallResult.OOB)
          else
            guest.ram.write(o.toLong, buf)
            setReg(instance, 7, HostCallResult.OK)

  // ===========================================================================
  // pages (11) — Omega_Z
  // ===========================================================================

  private def handlePages(instance: PvmInstance): Unit =
    val n = getReg(instance, 7)
    val p = getReg(instance, 8).toLong
    val c = getReg(instance, 9).toLong
    val r = getReg(instance, 10).toLong

    context.innerPvms.get(n.toLong) match
      case None =>
        setReg(instance, 7, HostCallResult.WHO)
      case Some(guest) =>
        // p and c are u64 naturals; anything ≥ the page-count bound is HUH, so
        // the Long views are safe once past this check.
        val outOfBounds =
          getReg(instance, 8) + getReg(instance, 9) >= ULong(GuestRam.TotalPages) ||
            getReg(instance, 8) >= ULong(GuestRam.TotalPages) ||
            getReg(instance, 9) >= ULong(GuestRam.TotalPages)
        if r > 4 || p < 16 || outOfBounds then
          setReg(instance, 7, HostCallResult.HUH)
        else if r > 2 && !guest.ram.pagesAccessible(p, c) then
          setReg(instance, 7, HostCallResult.HUH)
        else
          guest.ram.applyPages(p, c, r)
          setReg(instance, 7, HostCallResult.OK)

  // ===========================================================================
  // invoke (12) — Omega_K
  // ===========================================================================

  private def handleInvoke(instance: PvmInstance): Unit =
    val n = getReg(instance, 7)
    val o = getReg(instance, 8).toInt

    // The 112-byte block (encode8(gas) ++ 13 × encode8(reg)) must be WRITABLE
    // (it is read now and written back after the run), else panic.
    if !instance.isMemoryWritable(o, 112) then
      panic(
        s"Invoke PANIC: gas/register block not writable at 0x${o.toHexString}"
      )

    val block = new Array[Byte](112)
    if !readMemory(instance, o, block) then
      panic(s"Invoke PANIC: failed to read gas/register block")

    context.innerPvms.get(n.toLong) match
      case None =>
        setReg(instance, 7, HostCallResult.WHO)
      case Some(guest) =>
        val guestGas = decodeLE8(block, 0)
        val guestRegs = Array.tabulate(13)(i => decodeLE8(block, 8 + 8 * i))

        val guestInstance = GuestInstance.create(
          guest.module,
          guest.ram,
          guestRegs,
          guestGas,
          ProgramCounter(guest.pc.toInt)
        )

        var outcome: InterruptKind = InterruptKind.Panic
        var running = true
        while running do
          guestInstance.run() match
            case Right(InterruptKind.Step) => // step tracing only; continue
            case Right(interrupt) =>
              outcome = interrupt
              running = false
            case Left(_) =>
              outcome = InterruptKind.Panic
              running = false

        // Write back gas' and registers' regardless of outcome (m* / mem* in
        // Omega_K applies to every non-panic, non-WHO case).
        putLE8(block, 0, guestInstance.gas)
        var i = 0
        while i < 13 do
          putLE8(block, 8 + 8 * i, guestInstance.regs(i))
          i += 1
        writeMemory(instance, o, block)

        // Guest pc: past the ecalli on HOST, at the faulting/halting
        // instruction otherwise.
        val pcNow: Long = outcome match
          case InterruptKind.Ecalli(_) =>
            guestInstance.nextProgramCounter
              .map(_.value.toLong)
              .getOrElse(guest.pc)
          case _ =>
            guestInstance.programCounter
              .map(_.value.toLong)
              .getOrElse(guest.pc)
        guest.pc = pcNow

        outcome match
          case InterruptKind.Ecalli(hostId) =>
            setReg(instance, 7, HostCallResult.HOST)
            setReg(instance, 8, ULong(hostId.toLong))
          case InterruptKind.Segfault(info) =>
            setReg(instance, 7, HostCallResult.FAULT)
            setReg(instance, 8, ULong(info.pageAddress.toLong))
          case InterruptKind.OutOfGas =>
            setReg(instance, 7, HostCallResult.OOG)
          case InterruptKind.Panic =>
            setReg(instance, 7, HostCallResult.PANIC)
          case _ =>
            setReg(instance, 7, HostCallResult.HALT)

  private def decodeLE8(buf: Array[Byte], offset: Int): Long =
    var v = 0L
    var i = 0
    while i < 8 do
      v |= (buf(offset + i).toLong & 0xff) << (8 * i)
      i += 1
    v

  private def putLE8(buf: Array[Byte], offset: Int, value: Long): Unit =
    var i = 0
    while i < 8 do
      buf(offset + i) = ((value >> (8 * i)) & 0xff).toByte
      i += 1

  // ===========================================================================
  // expunge (13) — Omega_X
  // ===========================================================================

  private def handleExpunge(instance: PvmInstance): Unit =
    val n = getReg(instance, 7)
    context.innerPvms.get(n.toLong) match
      case None =>
        setReg(instance, 7, HostCallResult.WHO)
      case Some(guest) =>
        setReg(instance, 7, ULong(guest.pc))
        context.innerPvms.remove(n.toLong)

/** Fetch-value resolution shared between the refine and is-authorized
  * dispatchers. Absent inputs (`None`) make
  * their selectors resolve to none → NONE, which is exactly how the
  * is-authorized context (only the work package present) restricts fetch.
  */
object RefineFetch:

  def fetchValue(
      selector: ULong,
      r11: ULong,
      r12: ULong,
      config: ChainConfig,
      workPackage: Option[WorkPackage],
      entropy: Option[Array[Byte]],
      authorizerTrace: Option[Array[Byte]],
      workItemIndex: Option[Int],
      importSegments: Option[IndexedSeq[IndexedSeq[Array[Byte]]]],
      extrinsicData: Option[IndexedSeq[IndexedSeq[Array[Byte]]]]
  ): Option[Array[Byte]] =
    inline def idx(r: ULong, size: Int): Option[Int] =
      if r < ULong(size.toLong) then Some(r.toInt) else None

    selector.toLong match
      case 0 => Some(ConstantsBlob.build(config))
      case 1 => entropy
      case 2 => authorizerTrace
      case 3 =>
        for
          xbar <- extrinsicData
          i <- idx(r11, xbar.size)
          j <- idx(r12, xbar(i).size)
        yield xbar(i)(j)
      case 4 =>
        for
          xbar <- extrinsicData
          i <- workItemIndex
          j <- idx(r11, xbar(i).size)
        yield xbar(i)(j)
      case 5 =>
        for
          ibar <- importSegments
          i <- idx(r11, ibar.size)
          j <- idx(r12, ibar(i).size)
        yield ibar(i)(j)
      case 6 =>
        for
          ibar <- importSegments
          i <- workItemIndex
          j <- idx(r11, ibar(i).size)
        yield ibar(i)(j)
      case 7 =>
        workPackage.map(encodeWorkPackage)
      case 8 =>
        workPackage.map(_.authorizerConfig.toArray)
      case 9 =>
        workPackage.map(_.authorization.toArray)
      case 10 =>
        workPackage.map(wp => encodeScodec(summon[Codec[Context]], wp.context))
      case 11 =>
        workPackage.map { wp =>
          val buffer = new java.io.ByteArrayOutputStream()
          buffer.write(JamCodecs.encodeCompactInteger(wp.items.size.toLong))
          wp.items.foreach(w => buffer.write(workItemSummary(w)))
          buffer.toByteArray
        }
      case 12 =>
        for
          wp <- workPackage
          i <- idx(r11, wp.items.size)
        yield workItemSummary(wp.items(i))
      case 13 =>
        for
          wp <- workPackage
          i <- idx(r11, wp.items.size)
        yield wp.items(i).payload.toArray
      case _ => None

  def encodeWorkPackage(wp: WorkPackage): Array[Byte] =
    encodeScodec(summon[Codec[WorkPackage]], wp)

  /** Hash of the encoded work package, blake(p). */
  def workPackageHash(wp: WorkPackage): Hash =
    Hashing.blake2b256(encodeWorkPackage(wp))

  private def encodeScodec[A](codec: Codec[A], value: A): Array[Byte] =
    codec.encode(value) match
      case scodec.Attempt.Successful(bits) => bits.toByteArray
      case scodec.Attempt.Failure(err) =>
        throw new RuntimeException(s"fetch encoding failed: $err")

  /** Work-item summary S(w):
    * encode4(service) ‖ codeHash ‖ encode8(refineGas) ‖ encode8(accGas) ‖
    * encode2(exportCount) ‖ encode2(#imports) ‖ encode2(#extrinsics) ‖
    * encode4(len(payload)).
    */
  def workItemSummary(w: WorkItem): Array[Byte] =
    val out = new Array[Byte](4 + 32 + 8 + 8 + 2 + 2 + 2 + 4)
    var off = 0
    putLE(out, off, w.service.value.toLong, 4); off += 4
    System.arraycopy(w.codeHash.bytes.toArray, 0, out, off, 32); off += 32
    putLE(out, off, w.refineGasLimit.toLong, 8); off += 8
    putLE(out, off, w.accumulateGasLimit.toLong, 8); off += 8
    putLE(out, off, w.exportCount.toLong, 2); off += 2
    putLE(out, off, w.importSegments.size.toLong, 2); off += 2
    putLE(out, off, w.extrinsic.size.toLong, 2); off += 2
    putLE(out, off, w.payload.length.toLong, 4)
    out

  private def putLE(buf: Array[Byte], offset: Int, value: Long, size: Int): Unit =
    var i = 0
    while i < size do
      buf(offset + i) = ((value >>> (8 * i)) & 0xff).toByte
      i += 1
