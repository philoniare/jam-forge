package io.forge.jam.protocol.accumulation

import io.forge.jam.pvm.engine.{InterpretedInstance, InterpretedModule}
import io.forge.jam.pvm.native_.PvmRecompiler
import io.forge.jam.pvm.program.{JumpTable, ProgramBlob}
import io.forge.jam.pvm.recompiler.RecompilerAbi
import io.forge.jam.pvm.types.ProgramCounter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

class NativeInstanceWrapperSpec extends AnyFlatSpec with Matchers:

  private def libPath: Option[Path] =
    Option(System.getProperty("jam.pvm.recompiler.lib")).map(Path.of(_)).filter(Files.exists(_))

  private val RW_BASE = 0x20000
  private val RW_LEN = 4096
  private val PAGE_SHIFT = 12

  // ---- minimal hand-encoded program: load an immediate into r3, then trap ----
  // LoadImm64 opcode 20: [op, reg] ++ 8-byte imm LE. Trap opcode 0: [op].
  private def loadImmThenTrap(reg: Int, imm: Long): (Array[Byte], Array[Byte]) =
    val code = Array[Byte](20.toByte, reg.toByte) ++ Array.tabulate(8)(i => ((imm >>> (i * 8)) & 0xff).toByte) ++ Array[Byte](0)
    val bitmask = new Array[Byte]((code.length + 7) / 8)
    bitmask(0) = (bitmask(0) | 1 | (1 << 2)).toByte // instruction boundaries at byte 0 and byte 10
    (code, bitmask)

  private def regByte(a: Int, b: Int): Byte = ((a & 0xf) | ((b & 0xf) << 4)).toByte
  private def intLE(v: Int): Array[Byte] = Array.tabulate(4)(i => ((v >>> (i * 8)) & 0xff).toByte)

  private def storeThenLoadProgram(): (Array[Byte], Array[Byte]) =
    val storeInstr = Array[Byte](123.toByte, regByte(1, 0)) ++ intLE(0) // store r1 at [r0+0]
    val loadInstr = Array[Byte](130.toByte, regByte(2, 0)) ++ intLE(0) // load [r0+0] into r2
    val trapInstr = Array[Byte](0)
    val code = storeInstr ++ loadInstr ++ trapInstr
    val bitmask = new Array[Byte]((code.length + 7) / 8)
    val offsets = Seq(0, storeInstr.length, storeInstr.length + loadInstr.length)
    offsets.foreach(o => bitmask(o >> 3) = (bitmask(o >> 3) | (1 << (o & 7))).toByte)
    (code, bitmask)

  private def compileAndRunLive(
    rc: PvmRecompiler,
    code: Array[Byte],
    bitmask: Array[Byte],
    initRegs: Array[Long],
    gas: Long,
    regions: Array[PvmRecompiler.Region],
    backing: Array[Byte]
  ): (PvmRecompiler.ExecResult, NativeInstanceWrapper, PvmRecompiler#LiveExecution, PvmRecompiler#Block) =
    val pp = RecompilerAbi.prepareProgram(code, bitmask, JumpTable.Empty)
    val blk = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
    blk.isValid shouldBe true
    val live = rc.executeLive(blk, initRegs, gas, regions, backing, PAGE_SHIFT, 0)
    val out = live.run()
    (out, new NativeInstanceWrapper(live), live, blk)

  it should "honor a register set via the wrapper BEFORE run(), and expose the post-run value afterward" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val code = Array[Byte](0) // Trap
          val bitmask = Array[Byte](1)
          val pp = RecompilerAbi.prepareProgram(code, bitmask, JumpTable.Empty)
          val blk = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
          try
            blk.isValid shouldBe true
            val regs = Array.fill(13)(0L)
            val live = rc.executeLive(blk, regs, 100L, new Array[PvmRecompiler.Region](0), new Array[Byte](0), PAGE_SHIFT, 0)
            try
              val wrapper = new NativeInstanceWrapper(live)
              // Pre-run: set via the wrapper.
              wrapper.setReg(5, 0xDEADBEEFL)
              wrapper.reg(5) shouldBe 0xDEADBEEFL // wrapper reads back its own write immediately

              val out = live.run()
              out.exit shouldBe PvmRecompiler.EXIT_PANIC

              // Post-run: the wrapper still sees the value (Trap never touches r5).
              wrapper.reg(5) shouldBe 0xDEADBEEFL
            finally live.close()
          finally blk.close()
        finally rc.close()
  }

  it should "have wrapper post-run register reads match the native ExecResult (LoadImm64)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val (code, bitmask) = loadImmThenTrap(3, 0x1122334455667788L)
          val regs = Array.fill(13)(0L)
          val (out, wrapper, live, blk) =
            compileAndRunLive(rc, code, bitmask, regs, 100L, new Array[PvmRecompiler.Region](0), new Array[Byte](0))
          try
            out.exit shouldBe PvmRecompiler.EXIT_PANIC // program ends in Trap
            wrapper.reg(3) shouldBe 0x1122334455667788L
            wrapper.gas shouldBe out.gasRemaining
          finally
            live.close()
            blk.close()
        finally rc.close()
  }

  it should "have wrapper post-run reads see a native WRITE to memory (store-then-load round trip)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val (code, bitmask) = storeThenLoadProgram()
          val backing = new Array[Byte](RW_LEN)
          val regions = Array(new PvmRecompiler.Region(RW_BASE.toLong, RW_LEN.toLong, 0L, true))
          val regs = Array.fill(13)(0L)
          regs(0) = RW_BASE.toLong
          regs(1) = 0xCAFEBABEDEADBEEFL
          val (out, wrapper, live, blk) = compileAndRunLive(rc, code, bitmask, regs, 100L, regions, backing)
          try
            out.exit shouldBe PvmRecompiler.EXIT_PANIC // program ends in Trap
            wrapper.reg(2) shouldBe 0xCAFEBABEDEADBEEFL
            wrapper.readBytes(RW_BASE, 8) shouldBe defined
            val bytes = wrapper.readBytes(RW_BASE, 8).get
            val asLong = (0 until 8).foldLeft(0L)((acc, i) => acc | ((bytes(i).toLong & 0xff) << (i * 8)))
            asLong shouldBe 0xCAFEBABEDEADBEEFL
          finally
            live.close()
            blk.close()
        finally rc.close()
  }

  it should "have the wrapper's view after executeLive match a plain execute() call on the same program" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val (code, bitmask) = loadImmThenTrap(7, 0x0102030405060708L)
          val pp = RecompilerAbi.prepareProgram(code, bitmask, JumpTable.Empty)

          val blkA = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
          val copyRegs = Array.fill(13)(0L)
          val copyOut = try rc.execute(blkA, copyRegs, 100L) finally blkA.close()

          val blkB = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
          val liveRegs = Array.fill(13)(0L)
          val live = rc.executeLive(blkB, liveRegs, 100L, new Array[PvmRecompiler.Region](0), new Array[Byte](0), PAGE_SHIFT, 0)
          try
            val liveOut = live.run()
            val wrapper = new NativeInstanceWrapper(live)

            liveOut.exit shouldBe copyOut.exit
            liveOut.gasRemaining shouldBe copyOut.gasRemaining
            liveOut.pc shouldBe copyOut.pc
            wrapper.reg(7) shouldBe copyRegs(7)
            wrapper.gas shouldBe copyOut.gasRemaining
          finally
            live.close()
            blkB.close()
        finally rc.close()
  }

  private val roData = Array.fill(64)(0x11.toByte)
  private val rwData = Array.fill(RW_LEN)(0x22.toByte)

  private def buildInterpretedWrapper(): (InterpretedInstanceWrapper, io.forge.jam.pvm.recompiler.RecompilerMemory.Described) =
    val blob = ProgramBlob(
      code = Array[Byte](0), bitmask = Array[Byte](1), jumpTable = JumpTable.Empty,
      is64Bit = true, roData = roData, rwData = rwData, stackSize = 4096
    )
    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(e)  => fail(s"module create failed: $e")
    val inst = InterpretedInstance.fromModule(module, forceStepTracing = false)
    inst.setNextProgramCounter(ProgramCounter(0))
    val described = io.forge.jam.pvm.recompiler.RecompilerMemory.describe(inst)
    (new InterpretedInstanceWrapper(inst), described)

  private def buildNativeWrapperOverDescribed(
    rc: PvmRecompiler,
    described: io.forge.jam.pvm.recompiler.RecompilerMemory.Described
  ): (NativeInstanceWrapper, PvmRecompiler#LiveExecution, PvmRecompiler#Block) =
    val code = Array[Byte](0) // Trap — never executes before we inspect the wrapper
    val bitmask = Array[Byte](1)
    val pp = RecompilerAbi.prepareProgram(code, bitmask, JumpTable.Empty)
    val blk = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
    blk.isValid shouldBe true
    val regions = described.regions.map(r => new PvmRecompiler.Region(r.base, r.len, r.bufOffset, r.writable))
    val backing = described.backing.clone()
    val live = rc.executeLive(blk, Array.fill(13)(0L), 100L, regions, backing, described.pageShift, 0)
    (new NativeInstanceWrapper(live), live, blk)

  it should "match InterpretedInstanceWrapper on RO-region readability/writability" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val (interpWrapper, described) = buildInterpretedWrapper()
          val roRegion = described.regions.find(!_.writable).getOrElse(fail("expected a RO region"))
          val (nativeWrapper, live, blk) = buildNativeWrapperOverDescribed(rc, described)
          try
            val addr = roRegion.base.toInt
            interpWrapper.isMemoryReadable(addr, 8) shouldBe true
            nativeWrapper.isMemoryReadable(addr, 8) shouldBe true
            interpWrapper.isMemoryWritable(addr, 8) shouldBe false
            nativeWrapper.isMemoryWritable(addr, 8) shouldBe false
            interpWrapper.readByte(addr) shouldBe nativeWrapper.readByte(addr)
            nativeWrapper.writeByte(addr, 0x77) shouldBe false
          finally
            live.close()
            blk.close()
        finally rc.close()
  }

  it should "match InterpretedInstanceWrapper on RW-region readability/writability" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val (interpWrapper, described) = buildInterpretedWrapper()
          val rwRegion = described.regions.find(r => r.writable && r.base != described.regions.find(!_.writable).map(_.base).getOrElse(-1L))
            .getOrElse(fail("expected an RW region"))
          val (nativeWrapper, live, blk) = buildNativeWrapperOverDescribed(rc, described)
          try
            val addr = rwRegion.base.toInt
            interpWrapper.isMemoryReadable(addr, 8) shouldBe true
            nativeWrapper.isMemoryReadable(addr, 8) shouldBe true
            interpWrapper.isMemoryWritable(addr, 8) shouldBe true
            nativeWrapper.isMemoryWritable(addr, 8) shouldBe true
            nativeWrapper.writeByte(addr, 0x33) shouldBe true
            nativeWrapper.readByte(addr) shouldBe Some(0x33.toByte)
          finally
            live.close()
            blk.close()
        finally rc.close()
  }

  it should "match InterpretedInstanceWrapper on unmapped-address inaccessibility" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val (interpWrapper, described) = buildInterpretedWrapper()
          // An address far past every region (RO/RW/stack are all low/mid
          // addresses in the TINY memory map) — unmapped in both engines.
          val unmappedAddr = 0x7fff0000
          val (nativeWrapper, live, blk) = buildNativeWrapperOverDescribed(rc, described)
          try
            interpWrapper.isMemoryReadable(unmappedAddr, 8) shouldBe false
            nativeWrapper.isMemoryReadable(unmappedAddr, 8) shouldBe false
            interpWrapper.isMemoryWritable(unmappedAddr, 8) shouldBe false
            nativeWrapper.isMemoryWritable(unmappedAddr, 8) shouldBe false
            interpWrapper.readByte(unmappedAddr) shouldBe None
            nativeWrapper.readByte(unmappedAddr) shouldBe None
            nativeWrapper.writeByte(unmappedAddr, 1) shouldBe false
          finally
            live.close()
            blk.close()
        finally rc.close()
  }

  it should "match InterpretedInstanceWrapper on a span crossing a region boundary (RO tail into unmapped gap)" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val (interpWrapper, described) = buildInterpretedWrapper()
          val roRegion = described.regions.find(!_.writable).getOrElse(fail("expected a RO region"))
          // Start 8 bytes before the RO region's end so [addr, addr+16) spans
          // past the region's mapped end into whatever follows (unmapped gap
          // or a different region) — both engines must reject the WHOLE span
          // if it isn't entirely within one qualifying region/page run.
          val addr = (roRegion.base + roRegion.len - 8).toInt
          val (nativeWrapper, live, blk) = buildNativeWrapperOverDescribed(rc, described)
          try
            val interpReadable = interpWrapper.isMemoryReadable(addr, 16)
            val nativeReadable = nativeWrapper.isMemoryReadable(addr, 16)
            nativeReadable shouldBe interpReadable
            val interpBytes = interpWrapper.readBytes(addr, 16)
            val nativeBytes = nativeWrapper.readBytes(addr, 16)
            nativeBytes.isDefined shouldBe interpBytes.isDefined
          finally
            live.close()
            blk.close()
        finally rc.close()
  }

  it should "match InterpretedInstanceWrapper on bulk readBytes/writeBytes over a fully-mapped RW span" in {
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val (interpWrapper, described) = buildInterpretedWrapper()
          val roBase = described.regions.find(!_.writable).map(_.base).getOrElse(-1L)
          val rwRegion = described.regions.find(r => r.writable && r.base != roBase).getOrElse(fail("expected an RW region"))
          val (nativeWrapper, live, blk) = buildNativeWrapperOverDescribed(rc, described)
          try
            val addr = rwRegion.base.toInt
            val payload = Array.tabulate(32)(i => (i * 3).toByte)
            interpWrapper.writeBytes(addr, payload) shouldBe true
            nativeWrapper.writeBytes(addr, payload) shouldBe true
            interpWrapper.readBytes(addr, 32).map(_.toSeq) shouldBe Some(payload.toSeq)
            nativeWrapper.readBytes(addr, 32).map(_.toSeq) shouldBe Some(payload.toSeq)

            val dest = new Array[Byte](32)
            nativeWrapper.readInto(addr, dest, 0, 32) shouldBe true
            dest.toSeq shouldBe payload.toSeq
          finally
            live.close()
            blk.close()
        finally rc.close()
  }
