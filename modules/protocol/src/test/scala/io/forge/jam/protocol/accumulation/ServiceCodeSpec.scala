package io.forge.jam.protocol.accumulation

import io.forge.jam.core.JamBytes
import io.forge.jam.pvm.{InterruptKind, MemoryResult}
import io.forge.jam.pvm.engine.{InterpretedInstance, InterpretedModule}
import io.forge.jam.pvm.types.ProgramCounter
import org.scalatest.funsuite.AnyFunSuite
import spire.math.{UByte, UInt}

class ServiceCodeSpec extends AnyFunSuite:

  // deblob blob: no jump table, one trap instruction.
  private val pvmBlob = Array[Byte](0, 0, 1, 0, 1)

  private def le(v: Int, n: Int): Array[Byte] = Array.tabulate(n)(i => (v >>> (8 * i)).toByte)

  private def jamBlob(
      stack: Int = 0,
      p: Array[Byte] = pvmBlob,
      rw: Array[Byte] = Array.empty,
      heapPages: Int = 0
  ): Array[Byte] =
    le(0, 3) ++ le(rw.length, 3) ++ le(heapPages, 2) ++ le(stack, 3) ++ rw ++ le(p.length, 4) ++ p

  test("a standard JAM program blob decodes"):
    val blob = ServiceCode.parseBlob(jamBlob())
    assert(blob.map(_.code.toSeq) == Some(Seq[Byte](0)))

  test("a bare PVM blob is not a JAM program blob (Y = ∅)"):
    assert(ServiceCode.parseBlob(pvmBlob).isEmpty)

  test("a PolkaVM-style blob is not a JAM program blob (Y = ∅)"):
    assert(ServiceCode.parseBlob(Array[Byte](1, 0, 0, 0, 0, 0)).isEmpty)

  test("trailing bytes after the PVM blob make Y = ∅"):
    assert(ServiceCode.parseBlob(jamBlob() :+ 0.toByte).isEmpty)

  test("an inexact deblob encoding inside the program blob is rejected (deblob = ∇, Psi panics)"):
    // Trailing octet after the bitmask (the outer E_4 length covers it).
    assert(ServiceCode.parseBlob(jamBlob(p = pvmBlob :+ 0.toByte)).isEmpty)
    // Non-zero bitmask padding bit (|c| = 1).
    assert(ServiceCode.parseBlob(jamBlob(p = Array[Byte](0, 0, 1, 0, 3))).isEmpty)
    // |c| = 1 as the non-shortest [0x80, 0x01].
    assert(ServiceCode.parseBlob(jamBlob(p = Array[Byte](0, 0, -128, 1, 0, 1))).isEmpty)

  test("any E_3 stack size is accepted, up to 2^24 - 1"):
    assert(ServiceCode.parseBlob(jamBlob(stack = 1_000_001)).map(_.stackSize) == Some(1_000_001))
    assert(ServiceCode.parseBlob(jamBlob(stack = 0xFFFFFF)).map(_.stackSize) == Some(0xFFFFFF))

  test("module is compiled once per code hash and shared"):
    val hash = io.forge.jam.core.JamBytes(Array.fill[Byte](32)(0x5a))
    val code = jamBlob(stack = 4096)
    val a = ServiceCode.module(hash, code)
    assert(a.isDefined)
    assert(ServiceCode.module(hash, code.clone()).get eq a.get)
    assert(ServiceCode.module(hash, pvmBlob).isEmpty)

  // --- The cached module holds sizes, not the heap ----------------------------------------------------------------

  private val pageSize = 4096
  private val rwBase = 0x20000 // 2 * Z_Z, no read-only data

  test("a module declaring the largest heap retains only its real bytes, with the same memory layout"):
    val rw = Array[Byte](0x11, 0x22, 0x33)
    val module = ServiceCode.module(JamBytes(Array.fill[Byte](32)(0x61)), jamBlob(4096, rw = rw, heapPages = 65535)).get
    assert(module.rwData.toSeq == rw.toSeq)
    assert(module.blob.rwData.toSeq == rw.toSeq)
    assert(module.roData.length == 0)
    assert(module.retainedBytes < 16 * 1024)
    // The layout an instance gets is unchanged: one page of read-write data, then the declared heap.
    assert(module.heapEmptyPages == UInt(65535))
    assert(module.memoryMap.rwDataAddress == UInt(rwBase))
    assert(module.memoryMap.rwDataSize.toLong == pageSize + 65535L * pageSize)

    // Without a declared heap the template is not padded either.
    val small = ServiceCode.module(JamBytes(Array.fill[Byte](32)(0x62)), jamBlob(4096, rw = rw)).get
    assert(small.rwData.length == 3)
    assert(small.memoryMap.rwDataSize.toLong == pageSize)

  /** r7 = mem8[heap], r8 = mem8[rw]; then both are overwritten; halt. */
  private def dirtyingProgram(heapAddr: Int): Array[Byte] =
    def le4(v: Int): Seq[Int] = (0 until 4).map(i => (v >>> (8 * i)) & 0xff)
    val instructions = Seq(
      52 +: 0x07 +: le4(heapAddr), // load_u8 r7, [heap]
      52 +: 0x08 +: le4(rwBase), // load_u8 r8, [rw]
      (30 +: 4 +: le4(heapAddr)) :+ 0x55, // store_imm_u8 [heap], 0x55
      (30 +: 4 +: le4(rwBase)) :+ 0x66, // store_imm_u8 [rw], 0x66
      Seq(50, 0) // jump_ind r0 → halt
    )
    val code = instructions.flatten.map(_.toByte).toArray
    val bitmask = new Array[Byte]((code.length + 7) / 8)
    instructions.scanLeft(0)(_ + _.length).init.foreach(s => bitmask(s >> 3) = (bitmask(s >> 3) | (1 << (s & 7))).toByte)
    Array[Byte](0, 0, code.length.toByte) ++ code ++ bitmask

  test("each run starts from the template and a zeroed heap, whatever an earlier run wrote"):
    val heapPages = 512 // 2 MiB: large enough for the released buffer to be pooled for the next grow
    val heapAddr = rwBase + pageSize + 100
    val code = jamBlob(4096, dirtyingProgram(heapAddr), rw = Array[Byte](0x11), heapPages = heapPages)
    val module = ServiceCode.module(JamBytes(Array.fill[Byte](32)(0x63)), code).get
    val declaredEnd = rwBase + pageSize + heapPages * pageSize

    def byteAt(inst: InterpretedInstance, addr: Int): Int =
      inst.basicMemory.loadU8(UInt(addr)) match
        case MemoryResult.Success(v) => v.toInt
        case other => fail(s"0x${addr.toHexString} not readable: $other")

    def run(grow: Int): InterpretedInstance =
      val inst = InterpretedInstance.fromModule(module)
      inst.setReg(0, 0xffff0000L)
      inst.setGas(1_000_000L)
      inst.setNextProgramCounter(ProgramCounter(0))
      assert(inst.run() == Right(InterruptKind.Finished))
      // What the program saw before it wrote anything.
      assert(inst.reg(7) == 0L)
      assert(inst.reg(8) == 0x11L)
      assert(byteAt(inst, heapAddr) == 0x55)
      assert(byteAt(inst, rwBase) == 0x66)
      // Past the declared heap nothing is mapped until the heap grows; grown pages read as zero.
      assert(inst.basicMemory.loadU8(UInt(declaredEnd + 100)) != MemoryResult.Success(UByte(0)))
      assert(inst.basicMemory.sbrk(UInt(grow)).contains(UInt(declaredEnd)))
      assert(byteAt(inst, declaredEnd + 100) == 0)
      assert(byteAt(inst, declaredEnd + grow - 1) == 0)
      inst

    val first = run(grow = 4 << 20)
    assert(first.basicMemory.storeU8(UInt(declaredEnd + 100), UByte(0x77)) == MemoryResult.Success(()))
    assert(first.basicMemory.storeU8(UInt(declaredEnd + (1 << 20) - 1), UByte(0x78)) == MemoryResult.Success(()))
    first.basicMemory.release()

    // The second run may grow into the buffer the first one released; it must not see what the first wrote.
    val second = run(grow = 1 << 20)
    second.basicMemory.release()
    assert(module.rwData.toSeq == Seq[Byte](0x11))
