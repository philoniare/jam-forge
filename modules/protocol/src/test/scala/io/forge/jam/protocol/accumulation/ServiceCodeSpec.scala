package io.forge.jam.protocol.accumulation

import org.scalatest.funsuite.AnyFunSuite

class ServiceCodeSpec extends AnyFunSuite:

  // deblob blob: no jump table, one trap instruction.
  private val pvmBlob = Array[Byte](0, 0, 1, 0, 1)

  private def le(v: Int, n: Int): Array[Byte] = Array.tabulate(n)(i => (v >>> (8 * i)).toByte)

  private def jamBlob(stack: Int = 0, p: Array[Byte] = pvmBlob): Array[Byte] =
    le(0, 3) ++ le(0, 3) ++ le(0, 2) ++ le(stack, 3) ++ le(p.length, 4) ++ p

  test("GP: a standard JAM program blob decodes"):
    val blob = ServiceCode.parseBlob(jamBlob())
    assert(blob.map(_.code.toSeq) == Some(Seq[Byte](0)))

  test("GP: a bare PVM blob is not a JAM program blob (Y = ∅)"):
    assert(ServiceCode.parseBlob(pvmBlob).isEmpty)

  test("GP: a PolkaVM-style blob is not a JAM program blob (Y = ∅)"):
    assert(ServiceCode.parseBlob(Array[Byte](1, 0, 0, 0, 0, 0)).isEmpty)

  test("GP: trailing bytes after the PVM blob make Y = ∅"):
    assert(ServiceCode.parseBlob(jamBlob() :+ 0.toByte).isEmpty)

  test("GP: any E_3 stack size is accepted, up to 2^24 - 1"):
    assert(ServiceCode.parseBlob(jamBlob(stack = 1_000_001)).map(_.stackSize) == Some(1_000_001))
    assert(ServiceCode.parseBlob(jamBlob(stack = 0xFFFFFF)).map(_.stackSize) == Some(0xFFFFFF))

  test("module is compiled once per code hash and shared"):
    val hash = io.forge.jam.core.JamBytes(Array.fill[Byte](32)(0x5a))
    val code = jamBlob(stack = 4096)
    val a = ServiceCode.module(hash, code)
    assert(a.isDefined)
    assert(ServiceCode.module(hash, code.clone()).get eq a.get)
    assert(ServiceCode.module(hash, pvmBlob).isEmpty)
