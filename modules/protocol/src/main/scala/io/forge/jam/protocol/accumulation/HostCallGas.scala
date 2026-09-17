package io.forge.jam.protocol.accumulation

import spire.math.ULong

object HostCallGas:

  val Cgasunknown: Long = 1000L

  def fnmemgas(L: Long, l: ULong): Long =
    val product = BigInt(L) * unsignedBigInt(l)
    val ceilDiv = (product + 1023) / 1024
    saturate(ceilDiv)

  private def saturatingMulAdd(base: Long, rate: Long, count: ULong): Long =
    saturate(BigInt(base) + BigInt(rate) * unsignedBigInt(count))

  /** `base + count` (rate = 1), saturating. */
  private def saturatingAdd(base: Long, count: ULong): Long =
    saturate(BigInt(base) + unsignedBigInt(count))

  private def saturate(v: BigInt): Long =
    if v > BigInt(Long.MaxValue) then Long.MaxValue
    else if v < 0 then 0L // defensive: no formula below can go negative, but never return a negative "cost"
    else v.toLong

  private def unsignedBigInt(u: ULong): BigInt =
    val s = u.toLong
    if s >= 0 then BigInt(s) else BigInt(s) + (BigInt(1) << 64)

  val CgasG: Long = 48L

  private val fetchCaseCost: Map[Int, (Long, Long)] = Map(
    0 -> (390L, 0L), // protocol parameters
    1 -> (103L, 0L), // entropy
    2 -> (80L, 96L), // auth trace
    3 -> (85L, 96L), // any extrinsic, by index
    4 -> (85L, 96L), // our extrinsic, by index
    5 -> (171L, 0L), // any import, by index
    6 -> (171L, 0L), // our import, by index
    7 -> (85L, 96L), // encoded work-package
    8 -> (84L, 0L), // auth config
    9 -> (88L, 0L), // auth token
    10 -> (111L, 0L), // refine context
    11 -> (317L, 0L), // items summary
    12 -> (250L, 0L), // any item summary
    13 -> (95L, 96L), // any payload
    14 -> (287L, 400L), // accumulate items (all operands)
    15 -> (355L, 344L) // any accumulate item (single operand)
  )

  private val fetchOtherwiseCost: (Long, Long) = (80L, 0L)

  def fetchGas(selector: ULong, z: ULong): Long =
    val (c, l) =
      if selector < ULong(16L) then fetchCaseCost(selector.toInt)
      else fetchOtherwiseCost
    c + fnmemgas(l, z)

  val CgasLconst: Long = 600L
  val CgasLlinear: Long = 248L
  def lookupGas(z: ULong): Long = CgasLconst + fnmemgas(CgasLlinear, z)

  val CgasRconst: Long = 2407L
  val CgasRkeylinear: Long = 1736L
  val CgasRvallinear: Long = 248L
  def readGas(keyLen: ULong, valLen: ULong): Long =
    CgasRconst + fnmemgas(CgasRkeylinear, keyLen) + fnmemgas(CgasRvallinear, valLen)

  val CgasWconst: Long = 2442L
  val CgasWkeylinear: Long = 3358L
  val CgasWvallinear: Long = 216L
  def writeGas(keyLen: ULong, valLen: ULong): Long =
    CgasWconst + fnmemgas(CgasWkeylinear, keyLen) + fnmemgas(CgasWvallinear, valLen)

  val CgasI: Long = 703L

  val CgasHconst: Long = 1125L
  val CgasHlinear: Long = 264L
  def historicalLookupGas(z: ULong): Long = CgasHconst + fnmemgas(CgasHlinear, z)

  val CgasE: Long = 3521L

  val CgasMconst: Long = 1862L
  val CgasMlinear: Long = 112L
  def machineGas(codeLen: ULong): Long = CgasMconst + fnmemgas(CgasMlinear, codeLen)

  val CgasPconst: Long = 377L
  val CgasPlinear: Long = 336L
  def peekGas(z: ULong): Long = CgasPconst + fnmemgas(CgasPlinear, z)

  val CgasOconst: Long = 297L
  val CgasOlinear: Long = 224L
  def pokeGas(z: ULong): Long = CgasOconst + fnmemgas(CgasOlinear, z)

  val CgasZfreeconst: Long = 212L
  val CgasZfreelinear: Long = 118L
  val CgasZallocconst: Long = 275L
  val CgasZalloclinear: Long = 121L
  val CgasZsetmodeconst: Long = 130L
  val CgasZsetmodelinear: Long = 29L
  val CgasZinvalid: Long = 80L
  def pagesGas(r: ULong, pageCount: ULong): Long =
    r.toLong match
      case 0L => saturatingMulAdd(CgasZfreeconst, CgasZfreelinear, pageCount)
      case 1L | 2L => saturatingMulAdd(CgasZallocconst, CgasZalloclinear, pageCount)
      case 3L | 4L => saturatingMulAdd(CgasZsetmodeconst, CgasZsetmodelinear, pageCount)
      case _ => CgasZinvalid

  val CgasK: Long = 968L
  def invokeGasUpfront(innerGasLimit: ULong): Long = saturatingAdd(CgasK, innerGasLimit)

  val CgasX: Long = 335L

  val CgasBconst: Long = 422L
  val CgasBlinear: Long = 20L
  def blessGas(n: ULong): Long = saturatingMulAdd(CgasBconst, CgasBlinear, n)

  val CgasA: Long = 1818L

  val CgasDconst: Long = 1100L
  val CgasDlinear: Long = 302L
  def designateGas(z: ULong): Long = saturatingMulAdd(CgasDconst, CgasDlinear, z)

  val CgasC: Long = 103L

  val CgasN: Long = 3855L

  val CgasU: Long = 1028L

  val CgasT: Long = 575L

  val CgasJ: Long = 458L

  val CgasQ: Long = 643L

  val CgasS: Long = 2193L

  val CgasF: Long = 3250L

  val CgasTaurus: Long = 98L

  val CgasAriesconst: Long = 3980L
  val CgasArieslinear: Long = 2264L
  def provideGas(z: ULong): Long = CgasAriesconst + fnmemgas(CgasArieslinear, z)
