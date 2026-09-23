package io.forge.jam.protocol.accumulation

import io.forge.jam.pvm.native_.PvmRecompiler

import java.lang.foreign.{MemorySegment, ValueLayout}

object NativeInstanceWrapper:

  trait HeapGrowth:
    /** Index of the heap (RW-data) region in the live region table. */
    def heapRegionIndex: Int

    /** current heap end in pages, and the highest
      * page the heap may reach. */
    def pageBounds(): (Long, Long)

    /** Grow the heap by `deltaPages`; returns the heap region's new
      * page-aligned length, or a negative value when nothing changed. */
    def growPages(deltaPages: Long): Long

final class NativeInstanceWrapper(
    live: PvmRecompiler#LiveExecution,
    heapGrowth: Option[NativeInstanceWrapper.HeapGrowth] = None
) extends PvmInstance:

  private val regs: MemorySegment = live.regsSegment()
  private val gasSeg: MemorySegment = live.gasSegment()
  private val regionsSeg: MemorySegment = live.regionsSegment()
  private val nRegions: Long = live.regionCount()
  private val backing: MemorySegment = live.backingSegment()

  private val REGION_SIZE = 16L // base(4) + len(4) + bufOffset(4) + writable(4) — matches PvmRecompiler.REGION

  override def reg(regIdx: Int): Long =
    regs.getAtIndex(ValueLayout.JAVA_LONG, regIdx)

  override def setReg(regIdx: Int, value: Long): Unit =
    regs.setAtIndex(ValueLayout.JAVA_LONG, regIdx, value)

  override def gas: Long = gasSeg.get(ValueLayout.JAVA_LONG, 0)

  override def setGas(value: Long): Unit = gasSeg.set(ValueLayout.JAVA_LONG, 0, value)

  private final case class RegionView(base: Long, len: Long, bufOffset: Long, writable: Boolean):
    def end: Long = base + len // regions never wrap (page-aligned, len < 2^32)
    def contains(addr: Long, length: Long): Boolean =
      length >= 0 && addr >= base && addr + length <= end

  private def regionAt(i: Long): RegionView =
    val off = i * REGION_SIZE
    val base = Integer.toUnsignedLong(regionsSeg.get(ValueLayout.JAVA_INT, off))
    val len = Integer.toUnsignedLong(regionsSeg.get(ValueLayout.JAVA_INT, off + 4))
    val bufOffset = Integer.toUnsignedLong(regionsSeg.get(ValueLayout.JAVA_INT, off + 8))
    val writable = regionsSeg.get(ValueLayout.JAVA_INT, off + 12) != 0
    RegionView(base, len, bufOffset, writable)

  private val regions: Array[RegionView] = Array.tabulate(nRegions.toInt)(i => regionAt(i.toLong))

  def growRegion(regionIndex: Int, newLen: Long): Unit =
    val off = regionIndex * REGION_SIZE
    regionsSeg.set(ValueLayout.JAVA_INT, off + 4, newLen.toInt)
    val old = regions(regionIndex)
    regions(regionIndex) = old.copy(len = newLen)

  private def findRegion(addr: Long, length: Long, permitted: RegionView => Boolean): Option[RegionView] =
    var i = 0
    var found: Option[RegionView] = None
    while found.isEmpty && i < regions.length do
      val r = regions(i)
      if permitted(r) && r.contains(addr, length) then found = Some(r)
      i += 1
    found

  private def widen(address: Int): Long = address.toLong & 0xFFFFFFFFL

  override def isMemoryAccessible(address: Int, length: Int): Boolean =
    isMemoryReadable(address, length)

  override def isMemoryReadable(address: Int, length: Int): Boolean =
    if length < 0 then false
    else if length == 0 then true
    else findRegion(widen(address), length.toLong, _ => true).isDefined

  override def isMemoryWritable(address: Int, length: Int): Boolean =
    if length < 0 then false
    else if length == 0 then true
    else findRegion(widen(address), length.toLong, _.writable).isDefined

  override def readByte(address: Int): Option[Byte] =
    findRegion(widen(address), 1L, _ => true).map { r =>
      val off = r.bufOffset + (widen(address) - r.base)
      backing.get(ValueLayout.JAVA_BYTE, off)
    }

  override def writeByte(address: Int, value: Byte): Boolean =
    findRegion(widen(address), 1L, _.writable) match
      case Some(r) =>
        val off = r.bufOffset + (widen(address) - r.base)
        backing.set(ValueLayout.JAVA_BYTE, off, value)
        true
      case None => false

  override def readBytes(address: Int, length: Int): Option[Array[Byte]] =
    if length < 0 then None
    else if length == 0 then Some(Array.emptyByteArray)
    else
      findRegion(widen(address), length.toLong, _ => true).map { r =>
        val off = r.bufOffset + (widen(address) - r.base)
        val out = new Array[Byte](length)
        MemorySegment.copy(backing, ValueLayout.JAVA_BYTE, off, out, 0, length)
        out
      }

  override def readInto(address: Int, dest: Array[Byte], destOffset: Int, length: Int): Boolean =
    if length < 0 || destOffset < 0 || destOffset + length > dest.length then false
    else if length == 0 then true
    else
      findRegion(widen(address), length.toLong, _ => true) match
        case Some(r) =>
          val off = r.bufOffset + (widen(address) - r.base)
          MemorySegment.copy(backing, ValueLayout.JAVA_BYTE, off, dest, destOffset, length)
          true
        case None => false

  override def writeBytes(address: Int, data: Array[Byte]): Boolean =
    if data.isEmpty then true
    else
      findRegion(widen(address), data.length.toLong, _.writable) match
        case Some(r) =>
          val off = r.bufOffset + (widen(address) - r.base)
          MemorySegment.copy(data, 0, backing, ValueLayout.JAVA_BYTE, off, data.length)
          true
        case None => false

  private var forcedOutOfGas: Boolean = false

  override def forceOutOfGas(): Unit = forcedOutOfGas = true

  override def isForcedOutOfGas: Boolean = forcedOutOfGas

  override def growHeapPageBounds: Option[(Long, Long)] = heapGrowth.map(_.pageBounds())

  override def growHeapPages(deltaPages: Long): Unit =
    heapGrowth.foreach { hg =>
      val newRegionLen = hg.growPages(deltaPages)
      if newRegionLen >= 0 then growRegion(hg.heapRegionIndex, newRegionLen)
    }
