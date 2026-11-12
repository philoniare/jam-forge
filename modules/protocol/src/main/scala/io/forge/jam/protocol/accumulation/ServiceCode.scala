package io.forge.jam.protocol.accumulation

import io.forge.jam.core.JamBytes
import io.forge.jam.pvm.engine.{InterpretedModule, ModuleCache}
import io.forge.jam.pvm.program.{Deblob, ProgramBlob}

/** Service-code preimage handling shared by the accumulate and refine executors */
object ServiceCode:

  /** Byte budget of the module cache: 256 MiB, or an eighth of the JVM's maximum heap when that is less. */
  private val ModuleCacheBytes: Long = math.min(256L << 20, Runtime.getRuntime.maxMemory() / 8)

  /** Process-wide module cache shared by the accumulate, refine and is-authorized executors, bounded by entry count
    * and by the bytes the cached modules retain.
    */
  private val modules = ModuleCache[JamBytes](256, ModuleCacheBytes)

  /** The compiled module for the code blob `code` (the preimage after its metadata) found under `codeHash`. Built
    * once per process and shared, so a new importer or executor does not recompile it. None when the blob is not a
    * valid JAM program blob.
    */
  def module(codeHash: JamBytes, code: Array[Byte]): Option[InterpretedModule] =
    modules.getOrCreate(codeHash, code)(parseBlob(code).flatMap(blob => InterpretedModule.create(blob).toOption))

  def extractCodeBlob(preimage: Array[Byte]): Option[Array[Byte]] =
    if preimage.isEmpty then return None

    val firstByte = preimage(0).toInt & 0xff
    if firstByte == 0 then return Some(preimage.drop(1))

    val inverted = (~firstByte) & 0xff
    var byteLength = 0
    var i = 0
    while i < 8 && (inverted & (0x80 >> i)) == 0 do
      byteLength += 1
      i += 1

    if preimage.length < 1 + byteLength then return None

    var res: Long = 0
    i = 0
    while i < byteLength do
      res = res | ((preimage(1 + i).toLong & 0xff) << (8 * i))
      i += 1

    val mask = (1 << (8 - byteLength)) - 1
    val topBits = firstByte & mask
    val metaLength = (res + (topBits.toLong << (8 * byteLength))).toInt
    val metaLengthSize = 1 + byteLength
    val codeStart = metaLengthSize + metaLength

    if codeStart > preimage.length then return None
    Some(preimage.drop(codeStart))

  /** Parse an extracted code blob into a ProgramBlob. */
  def parseBlob(code: Array[Byte]): Option[ProgramBlob] = parseJamFormat(code)

  /** Parse JAM SPI blob format: roLen(3) rwLen(3) heapPages(2) stackSize(3)
    * roData rwData codeLen(4) code+jumptable.
    */
  private def parseJamFormat(data: Array[Byte]): Option[ProgramBlob] =
    if data.length < 15 then return None

    var offset = 0
    val roDataLen = readLE3(data, offset); offset += 3
    val rwDataLen = readLE3(data, offset); offset += 3
    val heapPages = readLE2(data, offset); offset += 2
    val stackSize = readLE3(data, offset); offset += 3

    if roDataLen > data.length || rwDataLen > data.length then return None

    if offset + roDataLen > data.length then return None
    val roData = new Array[Byte](roDataLen)
    System.arraycopy(data, offset, roData, 0, roDataLen)
    offset += roDataLen

    if offset + rwDataLen > data.length then return None
    val rwData = new Array[Byte](rwDataLen)
    System.arraycopy(data, offset, rwData, 0, rwDataLen)
    offset += rwDataLen

    if offset + 4 > data.length then return None
    val codeLen = readLE4(data, offset); offset += 4

    if codeLen < 0 || offset.toLong + codeLen != data.length then return None
    val codeAndJumpTable = new Array[Byte](codeLen)
    System.arraycopy(data, offset, codeAndJumpTable, 0, codeLen)
    if !Deblob.isExactEncoding(codeAndJumpTable) then return None

    // Only the initialised read-write bytes go into the blob. The declared heap (`heapPages`, up to 65,535 pages) is
    // a size: each instance gets it as fresh zeroed memory (BasicMemory.create), so the cached module does not hold it.
    ProgramBlob.fromCodeAndJumpTable(
      data = codeAndJumpTable,
      roData = roData,
      rwData = rwData,
      stackSize = stackSize,
      is64Bit = true,
      heapPages = heapPages,
      originalRwDataLen = rwDataLen
    )

  private def readLE2(data: Array[Byte], offset: Int): Int =
    (data(offset) & 0xff) | ((data(offset + 1) & 0xff) << 8)

  private def readLE3(data: Array[Byte], offset: Int): Int =
    (data(offset) & 0xff) | ((data(offset + 1) & 0xff) << 8) | ((data(
      offset + 2
    ) & 0xff) << 16)

  private def readLE4(data: Array[Byte], offset: Int): Int =
    (data(offset) & 0xff) | ((data(offset + 1) & 0xff) << 8) |
      ((data(offset + 2) & 0xff) << 16) | ((data(offset + 3) & 0xff) << 24)
