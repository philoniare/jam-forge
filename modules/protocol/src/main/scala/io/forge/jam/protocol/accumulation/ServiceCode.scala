package io.forge.jam.protocol.accumulation

import io.forge.jam.pvm.program.ProgramBlob

/** Service-code preimage handling shared by the accumulate and refine executors */
object ServiceCode:

  private val JAM_PAGE_SIZE = 4096
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

  /** Parse an extracted code blob into a ProgramBlob */
  def parseBlob(code: Array[Byte]): Option[ProgramBlob] =
    var blobOpt = parseJamFormat(code)

    if blobOpt.isEmpty then
      blobOpt = ProgramBlob.fromCodeAndJumpTable(
        data = code,
        roData = Array.empty,
        rwData = new Array[Byte](262144),
        stackSize = 65536,
        is64Bit = true
      )

    if blobOpt.isEmpty then blobOpt = ProgramBlob.parse(code)

    blobOpt

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

    if roDataLen > data.length || rwDataLen > data.length || stackSize > 1000000
    then return None

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

    if offset + codeLen > data.length then return None
    val codeAndJumpTable = new Array[Byte](codeLen)
    System.arraycopy(data, offset, codeAndJumpTable, 0, codeLen)

    val heapSize = heapPages * JAM_PAGE_SIZE
    val totalRwSize = rwDataLen + heapSize
    val minRwSize = Math.max(totalRwSize, 262144)
    val paddedRwData = new Array[Byte](minRwSize)
    System.arraycopy(rwData, 0, paddedRwData, 0, rwDataLen)

    ProgramBlob.fromCodeAndJumpTable(
      data = codeAndJumpTable,
      roData = roData,
      rwData = paddedRwData,
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
