package io.forge.jam.core

import spire.math.{UByte, UShort, UInt, ULong}

/**
 * Legacy JAM codec type classes and extension methods.
 *
 * This file contains the JamEncoder/JamDecoder type class infrastructure
 * that is being phased out in favor of scodec Codec instances.
 *
 * Types throughout the codebase still define their own JamEncoder/JamDecoder
 * instances, so these traits and extension methods must remain available.
 *
 * For new code, prefer using scodec codecs from io.forge.jam.core.scodec.*.
 *
 * @deprecated Use scodec codecs from io.forge.jam.core.scodec.* instead.
 */
object LegacyCodec:

  // ============================================================================
  // Type Class Traits
  // ============================================================================

  /**
   * Type class for encoding values to JAM binary format.
   *
   * @deprecated Use scodec Codec[A] from io.forge.jam.core.scodec.JamCodecs instead.
   */
  trait JamEncoder[A]:
    def encode(a: A): JamBytes

  /**
   * Type class for decoding values from JAM binary format.
   * Returns the decoded value and number of bytes consumed.
   *
   * @deprecated Use scodec Codec[A] from io.forge.jam.core.scodec.JamCodecs instead.
   */
  trait JamDecoder[A]:
    def decode(bytes: JamBytes, offset: Int): (A, Int)

  // ============================================================================
  // Extension Methods
  // ============================================================================

  extension [A](a: A)
    /**
     * Encode this value to JAM binary format using the implicit encoder.
     *
     * @deprecated Use scodec codec.encode(value).require.bytes instead.
     */
    def encode(using enc: JamEncoder[A]): JamBytes = enc.encode(a)

  extension (bytes: JamBytes)
    /**
     * Decode a value from JAM binary format using the implicit decoder.
     *
     * @deprecated Use scodec codec.decode(bits).require.value instead.
     */
    def decodeAs[A](offset: Int = 0)(using dec: JamDecoder[A]): (A, Int) =
      dec.decode(bytes, offset)

  // ============================================================================
  // Variable-length Integer Encoding (Varint)
  // ============================================================================

  /**
   * Encodes a 32-bit unsigned integer as a variable-length sequence.
   * Uses little-endian byte order with continuation bits.
   */
  def encodeVarint(value: UInt): Array[Byte] =
    var v = value.toLong
    val result = scala.collection.mutable.ArrayBuffer[Byte]()
    while v >= 0x80 do
      result += ((v & 0x7F) | 0x80).toByte
      v >>>= 7
    result += v.toByte
    result.toArray

  /**
   * Encodes a 64-bit unsigned integer as a variable-length sequence.
   */
  def encodeVarint64(value: ULong): Array[Byte] =
    var v = value.signed
    val result = scala.collection.mutable.ArrayBuffer[Byte]()
    while (v & ~0x7FL) != 0 do
      result += ((v & 0x7F) | 0x80).toByte
      v >>>= 7
    result += v.toByte
    result.toArray

  /**
   * Decodes a variable-length integer from bytes.
   * Returns the decoded value and the number of bytes consumed.
   */
  def decodeVarint(bytes: Array[Byte], offset: Int = 0): (UInt, Int) =
    var result: Long = 0
    var shift = 0
    var pos = offset
    var b: Int = 0
    var continue = true
    while continue && shift < 35 do
      b = bytes(pos) & 0xFF
      result |= (b & 0x7FL) << shift
      shift += 7
      pos += 1
      continue = (b & 0x80) != 0
    (UInt(result.toInt), pos - offset)

  /**
   * Decodes a 64-bit variable-length integer from bytes.
   */
  def decodeVarint64(bytes: Array[Byte], offset: Int = 0): (ULong, Int) =
    var result: Long = 0
    var shift = 0
    var pos = offset
    var b: Int = 0
    var continue = true
    while continue && shift < 70 do
      b = bytes(pos) & 0xFF
      result |= (b & 0x7FL) << shift
      shift += 7
      pos += 1
      continue = (b & 0x80) != 0
    (ULong(result), pos - offset)

  // ============================================================================
  // Little-Endian Fixed-Width Encoding
  // ============================================================================

  def encodeU8(v: UByte): Array[Byte] = Array(v.toByte)

  def encodeU16LE(v: UShort): Array[Byte] =
    val i = v.toInt
    Array((i & 0xFF).toByte, ((i >> 8) & 0xFF).toByte)

  def encodeU32LE(v: UInt): Array[Byte] =
    val i = v.signed
    Array(
      (i & 0xFF).toByte,
      ((i >> 8) & 0xFF).toByte,
      ((i >> 16) & 0xFF).toByte,
      ((i >> 24) & 0xFF).toByte
    )

  def encodeU64LE(v: ULong): Array[Byte] =
    val l = v.signed
    Array(
      (l & 0xFF).toByte,
      ((l >> 8) & 0xFF).toByte,
      ((l >> 16) & 0xFF).toByte,
      ((l >> 24) & 0xFF).toByte,
      ((l >> 32) & 0xFF).toByte,
      ((l >> 40) & 0xFF).toByte,
      ((l >> 48) & 0xFF).toByte,
      ((l >> 56) & 0xFF).toByte
    )

  def decodeU8(bytes: Array[Byte], offset: Int = 0): UByte =
    UByte(bytes(offset))

  def decodeU16LE(bytes: Array[Byte], offset: Int = 0): UShort =
    UShort(
      (bytes(offset) & 0xFF) |
      ((bytes(offset + 1) & 0xFF) << 8)
    )

  def decodeU32LE(bytes: Array[Byte], offset: Int = 0): UInt =
    UInt(
      (bytes(offset) & 0xFF) |
      ((bytes(offset + 1) & 0xFF) << 8) |
      ((bytes(offset + 2) & 0xFF) << 16) |
      ((bytes(offset + 3) & 0xFF) << 24)
    )

  def decodeU64LE(bytes: Array[Byte], offset: Int = 0): ULong =
    ULong(
      (bytes(offset) & 0xFFL) |
      ((bytes(offset + 1) & 0xFFL) << 8) |
      ((bytes(offset + 2) & 0xFFL) << 16) |
      ((bytes(offset + 3) & 0xFFL) << 24) |
      ((bytes(offset + 4) & 0xFFL) << 32) |
      ((bytes(offset + 5) & 0xFFL) << 40) |
      ((bytes(offset + 6) & 0xFFL) << 48) |
      ((bytes(offset + 7) & 0xFFL) << 56)
    )

  // ============================================================================
  // JAM Compact Integer Encoding
  // ============================================================================

  /**
   * Encodes a non-negative integer using JAM compact integer format.
   *
   * Encoding scheme:
   * - If x = 0, returns [0]
   * - If 2^(7l) <= x < 2^(7(l+1)) for some l in [0..8], then:
   *     [256 - 2^(8-l) + floor(x / 2^(8*l))] ++ E_l(x mod 2^(8*l))
   * - Otherwise (x < 2^64), returns [255] ++ E_8(x)
   *
   * Where E_l(r) means "r in little-endian form over l bytes."
   */
  def encodeCompactInteger(x: Long): Array[Byte] =
    require(x >= 0, "No negative values allowed")

    // Special case: x = 0
    if x == 0L then
      return Array[Byte](0)

    // Find l such that 2^(7l) <= x < 2^(7(l+1)) for l in [0..8]
    var l = 0
    while l <= 8 do
      val lowerBound = 1L << (7 * l)
      val upperBound = 1L << (7 * (l + 1))
      if x >= lowerBound && x < upperBound then
        // prefix = 256 - 2^(8-l) + floor(x / 2^(8*l))
        val prefixVal = (256 - (1 << (8 - l))) + (x >>> (8 * l))
        val prefixByte = prefixVal.toByte

        // remainder = x mod 2^(8*l)
        val remainder = x & ((1L << (8 * l)) - 1)

        // E_l(remainder) -> little-endian representation in l bytes
        val result = new Array[Byte](1 + l)
        result(0) = prefixByte
        for i <- 0 until l do
          result(1 + i) = ((remainder >> (8 * i)) & 0xFF).toByte
        return result
      l += 1

    // Fallback: [255] ++ E_8(x)
    val result = new Array[Byte](9)
    result(0) = 0xFF.toByte
    for i <- 0 until 8 do
      result(1 + i) = ((x >> (8 * i)) & 0xFF).toByte
    result

  /**
   * Decodes a JAM compact integer from bytes.
   * Returns the decoded value and the number of bytes consumed.
   */
  def decodeCompactInteger(data: Array[Byte], offset: Int = 0): (Long, Int) =
    if offset >= data.length then
      return (0L, 0)

    val prefix = data(offset) & 0xFF

    // Special case: prefix = 0 means value = 0
    if prefix == 0 then
      return (0L, 1)

    // Determine l from prefix
    val l =
      if prefix < 128 then 0
      else if prefix < 192 then 1
      else if prefix < 224 then 2
      else if prefix < 240 then 3
      else if prefix < 248 then 4
      else if prefix < 252 then 5
      else if prefix < 254 then 6
      else if prefix < 255 then 7
      else 8

    // Handle special case where prefix = 255 means 8 bytes follow
    if prefix == 255 then
      var value = 0L
      for i <- 0 until 8 do
        if offset + 1 + i < data.length then
          value = value | ((data(offset + 1 + i).toLong & 0xFF) << (8 * i))
      return (value, 9)

    // For l=0, the value is just the prefix itself
    if l == 0 then
      return (prefix.toLong, 1)

    // Calculate the high bits from prefix
    val base = 256 - (1 << (8 - l))
    val highBits = (prefix - base).toLong << (8 * l)

    // Read the low bytes (l bytes in little-endian)
    var lowBits = 0L
    for i <- 0 until l do
      if offset + 1 + i < data.length then
        lowBits = lowBits | ((data(offset + 1 + i).toLong & 0xFF) << (8 * i))

    (highBits | lowBits, 1 + l)

  // ============================================================================
  // Primitive Type Encoders
  // ============================================================================

  given JamEncoder[UByte] with
    def encode(a: UByte): JamBytes =
      JamBytes(encodeU8(a))

  given JamEncoder[UShort] with
    def encode(a: UShort): JamBytes =
      JamBytes(encodeU16LE(a))

  given JamEncoder[UInt] with
    def encode(a: UInt): JamBytes =
      JamBytes(encodeU32LE(a))

  given JamEncoder[ULong] with
    def encode(a: ULong): JamBytes =
      JamBytes(encodeU64LE(a))

  /**
   * JamBytes encoder with compact length prefix followed by raw bytes.
   */
  given JamEncoder[JamBytes] with
    def encode(a: JamBytes): JamBytes =
      val lengthPrefix = JamBytes(encodeCompactInteger(a.length.toLong))
      lengthPrefix ++ a

  // ============================================================================
  // Primitive Type Decoders
  // ============================================================================

  given JamDecoder[UByte] with
    def decode(bytes: JamBytes, offset: Int): (UByte, Int) =
      (decodeU8(bytes.toArray, offset), 1)

  given JamDecoder[UShort] with
    def decode(bytes: JamBytes, offset: Int): (UShort, Int) =
      (decodeU16LE(bytes.toArray, offset), 2)

  given JamDecoder[UInt] with
    def decode(bytes: JamBytes, offset: Int): (UInt, Int) =
      (decodeU32LE(bytes.toArray, offset), 4)

  given JamDecoder[ULong] with
    def decode(bytes: JamBytes, offset: Int): (ULong, Int) =
      (decodeU64LE(bytes.toArray, offset), 8)

  /**
   * JamBytes decoder: reads compact length prefix, then that many bytes.
   */
  given JamDecoder[JamBytes] with
    def decode(bytes: JamBytes, offset: Int): (JamBytes, Int) =
      val arr = bytes.toArray
      val (length, lengthBytes) = decodeCompactInteger(arr, offset)
      val data = bytes.slice(offset + lengthBytes, offset + lengthBytes + length.toInt)
      (data, lengthBytes + length.toInt)

  // ============================================================================
  // Compact Integer Codec
  // ============================================================================

  /**
   * Wrapper type for compact integer encoding.
   * Use this when you need compact integer encoding instead of fixed-width.
   */
  opaque type CompactInt = Long

  object CompactInt:
    def apply(v: Long): CompactInt = v
    def apply(v: Int): CompactInt = v.toLong

  extension (c: CompactInt)
    def value: Long = c
    def toInt: Int = c.toInt

  given JamEncoder[CompactInt] with
    def encode(a: CompactInt): JamBytes =
      JamBytes(encodeCompactInteger(a))

  given JamDecoder[CompactInt] with
    def decode(bytes: JamBytes, offset: Int): (CompactInt, Int) =
      val (value, consumed) = decodeCompactInteger(bytes.toArray, offset)
      (CompactInt(value), consumed)

  // ============================================================================
  // Optional Type Codec
  // ============================================================================

  /**
   * Encoder for Option[A] using 0/1 prefix byte pattern.
   */
  given optionEncoder[A](using enc: JamEncoder[A]): JamEncoder[Option[A]] with
    def encode(a: Option[A]): JamBytes = a match
      case None => JamBytes(Array[Byte](0))
      case Some(v) => JamBytes(Array[Byte](1)) ++ enc.encode(v)

  /**
   * Decoder for Option[A] using 0/1 prefix byte pattern.
   */
  given optionDecoder[A](using dec: JamDecoder[A]): JamDecoder[Option[A]] with
    def decode(bytes: JamBytes, offset: Int): (Option[A], Int) =
      val prefix = bytes(offset).toInt
      if prefix == 0 then
        (None, 1)
      else
        val (value, consumed) = dec.decode(bytes, offset + 1)
        (Some(value), 1 + consumed)

  // ============================================================================
  // List Type Codec
  // ============================================================================

  /**
   * Encoder for List[A] using compact integer length prefix.
   */
  given listEncoder[A](using enc: JamEncoder[A]): JamEncoder[List[A]] with
    def encode(a: List[A]): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= encodeCompactInteger(a.length.toLong)
      for elem <- a do
        builder ++= enc.encode(elem).toArray
      builder.result()

  /**
   * Decoder for List[A] using compact integer length prefix.
   */
  given listDecoder[A](using dec: JamDecoder[A]): JamDecoder[List[A]] with
    def decode(bytes: JamBytes, offset: Int): (List[A], Int) =
      val arr = bytes.toArray
      val (length, lengthBytes) = decodeCompactInteger(arr, offset)
      var pos = offset + lengthBytes
      val result = scala.collection.mutable.ListBuffer[A]()
      for _ <- 0 until length.toInt do
        val (elem, consumed) = dec.decode(bytes, pos)
        result += elem
        pos += consumed
      (result.toList, pos - offset)

  // ============================================================================
  // Fixed-Size List Encoding Helpers
  // ============================================================================

  /**
   * Encode a fixed-size list (no length prefix).
   */
  def encodeFixedList[A](list: List[A])(using enc: JamEncoder[A]): JamBytes =
    val builder = JamBytes.newBuilder
    for elem <- list do
      builder ++= enc.encode(elem).toArray
    builder.result()

  /**
   * Decode a fixed-size list.
   */
  def decodeFixedList[A](bytes: JamBytes, offset: Int, count: Int)(using dec: JamDecoder[A]): (List[A], Int) =
    var pos = offset
    val result = scala.collection.mutable.ListBuffer[A]()
    for _ <- 0 until count do
      val (elem, consumed) = dec.decode(bytes, pos)
      result += elem
      pos += consumed
    (result.toList, pos - offset)

  // ============================================================================
  // Fixed-Size Array Encoding Utilities
  // ============================================================================

  /**
   * Encode a fixed-size JamBytes (no length prefix).
   */
  def encodeFixedBytes(bytes: JamBytes): JamBytes = bytes

  /**
   * Decode a fixed-size JamBytes.
   */
  def decodeFixedBytes(bytes: JamBytes, offset: Int, size: Int): (JamBytes, Int) =
    (bytes.slice(offset, offset + size), size)
