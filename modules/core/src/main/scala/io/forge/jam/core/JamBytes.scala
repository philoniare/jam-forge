package io.forge.jam.core

import spire.math.UByte
import io.circe.Decoder

/** Immutable byte sequence backed by an array that is never exposed for mutation (except through the explicitly
  * unsafe accessors). Equality and hashing are by content.
  */
final class JamBytes private (private val underlying: Array[Byte]):
  def length: Int = underlying.length
  def size: Long = underlying.length.toLong
  def isEmpty: Boolean = underlying.length == 0
  def nonEmpty: Boolean = underlying.length != 0

  // Element access
  def apply(index: Long): Byte = underlying(index.toInt)
  def apply(index: Int): Byte = underlying(index)

  // Conversion methods
  def toArray: Array[Byte] = underlying.clone()

  /** The backing array without a copy; callers must not mutate it. */
  def toArrayUnsafe: Array[Byte] = underlying

  def toSeq: Seq[Byte] = underlying.toSeq
  def toHex: String = JamBytes.toHex(underlying)
  def toHexWithPrefix: String = "0x" + toHex

  // Concatenation
  def ++(other: JamBytes): JamBytes =
    if other.isEmpty then this
    else if isEmpty then other
    else
      val out = new Array[Byte](length + other.length)
      System.arraycopy(underlying, 0, out, 0, length)
      System.arraycopy(other.underlying, 0, out, length, other.length)
      new JamBytes(out)
  def :+(byte: Byte): JamBytes =
    val out = java.util.Arrays.copyOf(underlying, length + 1)
    out(length) = byte
    new JamBytes(out)
  def +:(byte: Byte): JamBytes =
    val out = new Array[Byte](length + 1)
    out(0) = byte
    System.arraycopy(underlying, 0, out, 1, length)
    new JamBytes(out)

  // Slicing (bounds are clamped, as on standard collections)
  def take(n: Long): JamBytes = slice(0, n)
  def drop(n: Long): JamBytes = slice(n, length.toLong)
  def slice(from: Long, until: Long): JamBytes =
    val lo = math.max(0L, math.min(from, length.toLong)).toInt
    val hi = math.max(lo.toLong, math.min(until, length.toLong)).toInt
    if lo == 0 && hi == length then this
    else new JamBytes(java.util.Arrays.copyOfRange(underlying, lo, hi))

  // Other operations
  def reverse: JamBytes = new JamBytes(underlying.reverse)
  def copyToArray(dest: Array[Byte], destPos: Int, srcPos: Int, len: Int): Unit =
    System.arraycopy(underlying, srcPos, dest, destPos, len)

  override def equals(obj: Any): Boolean = obj match
    case that: JamBytes => java.util.Arrays.equals(underlying, that.underlying)
    case _              => false
  override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  override def toString: String = s"JamBytes($toHex)"
  def signedAt(index: Int): Byte = underlying(index)

  // Fold operations
  def foldLeft[A](z: A)(op: (A, UByte) => A): A =
    var acc = z
    var i = 0
    while i < underlying.length do
      acc = op(acc, UByte(underlying(i)))
      i += 1
    acc

/** Companion object providing factory methods and utilities for JamBytes.
  */
object JamBytes:
  val empty: JamBytes = new JamBytes(Array.emptyByteArray)

  /** Copies `bytes`. */
  def apply(bytes: Array[Byte]): JamBytes = new JamBytes(bytes.clone())
  def apply(bytes: Byte*): JamBytes = new JamBytes(bytes.toArray)
  def fromSeq(bytes: Seq[Byte]): JamBytes = new JamBytes(bytes.toArray)

  /** Takes ownership of `bytes` without copying; the caller must not mutate it afterwards. */
  def wrap(bytes: Array[Byte]): JamBytes = new JamBytes(bytes)

  def zeros(size: Int): JamBytes = new JamBytes(new Array[Byte](size))
  def fill(size: Int)(value: Byte): JamBytes =
    val out = new Array[Byte](size)
    java.util.Arrays.fill(out, value)
    new JamBytes(out)

  def fromHex(hex: String): Either[String, JamBytes] =
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    parseHex(cleanHex).map(new JamBytes(_))

  def fromHexUnsafe(hex: String): JamBytes =
    fromHex(hex).fold(msg => throw new IllegalArgumentException(msg), identity)

  private val HexDigits: Array[Char] = "0123456789abcdef".toCharArray

  /** Lowercase hex without a prefix. */
  def toHex(bytes: Array[Byte]): String =
    val out = new Array[Char](bytes.length * 2)
    var i = 0
    while i < bytes.length do
      val b = bytes(i) & 0xff
      out(2 * i) = HexDigits(b >>> 4)
      out(2 * i + 1) = HexDigits(b & 0xf)
      i += 1
    new String(out)

  /** Parses hex digits (no prefix, either case). */
  def parseHex(hex: String): Either[String, Array[Byte]] =
    if hex.length % 2 != 0 then Left("Invalid hex: odd number of digits")
    else
      val out = new Array[Byte](hex.length / 2)
      var i = 0
      while i < out.length do
        val hi = Character.digit(hex.charAt(2 * i), 16)
        val lo = Character.digit(hex.charAt(2 * i + 1), 16)
        if hi < 0 || lo < 0 then return Left("Invalid hex character")
        out(i) = ((hi << 4) | lo).toByte
        i += 1
      Right(out)

  def compareUnsigned(a: Array[Byte], b: Array[Byte]): Int =
    java.util.Arrays.compareUnsigned(a, b)

  /** Builder for constructing JamBytes incrementally */
  final class Builder:
    private var buffer: Array[Byte] = new Array[Byte](64)
    private var len: Int = 0
    private def ensure(n: Int): Unit =
      if len + n > buffer.length then buffer = java.util.Arrays.copyOf(buffer, math.max(buffer.length * 2, len + n))
    def +=(b: Byte): this.type =
      ensure(1)
      buffer(len) = b
      len += 1
      this
    def ++=(bytes: Array[Byte]): this.type =
      ensure(bytes.length)
      System.arraycopy(bytes, 0, buffer, len, bytes.length)
      len += bytes.length
      this
    def ++=(bytes: JamBytes): this.type = this ++= bytes.underlying
    def result(): JamBytes = new JamBytes(java.util.Arrays.copyOf(buffer, len))
    def clear(): Unit = len = 0
    def size: Int = len

  def newBuilder: Builder = new Builder

  given Decoder[JamBytes] = Decoder.decodeString.emap(hex =>
    fromHex(hex).left.map(err => s"Invalid hex: $err")
  )

  /** Ordering for JamBytes based on lexicographic unsigned comparison.
    */
  given Ordering[JamBytes] = (a, b) => java.util.Arrays.compareUnsigned(a.underlying, b.underlying)
