package io.forge.jam.core

import scala.collection.mutable.ArrayBuffer
import spire.math.UByte
import io.circe.Decoder

/**
 * Immutable byte array wrapper with value semantics.
 *
 * Unlike raw Array[Byte], JamBytes:
 * - Has content-based equals/hashCode
 * - Is immutable (defensive copies on construction and access)
 * - Provides convenient operations for JAM serialization
 * - Returns UByte (unsigned) to match Gray Paper's octet semantics
 *
 * Internal storage uses Array[Byte] for Java interop with crypto libraries.
 * Public API uses Spire's UByte to match JAM's unsigned octet semantics.
 */
final class JamBytes private (private val underlying: Array[Byte]) extends Ordered[JamBytes]:

  /** Length in bytes */
  def length: Int = underlying.length

  /** Alias for length */
  def size: Int = underlying.length

  /** Check if empty */
  def isEmpty: Boolean = underlying.isEmpty

  /** Check if non-empty */
  def nonEmpty: Boolean = underlying.nonEmpty

  /** Get unsigned octet at index (matches Gray Paper semantics) */
  def apply(index: Int): UByte = UByte(underlying(index))

  /** Get signed byte at index (for Java interop) */
  def signedAt(index: Int): Byte = underlying(index)

  /** Get a defensive copy of the underlying bytes (for Java interop) */
  def toArray: Array[Byte] = underlying.clone()

  /** Get a slice as new JamBytes */
  def slice(from: Int, until: Int): JamBytes =
    JamBytes.wrap(underlying.slice(from, until))

  /** Take first n bytes */
  def take(n: Int): JamBytes = slice(0, math.min(n, length))

  /** Drop first n bytes */
  def drop(n: Int): JamBytes = slice(math.min(n, length), length)

  /** Concatenate with another JamBytes */
  def ++(other: JamBytes): JamBytes =
    JamBytes.wrap(underlying ++ other.underlying)

  /** Concatenate with raw bytes */
  def ++(other: Array[Byte]): JamBytes =
    JamBytes.wrap(underlying ++ other)

  /** Prepend a byte */
  def +:(byte: Byte): JamBytes =
    JamBytes.wrap(byte +: underlying)

  /** Append a byte */
  def :+(byte: Byte): JamBytes =
    JamBytes.wrap(underlying :+ byte)

  /** Check if starts with prefix */
  def startsWith(prefix: JamBytes): Boolean =
    length >= prefix.length &&
      (0 until prefix.length).forall(i => underlying(i) == prefix.underlying(i))

  /** Check if starts with raw prefix */
  def startsWith(prefix: Array[Byte]): Boolean =
    length >= prefix.length &&
      (0 until prefix.length).forall(i => underlying(i) == prefix(i))

  /** Copy bytes into destination array */
  def copyToArray(dest: Array[Byte], destPos: Int = 0, srcPos: Int = 0, len: Int = length): Unit =
    System.arraycopy(underlying, srcPos, dest, destPos, math.min(len, length - srcPos))

  /** Convert to hex string */
  def toHex: String = underlying.map(b => f"${b & 0xff}%02x").mkString

  /** Convert to hex string with 0x prefix */
  def toHexWithPrefix: String = "0x" + toHex

  /** Iterator over unsigned octets */
  def iterator: Iterator[UByte] = underlying.iterator.map(UByte(_))

  /** Fold over unsigned octets */
  def foldLeft[A](z: A)(op: (A, UByte) => A): A =
    underlying.foldLeft(z)((acc, b) => op(acc, UByte(b)))

  /** Map over unsigned octets */
  def map(f: UByte => UByte): JamBytes =
    JamBytes.wrap(underlying.map(b => f(UByte(b)).signed))

  /** Zip with another JamBytes (returns pairs of UByte) */
  def zip(other: JamBytes): Array[(UByte, UByte)] =
    underlying.zip(other.underlying).map((a, b) => (UByte(a), UByte(b)))

  // ══════════════════════════════════════════════════════════════════════════
  // Comparison (lexicographic unsigned byte comparison)
  // ══════════════════════════════════════════════════════════════════════════

  def compare(that: JamBytes): Int =
    val minLen = math.min(this.length, that.length)
    var i = 0
    while i < minLen do
      val b1 = this.underlying(i) & 0xff
      val b2 = that.underlying(i) & 0xff
      if b1 != b2 then return b1 - b2
      i += 1
    this.length - that.length

  // ══════════════════════════════════════════════════════════════════════════
  // Equality (content-based)
  // ══════════════════════════════════════════════════════════════════════════

  override def equals(obj: Any): Boolean = obj match
    case that: JamBytes => java.util.Arrays.equals(this.underlying, that.underlying)
    case _ => false

  override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  override def toString: String =
    if length <= 32 then s"JamBytes($toHex)"
    else s"JamBytes($length bytes, ${take(16).toHex}...)"

object JamBytes:
  /** Empty byte sequence */
  val empty: JamBytes = new JamBytes(Array.emptyByteArray)

  /**
   * Compare two byte arrays lexicographically as unsigned values.
   * Returns negative if a < b, zero if equal, positive if a > b.
   */
  def compareUnsigned(a: Array[Byte], b: Array[Byte]): Int =
    val len = math.min(a.length, b.length)
    var i = 0
    while i < len do
      val diff = (a(i).toInt & 0xff) - (b(i).toInt & 0xff)
      if diff != 0 then return diff
      i += 1
    a.length - b.length

  /** Create from byte array (defensive copy) */
  def apply(bytes: Array[Byte]): JamBytes = new JamBytes(bytes.clone())

  /** Create from varargs */
  def apply(bytes: Byte*): JamBytes = new JamBytes(bytes.toArray)

  /** Create from sequence of bytes */
  def fromSeq(bytes: Seq[Byte]): JamBytes = new JamBytes(bytes.toArray)

  /** Wrap existing array without copying (internal use only!) */
  private[core] def wrap(bytes: Array[Byte]): JamBytes = new JamBytes(bytes)

  /** Create with specific size, filled with zeros */
  def zeros(size: Int): JamBytes = new JamBytes(new Array[Byte](size))

  /** Create with specific size, filled with given value */
  def fill(size: Int)(value: Byte): JamBytes = new JamBytes(Array.fill(size)(value))

  /** Create from hex string */
  def fromHex(hex: String): Either[String, JamBytes] =
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    if cleanHex.length % 2 != 0 then
      Left("Hex string must have even length")
    else
      try
        val bytes = cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
        Right(wrap(bytes))
      catch
        case _: NumberFormatException => Left("Invalid hex character")

  /** Unsafe fromHex that throws on invalid input */
  def fromHexUnsafe(hex: String): JamBytes =
    fromHex(hex).fold(msg => throw new IllegalArgumentException(msg), identity)

  /** Concatenate multiple JamBytes */
  def concat(parts: JamBytes*): JamBytes =
    if parts.isEmpty then empty
    else if parts.length == 1 then parts.head
    else
      val totalLen = parts.map(_.length).sum
      val result = new Array[Byte](totalLen)
      var offset = 0
      for part <- parts do
        System.arraycopy(part.underlying, 0, result, offset, part.length)
        offset += part.length
      wrap(result)

  /** Builder for efficient incremental construction */
  def newBuilder: Builder = new Builder

  final class Builder:
    private val buffer = ArrayBuffer.empty[Byte]

    def +=(b: Byte): this.type =
      buffer += b; this
    def ++=(bytes: Array[Byte]): this.type =
      buffer ++= bytes; this
    def ++=(bytes: JamBytes): this.type =
      buffer ++= bytes.underlying; this
    def result(): JamBytes = wrap(buffer.toArray)
    def clear(): Unit = buffer.clear()
    def size: Int = buffer.size

  given Decoder[JamBytes] = Decoder.decodeString.emap(hex => fromHex(hex).left.map(err => s"Invalid hex: $err"))
