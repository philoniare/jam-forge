package io.forge.jam.core

import _root_.scodec.bits.ByteVector
import spire.math.UByte
import io.circe.Decoder

/**
 * JamBytes wraps scodec.bits.ByteVector to provide a stable API.
 */
final class JamBytes private (private val underlying: ByteVector) extends AnyVal:
  // Core ByteVector operations
  def length: Int = underlying.length.toInt
  def size: Long = underlying.size
  def isEmpty: Boolean = underlying.isEmpty
  def nonEmpty: Boolean = underlying.nonEmpty

  // Element access
  def apply(index: Long): Byte = underlying(index)
  def apply(index: Int): Byte = underlying(index.toLong)

  // Conversion methods
  def toArray: Array[Byte] = underlying.toArray
  def toByteVector: ByteVector = underlying
  def toSeq: Seq[Byte] = underlying.toSeq
  def toHex: String = underlying.toHex
  def toHexWithPrefix: String = "0x" + underlying.toHex

  // Concatenation
  def ++(other: JamBytes): JamBytes = new JamBytes(underlying ++ other.underlying)
  def :+(byte: Byte): JamBytes = new JamBytes(underlying :+ byte)
  def +:(byte: Byte): JamBytes = new JamBytes(byte +: underlying)

  // Slicing
  def take(n: Long): JamBytes = new JamBytes(underlying.take(n))
  def drop(n: Long): JamBytes = new JamBytes(underlying.drop(n))
  def slice(from: Long, until: Long): JamBytes = new JamBytes(underlying.slice(from, until))

  // Other operations
  def reverse: JamBytes = new JamBytes(underlying.reverse)
  def bytes: ByteVector = underlying
  def copyToArray(dest: Array[Byte], destPos: Int, srcPos: Int, len: Int): Unit =
    underlying.toArray.slice(srcPos, srcPos + len).copyToArray(dest, destPos)

  // Comparison
  override def toString: String = s"JamBytes(${underlying.toHex})"
  def signedAt(index: Int): Byte = underlying(index.toLong)

  // Fold operations
  def foldLeft[A](z: A)(op: (A, UByte) => A): A =
    underlying.toArray.foldLeft(z)((acc, b) => op(acc, UByte(b)))

/**
 * Companion object providing factory methods and utilities for JamBytes.
 */
object JamBytes:
  val empty: JamBytes = new JamBytes(ByteVector.empty)

  def apply(bytes: Array[Byte]): JamBytes = new JamBytes(ByteVector(bytes))
  def apply(bytes: Byte*): JamBytes = new JamBytes(ByteVector(bytes*))
  def fromSeq(bytes: Seq[Byte]): JamBytes = new JamBytes(ByteVector(bytes))
  def fromByteVector(bv: ByteVector): JamBytes = new JamBytes(bv)
  def zeros(size: Int): JamBytes = new JamBytes(ByteVector.fill(size.toLong)(0))
  def fill(size: Int)(value: Byte): JamBytes = new JamBytes(ByteVector.fill(size.toLong)(value))

  def fromHex(hex: String): Either[String, JamBytes] =
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    ByteVector.fromHex(cleanHex) match
      case Some(bv) => Right(new JamBytes(bv))
      case None => Left("Invalid hex character")

  def fromHexUnsafe(hex: String): JamBytes =
    fromHex(hex).fold(msg => throw new IllegalArgumentException(msg), identity)

  def concat(parts: JamBytes*): JamBytes =
    if parts.isEmpty then empty
    else new JamBytes(parts.map(_.underlying).reduce(_ ++ _))

  def compareUnsigned(a: Array[Byte], b: Array[Byte]): Int =
    val len = math.min(a.length, b.length)
    var i = 0
    while i < len do
      val diff = (a(i).toInt & 0xff) - (b(i).toInt & 0xff)
      if diff != 0 then return diff
      i += 1
    a.length - b.length

  /** Builder for constructing JamBytes incrementally */
  final class Builder:
    private var buffer: ByteVector = ByteVector.empty
    def +=(b: Byte): this.type =
      buffer = buffer :+ b; this
    def ++=(bytes: Array[Byte]): this.type =
      buffer = buffer ++ ByteVector(bytes); this
    def ++=(bytes: JamBytes): this.type =
      buffer = buffer ++ bytes.underlying; this
    def result(): JamBytes = new JamBytes(buffer)
    def clear(): Unit = buffer = ByteVector.empty
    def size: Int = buffer.size.toInt

  def newBuilder: Builder = new Builder

  given Decoder[JamBytes] = Decoder.decodeString.emap(hex => fromHex(hex).left.map(err => s"Invalid hex: $err"))

  /** Ordering for JamBytes based on lexicographic byte comparison */
  given Ordering[JamBytes] = Ordering.by(_.toHex)
