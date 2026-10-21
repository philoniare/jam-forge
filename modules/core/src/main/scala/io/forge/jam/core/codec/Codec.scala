package io.forge.jam.core.codec

import io.forge.jam.core.JamBytes

import scala.annotation.targetName
import scala.deriving.Mirror

/** Thrown by [[ByteReader]] and codecs when input bytes cannot be decoded. */
final class CodecDecodingException(message: String, cause: Throwable = null)
    extends IllegalArgumentException(message, cause)

/** Growable little-endian byte sink that codecs write into. */
final class ByteWriter(initialCapacity: Int = 64):
  private var buf: Array[Byte] = new Array[Byte](math.max(initialCapacity, 16))
  private var len: Int = 0

  def size: Int = len

  private inline def ensure(n: Int): Unit =
    if len + n > buf.length then grow(len + n)

  private def grow(min: Int): Unit =
    var cap = buf.length * 2
    if cap < min then cap = min
    buf = java.util.Arrays.copyOf(buf, cap)

  def u8(v: Int): Unit =
    ensure(1)
    buf(len) = v.toByte
    len += 1

  def u16(v: Int): Unit =
    ensure(2)
    buf(len) = v.toByte
    buf(len + 1) = (v >>> 8).toByte
    len += 2

  def u24(v: Int): Unit =
    ensure(3)
    buf(len) = v.toByte
    buf(len + 1) = (v >>> 8).toByte
    buf(len + 2) = (v >>> 16).toByte
    len += 3

  def u32(v: Long): Unit = i32(v.toInt)

  def i32(v: Int): Unit =
    ensure(4)
    buf(len) = v.toByte
    buf(len + 1) = (v >>> 8).toByte
    buf(len + 2) = (v >>> 16).toByte
    buf(len + 3) = (v >>> 24).toByte
    len += 4

  def u64(v: Long): Unit =
    ensure(8)
    var i = 0
    while i < 8 do
      buf(len + i) = (v >>> (8 * i)).toByte
      i += 1
    len += 8

  /** Little-endian unsigned integer of `size` bytes (GP 0.8.0 appendix C, E_l). */
  def uintLE(v: Long, size: Int): Unit =
    ensure(size)
    var i = 0
    while i < size do
      buf(len + i) = (v >>> (8 * i)).toByte
      i += 1
    len += size

  def bytes(a: Array[Byte]): Unit = bytes(a, 0, a.length)

  def bytes(a: Array[Byte], offset: Int, length: Int): Unit =
    ensure(length)
    System.arraycopy(a, offset, buf, len, length)
    len += length

  def bytes(a: JamBytes): Unit = bytes(a.toArrayUnsafe)

  /** Compact natural number (GP 0.8.0 appendix C, eq. C.6). */
  def compact(value: Long): Unit =
    ensure(9)
    if value >= 0 && value < 128 then
      buf(len) = value.toByte
      len += 1
    else if java.lang.Long.compareUnsigned(value, 1L << 56) >= 0 then
      buf(len) = 0xff.toByte
      var i = 0
      while i < 8 do
        buf(len + 1 + i) = (value >>> (8 * i)).toByte
        i += 1
      len += 9
    else
      var l = 1
      while value >>> (7 * (l + 1)) != 0 do l += 1
      buf(len) = ((256 - (1 << (8 - l))) + (value >>> (8 * l)).toInt).toByte
      var i = 0
      while i < l do
        buf(len + 1 + i) = (value >>> (8 * i)).toByte
        i += 1
      len += 1 + l

  def toArray: Array[Byte] =
    if len == buf.length then buf else java.util.Arrays.copyOf(buf, len)

/** Cursor over a byte array that codecs read from. Every read is bounds-checked and fails with
  * [[CodecDecodingException]].
  */
final class ByteReader(private val buf: Array[Byte], start: Int, private val end: Int):
  private var pos: Int = start

  def this(buf: Array[Byte]) = this(buf, 0, buf.length)

  def position: Int = pos
  def remaining: Int = end - pos
  def isEmpty: Boolean = pos >= end

  def fail(message: String): Nothing = throw new CodecDecodingException(message)

  private inline def need(n: Int): Unit =
    if n < 0 || end - pos < n then
      fail(s"cannot read $n byte(s): only ${end - pos} remaining")

  def u8(): Int =
    need(1)
    val v = buf(pos) & 0xff
    pos += 1
    v

  def u16(): Int =
    need(2)
    val v = (buf(pos) & 0xff) | ((buf(pos + 1) & 0xff) << 8)
    pos += 2
    v

  def u24(): Int =
    need(3)
    val v = (buf(pos) & 0xff) | ((buf(pos + 1) & 0xff) << 8) | ((buf(pos + 2) & 0xff) << 16)
    pos += 3
    v

  def i32(): Int =
    need(4)
    val v = (buf(pos) & 0xff) | ((buf(pos + 1) & 0xff) << 8) | ((buf(pos + 2) & 0xff) << 16) |
      ((buf(pos + 3) & 0xff) << 24)
    pos += 4
    v

  def u32(): Long = i32().toLong & 0xffffffffL

  def u64(): Long = uintLE(8)

  def uintLE(size: Int): Long =
    need(size)
    var acc = 0L
    var i = 0
    while i < size do
      acc |= (buf(pos + i).toLong & 0xff) << (8 * i)
      i += 1
    pos += size
    acc

  def bytes(n: Int): Array[Byte] =
    need(n)
    val out = java.util.Arrays.copyOfRange(buf, pos, pos + n)
    pos += n
    out

  def skip(n: Int): Unit =
    need(n)
    pos += n

  /** A reader over the next `n` bytes; this reader moves past them. */
  def sub(n: Int): ByteReader =
    need(n)
    val r = new ByteReader(buf, pos, pos + n)
    pos += n
    r

  /** Compact natural number (GP 0.8.0 appendix C, eq. C.6). Rejects non-minimal encodings. */
  def compact(): Long =
    val prefix = u8()
    if prefix < 128 then prefix.toLong
    else if prefix == 0xff then
      val v = uintLE(8)
      if java.lang.Long.compareUnsigned(v, 1L << 56) < 0 then
        fail("compactInteger: non-minimal encoding (9-byte prefix)")
      v
    else
      val l = Integer.numberOfLeadingZeros(~(prefix << 24))
      val base = 256 - (1 << (8 - l))
      val v = ((prefix - base).toLong << (8 * l)) | uintLE(l)
      if v < (1L << (7 * l)) then fail(s"compactInteger: non-minimal encoding (l=$l prefix)")
      v

  /** Compact natural number that must fit an Int (used for lengths and counts). */
  def compactInt(): Int =
    val v = compact()
    if v < 0 || v > Int.MaxValue then fail(s"compactInt value $v exceeds Int.MaxValue")
    v.toInt

/** Binary codec for the JAM serialization format (GP 0.8.0 appendix C).
  *
  * Codecs write into a [[ByteWriter]] and read from a [[ByteReader]], so composing them allocates no intermediate
  * buffers. Decode failures are [[CodecDecodingException]]s inside codecs and `Left` at the `decode*` entry points.
  */
trait Codec[A]:
  self =>

  def write(out: ByteWriter, value: A): Unit
  def read(in: ByteReader): A

  /** Initial buffer size for [[encode]]; the writer grows as needed. */
  def sizeHint: Int = 64

  final def encode(value: A): Array[Byte] =
    val w = new ByteWriter(sizeHint)
    write(w, value)
    w.toArray

  final def encodeJam(value: A): JamBytes = JamBytes.wrap(encode(value))

  /** Decodes a value from the start of `bytes`, ignoring any trailing bytes. */
  final def decode(bytes: Array[Byte]): Either[String, A] = decodeAt(bytes, 0).map(_._1)

  final def decode(bytes: JamBytes): Either[String, A] = decode(bytes.toArrayUnsafe)

  /** Decodes a value at `offset`, returning it with the number of bytes consumed. */
  final def decodeAt(bytes: Array[Byte], offset: Int): Either[String, (A, Int)] =
    if offset < 0 || offset > bytes.length then Left(s"offset $offset out of range 0..${bytes.length}")
    else
      try
        val in = new ByteReader(bytes, offset, bytes.length)
        val v = read(in)
        Right((v, in.position - offset))
      catch case e: CodecDecodingException => Left(e.getMessage)

  /** Decodes a value that must use every byte of `bytes`. */
  final def decodeExact(bytes: Array[Byte]): Either[String, A] =
    decodeAt(bytes, 0).flatMap { case (v, used) =>
      if used == bytes.length then Right(v)
      else Left(s"${bytes.length - used} trailing byte(s) after a valid value")
    }

  /** Decodes from the start of `bytes`, throwing [[CodecDecodingException]] on failure. */
  final def decodeUnsafe(bytes: Array[Byte]): A = read(new ByteReader(bytes))

  final def xmap[B](f: A => B, g: B => A): Codec[B] = new Codec[B]:
    def write(out: ByteWriter, value: B): Unit = self.write(out, g(value))
    def read(in: ByteReader): B = f(self.read(in))
    override def sizeHint: Int = self.sizeHint

  /** Like [[xmap]], but decoding can reject a value. */
  final def emap[B](f: A => Either[String, B], g: B => A): Codec[B] = new Codec[B]:
    def write(out: ByteWriter, value: B): Unit = self.write(out, g(value))
    def read(in: ByteReader): B = f(self.read(in)) match
      case Right(b)  => b
      case Left(err) => in.fail(err)
    override def sizeHint: Int = self.sizeHint

  /** Fails decoding unless the reader is exhausted afterwards. */
  final def complete: Codec[A] = new Codec[A]:
    def write(out: ByteWriter, value: A): Unit = self.write(out, value)
    def read(in: ByteReader): A =
      val v = self.read(in)
      if !in.isEmpty then in.fail(s"${in.remaining} trailing byte(s) after a valid value")
      v
    override def sizeHint: Int = self.sizeHint

object Codec:
  def apply[A](using c: Codec[A]): Codec[A] = c

  /** Builds a codec from a writer and a reader. */
  def of[A](w: (ByteWriter, A) => Unit, r: ByteReader => A, hint: Int = 64): Codec[A] = new Codec[A]:
    def write(out: ByteWriter, value: A): Unit = w(out, value)
    def read(in: ByteReader): A = r(in)
    override def sizeHint: Int = hint

  /** Defers construction, for recursive codecs. */
  def lazily[A](c: => Codec[A]): Codec[A] = new Codec[A]:
    private lazy val inner = c
    def write(out: ByteWriter, value: A): Unit = inner.write(out, value)
    def read(in: ByteReader): A = inner.read(in)

  extension [A](head: Codec[A])
    /** `a :: b :: c` builds a codec for the tuple `(A, B, C)`, encoding fields in order. */
    def ::[T <: Tuple](tail: Codec[T]): Codec[A *: T] = new Codec[A *: T]:
      def write(out: ByteWriter, value: A *: T): Unit =
        head.write(out, value.head)
        tail.write(out, value.tail)
      def read(in: ByteReader): A *: T =
        val a = head.read(in)
        a *: tail.read(in)

    @targetName("consPair")
    def ::[B](last: Codec[B]): Codec[(A, B)] = new Codec[(A, B)]:
      def write(out: ByteWriter, value: (A, B)): Unit =
        head.write(out, value._1)
        last.write(out, value._2)
      def read(in: ByteReader): (A, B) =
        val a = head.read(in)
        (a, last.read(in))

  extension [T <: Tuple](c: Codec[T])
    /** Maps a tuple codec onto the case class whose fields have the tuple's types. */
    def as[P <: Product](using m: Mirror.ProductOf[P] { type MirroredElemTypes = T }): Codec[P] =
      c.xmap(t => m.fromProduct(t), p => Tuple.fromProductTyped(p))

  extension (out: ByteWriter)
    def put[A](value: A)(using c: Codec[A]): Unit = c.write(out, value)

  extension (in: ByteReader)
    def get[A](using c: Codec[A]): A = c.read(in)
