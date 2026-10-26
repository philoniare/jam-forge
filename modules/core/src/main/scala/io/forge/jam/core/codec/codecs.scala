package io.forge.jam.core.codec

import io.forge.jam.core.JamBytes

import scala.collection.mutable.ListBuffer

/** Primitive codecs and combinators for the JAM serialization format (GP 0.8.0 appendix C). Integers are
  * little-endian; "compact" is the variable-length natural encoding of eq. (C.6).
  */
object codecs:

  val byte: Codec[Byte] = Codec.of((w, v) => w.u8(v), r => r.u8().toByte, 1)
  val uint8: Codec[Int] = Codec.of((w, v) => w.u8(v), _.u8(), 1)
  val uint16L: Codec[Int] = Codec.of((w, v) => w.u16(v), _.u16(), 2)
  val uint24L: Codec[Int] = Codec.of((w, v) => w.u24(v), _.u24(), 3)
  val uint32L: Codec[Long] = Codec.of((w, v) => w.u32(v), _.u32(), 4)
  val int32L: Codec[Int] = Codec.of((w, v) => w.i32(v), _.i32(), 4)
  val int64L: Codec[Long] = Codec.of((w, v) => w.u64(v), _.u64(), 8)

  /** Single-byte boolean: 0 or 1. */
  val bool: Codec[Boolean] = Codec.of(
    (w, v) => w.u8(if v then 1 else 0),
    r =>
      r.u8() match
        case 0 => false
        case 1 => true
        case b => r.fail(s"invalid boolean byte $b (expected 0 or 1)"),
    1
  )

  /** Compact natural (eq. C.6), 0 to 2^64-1 held as an unsigned Long. */
  val compactLong: Codec[Long] = Codec.of((w, v) => w.compact(v), _.compact(), 9)

  /** Compact natural that must fit an Int, for lengths and counts. */
  val compactInt: Codec[Int] = Codec.of((w, v) => w.compact(v.toLong), _.compactInt(), 9)

  /** Exactly `n` bytes. */
  def byteArray(n: Int): Codec[Array[Byte]] = Codec.of(
    (w, v) =>
      if v.length != n then throw new IllegalArgumentException(s"expected $n bytes, got ${v.length}")
      w.bytes(v),
    _.bytes(n),
    n
  )

  /** Exactly `n` bytes. */
  def bytes(n: Int): Codec[JamBytes] = Codec.of(
    (w, v) =>
      if v.length != n then throw new IllegalArgumentException(s"expected $n bytes, got ${v.length}")
      w.bytes(v),
    r => JamBytes.wrap(r.bytes(n)),
    n
  )

  /** All remaining bytes; only meaningful as the last field. */
  val remainingBytes: Codec[JamBytes] = Codec.of((w, v) => w.bytes(v), r => JamBytes.wrap(r.bytes(r.remaining)))

  /** Compact length prefix followed by that many bytes (eq. C.8, the `↕` prefix). */
  val compactBytes: Codec[JamBytes] = Codec.of(
    (w, v) =>
      w.compact(v.length.toLong)
      w.bytes(v),
    r => JamBytes.wrap(r.bytes(r.compactInt()))
  )

  /** Compact byte length, then the UTF-8 bytes of the string; malformed UTF-8 fails decoding. */
  val compactUtf8: Codec[String] = Codec.of(
    (w, v) =>
      val b = v.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      w.compact(b.length.toLong)
      w.bytes(b),
    r =>
      val b = r.bytes(r.compactInt())
      try
        java.nio.charset.StandardCharsets.UTF_8.newDecoder().decode(java.nio.ByteBuffer.wrap(b)).toString
      catch case e: java.nio.charset.CharacterCodingException => r.fail(s"invalid UTF-8: ${e.getMessage}")
  )

  /** Writes nothing and decodes to `value`. */
  def provide[A](value: A): Codec[A] = Codec.of((_, _) => (), _ => value, 0)

  /** `size` encoded with `sizeCodec`, then the value, which must decode within that many bytes. */
  def variableSize[A](sizeCodec: Codec[Int], valueCodec: Codec[A]): Codec[A] = new Codec[A]:
    def write(out: ByteWriter, value: A): Unit =
      val encoded = valueCodec.encode(value)
      sizeCodec.write(out, encoded.length)
      out.bytes(encoded)
    def read(in: ByteReader): A =
      val n = sizeCodec.read(in)
      valueCodec.read(in.sub(n))

  /** Option with a 0/1 discriminator byte (eq. C.11). */
  def option[A](c: Codec[A]): Codec[Option[A]] = new Codec[Option[A]]:
    def write(out: ByteWriter, value: Option[A]): Unit = value match
      case None => out.u8(0)
      case Some(a) =>
        out.u8(1)
        c.write(out, a)
    def read(in: ByteReader): Option[A] = in.u8() match
      case 0     => None
      case 1     => Some(c.read(in))
      case other => in.fail(s"Invalid option discriminator: $other (expected 0 or 1)")
    override def sizeHint: Int = 1 + c.sizeHint

  /** Exactly `n` elements, no length prefix. */
  def fixedSizeList[A](c: Codec[A], n: Int): Codec[List[A]] = new Codec[List[A]]:
    def write(out: ByteWriter, value: List[A]): Unit =
      if value.length != n then throw new IllegalArgumentException(s"expected $n elements, got ${value.length}")
      value.foreach(c.write(out, _))
    def read(in: ByteReader): List[A] = readN(in, c, n)
    override def sizeHint: Int = math.max(16, math.min(n * c.sizeHint, 1 << 20))

  /** Count encoded with `countCodec`, then that many elements. */
  def listOfN[A](countCodec: Codec[Int], c: Codec[A]): Codec[List[A]] = new Codec[List[A]]:
    def write(out: ByteWriter, value: List[A]): Unit =
      countCodec.write(out, value.length)
      value.foreach(c.write(out, _))
    def read(in: ByteReader): List[A] = readN(in, c, countCodec.read(in))

  /** Compact length prefix, then that many elements (eq. C.8). */
  def compactPrefixedList[A](c: Codec[A]): Codec[List[A]] = listOfN(compactInt, c)

  /** As [[compactPrefixedList]], for sequences read by index. */
  def compactPrefixedVector[A](c: Codec[A]): Codec[IndexedSeq[A]] = new Codec[IndexedSeq[A]]:
    def write(out: ByteWriter, value: IndexedSeq[A]): Unit =
      compactInt.write(out, value.length)
      value.foreach(c.write(out, _))
    def read(in: ByteReader): IndexedSeq[A] =
      val n = compactInt.read(in)
      if n < 0 then in.fail(s"negative element count $n")
      val b = Vector.newBuilder[A]
      var i = 0
      while i < n do
        b += c.read(in)
        i += 1
      b.result()

  /** Elements until the input is exhausted, no length prefix. */
  def listToEnd[A](c: Codec[A]): Codec[List[A]] = new Codec[List[A]]:
    def write(out: ByteWriter, value: List[A]): Unit = value.foreach(c.write(out, _))
    def read(in: ByteReader): List[A] =
      val buf = ListBuffer.empty[A]
      while !in.isEmpty do buf += c.read(in)
      buf.toList

  private def readN[A](in: ByteReader, c: Codec[A], n: Int): List[A] =
    if n < 0 then in.fail(s"negative element count $n")
    val buf = ListBuffer.empty[A]
    var i = 0
    while i < n do
      buf += c.read(in)
      i += 1
    buf.toList

  /** Either with a 0/1 discriminator byte: 0 = `Right`, 1 = `Left`, as STF results are encoded. */
  def result[E, A](ok: Codec[A], err: Codec[E]): Codec[Either[E, A]] = new Codec[Either[E, A]]:
    def write(out: ByteWriter, value: Either[E, A]): Unit = value match
      case Right(a) =>
        out.u8(0)
        ok.write(out, a)
      case Left(e) =>
        out.u8(1)
        err.write(out, e)
    def read(in: ByteReader): Either[E, A] = in.u8() match
      case 0     => Right(ok.read(in))
      case 1     => Left(err.read(in))
      case other => in.fail(s"Invalid StfResult discriminator: $other (expected 0 or 1)")
