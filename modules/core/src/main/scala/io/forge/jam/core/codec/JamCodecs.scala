package io.forge.jam.core.codec

import spire.math.{UByte, UInt}
import io.forge.jam.core.primitives.*
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.JamBytes

/** JAM codecs for Spire unsigned integers and primitive types (GP 0.8.0 appendix C).
  */
object JamCodecs:

  /** All remaining bytes; only meaningful as the last field. */
  given jamBytesCodec: Codec[JamBytes] = codecs.remainingBytes

  given ubyteCodec: Codec[UByte] = Codec.of((w, u) => w.u8(u.toInt), r => UByte(r.u8()), 1)

  given uintCodec: Codec[UInt] = Codec.of((w, u) => w.i32(u.signed), r => UInt(r.i32()), 4)

  given hashCodec: Codec[Hash] = Codec.of((w, h) => w.bytes(h.toArrayUnsafe), r => Hash.wrap(r.bytes(Hash.Size)), 32)

  given bandersnatchPublicKeyCodec: Codec[BandersnatchPublicKey] = Codec.of(
    (w, k) => w.bytes(k.toArrayUnsafe),
    r => BandersnatchPublicKey.wrap(r.bytes(BandersnatchPublicKey.Size)),
    BandersnatchPublicKey.Size
  )

  given ed25519PublicKeyCodec: Codec[Ed25519PublicKey] = Codec.of(
    (w, k) => w.bytes(k.toArrayUnsafe),
    r => Ed25519PublicKey.wrap(r.bytes(Ed25519PublicKey.Size)),
    Ed25519PublicKey.Size
  )

  given blsPublicKeyCodec: Codec[BlsPublicKey] = Codec.of(
    (w, k) => w.bytes(k.toArrayUnsafe),
    r => BlsPublicKey.wrap(r.bytes(BlsPublicKey.Size)),
    BlsPublicKey.Size
  )

  /** Re-export TicketMark codec from its companion object (tickets.scala) */
  val ticketMarkCodec: Codec[TicketMark] = {
    import io.forge.jam.core.types.tickets.TicketMark.given
    summon[Codec[TicketMark]]
  }

  /** JAM compact integer codec for non-negative Long values (0 to 2^64-1), GP 0.8.0 eq. (C.6). */
  val compactInteger: Codec[Long] = codecs.compactLong

  val compactInt: Codec[Int] = codecs.compactInt

  def optionCodec[A](codec: Codec[A]): Codec[Option[A]] = codecs.option(codec)

  def compactPrefixedList[A](codec: Codec[A]): Codec[List[A]] = codecs.compactPrefixedList(codec)

  def fixedSizeList[A](codec: Codec[A], size: Int): Codec[List[A]] = codecs.fixedSizeList(codec, size)

  def stfResultCodec[A, E](using okCodec: Codec[A], errCodec: Codec[E]): Codec[Either[E, A]] =
    codecs.result(okCodec, errCodec)

  /** Extension method to encode values with their given codec */
  extension [A](value: A)
    def encode(using codec: Codec[A]): JamBytes = codec.encodeJam(value)

  /** Encode a non-negative Long value as a JAM compact integer.
    *
    * @param value
    *   non-negative Long value (0 to 2^64-1)
    * @return
    *   byte array containing the compact integer encoding
    */
  def encodeCompactInteger(value: Long): Array[Byte] =
    val w = new ByteWriter(9)
    w.compact(value)
    w.toArray

  /** Helper to decode compact integer from byte array at offset.
    *
    * @param data
    *   byte array to decode from
    * @param offset
    *   starting offset in bytes
    * @return
    *   tuple of (decoded value, bytes consumed)
    * @throws IllegalArgumentException
    *   if offset is negative or exceeds data length
    */
  def decodeCompactInteger(data: Array[Byte], offset: Int): (Long, Int) =
    require(offset >= 0, s"Offset must be non-negative: $offset")
    require(offset < data.length, s"Offset $offset >= data length ${data.length}")
    val in = new ByteReader(data, offset, data.length)
    val v =
      try in.compact()
      catch case e: CodecDecodingException => throw new CodecDecodingException(s"decodeCompactInteger: ${e.getMessage}")
    (v, in.position - offset)

  /** Helper to decode u32 little-endian from byte array at offset */
  def decodeU32LE(data: Array[Byte], offset: Int): spire.math.UInt =
    val in = new ByteReader(data, offset, data.length)
    try UInt(in.i32())
    catch case e: CodecDecodingException => throw new CodecDecodingException(s"decodeU32LE: ${e.getMessage}")

  /** Helper to encode u32 little-endian to byte array */
  def encodeU32LE(value: spire.math.UInt): Array[Byte] =
    val w = new ByteWriter(4)
    w.i32(value.signed)
    w.toArray

  /** Helper to encode u64 little-endian to byte array */
  def encodeU64LE(value: spire.math.ULong): Array[Byte] =
    val w = new ByteWriter(8)
    w.u64(value.signed)
    w.toArray
