package io.forge.jam.core.scodec

import scodec.*
import scodec.bits.*
import scodec.codecs.*
import spire.math.{UByte, UShort, UInt, ULong}
import io.forge.jam.core.primitives.*
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.JamBytes

final class CodecDecodingException(message: String, cause: Throwable = null)
    extends IllegalArgumentException(message, cause)

/** JAM-specific scodec codecs for Spire unsigned integers and primitive types.
  */
object JamCodecs:

  private inline def requireDecode[A](decoder: String, attempt: Attempt[A]): A =
    attempt match
      case Attempt.Successful(v) => v
      case Attempt.Failure(err)  =>
        throw new CodecDecodingException(s"$decoder: ${err.messageWithContext}")

  /** Codec for JamBytes wrapper around ByteVector */
  given jamBytesCodec: Codec[JamBytes] = bytes.xmap(
    bv => JamBytes.fromByteVector(bv),
    jb => jb.toByteVector
  )

  given ubyteCodec: Codec[UByte] = byte.xmap(
    b => UByte(b),
    u => u.toByte
  )

  given ushortCodec: Codec[UShort] = uint16L.xmap(
    i => UShort(i),
    u => u.toInt
  )

  given uintCodec: Codec[UInt] = uint32L.xmap(
    l => UInt(l.toInt),
    u => u.toLong & 0xffffffffL
  )

  given ulongCodec: Codec[ULong] = int64L.xmap(
    l => ULong(l),
    u => u.signed
  )

  given hashCodec: Codec[Hash] = fixedSizeBytes(Hash.Size.toLong, bytes).xmap(
    bv => Hash.fromByteVectorUnsafe(bv),
    hash => hash.toByteVector
  )

  given bandersnatchPublicKeyCodec: Codec[BandersnatchPublicKey] =
    fixedSizeBytes(BandersnatchPublicKey.Size.toLong, bytes).xmap(
      bv => BandersnatchPublicKey.fromByteVectorUnsafe(bv),
      key => key.toByteVector
    )

  given ed25519PublicKeyCodec: Codec[Ed25519PublicKey] =
    fixedSizeBytes(Ed25519PublicKey.Size.toLong, bytes).xmap(
      bv => Ed25519PublicKey.fromByteVectorUnsafe(bv),
      key => key.toByteVector
    )

  given blsPublicKeyCodec: Codec[BlsPublicKey] =
    fixedSizeBytes(BlsPublicKey.Size.toLong, bytes).xmap(
      bv => BlsPublicKey.fromByteVectorUnsafe(bv),
      key => key.toByteVector
    )

  /** Re-export TicketMark codec from its companion object (tickets.scala) */
  val ticketMarkCodec: Codec[TicketMark] = {
    import io.forge.jam.core.types.tickets.TicketMark.given
    summon[Codec[TicketMark]]
  }

  /** JAM compact integer codec for encoding non-negative Long values (0 to
    * 2^64-1).
    */
  val compactInteger: Codec[Long] = new Codec[Long]:

    override def sizeBound: SizeBound = SizeBound.bounded(8, 72) // 1-9 bytes

    override def encode(value: Long): Attempt[BitVector] =
      Attempt.successful(BitVector(encodeCompactInteger(value)))

    override def decode(bits: BitVector): Attempt[DecodeResult[Long]] =
      val bytes = bits.bytes
      if bytes.isEmpty then Attempt.failure(Err.insufficientBits(8, 0))
      else
        val prefix = bytes(0) & 0xff

        // Special case: prefix = 0 means value = 0
        if prefix == 0 then Attempt.successful(DecodeResult(0L, bits.drop(8)))
        else
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

          val totalBytes = 1 + l
          val totalBits = totalBytes * 8L

          if bits.sizeLessThan(totalBits) then
            Attempt.failure(Err.insufficientBits(totalBits, bits.size))
          else
            // Handle special case where prefix = 255 means 8 bytes follow
            if prefix == 255 then
              var value = 0L
              for i <- 0 until 8 do
                value = value | ((bytes(1 + i) & 0xffL) << (8 * i))
              Attempt.successful(DecodeResult(value, bits.drop(totalBits)))
            // For l=0, the value is just the prefix itself
            else if l == 0 then
              Attempt.successful(DecodeResult(prefix.toLong, bits.drop(8)))
            else
              // Calculate the high bits from prefix
              val base = 256 - (1 << (8 - l))
              val highBits = (prefix - base).toLong << (8 * l)

              // Read the low bytes (l bytes in little-endian)
              var lowBits = 0L
              for i <- 0 until l do
                lowBits = lowBits | ((bytes(1 + i) & 0xffL) << (8 * i))

              Attempt.successful(
                DecodeResult(highBits | lowBits, bits.drop(totalBits))
              )

  val compactInt: Codec[Int] = compactInteger.narrow(
    value =>
      if value > Int.MaxValue then
        Attempt.failure(Err(s"compactInt value $value exceeds Int.MaxValue"))
      else if value < Int.MinValue then
        Attempt.failure(Err(s"compactInt value $value is below Int.MinValue"))
      else Attempt.successful(value.toInt),
    _.toLong
  )

  def optionCodec[A](codec: Codec[A]): Codec[Option[A]] = new Codec[Option[A]]:

    override def sizeBound: SizeBound = SizeBound.bounded(
      8,
      codec.sizeBound.upperBound.map(_ + 8).getOrElse(Long.MaxValue)
    )

    override def encode(value: Option[A]): Attempt[BitVector] = value match
      case None =>
        Attempt.successful(BitVector(0x00))
      case Some(a) =>
        codec.encode(a).map(bits => BitVector(0x01) ++ bits)

    override def decode(bits: BitVector): Attempt[DecodeResult[Option[A]]] =
      if bits.sizeLessThan(8) then
        Attempt.failure(Err.insufficientBits(8, bits.size))
      else
        val discriminator = (bits.take(8).toByteVector(0) & 0xff).toByte
        val remainder = bits.drop(8)
        discriminator match
          case 0 =>
            Attempt.successful(DecodeResult(None, remainder))
          case 1 =>
            codec
              .decode(remainder)
              .map(result => DecodeResult(Some(result.value), result.remainder))
          case other =>
            Attempt.failure(
              Err(s"Invalid option discriminator: $other (expected 0 or 1)")
            )

  def compactPrefixedList[A](codec: Codec[A]): Codec[List[A]] =
    listOfN(compactInt, codec)

  def fixedSizeList[A](codec: Codec[A], size: Int): Codec[List[A]] =
    vectorOfN(provide(size), codec).xmap(_.toList, _.toVector)

  def fixedSizeByteVector(size: Long): Codec[ByteVector] =
    fixedSizeBytes(size, bytes)

  sealed trait TicketsOrKeys

  object TicketsOrKeys:
    final case class Tickets(tickets: List[TicketMark]) extends TicketsOrKeys
    final case class Keys(keys: List[BandersnatchPublicKey])
        extends TicketsOrKeys

  def ticketsOrKeysCodec(epochLength: Int): Codec[TicketsOrKeys] =
    val ticketsListCodec: Codec[List[TicketMark]] =
      fixedSizeList(ticketMarkCodec, epochLength)
    val keysListCodec: Codec[List[BandersnatchPublicKey]] =
      fixedSizeList(bandersnatchPublicKeyCodec, epochLength)

    discriminated[TicketsOrKeys]
      .by(byte)
      .subcaseP(0) { case t: TicketsOrKeys.Tickets => t }(
        ticketsListCodec.xmap(TicketsOrKeys.Tickets.apply, _.tickets)
      )
      .subcaseP(1) { case k: TicketsOrKeys.Keys => k }(
        keysListCodec.xmap(TicketsOrKeys.Keys.apply, _.keys)
      )

  def stfResultCodec[A, E](using
      okCodec: Codec[A],
      errCodec: Codec[E]
  ): Codec[Either[E, A]] =
    new Codec[Either[E, A]]:
      override def sizeBound: SizeBound =
        val okBound = okCodec.sizeBound
        val errBound = errCodec.sizeBound
        SizeBound.bounded(
          8 + math.min(okBound.lowerBound, errBound.lowerBound),
          8 + math.max(
            okBound.upperBound.getOrElse(Long.MaxValue),
            errBound.upperBound.getOrElse(Long.MaxValue)
          )
        )

      override def encode(value: Either[E, A]): Attempt[BitVector] = value match
        case Right(ok) =>
          okCodec.encode(ok).map(bits => BitVector(0x00) ++ bits)
        case Left(err) =>
          errCodec.encode(err).map(bits => BitVector(0x01) ++ bits)

      override def decode(
          bits: BitVector
      ): Attempt[DecodeResult[Either[E, A]]] =
        if bits.sizeLessThan(8) then
          Attempt.failure(Err.insufficientBits(8, bits.size))
        else
          val discriminator = (bits.take(8).toByteVector(0) & 0xff).toByte
          val remainder = bits.drop(8)
          discriminator match
            case 0 =>
              okCodec
                .decode(remainder)
                .map(result =>
                  DecodeResult(Right(result.value), result.remainder)
                )
            case 1 =>
              errCodec
                .decode(remainder)
                .map(result =>
                  DecodeResult(Left(result.value), result.remainder)
                )
            case other =>
              Attempt.failure(
                Err(
                  s"Invalid StfResult discriminator: $other (expected 0 or 1)"
                )
              )

  /** Extension method to encode values using scodec Codec */
  extension [A](value: A)
    def encode(using codec: Codec[A]): JamBytes =
      JamBytes.fromByteVector(codec.encode(value).require.bytes)

  /** Encode a non-negative Long value as a JAM compact integer.
    *
    * @param value
    *   non-negative Long value (0 to 2^64-1)
    * @return
    *   byte array containing the compact integer encoding
    * @throws IllegalArgumentException
    *   if value is negative
    */
  def encodeCompactInteger(value: Long): Array[Byte] =
    if value == 0L then return Array[Byte](0)

    if java.lang.Long.compareUnsigned(value, 1L << 56) >= 0 then
      val result = new Array[Byte](9)
      result(0) = 0xff.toByte
      var i = 0
      while i < 8 do
        result(1 + i) = ((value >>> (8 * i)) & 0xff).toByte
        i += 1
      return result

    var l = 0
    while l <= 7 do
      val upperBound = 1L << (7 * (l + 1))
      if java.lang.Long.compareUnsigned(value, upperBound) < 0 then
        val prefixVal = (256 - (1 << (8 - l))) + (value >>> (8 * l)).toInt
        val prefixByte = prefixVal.toByte
        val remainder =
          if l == 0 then 0L else value & ((1L << (8 * l)) - 1)
        val result = new Array[Byte](1 + l)
        result(0) = prefixByte
        var i = 0
        while i < l do
          result(1 + i) = ((remainder >>> (8 * i)) & 0xff).toByte
          i += 1
        return result
      l += 1

    throw new AssertionError(s"unreachable: value=$value")

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
    require(
      offset < data.length,
      s"Offset $offset >= data length ${data.length}"
    )
    val bits = BitVector(data).drop(offset * 8L)
    val result =
      requireDecode("decodeCompactInteger", compactInteger.decode(bits))
    val consumed = ((bits.size - result.remainder.size) / 8).toInt
    (result.value, consumed)

  /** Helper to decode u32 little-endian from byte array at offset */
  def decodeU32LE(data: Array[Byte], offset: Int): spire.math.UInt =
    val bits = BitVector(data).drop(offset * 8L)
    val result = requireDecode("decodeU32LE", uint32L.decode(bits))
    spire.math.UInt((result.value & 0xffffffffL).toInt)

  /** Helper to encode u32 little-endian to byte array */
  def encodeU32LE(value: spire.math.UInt): Array[Byte] =
    uint32L.encode(value.toLong & 0xffffffffL).require.toByteArray

  /** Helper to encode u64 little-endian to byte array */
  def encodeU64LE(value: spire.math.ULong): Array[Byte] =
    int64L.encode(value.signed).require.toByteArray
