package io.forge.jam.core

/** Re-exports from LegacyCodec for backward compatibility. */
object codec:
  @deprecated("Use scodec Codec[A] from io.forge.jam.core.scodec.JamCodecs", "")
  type JamEncoder[A] = LegacyCodec.JamEncoder[A]

  @deprecated("Use scodec Codec[A] from io.forge.jam.core.scodec.JamCodecs", "")
  type JamDecoder[A] = LegacyCodec.JamDecoder[A]

  @deprecated("Use JamCodecs.compactInteger from scodec package", "")
  type CompactInt = LegacyCodec.CompactInt

  @deprecated("Use JamCodecs.compactInteger from scodec package", "")
  val CompactInt = LegacyCodec.CompactInt

  export LegacyCodec.{value, toInt}
  export LegacyCodec.encode
  export LegacyCodec.decodeAs

  export LegacyCodec.{
    given_JamEncoder_UByte,
    given_JamEncoder_UShort,
    given_JamEncoder_UInt,
    given_JamEncoder_ULong,
    given_JamEncoder_JamBytes,
    given_JamEncoder_CompactInt,
    given_JamDecoder_UByte,
    given_JamDecoder_UShort,
    given_JamDecoder_UInt,
    given_JamDecoder_ULong,
    given_JamDecoder_JamBytes,
    given_JamDecoder_CompactInt,
    optionEncoder,
    optionDecoder,
    listEncoder,
    listDecoder
  }

  export LegacyCodec.{
    encodeU8,
    encodeU16LE,
    encodeU32LE,
    encodeU64LE,
    decodeU8,
    decodeU16LE,
    decodeU32LE,
    decodeU64LE,
    encodeVarint,
    encodeVarint64,
    decodeVarint,
    decodeVarint64,
    encodeCompactInteger,
    decodeCompactInteger,
    encodeFixedList,
    decodeFixedList,
    encodeFixedBytes,
    decodeFixedBytes
  }
