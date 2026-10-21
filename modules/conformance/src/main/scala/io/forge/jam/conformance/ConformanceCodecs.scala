package io.forge.jam.conformance

import io.forge.jam.core.JamBytes
import io.forge.jam.core.codec.Codec

/**
 * Minimal codec bridge for conformance module.
 * Provides .encode and .decodeAs extension methods.
 */
object ConformanceCodecs:

  /** Extension method to encode values using their given Codec */
  extension [A](value: A)
    def encode(using codec: Codec[A]): JamBytes =
      codec.encodeJam(value)

  /** Extension method to decode values using their given Codec */
  extension (bytes: JamBytes)
    def decodeAs[A](offset: Int)(using codec: Codec[A]): (A, Int) =
      codec.decodeAt(bytes.toArrayUnsafe, offset) match
        case Right(result) => result
        case Left(err)     => throw new io.forge.jam.core.codec.CodecDecodingException(err)
