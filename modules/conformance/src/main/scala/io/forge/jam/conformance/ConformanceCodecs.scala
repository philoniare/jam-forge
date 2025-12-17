package io.forge.jam.conformance

import io.forge.jam.core.JamBytes
import _root_.scodec.{Codec, Attempt}
import _root_.scodec.bits.BitVector

/**
 * Minimal codec bridge for conformance module.
 * Provides .encode and .decodeAs extension methods using scodec under the hood.
 */
object ConformanceCodecs:

  /** Extension method to encode values using scodec Codec */
  extension [A](value: A)
    def encode(using codec: Codec[A]): JamBytes =
      JamBytes.fromByteVector(codec.encode(value).require.bytes)

  /** Extension method to decode values using scodec Codec */
  extension (bytes: JamBytes)
    def decodeAs[A](offset: Int)(using codec: Codec[A]): (A, Int) =
      val bits = bytes.drop(offset).toByteVector.bits
      val result = codec.decode(bits).require
      val consumed = ((bytes.length - offset) * 8 - result.remainder.size) / 8
      (result.value, consumed.toInt)
