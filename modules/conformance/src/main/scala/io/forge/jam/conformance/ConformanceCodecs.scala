package io.forge.jam.conformance

import io.forge.jam.core.JamBytes
import io.forge.jam.core.codec.{ByteReader, Codec, CodecDecodingException}

/**
 * Minimal codec bridge for conformance module.
 * Provides .encode and .decodeAs extension methods.
 */
object ConformanceCodecs:

  /** Extension method to encode values using their given Codec */
  extension [A](value: A)
    def encode(using codec: Codec[A]): JamBytes =
      codec.encodeJam(value)

  /** Extension method to decode values using their given Codec. Throws the codec's own [[CodecDecodingException]],
    * which says whether the bytes were malformed or a well-formed encoding of an invalid value.
    */
  extension (bytes: JamBytes)
    def decodeAs[A](offset: Int)(using codec: Codec[A]): (A, Int) =
      val array = bytes.toArrayUnsafe
      if offset < 0 || offset > array.length then
        throw new CodecDecodingException(s"offset $offset out of range 0..${array.length}")
      val in = new ByteReader(array, offset, array.length)
      val value = codec.read(in)
      (value, in.position - offset)
