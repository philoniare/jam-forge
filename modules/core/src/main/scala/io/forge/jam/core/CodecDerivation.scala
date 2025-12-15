package io.forge.jam.core

/**
 * Generic codec derivation for JAM protocol types.
 */
object CodecDerivation:

  /**
   * Creates a JamEncoder for enum types using ordinal-based encoding.
   *
   * Enum values are encoded as a single byte containing the ordinal value.
   */
  def enumEncoder[E](toOrdinal: E => Int): codec.JamEncoder[E] =
    new codec.JamEncoder[E]:
      def encode(a: E): JamBytes =
        JamBytes(Array(toOrdinal(a).toByte))

  /**
   * Creates a JamDecoder for enum types using ordinal-based decoding.
   *
   * Decodes a single byte as an ordinal value and converts to enum using
   * the companion object's fromOrdinal method.
   */
  def enumDecoder[E](fromOrdinal: Int => E): codec.JamDecoder[E] =
    new codec.JamDecoder[E]:
      def decode(bytes: JamBytes, offset: Int): (E, Int) =
        val ordinal = bytes.toArray(offset).toInt & 0xff
        (fromOrdinal(ordinal), 1)
