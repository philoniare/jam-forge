package io.forge.jam.core.types

import _root_.scodec.*
import _root_.scodec.bits.*
import _root_.scodec.codecs.*
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.scodec.JamCodecs.{hashCodec, compactInt}
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder

/**
 * Preimage-related types shared across STFs.
 */
object preimage:

  /**
   * Preimage hash with blob data.
   * Used by: Preimage STF, Accumulation STF
   *
   * @param hash Hash of the preimage (32 bytes)
   * @param blob The preimage data (variable length)
   */
  final case class PreimageHash(
    hash: Hash,
    blob: JamBytes
  )

  object PreimageHash:
    given Codec[PreimageHash] =
      (hashCodec :: variableSizeBytes(compactInt, bytes)).xmap(
        { case (hash, blob) => PreimageHash(hash, JamBytes.fromByteVector(blob)) },
        ph => (ph.hash, ph.blob.toByteVector)
      )

    given Decoder[PreimageHash] =
      Decoder.instance { cursor =>
        for
          hash <- cursor.get[String]("hash").map(h => Hash(parseHex(h)))
          blob <- cursor.get[String]("blob").map(b => JamBytes(parseHex(b)))
        yield PreimageHash(hash, blob)
      }
