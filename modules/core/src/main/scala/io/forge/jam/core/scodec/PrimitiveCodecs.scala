package io.forge.jam.core.scodec

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Ed25519Signature, Timeslot, ValidatorIndex}
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{
  bytes,
  fixedSizeBytes,
  uint16L,
  uint32L,
  variableSizeBytesLong
}
import spire.math.{UInt, UShort}

object PrimitiveCodecs:

  val ed25519Signature: Codec[Ed25519Signature] =
    fixedSizeBytes(Ed25519Signature.Size.toLong, bytes).xmap(
      bv => Ed25519Signature(bv.toArray),
      sig => ByteVector(sig.bytes)
    )

  val timeslot: Codec[Timeslot] = uint32L.xmap(
    v => Timeslot(UInt(v.toInt)),
    ts => ts.value.toLong & 0xffffffffL
  )

  val validatorIndex: Codec[ValidatorIndex] = uint16L.xmap(
    v => ValidatorIndex(UShort(v)),
    vi => vi.value.toInt
  )

  val compactBytes: Codec[JamBytes] =
    variableSizeBytesLong(JamCodecs.compactInteger, bytes).xmap(
      bv => JamBytes.fromByteVector(bv),
      jb => jb.toByteVector
    )
