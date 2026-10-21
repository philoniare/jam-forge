package io.forge.jam.core.codec

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Ed25519Signature, Timeslot, ValidatorIndex}
import spire.math.{UInt, UShort}

object PrimitiveCodecs:

  val ed25519Signature: Codec[Ed25519Signature] = Codec.of(
    (w, sig) => w.bytes(sig.bytes),
    r => Ed25519Signature(r.bytes(Ed25519Signature.Size)),
    Ed25519Signature.Size
  )

  val timeslot: Codec[Timeslot] = Codec.of((w, ts) => w.i32(ts.value.signed), r => Timeslot(UInt(r.i32())), 4)

  val validatorIndex: Codec[ValidatorIndex] =
    Codec.of((w, vi) => w.u16(vi.value.toInt), r => ValidatorIndex(UShort(r.u16())), 2)

  val compactBytes: Codec[JamBytes] = codecs.compactBytes
