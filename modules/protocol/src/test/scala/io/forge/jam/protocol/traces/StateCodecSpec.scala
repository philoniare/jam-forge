package io.forge.jam.protocol.traces

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Ed25519PublicKey
import io.forge.jam.protocol.dispute.DisputeTypes.Psi
import _root_.scodec.Codec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateCodecSpec extends AnyFlatSpec with Matchers:

  "StateCodec.decodeSafroleState" should "decode postOffenders from the JUDGEMENTS psi offenders" in {
    val offenders = List(
      Ed25519PublicKey(Array.fill(32)(0x11.toByte)),
      Ed25519PublicKey(Array.fill(32)(0x22.toByte))
    )
    val psi = Psi(good = List.empty, bad = List.empty, wonky = List.empty, offenders = offenders)
    val psiBytes = summon[Codec[Psi]].encode(psi).require.toByteArray

    // Simple JUDGEMENTS key: prefix byte 5, then 30 zero bytes.
    val keyBytes = new Array[Byte](31)
    keyBytes(0) = StateKeys.JUDGEMENTS
    val kv = KeyValue(JamBytes(keyBytes), JamBytes(psiBytes))

    val safrole = StateCodec.decodeSafroleState(List(kv), ChainConfig.TINY)
    safrole.postOffenders shouldBe offenders
  }

  it should "default postOffenders to empty when no JUDGEMENTS key is present" in {
    val safrole = StateCodec.decodeSafroleState(List.empty, ChainConfig.TINY)
    safrole.postOffenders shouldBe empty
  }
