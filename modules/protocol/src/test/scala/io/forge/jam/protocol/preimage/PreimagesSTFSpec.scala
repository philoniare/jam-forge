package io.forge.jam.protocol.preimage

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.ServiceId
import io.forge.jam.core.types.extrinsic.Preimage
import io.forge.jam.protocol.preimage.PreimageTypes.*
import spire.math.UInt

/**
 * Preimage STF behaviour not exercised by the stf/preimages vectors
 * (every vector's requesters exist in the pre-state accounts).
 */
class PreimagesSTFSpec extends AnyFunSuite with Matchers:

  test("preimage for a requester with no account is rejected with PreimageUnneeded") {
    val preimage = Preimage(requester = ServiceId(UInt(42)), blob = JamBytes(Array.fill(16)(0x5a.toByte)))
    val emptyState = PreimageState(accounts = List.empty, statistics = List.empty)

    val (postState, output) = PreimageTransition.stfInternal(PreimageInput(List(preimage), slot = 100L), emptyState)
    output shouldBe Left(PreimageErrorCode.PreimageUnneeded)
    postState shouldBe emptyState
  }
