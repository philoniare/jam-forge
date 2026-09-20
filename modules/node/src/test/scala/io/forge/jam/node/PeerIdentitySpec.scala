package io.forge.jam.node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PeerIdentitySpec extends AnyFunSuite with Matchers:

  test("verifyPeerKey accepts a match and rejects a mismatch") {
    val expected = Array.fill[Byte](32)(1)
    JamNode.verifyPeerKey(expected, expected) shouldBe Right(())
    JamNode.verifyPeerKey(expected, Array.fill[Byte](32)(2)).isLeft shouldBe true
    JamNode.verifyPeerKey(expected, Array.fill[Byte](31)(1)).isLeft shouldBe true // wrong length
  }
