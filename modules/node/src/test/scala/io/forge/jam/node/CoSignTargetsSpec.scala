package io.forge.jam.node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CoSignTargetsSpec extends AnyFunSuite with Matchers:

  private val peers = List("a", "b", "x", "y")
  private def targets(assigned: Seq[String], severalKeys: Boolean = false) =
    GuarantorService.coSignTargets[String, String](peers, identity, assigned, severalKeys)

  test("connected co-guarantors are shared with, and the unconnected ones dialed") {
    targets(Seq("a", "c")) shouldBe (List("a"), Seq("c"))
  }

  test("with no co-guarantor connected, a node holding one key dials them and shares with no other peer") {
    targets(Seq("c", "d")) shouldBe (Nil, Seq("c", "d"))
  }

  test("a node holding several keys, with no co-guarantor known by key, shares with every peer as before") {
    targets(Seq("c", "d"), severalKeys = true) shouldBe (peers, Nil)
  }
