package io.forge.jam.network

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Regression: a node that already accepted an inbound connection must still
  * be able to dial out (guarantor B accepts the builder then dials co-signer
  * C for CE 134).
  */
class ThreeNodeMeshSpec extends AnyFunSuite with Matchers:

  test("A->B, A->C, then B->C all connect") {
    val config = JamnpConfig(genesisHashPrefix = "deadbeef")
    val a = new JamnpNode(NodeIdentity.generate(), config)
    val b = new JamnpNode(NodeIdentity.generate(), config)
    val c = new JamnpNode(NodeIdentity.generate(), config)
    try
      a.start(new InetSocketAddress("127.0.0.1", 0))
      b.start(new InetSocketAddress("127.0.0.1", 0))
      c.start(new InetSocketAddress("127.0.0.1", 0))

      val ab = a.connect(new InetSocketAddress("127.0.0.1", b.boundPort)).get(10, TimeUnit.SECONDS)
      val ac = a.connect(new InetSocketAddress("127.0.0.1", c.boundPort)).get(10, TimeUnit.SECONDS)
      val bc = b.connect(new InetSocketAddress("127.0.0.1", c.boundPort)).get(10, TimeUnit.SECONDS)

      ab.isOpen shouldBe true
      ac.isOpen shouldBe true
      bc.isOpen shouldBe true
    finally
      a.shutdown()
      b.shutdown()
      c.shutdown()
  }
