package io.forge.jam.crypto

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BlsTest extends AnyFunSuite with Matchers:

  private def seed(i: Int) = Array.fill(32)(i.toByte)
  private val msg = "$jam_beefy".getBytes("US-ASCII") ++ Array.fill(32)(9.toByte)

  test("BLS key k_bls is 144 octets and signatures verify") {
    val pk = Bls.publicFromSeed(seed(1))
    pk.length shouldBe Bls.PublicKeySize
    val sig = Bls.sign(seed(1), msg)
    sig.length shouldBe Bls.SignatureSize
    Bls.verify(pk, msg, sig) shouldBe true
    Bls.keyConsistent(pk) shouldBe true
  }

  test("wrong key, message or signature fails") {
    val pk = Bls.publicFromSeed(seed(1))
    val sig = Bls.sign(seed(1), msg)
    Bls.verify(Bls.publicFromSeed(seed(2)), msg, sig) shouldBe false
    Bls.verify(pk, msg.updated(0, 0), sig) shouldBe false
    Bls.verify(pk, msg, sig.updated(5, (sig(5) ^ 1).toByte)) shouldBe false
    Bls.verify(new Array[Byte](144), msg, sig) shouldBe false
    Bls.keyConsistent(new Array[Byte](144)) shouldBe false
  }

  test("aggregate of n signatures verifies against the n keys") {
    val seeds = (1 to 6).map(seed)
    val pks = seeds.map(Bls.publicFromSeed)
    val agg = Bls.aggregate(seeds.map(Bls.sign(_, msg))).get
    agg.length shouldBe Bls.AggregateSize
    Bls.verifyAggregate(pks, msg, agg) shouldBe true
    Bls.verifyAggregate(pks.tail, msg, agg) shouldBe false
    Bls.verifyAggregate(pks, msg.updated(0, 0), agg) shouldBe false
  }
