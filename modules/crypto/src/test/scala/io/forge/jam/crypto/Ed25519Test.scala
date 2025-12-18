package io.forge.jam.crypto

import io.circe.{Decoder, HCursor}
import io.circe.parser.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source

/**
 * Tests for Ed25519 signature verification with canonicity checks.
 */
class Ed25519Test extends AnyFunSuite with Matchers:

  /** Test vector case from vectors.json */
  case class TestVector(
    number: Int,
    desc: String,
    pk: String,
    r: String,
    s: String,
    msg: String,
    pk_canonical: Boolean,
    r_canonical: Boolean
  )

  object TestVector:
    given Decoder[TestVector] = new Decoder[TestVector]:
      def apply(c: HCursor): Decoder.Result[TestVector] =
        for
          number <- c.downField("number").as[Int]
          desc <- c.downField("desc").as[String]
          pk <- c.downField("pk").as[String]
          r <- c.downField("r").as[String]
          s <- c.downField("s").as[String]
          msg <- c.downField("msg").as[String]
          pk_canonical <- c.downField("pk_canonical").as[Boolean]
          r_canonical <- c.downField("r_canonical").as[Boolean]
        yield TestVector(number, desc, pk, r, s, msg, pk_canonical, r_canonical)

  private def hexToBytes(hex: String): Array[Byte] =
    val cleanHex = hex.stripPrefix("0x")
    cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  private def loadTestVectors(): List[TestVector] =
    val stream = getClass.getResourceAsStream("/ed25519-vectors.json")
    val source = Source.fromInputStream(stream)
    try
      val json = source.mkString
      decode[List[TestVector]](json)(using Decoder.decodeList(using TestVector.given_Decoder_TestVector)) match
        case Right(vectors) => vectors
        case Left(err) => throw new RuntimeException(s"Failed to parse test vectors: $err")
    finally
      source.close()

  test("Should return false on invalid input sizes") {
    val emptyMsg: Array[Byte] = Array.empty[Byte]

    // Invalid public key size (should be 32 bytes)
    Ed25519.verify(Array.fill(31)(0.toByte), emptyMsg, Array.fill(64)(0.toByte)) shouldBe false
    Ed25519.verify(Array.fill(33)(0.toByte), emptyMsg, Array.fill(64)(0.toByte)) shouldBe false

    // Invalid signature size (should be 64 bytes)
    Ed25519.verify(Array.fill(32)(0.toByte), emptyMsg, Array.fill(63)(0.toByte)) shouldBe false
    Ed25519.verify(Array.fill(32)(0.toByte), emptyMsg, Array.fill(65)(0.toByte)) shouldBe false
  }

  /**
   * ZIP-215 Compliance Test
   *
   * The test checks:
   * 1. Canonical public keys (pk_canonical: true) should be ACCEPTED by the library
   * 2. Non-canonical public keys (pk_canonical: false) should be REJECTED
   * 3. Canonical R values (r_canonical: true) should be ACCEPTED
   * 4. Non-canonical R values (r_canonical: false) should be REJECTED
   *
   * A ZIP-215 compliant implementation must:
   * - Accept all canonically-encoded points
   * - Reject non-canonically encoded points (y >= p)
   * - Provide consistent results between single and batch verification
   */
  test("ZIP-215 compliance validation") {
    val vectors = loadTestVectors()

    // Track failed canonical vectors (should all verify)
    var canonicalPkRejected = 0
    val canonicalPkRejectedNumbers = scala.collection.mutable.ListBuffer[Int]()

    // Track incorrectly accepted non-canonical inputs
    val nonCanonicalPkVectors = vectors.filter(!_.pk_canonical)
    var nonCanonicalPkAccepted = 0

    val nonCanonicalRVectors = vectors.filter(v => v.pk_canonical && !v.r_canonical)
    var nonCanonicalRAccepted = 0

    // Test canonical vectors - should verify successfully
    val fullyCanonicalVectors = vectors.filter(v => v.pk_canonical && v.r_canonical)
    for vector <- fullyCanonicalVectors do
      val pk = hexToBytes(vector.pk)
      val r = hexToBytes(vector.r)
      val s = hexToBytes(vector.s)
      val msg = hexToBytes(vector.msg)
      val signature = r ++ s

      if !Ed25519.verify(pk, msg, signature) then
        canonicalPkRejected += 1
        canonicalPkRejectedNumbers += vector.number

    // Test non-canonical public keys - should be REJECTED
    for vector <- nonCanonicalPkVectors do
      val pk = hexToBytes(vector.pk)
      val r = hexToBytes(vector.r)
      val s = hexToBytes(vector.s)
      val msg = hexToBytes(vector.msg)
      val signature = r ++ s

      if Ed25519.verify(pk, msg, signature) then
        nonCanonicalPkAccepted += 1

    // Test non-canonical R values - should be REJECTED
    for vector <- nonCanonicalRVectors do
      val pk = hexToBytes(vector.pk)
      val r = hexToBytes(vector.r)
      val s = hexToBytes(vector.s)
      val msg = hexToBytes(vector.msg)
      val signature = r ++ s

      if Ed25519.verify(pk, msg, signature) then
        nonCanonicalRAccepted += 1

    // Assert all compliance requirements
    withClue(s"Canonical vectors should verify (failed: ${canonicalPkRejectedNumbers.mkString(", ")})") {
      canonicalPkRejected shouldBe 0
    }
    withClue("Non-canonical public keys should be rejected") {
      nonCanonicalPkAccepted shouldBe 0
    }
    withClue("Non-canonical R values should be rejected") {
      nonCanonicalRAccepted shouldBe 0
    }
  }
