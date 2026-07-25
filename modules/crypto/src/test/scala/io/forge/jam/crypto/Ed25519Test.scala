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
   * ZIP 215 (https://zips.z.cash/zip-0215) is asymmetric about canonicality, and the
   * distinction is the whole point of these vectors:
   *   - the SCALAR s MUST be canonical (s < q);
   *   - the POINTS A and R MAY be non-canonical -- "y-coordinates need not be reduced
   *     mod p". Such an encoding denotes the same curve point as its reduction.
   *
   * So a non-canonical point encoding must NOT be rejected for being non-canonical.
   * Rejecting it is a ZIP-215 violation, not compliance.
   *
   * We cannot assert a per-vector accept/reject verdict here: vectors.json carries
   * inputs and canonicality metadata but no expected results, so any blanket verdict
   * would be pinning observed behaviour rather than testing the rule. Instead we test
   * the invariant the rule implies:
   *
   *   verify(non-canonical encoding) == verify(its canonical reduction)
   *
   * A canonicality pre-filter (the ZIP-215 violation this test exists to catch) breaks
   * that equality immediately -- it rejects the non-canonical form while accepting its
   * canonical twin.
   */
  /** Ed25519 field prime p = 2^255 - 19 */
  private val FieldPrime: BigInt = (BigInt(1) << 255) - 19

  /** Reduce a 32-byte little-endian point encoding to its canonical form (y mod p),
    * preserving the sign bit. Returns None when the encoding is already canonical.
    */
  private def canonicalReduction(hex: String): Option[String] =
    val n = BigInt(1, hexToBytes(hex).reverse)
    val sign = (n >> 255) & 1
    val y = n & ((BigInt(1) << 255) - 1)
    if y < FieldPrime then None
    else
      val reduced = (y % FieldPrime) | (sign << 255)
      val le = reduced.toByteArray.reverse.padTo(32, 0.toByte).take(32)
      Some(le.map(b => f"${b & 0xff}%02x").mkString)

  private def verifyVector(v: TestVector): Boolean =
    Ed25519.verify(hexToBytes(v.pk), hexToBytes(v.msg), hexToBytes(v.r) ++ hexToBytes(v.s))

  test("ZIP-215 compliance validation") {
    val vectors = loadTestVectors()

    // 1. Fully canonical vectors establish the baseline.
    val fullyCanonical = vectors.filter(v => v.pk_canonical && v.r_canonical)
    val canonicalRejected = fullyCanonical.filterNot(verifyVector).map(_.number)
    withClue(s"Canonical vectors should verify (failed: ${canonicalRejected.mkString(", ")})") {
      canonicalRejected shouldBe empty
    }

    // 2. Rule 3: a non-canonical point encoding denotes the same point as its
    //    reduction, so it must produce the same verdict. Pair each non-canonical
    //    vector with the vector holding its reduced encoding, all else equal.
    val byKey = vectors.map(v => (v.pk, v.r, v.s, v.msg) -> v).toMap
    val pairs = vectors.flatMap { v =>
      canonicalReduction(v.pk)
        .flatMap(red => byKey.get((red, v.r, v.s, v.msg)))
        .map(twin => (v, twin))
    }

    withClue("vectors.json should contain non-canonical/canonical point pairs to compare") {
      pairs should not be empty
    }

    val divergent = pairs.collect {
      case (v, twin) if verifyVector(v) != verifyVector(twin) =>
        s"#${v.number} (${v.desc}) => ${verifyVector(v)} but canonical twin #${twin.number} => ${verifyVector(twin)}"
    }
    withClue(
      s"ZIP-215 rule 3: non-canonical point encodings must verify identically to their " +
        s"canonical reduction. Divergences indicate a canonicality pre-filter:\n  ${divergent.mkString("\n  ")}\n"
    ) {
      divergent shouldBe empty
    }
  }
