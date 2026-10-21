package io.forge.jam.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The `fuzz` spec from davxy/jam-conformance#215 and the spec-name lookup. */
class ChainConfigSpec extends AnyFunSuite with Matchers:

  private val fuzz = ChainConfig.FUZZ
  private val full = ChainConfig.FULL

  test("fuzz spec: the nine parameters that differ from full (jam-conformance#215)") {
    fuzz.coresCount shouldBe 85 // C
    fuzz.epochLength shouldBe 60 // E
    fuzz.contestDuration shouldBe 50 // Y
    fuzz.ticketCutoff shouldBe 50 // Y
    fuzz.rotationPeriod shouldBe 5 // R
    fuzz.maxTicketsPerExtrinsic shouldBe 4 // K
    fuzz.maxBlockGas shouldBe 875_000_000L // G_T
    fuzz.maxRefineGas shouldBe 1_250_000_000L // G_R
    fuzz.preimageExpungePeriod shouldBe 900 // D
    fuzz.maxLookupAnchorAge shouldBe 600L // L
  }

  test("fuzz spec: every other parameter keeps its full value") {
    val sameAsFull = fuzz.copy(
      validatorCount = full.validatorCount,
      coresCount = full.coresCount,
      epochLength = full.epochLength,
      contestDuration = full.contestDuration,
      ticketCutoff = full.ticketCutoff,
      rotationPeriod = full.rotationPeriod,
      maxTicketsPerExtrinsic = full.maxTicketsPerExtrinsic,
      maxBlockGas = full.maxBlockGas,
      maxRefineGas = full.maxRefineGas,
      preimageExpungePeriod = full.preimageExpungePeriod,
      maxLookupAnchorAge = full.maxLookupAnchorAge,
      ecPieceSize = full.ecPieceSize,
      numEcPiecesPerSegment = full.numEcPiecesPerSegment
    )
    sameAsFull shouldBe full
  }

  test("GP: fuzz genesis has V = 3C = 255 validators") {
    fuzz.validatorCount shouldBe 3 * fuzz.coresCount
    fuzz.validatorCount shouldBe 255
  }

  test("GP: fuzz validator-set sizes are {6, 9, ..., 255} (GP 0.8.0 §safrole eq:valcount)") {
    val allowed = (0 to 300).filter(fuzz.isValidValidatorCount)
    allowed shouldBe (6 to 255 by 3)
  }

  test("GP: fuzz erasure-coding piece size follows V = 255 (GP 0.8.0 §erasure_coding)") {
    // Largest d < 255/3 + 2 with W_G = 4104 divisible by 2d is 76, so pieces are 152 octets, 27 per segment.
    fuzz.ecPieceSize shouldBe 152
    fuzz.numEcPiecesPerSegment shouldBe 27
    full.ecPieceSize shouldBe ChainConfig.ecPieceSizeFor(full.validatorCount)
  }

  test("spec names round-trip and parse case-insensitively") {
    ChainConfig.fromName("fuzz") shouldBe Right(fuzz)
    ChainConfig.fromName(" FUZZ ") shouldBe Right(fuzz)
    ChainConfig.fromName("tiny") shouldBe Right(ChainConfig.TINY)
    ChainConfig.fromName("full") shouldBe Right(full)
    ChainConfig.fromName("medium").isLeft shouldBe true
    for (name, config) <- ChainConfig.byName do ChainConfig.nameOf(config) shouldBe name
    ChainConfig.nameOf(full.copy(epochLength = 7)) shouldBe "custom"
  }
