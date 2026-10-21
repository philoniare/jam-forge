package io.forge.jam.protocol.pipeline

import io.circe.Decoder
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.header.Header
import io.forge.jam.crypto.SigningContext
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.benchmark.{BlockGenerator, DevKeyStore}
import io.forge.jam.protocol.safrole.SafroleTransition
import io.forge.jam.protocol.safrole.SafroleTypes.{SafroleInput, TicketsOrKeys}
import io.forge.jam.protocol.state.{ServiceStorageView, TrieBackedJamState}
import io.forge.jam.protocol.traces.{BlockImporter, Genesis, ImportResult, RawState}
import io.forge.jam.vrfs.BandersnatchWrapper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.CompletableFuture

/**
 * The seal and entropy-source signatures are verified on the shared pool while the
 * rest of the block is imported. A failed check must give the error a sequential import would: checks in the order
 * they were started, and a failed check ahead of any error raised by a later pipeline step.
 */
class PendingChecksSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY

  test("firstFailure reports the first check started, whichever finishes first") {
    val checks = new PendingChecks
    val slow = new CompletableFuture[Option[PipelineError]]
    checks.add(slow)
    checks.start(Some(PipelineError.InvalidBlockSeal))
    checks.start(None)
    val reported = CompletableFuture.supplyAsync(() => checks.firstFailure())
    Thread.sleep(50)
    reported.isDone shouldBe false // waits for the earlier check
    slow.complete(Some(PipelineError.HeaderVerificationErr("first")))
    reported.get() shouldBe Some(PipelineError.HeaderVerificationErr("first"))
  }

  test("firstFailure is empty when every check passes") {
    val checks = new PendingChecks
    checks.start(None)
    checks.add(CompletableFuture.completedFuture(None))
    checks.firstFailure() shouldBe None
  }

  /** Block 1 on the fuzzy genesis, plus what is needed to sign variants of it as its author. */
  private final case class Fixture(genesis: RawState, block: Block, secret: Array[Byte], sealInput: Array[Byte])

  private lazy val fixture: Fixture =
    given Decoder[Genesis] = Genesis.decoder
    val genesis = TestFileLoader
      .loadJsonFromTestVectors[Genesis]("traces/fuzzy", "genesis")
      .fold(e => fail(s"cannot load genesis: $e"), _.state)
    val importer = new BlockImporter(config)
    importer.store.bootstrap(genesis.keyvals.map(kv => (kv.key, kv.value)))
    val validators = DevKeyStore.getAllTinyValidators()
    val block = new BlockGenerator(config, validators, importer.store).generateBlock(genesis.stateRoot, slot = 1)
    // The seal's VRF input, from the posterior entropy and sealer sequence, as the block author computes it.
    val trie = importer.store.at(genesis.stateRoot)
    val view = new TrieBackedJamState(trie, config, new ServiceStorageView(trie), Some(importer.store))
    SafroleTransition.stfView(SafroleInput(1L, Hash(new Array[Byte](32)), Nil), view)
    val entropy = view.entropy.pool(3).bytes
    val sealInput = view.gamma.st match
      case TicketsOrKeys.Keys(_) => SigningContext.fallbackSealInputData(entropy)
      case TicketsOrKeys.Tickets(t) =>
        SigningContext.safroleTicketInputData(entropy, t((1 % config.epochLength).toInt).attempt.toByte)
    Fixture(genesis, block, validators(block.header.authorIndex.value.toInt).bandersnatchSecret, sealInput)

  private def resealed(header: Header): Block =
    val codec = Header.headerCodec(config.validatorCount, config.epochLength)
    val unsigned = codec.encode(header.copy(seal = JamBytes.zeros(96))).dropRight(96)
    val seal = BandersnatchWrapper.ietfVrfSign(fixture.secret, fixture.sealInput, unsigned)
    fixture.block.copy(header = header.copy(seal = JamBytes(seal)))

  /** A valid VRF signature by the author, but over a different input than the seal's output. */
  private def wrongEntropySource: JamBytes =
    JamBytes(BandersnatchWrapper.ietfVrfSign(fixture.secret, SigningContext.entropyInputData(new Array[Byte](32)), Array.empty))

  /** Flips a byte of the proof, after the 32-byte output, so the VRF output is unchanged. */
  private def corrupt(signature: JamBytes): JamBytes =
    val b = signature.toArray
    b(40) = (b(40) ^ 1).toByte
    JamBytes(b)

  private val offender = Hash(Array.fill[Byte](32)(7))

  private def importResult(block: Block): ImportResult =
    new BlockImporter(config).importBlock(block, fixture.genesis)

  private def failsWith(block: Block, expected: PipelineError): Unit =
    importResult(block) match
      case ImportResult.Failure(_, message) => message shouldBe expected.message
      case other                            => fail(s"expected ${expected.message}, got $other")

  test("the untampered block imports") {
    importResult(fixture.block) shouldBe a[ImportResult.Success]
  }

  test("GP: a bad seal is reported as InvalidBlockSeal (eq. 6.16)") {
    val h = fixture.block.header
    failsWith(fixture.block.copy(header = h.copy(seal = corrupt(h.seal))), PipelineError.InvalidBlockSeal)
  }

  test("GP: a bad entropy source under a valid seal is reported as an invalid entropy signature (eq. 6.17)") {
    failsWith(
      resealed(fixture.block.header.copy(entropySource = wrongEntropySource)),
      PipelineError.HeaderVerificationErr("Invalid entropy VRF signature")
    )
  }

  test("GP: a bad seal wins over a bad entropy source, as the seal is checked first") {
    val h = fixture.block.header.copy(entropySource = wrongEntropySource)
    failsWith(fixture.block.copy(header = h.copy(seal = corrupt(resealed(h).header.seal))), PipelineError.InvalidBlockSeal)
  }

  test("a bad entropy source wins over an error of a later step (offenders mark)") {
    val h = fixture.block.header.copy(entropySource = wrongEntropySource, offendersMark = List(offender))
    failsWith(resealed(h), PipelineError.HeaderVerificationErr("Invalid entropy VRF signature"))
  }

  test("a bad seal wins over an error of a later step (offenders mark)") {
    // The mark changes the signed header, so the original seal no longer verifies.
    val h = fixture.block.header.copy(offendersMark = List(offender))
    failsWith(fixture.block.copy(header = h), PipelineError.InvalidBlockSeal)
  }

  test("the later step's error is reported when the signatures are valid") {
    failsWith(resealed(fixture.block.header.copy(offendersMark = List(offender))), PipelineError.InvalidOffendersMark)
  }

  test("GP: an offenders mark holding the new offenders' keys is accepted (§10.3)") {
    val key = io.forge.jam.core.primitives.Ed25519PublicKey(Array.fill[Byte](32)(7))
    val block = fixture.block.copy(header = fixture.block.header.copy(offendersMark = List(offender)))
    val ctx = PipelineContext(config, block, new EncodedHeader(block.header), 0L, disputeOffendersMark = List(key))
    // The step reads only the context.
    IntermediateSteps.validateOffendersMark.run((null, ctx)).isRight shouldBe true
    val wrong = ctx.copy(disputeOffendersMark = Nil)
    IntermediateSteps.validateOffendersMark.run((null, wrong)) shouldBe Left(PipelineError.InvalidOffendersMark)
  }
