package io.forge.jam.protocol.pipeline

import io.circe.Decoder
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes, constants}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.{CoreIndex, Ed25519Signature, Gas, Hash, ServiceId, Timeslot, ValidatorIndex}
import io.forge.jam.core.types.block.{Block, Extrinsic}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.dispute.GuaranteeSignature
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, GuaranteeExtrinsic, Preimage}
import io.forge.jam.core.types.header.Header
import io.forge.jam.core.types.work.PackageSpec
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.crypto.{Ed25519ZebraWrapper, SigningContext}
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.assurance.AssuranceTypes.AssuranceErrorCode
import io.forge.jam.protocol.benchmark.{BlockGenerator, DevKeyStore}
import io.forge.jam.protocol.preimage.PreimageTypes.PreimageErrorCode
import io.forge.jam.protocol.report.ReportTypes.ReportErrorCode
import io.forge.jam.protocol.safrole.SafroleTransition
import io.forge.jam.protocol.safrole.SafroleTypes.{SafroleInput, TicketsOrKeys}
import io.forge.jam.protocol.state.{ServiceStorageView, TrieBackedJamState}
import io.forge.jam.protocol.traces.{BlockImporter, Genesis, ImportResult, RawState}
import io.forge.jam.vrfs.BandersnatchWrapper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

/**
 * The block's assurance and guarantee signatures are verified as one batch on the shared pool, started in the Reports
 * step where a sequential import verifies it, while the rest of the block is imported. A bad batch must give the error
 * a sequential import gives: it loses to every error raised up to that point and to the header checks started before
 * it, wins over every error raised after it, and the block is not committed.
 */
class DeferredSignatureBatchSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private val validators = DevKeyStore.getAllTinyValidators()

  /** Block 1 on the fuzzy genesis, plus what is needed to seal variants of it as its author. */
  private final case class Fixture(genesis: RawState, block: Block, secret: Array[Byte], sealInput: Array[Byte])

  private lazy val fixture: Fixture =
    given Decoder[Genesis] = Genesis.decoder
    val genesis = TestFileLoader
      .loadJsonFromTestVectors[Genesis]("traces/fuzzy", "genesis")
      .fold(e => fail(s"cannot load genesis: $e"), _.state)
    val importer = new BlockImporter(config)
    importer.store.bootstrap(genesis.keyvals.map(kv => (kv.key, kv.value)))
    val block = new BlockGenerator(config, validators, importer.store).generateBlock(genesis.stateRoot, slot = 1)
    val trie = importer.store.at(genesis.stateRoot)
    val view = new TrieBackedJamState(trie, config, new ServiceStorageView(trie), Some(importer.store))
    SafroleTransition.stfView(SafroleInput(1L, Hash(new Array[Byte](32)), Nil), view)
    val entropy = view.entropy.pool(3).bytes
    val sealInput = view.gamma.st match
      case TicketsOrKeys.Keys(_) => SigningContext.fallbackSealInputData(entropy)
      case TicketsOrKeys.Tickets(t) =>
        SigningContext.safroleTicketInputData(entropy, t((1 % config.epochLength).toInt).attempt.toByte)
    Fixture(genesis, block, validators(block.header.authorIndex.toInt).bandersnatchSecret, sealInput)

  /** The fixture block carrying `extrinsic`, with the header's extrinsic hash and seal redone to match. */
  private def withExtrinsic(change: Extrinsic => Extrinsic): Block =
    val extrinsic = change(fixture.block.extrinsic)
    val header = fixture.block.header.copy(extrinsicHash = ExtrinsicHashing.computeExtrinsicHash(extrinsic, config))
    val unsigned = Header.headerCodec(config.epochLength).encode(header.copy(seal = JamBytes.zeros(96))).dropRight(96)
    val seal = BandersnatchWrapper.ietfVrfSign(fixture.secret, fixture.sealInput, unsigned)
    Block(header.copy(seal = JamBytes(seal)), extrinsic)

  private def badSeal(block: Block): Block =
    val seal = block.header.seal.toArray
    seal(40) = (seal(40) ^ 1).toByte
    block.copy(header = block.header.copy(seal = JamBytes(seal)))

  private def corrupt(signature: Ed25519Signature): Ed25519Signature =
    val bytes = signature.toArrayUnsafe.clone()
    bytes(3) = (bytes(3) ^ 1).toByte
    Ed25519Signature(bytes)

  /** An assurance of no core by validator `index`, correctly signed. */
  private def assurance(index: Int): AssuranceExtrinsic =
    val parent = fixture.block.header.parent
    val bitfield = new Array[Byte]((config.coresCount + 7) / 8)
    val message = constants.JAM_AVAILABLE_BYTES ++ Hashing.blake2b256(parent.bytes ++ bitfield).bytes
    val signature = Ed25519ZebraWrapper.sign(validators(index).ed25519Secret, message)
    AssuranceExtrinsic(parent, JamBytes(bitfield), ValidatorIndex(index), Ed25519Signature(signature))

  private def badAssurance(index: Int): AssuranceExtrinsic =
    val good = assurance(index)
    good.copy(signature = corrupt(good.signature))

  /**
   * A guarantee for core 0 signed by validators 0 and 1. Its report claims no erasure shards, so with good signatures
   * the Reports step rejects it after the point where the signatures are checked.
   */
  private def guarantee(signers: List[Int] = List(0, 1)): GuaranteeExtrinsic =
    val report = WorkReport(
      PackageSpec(Hash(Array.fill[Byte](32)(9)), UInt(100), Hash.zero, UShort(0), Hash.zero, UShort(0)),
      Context(Hash.zero, Timeslot(0), Hash.zero, Hash.zero, Hash.zero, Timeslot(0), Hash.zero, List.empty),
      CoreIndex(0),
      Hash.zero,
      Gas(0L),
      JamBytes.empty,
      List.empty,
      List.empty
    )
    val message = constants.JAM_GUARANTEE_BYTES ++ Hashing.blake2b256(report.encode).bytes
    val signatures = signers.map(i =>
      GuaranteeSignature(ValidatorIndex(i), Ed25519Signature(Ed25519ZebraWrapper.sign(validators(i).ed25519Secret, message)))
    )
    GuaranteeExtrinsic(report, Timeslot(1), signatures)

  private def badGuarantee: GuaranteeExtrinsic =
    val good = guarantee()
    good.copy(signatures = good.signatures.init :+ good.signatures.last.copy(signature = corrupt(good.signatures.last.signature)))

  private val unneededPreimage = Preimage(ServiceId(UInt(0)), JamBytes(Array.fill[Byte](8)(0x5a)))

  private val badBatch = PipelineError.BadSignatureBatch
  private val laterReportError = PipelineError.ReportErr(ReportErrorCode.BadErasureShards)
  private val laterStepError = PipelineError.PreimageErr(PreimageErrorCode.PreimageUnneeded)

  private def importResult(block: Block): ImportResult =
    new BlockImporter(config).importBlock(block, fixture.genesis)

  private def failsWith(block: Block, expected: PipelineError): Unit =
    importResult(block) match
      case ImportResult.Failure(_, _, error) => error shouldBe Some(expected)
      case other                             => fail(s"expected ${expected.message}, got $other")

  test("a block whose assurance signatures verify imports") {
    importResult(withExtrinsic(_.copy(assurances = List(assurance(0), assurance(1))))) shouldBe a[ImportResult.Success]
  }

  test("a bad assurance signature is reported as a bad signature batch") {
    failsWith(withExtrinsic(_.copy(assurances = List(assurance(0), badAssurance(1)))), badBatch)
  }

  test("a bad guarantee signature is reported as a bad signature batch, ahead of a later error of the Reports step") {
    failsWith(withExtrinsic(_.copy(guarantees = List(guarantee()))), laterReportError)
    failsWith(withExtrinsic(_.copy(guarantees = List(badGuarantee))), badBatch)
  }

  test("a bad assurance signature wins over a later error of the Reports step") {
    failsWith(withExtrinsic(_.copy(assurances = List(badAssurance(0)), guarantees = List(guarantee()))), badBatch)
  }

  test("a bad signature batch wins over an error of a later step (preimages)") {
    failsWith(withExtrinsic(_.copy(preimages = List(unneededPreimage))), laterStepError)
    failsWith(withExtrinsic(_.copy(preimages = List(unneededPreimage), assurances = List(badAssurance(0)))), badBatch)
    failsWith(withExtrinsic(_.copy(preimages = List(unneededPreimage), guarantees = List(badGuarantee))), badBatch)
  }

  test("an error of the Assurances step wins over a bad signature batch") {
    failsWith(
      withExtrinsic(_.copy(assurances = List(badAssurance(1), assurance(0)), guarantees = List(badGuarantee))),
      PipelineError.AssuranceErr(AssuranceErrorCode.NotSortedOrUniqueAssurers)
    )
  }

  test("an error of the Reports step raised before the signatures are checked wins over a bad signature batch") {
    failsWith(
      withExtrinsic(_.copy(assurances = List(badAssurance(0)), guarantees = List(guarantee(signers = List(0))))),
      PipelineError.ReportErr(ReportErrorCode.InsufficientGuarantees)
    )
  }

  test("a bad seal wins over a bad signature batch, as the header checks are started first") {
    failsWith(
      badSeal(withExtrinsic(_.copy(assurances = List(badAssurance(0))))),
      PipelineError.HeaderErr(HeaderErrorCode.BadSealSignature)
    )
  }

  test("a block with a bad signature batch commits nothing") {
    val importer = new BlockImporter(config)
    val bad = withExtrinsic(_.copy(assurances = List(badAssurance(0))))
    importer.importBlock(bad, fixture.genesis) shouldBe a[ImportResult.Failure]
    importer.currentTrieRoot shouldBe fixture.genesis.stateRoot
    // The same importer then imports the untampered block to the root a fresh importer gives.
    importer.importBlock(fixture.block, fixture.genesis) shouldBe importResult(fixture.block)
    importResult(fixture.block) shouldBe a[ImportResult.Success]
  }
