package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes, constants}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.{CoreIndex, Ed25519Signature, Gas, Hash, Timeslot, ValidatorIndex}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.dispute.GuaranteeSignature
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.work.PackageSpec
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.crypto.Ed25519ZebraWrapper
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

/** CE 135 guarantees enter the pool only with credentials the Reports STF would accept, and the pool is capped. */
class GuaranteeFloodSpec extends AnyFunSuite with Matchers:

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def tempDir(p: String): Path = Files.createTempDirectory(p)
  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  private val config = ChainConfig.TINY
  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  private def report(core: Int, tag: String, lookupSlot: Long = 0): WorkReport =
    WorkReport(
      packageSpec = PackageSpec(Hashing.blake2b256(tag.getBytes("UTF-8")), UInt(0), Hash.zero, UShort(0), Hash.zero, UShort(0)),
      context = Context(Hash.zero, Timeslot(UInt(lookupSlot.toInt)), Hash.zero, Hash.zero, Hash.zero,
        Timeslot(UInt(lookupSlot.toInt)), Hash.zero, List.empty),
      coreIndex = CoreIndex(core),
      authorizerHash = Hash.zero,
      authGasUsed = Gas(0),
      authOutput = JamBytes.empty,
      segmentRootLookup = List.empty,
      results = List.empty
    )

  private def sign(r: WorkReport, slot: Long, signers: Seq[(Int, ValidatorKeySet)]): GuaranteeExtrinsic =
    val message = constants.JAM_GUARANTEE_BYTES ++ Hashing.blake2b256(r.encode.toArray).bytes
    GuaranteeExtrinsic(
      r,
      Timeslot(UInt(slot.toInt)),
      signers.sortBy(_._1).toList.map { (idx, keys) =>
        GuaranteeSignature(ValidatorIndex(idx), Ed25519Signature(Ed25519ZebraWrapper.sign(keys.ed25519Secret, message)))
      }
    )

  test("forged guarantees never reach the pool; valid ones do, once, and are offered to the author") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val spec = ChainSpec(
      id = "guarantee-flood-devnet",
      config = config,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dir = tempDir("jam-guarantee-flood")
    var node: JamNode = null
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false))
      node.chain.initializeOrRestore(spec)
      val chain = node.chain
      val author = new BlockAuthor(chain, devKeys)
      for slot <- 1L to 5L do
        chain.importBlock(chain.encodeBlock(author.tryAuthor(slot).getOrElse(fail(s"author $slot")))).isRight shouldBe true

      // The guarantors of each core at the next slot, by the guarantor service's own assignment (not the STF's).
      val slot = chain.best.slot + 1
      val (entropy, active) = GuarantorService.assignmentContext(chain.stateView(), slot, config)
      val assignments = GuarantorService.coreAssignments(entropy, slot, config)
      def keysOf(i: Int): ValidatorKeySet =
        devKeys.find(k => java.util.Arrays.equals(k.ed25519Public, active(i).ed25519.bytes.toArray)).getOrElse(fail("dev key"))
      def guarantorsOf(core: Int): Seq[(Int, ValidatorKeySet)] =
        active.indices.filter(assignments(_) == core).map(i => i -> keysOf(i))
      guarantorsOf(0).size shouldBe 3
      val (core0, core1) = (guarantorsOf(0), guarantorsOf(1))

      val valid = sign(report(0, "valid"), slot, core0)
      node.distribution.receiveGuarantee(valid) shouldBe Right(true)
      node.pools.guaranteeCount shouldBe 1
      // Duplicate: kept once, no second check.
      node.distribution.receiveGuarantee(valid) shouldBe Right(false)
      node.distribution.receiveGuarantee(valid.copy(signatures = valid.signatures.take(2))) shouldBe Right(false)
      node.pools.guaranteeCount shouldBe 1

      // 6000 forged guarantees, each a distinct report.
      val garbage = Ed25519Signature(new Array[Byte](64))
      val forged = (0 until 6000).map { n =>
        val r = report(n % 2, s"forged-$n")
        n % 6 match
          // signatures that do not verify, from the right indices
          case 0 => GuaranteeExtrinsic(r, Timeslot(UInt(slot.toInt)), guarantorsOf(n % 2).toList.map((i, _) => GuaranteeSignature(ValidatorIndex(i), garbage)))
          // real validators, but of the other core
          case 1 => sign(r, slot, guarantorsOf(1 - n % 2))
          // the right validators signing a different report
          case 2 => sign(report(n % 2, s"other-$n"), slot, guarantorsOf(n % 2)).copy(report = r)
          // one credential only
          case 3 => sign(r, slot, guarantorsOf(n % 2).take(1))
          // a core that does not exist
          case 4 => sign(report(7, s"forged-$n"), slot, core0)
          // no credentials
          case _ => GuaranteeExtrinsic(r, Timeslot(UInt(slot.toInt)), Nil)
      }
      forged.foreach { g =>
        val result = node.distribution.receiveGuarantee(g)
        withClue(s"forged guarantee: $result") { result.isLeft shouldBe true }
      }
      node.pools.guaranteeCount shouldBe 1

      // Slot window: correctly signed, but for a slot ahead of the node or from a rotation that is over.
      node.distribution.receiveGuarantee(sign(report(0, "future"), slot + 2, core0)).isLeft shouldBe true
      node.distribution.receiveGuarantee(sign(report(0, "ancient"), 0, core0)).isLeft shouldBe true
      // A lookup anchor older than the maximum lookup-anchor age.
      for s <- 6L to 30L do
        chain.importBlock(chain.encodeBlock(author.tryAuthor(s).getOrElse(fail(s"author $s")))).isRight shouldBe true
      // What a started node does after each import: the valid one ages out with its rotation.
      node.pools.pruneGuaranteesBefore((chain.best.slot / config.rotationPeriod - 1) * config.rotationPeriod)
      node.pools.guaranteeCount shouldBe 0
      val later = chain.best.slot + 1
      val (laterEntropy, laterActive) = GuarantorService.assignmentContext(chain.stateView(), later, config)
      val laterAssignments = GuarantorService.coreAssignments(laterEntropy, later, config)
      def laterGuarantors(core: Int) = laterActive.indices
        .filter(laterAssignments(_) == core)
        .map(i => i -> devKeys.find(k => java.util.Arrays.equals(k.ed25519Public, laterActive(i).ed25519.bytes.toArray)).get)
      node.distribution.receiveGuarantee(sign(report(0, "stale-lookup", lookupSlot = 0), later, laterGuarantors(0))).isLeft shouldBe true

      // Valid guarantees after the flood are pooled and offered to the author, one per core, by core index.
      val g0 = sign(report(0, "after-0", lookupSlot = later - 1), later, laterGuarantors(0))
      val g1 = sign(report(1, "after-1", lookupSlot = later - 1), later, laterGuarantors(1).take(2))
      node.distribution.receiveGuarantee(g1) shouldBe Right(true)
      node.distribution.receiveGuarantee(g0) shouldBe Right(true)
      node.pools.takeGuarantees() shouldBe List(g0, g1)

      // Real guarantors signing report after report for their core: the core's share of the pool stays capped and
      // the oldest leave first.
      val many = (0 until 50).map(n => sign(report(0, s"many-$n", lookupSlot = later - 1), later, laterGuarantors(0)))
      many.foreach(g => node.distribution.receiveGuarantee(g) shouldBe Right(true))
      node.pools.guaranteeCount shouldBe ExtrinsicPools.MaxGuaranteesPerCore + 1
      node.pools.pendingGuarantees.toSet shouldBe (many.takeRight(ExtrinsicPools.MaxGuaranteesPerCore).toSet + g1)
    finally
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)
  }

  test("the guarantee pool is capped in total and evicts the oldest") {
    val pools = new ExtrinsicPools(maxGuaranteesPerCore = 3, maxGuarantees = 5)
    val gs = (0 until 4000).map(n => GuaranteeExtrinsic(report(n % 400, s"g-$n"), Timeslot(UInt(n)), Nil))
    gs.foreach(pools.addGuarantee)
    pools.guaranteeCount shouldBe 5
    pools.pendingGuarantees.toSet shouldBe gs.takeRight(5).toSet
    // Per core: three of core 9, the newest kept.
    val sameCore = (0 until 10).map(n => GuaranteeExtrinsic(report(9, s"c-$n"), Timeslot(UInt(n)), Nil))
    sameCore.foreach(pools.addGuarantee)
    pools.pendingGuarantees.filter(_.report.coreIndex.toInt == 9).toSet shouldBe sameCore.takeRight(3).toSet
    pools.guaranteeCount shouldBe 5
    // Rotation pruning.
    pools.pruneGuaranteesBefore(8)
    pools.pendingGuarantees.map(_.slot.toLong).forall(_ >= 8) shouldBe true
  }
