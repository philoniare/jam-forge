package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes, Shuffle, constants}
import io.forge.jam.core.primitives.{CoreIndex, Ed25519Signature, Gas, Hash, ServiceId, Timeslot, ValidatorIndex}
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.dispute.GuaranteeSignature
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, GuaranteeExtrinsic}
import io.forge.jam.core.types.work.{ExecutionResult, PackageSpec}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.workresult.{RefineLoad, WorkResult}
import io.forge.jam.crypto.Ed25519ZebraWrapper
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

/** The full report lifecycle across networked nodes on the dev genesis:
  * a guarantor node signs a work report and distributes it via CE 135; the
  * author pools and includes it; assurers distribute CE 141 assurances; the
  * next authored block makes the report available and accumulation executes
  * the service's PVM code — the complete guarantee → assure → accumulate
  * pipeline over JAMNP.
  */
class DevnetReportFlowSpec extends AnyFunSuite with Matchers:

  private val baseDir =
    sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath =
    baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def tempDir(p: String): Path = Files.createTempDirectory(p)
  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  /** Validator→core assignment (mirrors ReportTransition's rotation). */
  private def coreAssignments(randomness: Hash, slot: Long, config: ChainConfig): List[Int] =
    val source =
      (0 until config.validatorCount).map(i => (config.coresCount * i) / config.validatorCount).toList
    val shuffled = Shuffle.jamComputeShuffle(config.validatorCount, randomness)
    val shift = (math.floorMod(slot, config.epochLength) / config.rotationPeriod).toInt
    shuffled.map(idx => math.floorMod(source(idx) + shift, config.coresCount))

  /** Build a valid signed guarantee for core 0 against `node`'s current
    * state (the report carries a real-trace result template that service 0's
    * accumulate code executes).
    */
  private def buildGuarantee(node: JamNode, slot: Long): GuaranteeExtrinsic =
    val view = node.chain.stateView()
    val history = view.beta.history
    require(history.size >= 2, "need two recent-history entries for the anchor")

    val anchor = history(history.size - 2)
    val authorizerHash = view.authPools.head.head
    val serviceCodeHash = view.accumulation.serviceAccounts
      .find(_.id == 0)
      .map(_.data.service.codeHash)
      .getOrElse(fail("service 0 missing from dev genesis"))

    // 0x0b template: minimal checkpoint/yield accumulate op (from real traces).
    val resultData = Array[Byte](0x01, 0x0b) ++ new Array[Byte](17)

    val report = WorkReport(
      packageSpec = PackageSpec(
        Hashing.blake2b256("devnet-package-1".getBytes),
        UInt(200),
        Hash.zero,
        Hash.zero,
        UShort(0)
      ),
      context = Context(
        anchor = anchor.headerHash,
        stateRoot = anchor.stateRoot,
        beefyRoot = anchor.beefyRoot,
        lookupAnchor = anchor.headerHash,
        lookupAnchorSlot = Timeslot(UInt(view.timeslot.toInt)),
        prerequisites = List.empty
      ),
      coreIndex = CoreIndex(0),
      authorizerHash = authorizerHash,
      authGasUsed = Gas(3),
      authOutput = JamBytes.empty,
      segmentRootLookup = List.empty,
      results = List(
        WorkResult(
          serviceId = ServiceId(UInt(0)),
          codeHash = serviceCodeHash,
          payloadHash = Hashing.blake2b256("devnet-payload".getBytes),
          accumulateGas = Gas(node.spec.config.reportAccGas),
          result = ExecutionResult.Ok(JamBytes(resultData)),
          refineLoad = RefineLoad(Gas(1000L), UShort(0), UShort(1), UInt(100), UShort(0))
        )
      )
    )

    val reportHash = Hashing.blake2b256(report.encode.toArray)
    val message = constants.JAM_GUARANTEE_BYTES ++ reportHash.bytes

    val assigned = coreAssignments(view.entropy.pool(2), slot, node.spec.config).zipWithIndex
      .filter(_._1 == 0)
      .map(_._2)
      .take(3)
    assigned.size should be >= 2

    val signatures = assigned.map { vi =>
      val sig = Ed25519ZebraWrapper.sign(devKeys(vi).ed25519Secret, message)
      GuaranteeSignature(ValidatorIndex(vi), Ed25519Signature(sig))
    }.sortBy(_.validatorIndex.value.toInt)

    GuaranteeExtrinsic(report, Timeslot(UInt(slot.toInt)), signatures)

  /** Assurances for core 0 from validators 0..4 anchored at `parent`. */
  private def buildAssurances(parent: Hash): List[AssuranceExtrinsic] =
    val bitfield = Array[Byte](1) // core 0
    val dataHash = Hashing.blake2b256(parent.bytes ++ bitfield)
    val message = constants.JAM_AVAILABLE_BYTES ++ dataHash.bytes
    (0 until 5).map { vi =>
      val sig = Ed25519ZebraWrapper.sign(devKeys(vi).ed25519Secret, message)
      AssuranceExtrinsic(parent, JamBytes(bitfield), ValidatorIndex(vi), Ed25519Signature(sig))
    }.toList

  test("guarantee -> inclusion -> assurances -> availability -> accumulation over JAMNP") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )

    val spec = ChainSpec(
      id = "report-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-flow-a")
    val dirB = tempDir("jam-flow-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA)).start()
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB)).start()
      nodeA.enableAuthoring(devKeys)

      val connBtoA =
        nodeB.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeA.listenPort))

      // Two empty blocks so recent history can anchor a report.
      nodeA.authorSlot(1).isDefined shouldBe true
      nodeA.authorSlot(2).isDefined shouldBe true

      // Wait for B to follow (it must build the guarantee on the same state).
      awaitSync(nodeA, nodeB)

      // B signs and distributes a guarantee for the next slot via CE 135.
      val guarantee = buildGuarantee(nodeB, slot = 3)
      nodeB.distribution.distributeGuarantee(connBtoA, guarantee)

      val poolDeadline = System.currentTimeMillis() + 10000
      while nodeA.pools.guaranteeCount == 0 && System.currentTimeMillis() < poolDeadline do
        Thread.sleep(50)
      nodeA.pools.guaranteeCount shouldBe 1

      // A authors slot 3 including the guarantee.
      nodeA.authorSlot(3).isDefined shouldBe true
      val blockWithGuarantee =
        nodeA.chain.decodeBlock(nodeA.chain.blockStore.getBlock(nodeA.chain.best.hash).get).toOption.get
      blockWithGuarantee.extrinsic.guarantees.size shouldBe 1

      // The report is now pending on core 0.
      nodeA.chain.stateView().cores.reports(0).isDefined shouldBe true

      // Assurers distribute CE 141 assurances anchored at the new head.
      buildAssurances(nodeA.chain.best.hash).foreach { a =>
        nodeB.distribution.distributeAssurance(connBtoA, a)
      }
      val assuranceDeadline = System.currentTimeMillis() + 10000
      while nodeA.pools.assuranceCount < 5 && System.currentTimeMillis() < assuranceDeadline do
        Thread.sleep(50)
      nodeA.pools.assuranceCount shouldBe 5

      // A authors slot 4: the assurances make the report available and its
      // results accumulate (service 0's PVM code runs).
      nodeA.authorSlot(4).isDefined shouldBe true
      val blockWithAssurances =
        nodeA.chain.decodeBlock(nodeA.chain.blockStore.getBlock(nodeA.chain.best.hash).get).toOption.get
      blockWithAssurances.extrinsic.assurances.size shouldBe 5

      // Core 0 is free again (report accumulated out of rho).
      nodeA.chain.stateView().cores.reports(0).isDefined shouldBe false

      // B follows the whole chain.
      awaitSync(nodeA, nodeB)
      nodeB.chain.best.stateRoot shouldBe nodeA.chain.best.stateRoot
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }

  private def awaitSync(a: JamNode, b: JamNode): Unit =
    val deadline = System.currentTimeMillis() + 30000
    while b.chain.best.hash != a.chain.best.hash && System.currentTimeMillis() < deadline do
      Thread.sleep(100)
    b.chain.best.hash shouldBe a.chain.best.hash
