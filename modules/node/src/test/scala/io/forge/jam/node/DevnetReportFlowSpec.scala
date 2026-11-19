package io.forge.jam.node


import io.forge.jam.core.{ChainConfig, Hashing, JamBytes, LittleEndian, constants}
import io.forge.jam.core.primitives.{CoreIndex, Ed25519Signature, Gas, Hash, ServiceId, Timeslot, ValidatorIndex}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.dispute.GuaranteeSignature
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, GuaranteeExtrinsic}
import io.forge.jam.core.types.work.{ExecutionResult, PackageSpec}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.workresult.{RefineLoad, WorkResult}
import io.forge.jam.crypto.Ed25519ZebraWrapper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

/** The full report lifecycle across networked nodes on the dev genesis:
  * a guarantor node signs a work report and distributes it via CE 135; the
  * author pools and includes it; assurers distribute CE 141 assurances; the
  * next authored block makes the report available and accumulation executes
  * the service's PVM code — the complete guarantee → assure → accumulate
  * pipeline over JAMNP. Both nodes report to a JIP-3 telemetry server, and each
  * step's events are checked as the server decodes them.
  */
class DevnetReportFlowSpec extends AnyFunSuite with Matchers with NodeFixture:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

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
        UShort(view.config.validatorCount),
        Hash.zero,
        UShort(0)
      ),
      context = Context(
        anchor = anchor.headerHash,
        anchorSlot = Timeslot(anchor.slot.toInt),
        stateRoot = anchor.stateRoot,
        beefyRoot = anchor.beefyRoot,
        lookupAnchor = anchor.headerHash,
        lookupAnchorSlot = Timeslot(anchor.slot.toInt),
        lookupAnchorStateRoot = anchor.stateRoot,
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

    val assigned = GuarantorService.assignments(view, slot, node.spec.config)._1.zipWithIndex
      .filter(_._1 == 0)
      .map(_._2)
      .take(3)
      .toList
    assigned.size should be >= 2

    val signatures = assigned.map { vi =>
      val sig = Ed25519ZebraWrapper.sign(devKeys(vi).ed25519Secret, message)
      GuaranteeSignature(ValidatorIndex(vi), Ed25519Signature(sig))
    }.sortBy(_.validatorIndex.toInt)

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

  test("guarantee -> inclusion -> assurances -> availability -> accumulation over JAMNP, each step on JIP-3 telemetry") {
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
    val telemetryA = new TelemetryCollector
    val telemetryB = new TelemetryCollector
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false, telemetry = Some(telemetryA.address))).start()
      // B sends the validators' assurances (CE 141), which are taken from a validator only: it is validator 1.
      nodeB = new JamNode(
        spec,
        NodeConfig(
          dataDir = dirB,
          slotTicking = false,
          ed25519Seed = Some(devKeys(1).ed25519Secret),
          telemetry = Some(telemetryB.address)
        )
      ).start()
      val connected = System.currentTimeMillis() + 20000
      while (telemetryA.nodeInformation.isEmpty || telemetryB.nodeInformation.isEmpty) && System.currentTimeMillis() < connected do
        Thread.sleep(20)
      telemetryA.nodeInformation.isDefined shouldBe true
      telemetryB.nodeInformation.isDefined shouldBe true
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

      val erasureRoot = guarantee.report.packageSpec.erasureRoot
      val current = nodeA.chain.stateView().validators.current
      val signers = guarantee.signatures.map(s => current(s.validatorIndex.toInt).ed25519).toSet
      signers should not be empty
      val indexDeadline = System.currentTimeMillis() + 10000
      while nodeA.guarantorIndex.guarantorsOf(erasureRoot).isEmpty && System.currentTimeMillis() < indexDeadline do
        Thread.sleep(20)
      nodeA.guarantorIndex.guarantorsOf(erasureRoot) should contain allElementsOf signers
      val none: Byte => Option[io.forge.jam.network.PeerStreamHandler] = _ => None
      val stranger = Array.fill[Byte](32)(0x55)
      val toStranger = io.forge.jam.network.Loopback.pair(stranger, stranger, none, none)._1
      val toGuarantor = io.forge.jam.network.Loopback.pair(stranger, signers.head.bytes.toArray, none, none)._1
      nodeA.guarantorIndex.first(erasureRoot, List(toStranger, toGuarantor)) shouldBe List(toGuarantor, toStranger)
      nodeA.guarantorIndex.first(Hash(Array.fill[Byte](32)(0x77)), List(toStranger, toGuarantor)) shouldBe List(toStranger, toGuarantor)

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

      checkTelemetry(nodeA, nodeB, telemetryA, telemetryB, guarantee, accumulatedAt = nodeA.chain.best.hash)
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      telemetryA.close()
      telemetryB.close()
      cleanup(dirA)
      cleanup(dirB)
  }

  private def same(a: Array[Byte], b: Array[Byte]): Boolean = java.util.Arrays.equals(a, b)

  private def checkTelemetry(
      nodeA: JamNode,
      nodeB: JamNode,
      a: TelemetryCollector,
      b: TelemetryCollector,
      guarantee: GuaranteeExtrinsic,
      accumulatedAt: Hash
  ): Unit =
    val keyA = nodeA.identity.publicKeyBytes
    val keyB = nodeB.identity.publicKeyBytes

    val connectingIn = a.await("21 connecting in")(a.all(21)().headOption)
    a.await("23 connected in, from B")(a.referring(23, connectingIn.id).find(e => same(e.bytes(8, 32), keyB)))
    val connectingOut = b.await("24 connecting out, to A")(b.all(24)(body => same(body.take(32), keyA)).headOption)
    b.await("26 connected out")(b.referring(26, connectingOut.id).headOption)

    b.await("60 on B: opened here")(b.all(60)(body => same(body.take(32), keyA) && body(32) == 0).headOption)
    a.await("60 on A: opened by B")(a.all(60)(body => same(body.take(32), keyB) && body(32) == 1).headOption)
    val announced = b.await("62 on B: slot 4 announced by A")(
      b.all(62)(body => same(body.take(32), keyA) && body(32) == 1 && LittleEndian.get(body, 33, 4) == 4L).headOption
    )
    announced.bytes(37, 32) shouldBe accumulatedAt.bytes.toArray
    a.await("62 on A: slot 4 announced to B")(
      a.all(62)(body => same(body.take(32), keyB) && body(32) == 0 && LittleEndian.get(body, 33, 4) == 4L).headOption
    )

    val sending = b.await("63 on B: a block request to A")(b.all(63)(body => same(body.take(32), keyA)).headOption)
    b.await("66 on B: request sent")(b.referring(66, sending.id).headOption)
    val transferred = b.await("68 on B: a block received")(b.referring(68, sending.id).headOption)
    transferred.body.length shouldBe 8 + 4 + Telemetry.blockOutline(0, Hash.zero, 0, 0, 0, 0, 0, 0).length + 1
    val receiving = a.await("64 on A: a block request from B")(a.all(64)(body => same(body, keyB)).headOption)
    a.await("67 on A: request received")(a.referring(67, receiving.id).headOption)
    a.await("68 on A: a block sent")(a.referring(68, receiving.id).headOption)

    val importing = b.await("43 on B: importing slot 4")(b.all(43)(body => LittleEndian.get(body, 0, 4) == 4L).headOption)
    importing.bytes(8, 32) shouldBe accumulatedAt.bytes.toArray
    b.await("45 on B")(b.referring(45, importing.id).headOption)
    val executedOnB = b.await("47 on B")(b.referring(47, importing.id).headOption)

    val authoring = a.await("40 on A: slot 4")(a.all(40)(body => LittleEndian.get(body, 0, 4) == 4L).headOption)
    a.await("42 on A")(a.referring(42, authoring.id).headOption)
    val executedOnA = a.await("47 on A")(a.referring(47, authoring.id).headOption)
    for executed <- List(executedOnA, executedOnB) do
      val (count, used) = io.forge.jam.core.codec.JamCodecs.decodeCompactInteger(executed.body, 8)
      val entry = 4 + Telemetry.accumulateCost(io.forge.jam.protocol.pvmhost.AccumulateCost.zero).length
      executed.body.length shouldBe 8 + used + count.toInt * entry
      val service0 = (0 until count.toInt).map(i => 8 + used + i * entry).find(at => executed.u32(at) == 0L)
        .getOrElse(fail("service 0 is not among the accumulated services"))
      executed.u32(service0 + 4) should be >= 1L // accumulate calls
      executed.u32(service0 + 12) shouldBe 1L // items accumulated
      executed.u64(service0 + 16) should be > 0L // gas used

    val generating = a.await("80 on A: tickets for epoch 1")(a.all(80)(body => LittleEndian.get(body, 0, 4) == 1L).headOption)
    val generated = a.await("82 on A")(a.referring(82, generating.id).headOption)
    generated.body.length shouldBe 8 + 1 + 4 * 32
    generated.body(8) shouldBe 4.toByte

    val reportHash = Hashing.blake2b256(guarantee.report.encode.toArray).bytes.toArray
    val receivingGuarantee = a.await("110 on A: a guarantee from B")(a.all(110)(body => same(body, keyB)).headOption)
    val outline = a.await("112 on A")(a.referring(112, receivingGuarantee.id).headOption)
    outline.bytes(8, 32) shouldBe reportHash
    outline.u32(40) shouldBe 3L
    outline.body.length shouldBe 8 + 32 + 4 + 1 + 2 * guarantee.signatures.size
    val discarded = a.await("113 on A: reported on chain")(a.all(113)(body => same(body.take(32), reportHash)).headOption)
    discarded.body.last shouldBe Telemetry.GuaranteeDiscard.ReportedOnChain.toByte

    eventually(10)(a.all(131)(body => same(body.take(32), keyB)).size >= 5) shouldBe true

  private def eventually(seconds: Int)(cond: => Boolean): Boolean =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(20)
    cond

  private def awaitSync(a: JamNode, b: JamNode): Unit =
    val deadline = System.currentTimeMillis() + 30000
    while b.chain.best.hash != a.chain.best.hash && System.currentTimeMillis() < deadline do
      Thread.sleep(100)
    b.chain.best.hash shouldBe a.chain.best.hash
