package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.network.StreamKind
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UShort

/** The auditor path over the network: once a report becomes available, both nodes draw it in
  * tranche 0 (two cores, so every report), reconstruct the audit bundle from DA custody, re-execute is-authorized +
  * refine in the PVM, and exchange CE 144 announcements and CE 145 positive judgments, after which the block reads as
  * audited. A tampered report fails the re-execution comparison.
  */
class DevnetAuditorSpec extends AnyFunSuite with Matchers:

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

  test("available report is audited by both nodes and its block becomes audited; tampering is detected") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val nullAuthPreimage = genesis.state.keyvals
      .map(_.value.toArray)
      .find(_.length == 50)
      .getOrElse(fail("NULL Authorizer preimage not found in genesis"))
    val authCodeHash = Hashing.blake2b256(nullAuthPreimage)

    val spec = ChainSpec(
      id = "auditor-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-audit-a")
    val dirB = tempDir("jam-audit-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      // CE 144 names the announcer by its connection key, so each node uses its auditing validator's Ed25519 key.
      nodeA = new JamNode(spec, NodeConfig(dirA, slotTicking = false, ed25519Seed = Some(devKeys(0).ed25519Secret)))
        .start()
      nodeB = new JamNode(spec, NodeConfig(dirB, slotTicking = false, ed25519Seed = Some(devKeys(1).ed25519Secret)))
        .start()
      // A: author + assurer + auditor (audits via CE 138 pulls from B's
      // custody); B: guarantor + auditor (audits from local custody).
      nodeA.enableAuthoring(devKeys)
      nodeA.enableAssuring(devKeys)
      val auditorA = nodeA.enableAuditing(devKeys.take(1)) // validator 0 announces
      nodeB.enableGuaranteeing(devKeys)
      val auditorB = nodeB.enableAuditing(devKeys.drop(1).take(1)) // validator 1 announces
      // The verdict waits below rely on an unaudited report reading as None.
      auditorA.verdictFor(Hash.zero) shouldBe None

      // Before anything is audited, A is flooded with CE 144/145 messages for blocks and reports it does not hold:
      // none may pile up, and the honest announcements and judgments below must still get through.
      AuditFlood.run(auditorA, nodeA.pools, devKeys)

      val connAtoB =
        nodeA.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeB.listenPort))

      nodeA.authorSlot(1).isDefined shouldBe true
      nodeA.authorSlot(2).isDefined shouldBe true
      awaitSync(nodeA, nodeB)

      val view = nodeB.chain.stateView()
      val anchor = view.beta.history(view.beta.history.size - 2)
      val serviceCodeHash = view.accumulation.serviceAccounts
        .find(_.id == 0).map(_.data.service.codeHash).getOrElse(fail("no service 0"))
      val wp = WorkPackage(
        authCodeHost = ServiceId(0),
        authCodeHash = authCodeHash,
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
        authorization = JamBytes.empty,
        authorizerConfig = JamBytes.empty,
        items = List(
          WorkItem(
            service = ServiceId(0),
            codeHash = serviceCodeHash,
            payload = JamBytes("audit me".getBytes("UTF-8")),
            refineGasLimit = Gas(100_000_000L),
            accumulateGasLimit = Gas(10_000_000L),
            importSegments = List.empty,
            extrinsic = List.empty,
            exportCount = UShort(0)
          )
        )
      )
      // Builder (node A) submits via CE 133: core index (2 LE) ++ package, then the (empty) extrinsic-data
      // message, then FIN. B refines it for real (NULL Authorizer is-authorized, service 0's refine in the PVM).
      val stream = connAtoB.openStream(StreamKind.WorkPackageSubmission).get(10, TimeUnit.SECONDS)
      stream.send(Array[Byte](0, 0) ++ wp.encode.toArray)
      stream.send(Array.emptyByteArray)
      stream.finish()

      val poolDeadline = System.currentTimeMillis() + 30000
      while nodeA.pools.guaranteeCount == 0 && System.currentTimeMillis() < poolDeadline do
        Thread.sleep(50)
      nodeA.pools.guaranteeCount shouldBe 1

      // Slot 3 includes the guarantee; A assures it, and slot 4 makes it available, which starts the audit.
      nodeA.authorSlot(3).isDefined shouldBe true
      val block =
        nodeA.chain.decodeBlock(nodeA.chain.blockStore.getBlock(nodeA.chain.best.hash).get).toOption.get
      block.extrinsic.guarantees.size shouldBe 1
      val report = block.extrinsic.guarantees.head.report
      report.packageSpec.hash shouldBe Hashing.blake2b256(wp.encode.toArray)
      report.coreIndex.toInt shouldBe 0
      // The availability commitments were computed by the guarantor.
      report.packageSpec.erasureRoot.bytes.toArray should not be new Array[Byte](32)
      // The report is pending on core 0 (awaiting assurances).
      nodeA.chain.stateView().cores.reports(0).isDefined shouldBe true
      val reportHash = Hashing.blake2b256(report.encode.toArray)
      auditorA.verdictFor(reportHash) shouldBe None // included, not yet available
      // B now serves a wrong bundle of the right length on CE 147: A must not judge on it, but rebuild the bundle
      // from erasure-coded shards and judge that.
      nodeB.guarantorService.getOrElse(fail("no guarantor")).recordBundle(
        report.packageSpec.erasureRoot,
        Array.fill[Byte](report.packageSpec.length.toLong.toInt)(7)
      )
      // A judgment that outruns the block making its report available is kept for that block.
      auditorA.receiveJudgment(AuditFlood.judgment(3, devKeys(3), reportHash, valid = true))

      val assuranceDeadline = System.currentTimeMillis() + 30000
      while nodeA.pools.assuranceCount < 5 && System.currentTimeMillis() < assuranceDeadline do Thread.sleep(50)
      nodeA.pools.assuranceCount should be >= 5 // A's assurer reacted to the inclusion on its own
      val available = nodeA.authorSlot(4).getOrElse(fail("slot 4 not authored"))
      // Slot 4: assurances on-chain -> report available -> accumulated, and the core is free again.
      val block4 =
        nodeA.chain.decodeBlock(nodeA.chain.blockStore.getBlock(available.hash).get).toOption.get
      block4.extrinsic.assurances.size should be >= 5
      nodeA.chain.stateView().cores.reports(0).isDefined shouldBe false
      awaitSync(nodeA, nodeB)
      nodeB.chain.best.stateRoot shouldBe nodeA.chain.best.stateRoot

      val verdictDeadlineA = System.currentTimeMillis() + 30000
      while auditorA.verdictFor(reportHash).isEmpty && System.currentTimeMillis() < verdictDeadlineA do
        Thread.sleep(50)
      auditorA.verdictFor(reportHash) shouldBe Some(true)

      val deadline = System.currentTimeMillis() + 30000
      while auditorB.verdictFor(reportHash).isEmpty && System.currentTimeMillis() < deadline do
        Thread.sleep(100)
      auditorB.verdictFor(reportHash) shouldBe Some(true)

      while (auditorA.announcementsFor(reportHash) < 2 || auditorB.announcementsFor(reportHash) < 2) &&
        System.currentTimeMillis() < deadline
      do Thread.sleep(100)
      auditorA.announcementsFor(reportHash) shouldBe 2 // its own and B's
      auditorB.announcementsFor(reportHash) shouldBe 2

      // Both announced auditors judged positively: the block is audited on both nodes.
      while !(auditorA.isAudited(available.hash) && auditorB.isAudited(available.hash)) &&
        System.currentTimeMillis() < deadline
      do Thread.sleep(100)
      auditorA.isAudited(available.hash) shouldBe true
      auditorB.isAudited(available.hash) shouldBe true
      auditorA.judgesOf(reportHash)._1 should contain allOf (0, 1, 3)
      auditorA.bufferSizes._1 shouldBe 1 // judgments are recorded for the one report being audited

      // Validator 2 announces the report in tranche 0 but never judges it: a no-show, so A no longer counts the
      // block as audited (A_0 = {0, 1, 2} is not within J⊤ = {0, 1}).
      val dirC = tempDir("jam-audit-c")
      val nodeC =
        new JamNode(spec, NodeConfig(dirC, slotTicking = false, ed25519Seed = Some(devKeys(2).ed25519Secret))).start()
      try
        val connCtoA = nodeC.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeA.listenPort))
        val header = nodeA.chain.header(available.hash).getOrElse(fail("no header"))
        val y = io.forge.jam.crypto.BandersnatchVrf.getVrfOutput(header.entropySource.toArray).get
        val items = Seq(report.coreIndex.toInt -> reportHash)
        val s0 = io.forge.jam.vrfs.BandersnatchWrapper
          .ietfVrfSign(devKeys(2).bandersnatchSecret, AuditSelection.initialInput(y), Array.emptyByteArray)
        val sig = io.forge.jam.crypto.Ed25519ZebraWrapper
          .sign(devKeys(2).ed25519Secret, AuditSelection.announcementMessage(0, items, available.hash))
        val stream = connCtoA.openStream(StreamKind.AuditAnnouncement).get(10, TimeUnit.SECONDS)
        stream.send(AuditSelection.encodeHead(available.hash, 0, AuditSelection.Announcement(items, sig)))
        stream.send(AuditSelection.encodeFirstEvidence(s0))
        stream.finish()
        while auditorA.announcementsFor(reportHash) < 3 && System.currentTimeMillis() < deadline + 15000 do
          Thread.sleep(100)
        auditorA.announcementsFor(reportHash) shouldBe 3
        auditorA.isAudited(available.hash) shouldBe false
      finally
        nodeC.shutdown()
        cleanup(dirC)

      // No adverse judgments for a valid report.
      nodeA.pools.negativeJudgmentCount shouldBe 0

      // Validators 2 and 4 judge the report valid and 5 invalid: five of six valid, so A authors a good verdict with
      // validator 5 as a fault, and both nodes import it.
      auditorA.receiveJudgment(AuditFlood.judgment(2, devKeys(2), reportHash, valid = true))
      auditorA.receiveJudgment(AuditFlood.judgment(4, devKeys(4), reportHash, valid = true))
      auditorA.receiveJudgment(AuditFlood.judgment(5, devKeys(5), reportHash, valid = false))
      val judgedDeadline = System.currentTimeMillis() + 10000
      while nodeA.pools.negativeJudgmentCount < 1 && System.currentTimeMillis() < judgedDeadline do Thread.sleep(20)
      val disputeBlock = nodeA.authorSlot(5).getOrElse(fail("slot 5 not authored"))
      val b5 = nodeA.chain.block(disputeBlock.hash).getOrElse(fail("no block"))
      b5.extrinsic.disputes.verdicts.map(_.target) shouldBe List(reportHash)
      b5.extrinsic.disputes.verdicts.head.votes.count(_.vote) shouldBe 5
      b5.extrinsic.disputes.faults.map(_.key) shouldBe List(devKeys(5).ed25519Key)
      b5.header.offendersMark shouldBe List(Hash(devKeys(5).ed25519Public))
      val psi = nodeA.chain.stateView().psi
      psi.good shouldBe List(reportHash)
      psi.offenders shouldBe List(devKeys(5).ed25519Key)
      awaitSync(nodeA, nodeB)
      nodeB.chain.best.stateRoot shouldBe nodeA.chain.best.stateRoot
      // Judged now: the next block carries no dispute.
      val after = nodeA.authorSlot(6).getOrElse(fail("slot 6 not authored"))
      nodeA.chain.block(after.hash).get.extrinsic.disputes.verdicts shouldBe Nil

      // Tampering with the report is caught by re-execution.
      val tampered = report.copy(authGasUsed = Gas(report.authGasUsed.toLong + 1))
      auditorA.auditReport(tampered) shouldBe false
      auditorB.auditReport(tampered) shouldBe false
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("a block above finality the auditor has not looked at is not audited until it has been tracked") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val spec = ChainSpec(
      id = "auditor-untracked",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dir = tempDir("jam-audit-untracked")
    var node: JamNode = null
    try
      node = new JamNode(spec, NodeConfig(dir, slotTicking = false)).start()
      node.enableAuthoring(devKeys)
      // Imported before the auditor exists, so its import listener never saw it (as across a restart).
      val early = node.authorSlot(1).getOrElse(fail("slot 1 not authored"))
      val auditor = node.enableAuditing(devKeys.take(1))
      auditor.isAudited(node.chain.finalized.hash) shouldBe true
      auditor.isAudited(Hash.zero) shouldBe false
      auditor.isAudited(early.hash) shouldBe false
      // It made no report available, so it is audited once looked at.
      val deadline = System.currentTimeMillis() + 10000
      while !auditor.isAudited(early.hash) && System.currentTimeMillis() < deadline do Thread.sleep(20)
      auditor.isAudited(early.hash) shouldBe true
    finally
      if node != null then node.shutdown()
      cleanup(dir)
  }

  private def awaitSync(a: JamNode, b: JamNode): Unit =
    val deadline = System.currentTimeMillis() + 30000
    while b.chain.best.hash != a.chain.best.hash && System.currentTimeMillis() < deadline do
      Thread.sleep(100)
    b.chain.best.hash shouldBe a.chain.best.hash
