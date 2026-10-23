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
      val stream = connAtoB.openStream(StreamKind.WorkPackageSubmission).get(10, TimeUnit.SECONDS)
      stream.send(Array[Byte](0, 0) ++ wp.encode.toArray)
      stream.finish()

      val poolDeadline = System.currentTimeMillis() + 30000
      while nodeA.pools.guaranteeCount == 0 && System.currentTimeMillis() < poolDeadline do
        Thread.sleep(50)
      nodeA.pools.guaranteeCount shouldBe 1

      // Slot 3 includes the guarantee; A assures it, and slot 4 makes it available, which starts the audit.
      nodeA.authorSlot(3).isDefined shouldBe true
      val block =
        nodeA.chain.decodeBlock(nodeA.chain.blockStore.getBlock(nodeA.chain.best.hash).get).toOption.get
      val report = block.extrinsic.guarantees.head.report
      val reportHash = Hashing.blake2b256(report.encode.toArray)
      auditorA.verdictFor(reportHash) shouldBe None // included, not yet available

      val assuranceDeadline = System.currentTimeMillis() + 30000
      while nodeA.pools.assuranceCount < 5 && System.currentTimeMillis() < assuranceDeadline do Thread.sleep(50)
      val available = nodeA.authorSlot(4).getOrElse(fail("slot 4 not authored"))
      awaitSync(nodeA, nodeB)

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
      nodeA.pools.judgmentCount shouldBe 0

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

  private def awaitSync(a: JamNode, b: JamNode): Unit =
    val deadline = System.currentTimeMillis() + 30000
    while b.chain.best.hash != a.chain.best.hash && System.currentTimeMillis() < deadline do
      Thread.sleep(100)
    b.chain.best.hash shouldBe a.chain.best.hash
