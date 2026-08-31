package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.network.StreamKind
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

/** The auditor path over the network: after a report lands on-chain, both
  * nodes reconstruct the audit bundle from DA custody, re-execute is-authorized + refine in the
  * PVM, and exchange CE 144 tranche-0 announcements. A tampered report fails
  * the re-execution comparison.
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

  test("included report is audited by both nodes; tampering is detected") {
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
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false)).start()
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false)).start()
      // A: author + assurer + auditor (audits via CE 138 pulls from B's
      // custody); B: guarantor + auditor (audits from local custody).
      nodeA.enableAuthoring(devKeys)
      nodeA.enableAssuring(devKeys)
      val auditorA = nodeA.enableAuditing(devKeys.take(1)) // validator 0 announces
      nodeB.enableGuaranteeing(devKeys)
      val auditorB = nodeB.enableAuditing(devKeys.drop(1).take(1)) // validator 1 announces

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
          stateRoot = anchor.stateRoot,
          beefyRoot = anchor.beefyRoot,
          lookupAnchor = anchor.headerHash,
          lookupAnchorSlot = Timeslot(UInt(view.timeslot.toInt)),
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

      // Slot 3 includes the guarantee; both nodes audit on import.
      nodeA.authorSlot(3).isDefined shouldBe true
      val block =
        nodeA.chain.decodeBlock(nodeA.chain.blockStore.getBlock(nodeA.chain.best.hash).get).toOption.get
      val report = block.extrinsic.guarantees.head.report
      val reportHash = Hashing.blake2b256(report.encode.toArray)

      // A audited synchronously during authorSlot(3)'s import.
      auditorA.verdictFor(reportHash) shouldBe Some(true)

      // B audits when the announced block syncs; then its CE 144 reaches A.
      val deadline = System.currentTimeMillis() + 30000
      while auditorB.verdictFor(reportHash).isEmpty && System.currentTimeMillis() < deadline do
        Thread.sleep(100)
      auditorB.verdictFor(reportHash) shouldBe Some(true)

      while auditorA.announcementsFor(reportHash) == 0 &&
        System.currentTimeMillis() < deadline
      do Thread.sleep(100)
      auditorA.announcementsFor(reportHash) should be >= 1 // B's announcement
      auditorB.announcementsFor(reportHash) should be >= 1 // A's announcement

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
