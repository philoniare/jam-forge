package io.forge.jam.node

import java.nio.file.Path
import java.util.concurrent.TimeUnit

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.network.StreamKind
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UShort

/** CE 134 co-signing: two guarantor nodes each hold exactly ONE validator key
  * assigned to the target core, so neither can credential a report alone
  * (>= 2 signatures required). The receiving guarantor shares the bundle via
  * CE 134; the peer re-executes it in the PVM and returns its signature; the
  * combined 2-credential guarantee lands on-chain.
  */
class DevnetCoSignSpec extends AnyFunSuite with Matchers with NodeFixture:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  test("two single-key guarantors co-sign a report via CE 134") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val nullAuthPreimage = genesis.state.keyvals
      .map(_.value.toArray)
      .find(_.length == 50)
      .getOrElse(fail("NULL Authorizer preimage not found in genesis"))
    val authCodeHash = Hashing.blake2b256(nullAuthPreimage)

    val spec = ChainSpec(
      id = "cosign-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

    val dirs = Seq("jam-cosign-a", "jam-cosign-b", "jam-cosign-c").map(tempDir)
    var nodes: Seq[JamNode] = Nil
    try
      val nodeA = new JamNode(spec, NodeConfig(dataDir = dirs(0), slotTicking = false)).start()
      nodes = Seq(nodeA)
      nodeA.enableAuthoring(devKeys)
      nodeA.authorSlot(1).isDefined shouldBe true
      nodeA.authorSlot(2).isDefined shouldBe true

      // The report will be signed for slot 3: find the validators assigned
      // to core 0 then and give one key each to B and C.
      val assignments = GuarantorService.assignments(nodeA.chain.stateView(), 3L, spec.config)._1
      val core0Validators = assignments.zipWithIndex.collect { case (0, idx) => idx }
      core0Validators.size should be >= 2

      // A guarantor's network key is its validator key: CE 134 is taken only from validators, and a guarantor
      // looks for its co-guarantors among its peers by that key.
      def guarantorNode(dir: Path, validator: Int): JamNode =
        val node = new JamNode(
          spec,
          NodeConfig(dataDir = dir, slotTicking = false, ed25519Seed = Some(devKeys(validator).ed25519Secret))
        ).start()
        nodes = nodes :+ node
        node
      val nodeB = guarantorNode(dirs(1), core0Validators(0))
      val nodeC = guarantorNode(dirs(2), core0Validators(1))

      // Mesh: A announces to B and C; B reaches C for CE 134.
      nodeA.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeB.listenPort))
      nodeA.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeC.listenPort))
      nodeB.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeC.listenPort))
      awaitSync(nodeA, nodeB)
      awaitSync(nodeA, nodeC)
      val view = nodeB.chain.stateView()
      nodeB.enableGuaranteeing(Seq(devKeys(core0Validators(0))))
      nodeC.enableGuaranteeing(Seq(devKeys(core0Validators(1))))

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
            payload = JamBytes("co-sign me".getBytes("UTF-8")),
            refineGasLimit = Gas(100_000_000L),
            accumulateGasLimit = Gas(10_000_000L),
            importSegments = List.empty,
            extrinsic = List.empty,
            exportCount = UShort(0)
          )
        )
      )

      // Builder submits to B only; B cannot credential alone.
      val connBuilderToB = nodeA.connectPeer(
        new java.net.InetSocketAddress("127.0.0.1", nodeB.listenPort)
      )
      val stream =
        connBuilderToB.openStream(StreamKind.WorkPackageSubmission).get(10, TimeUnit.SECONDS)
      stream.send(Array[Byte](0, 0) ++ wp.encode.toArray)
      stream.finish()

      // B refines, obtains C's CE 134 signature, and distributes CE 135 to A.
      val poolDeadline = System.currentTimeMillis() + 60000
      while nodeA.pools.guaranteeCount == 0 && System.currentTimeMillis() < poolDeadline do
        Thread.sleep(50)
      nodeA.pools.guaranteeCount shouldBe 1

      nodeA.authorSlot(3).isDefined shouldBe true
      val block =
        nodeA.chain.decodeBlock(nodeA.chain.blockStore.getBlock(nodeA.chain.best.hash).get).toOption.get
      block.extrinsic.guarantees.size shouldBe 1
      val guarantee = block.extrinsic.guarantees.head
      guarantee.signatures.size shouldBe 2
      guarantee.signatures.map(_.validatorIndex.toInt).toSet shouldBe
        Set(core0Validators(0), core0Validators(1))
      nodeA.chain.stateView().cores.reports(0).isDefined shouldBe true

      // B and C both refine at once, each waiting for the other's CE 134 signature: neither may wait out the
      // co-sign timeout behind its own request.
      // Both guarantors must be on block 3: they sign for slot 4, and one still on block 2 would rightly refuse a
      // package for slot 4's core.
      awaitSync(nodeA, nodeB)
      awaitSync(nodeA, nodeC)
      val assignments4 = GuarantorService.assignments(nodeB.chain.stateView(), 4L, spec.config)._1
      val core = assignments4(core0Validators(0))
      assignments4(core0Validators(1)) shouldBe core
      def submit(from: JamNode, to: JamNode, payload: String): Unit =
        val conn = from.connectPeer(new java.net.InetSocketAddress("127.0.0.1", to.listenPort))
        val s = conn.openStream(StreamKind.WorkPackageSubmission).get(10, TimeUnit.SECONDS)
        val item = wp.items.head.copy(payload = JamBytes(payload.getBytes("UTF-8")))
        s.send(Array[Byte](core.toByte, 0) ++ wp.copy(items = List(item)).encode.toArray)
        s.finish()
      val before = nodeA.pools.guaranteeCount
      val started = System.currentTimeMillis()
      submit(nodeA, nodeB, "to B")
      submit(nodeA, nodeC, "to C")
      val bothDeadline = started + GuarantorService.CoSignTimeoutMillis
      while nodeA.pools.guaranteeCount < before + 2 && System.currentTimeMillis() < bothDeadline do Thread.sleep(50)
      nodeA.pools.guaranteeCount shouldBe before + 2
    finally
      nodes.foreach(n => if n != null then n.shutdown())
      dirs.foreach(cleanup)
  }

  test("a bundle shared a second time over CE 134 gets the first answer again and is not refined again") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val nullAuth = genesis.state.keyvals.map(_.value.toArray).find(_.length == 50).getOrElse(fail("no NULL Authorizer"))
    val spec = ChainSpec(
      id = "cosign-replay",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dirs = Seq("jam-cosign-replay-a", "jam-cosign-replay-b", "jam-cosign-replay-c").map(tempDir)
    val telemetry = new TelemetryCollector
    var nodes: Seq[JamNode] = Nil
    try
      val author = new JamNode(spec, NodeConfig(dataDir = dirs(0), slotTicking = false)).start()
      nodes = Seq(author)
      author.enableAuthoring(devKeys)
      author.authorSlot(1).isDefined shouldBe true
      author.authorSlot(2).isDefined shouldBe true
      val core0 = GuarantorService.assignments(author.chain.stateView(), 3L, spec.config)._1.zipWithIndex.collect { case (0, v) => v }
      core0.size should be >= 2
      val requester = new JamNode(
        spec,
        NodeConfig(dataDir = dirs(1), slotTicking = false, ed25519Seed = Some(devKeys(core0(0)).ed25519Secret))
      ).start()
      nodes = nodes :+ requester
      val coSigner = new JamNode(
        spec,
        NodeConfig(
          dataDir = dirs(2),
          slotTicking = false,
          ed25519Seed = Some(devKeys(core0(1)).ed25519Secret),
          telemetry = Some(telemetry.address)
        )
      ).start()
      nodes = nodes :+ coSigner
      author.connectPeer(new java.net.InetSocketAddress("127.0.0.1", requester.listenPort))
      author.connectPeer(new java.net.InetSocketAddress("127.0.0.1", coSigner.listenPort))
      awaitSync(author, requester)
      awaitSync(author, coSigner)
      coSigner.enableGuaranteeing(Seq(devKeys(core0(1))))

      val view = coSigner.chain.stateView()
      val anchor = view.beta.history(view.beta.history.size - 2)
      val wp = WorkPackage(
        authCodeHost = ServiceId(0),
        authCodeHash = Hashing.blake2b256(nullAuth),
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
            codeHash = view.accumulation.serviceAccounts.find(_.id == 0).map(_.data.service.codeHash).getOrElse(fail("no service 0")),
            payload = JamBytes("share me twice".getBytes("UTF-8")),
            refineGasLimit = Gas(100_000_000L),
            accumulateGasLimit = Gas(10_000_000L),
            importSegments = List.empty,
            extrinsic = List.empty,
            exportCount = UShort(0)
          )
        )
      )
      val none = IndexedSeq(IndexedSeq.empty)
      val bundle = io.forge.jam.protocol.refine.WorkPackageBundle(wp, none, none, IndexedSeq(IndexedSeq.empty)).encode
      val header = GuarantorService.encodeSharingHeader(0, spec.config.validatorCount, Nil)
      val conn = requester.connectPeer(new java.net.InetSocketAddress("127.0.0.1", coSigner.listenPort))
      def share(): Array[Byte] =
        val answer = new java.util.concurrent.CompletableFuture[Array[Byte]]()
        val stream = conn.openStream(StreamKind.WorkPackageSharing).get(10, TimeUnit.SECONDS)
        stream.onMessage(m => answer.complete(m): Unit)
        stream.onClosed(() => answer.completeExceptionally(new IllegalStateException("closed without an answer")): Unit)
        stream.send(header)
        stream.send(bundle)
        stream.finish()
        answer.get(60, TimeUnit.SECONDS)

      val first = share()
      first.length shouldBe 32 + 64
      val second = share()
      second.toSeq shouldBe first.toSeq
      val deadline = System.currentTimeMillis() + 20000
      while telemetry.all(103)().size < 2 && System.currentTimeMillis() < deadline do Thread.sleep(20)
      telemetry.all(103)().size shouldBe 2
      withClue("work reports built (102) by the co-signer: ")(telemetry.all(102)().size shouldBe 1)
    finally
      nodes.foreach(_.shutdown())
      dirs.foreach(cleanup)
      telemetry.close()
  }

  private def awaitSync(a: JamNode, b: JamNode): Unit =
    val deadline = System.currentTimeMillis() + 30000
    while b.chain.best.hash != a.chain.best.hash && System.currentTimeMillis() < deadline do
      Thread.sleep(100)
    b.chain.best.hash shouldBe a.chain.best.hash
