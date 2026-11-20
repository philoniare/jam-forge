package io.forge.jam.node

import java.util.concurrent.TimeUnit

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.network.StreamKind
import io.forge.jam.protocol.state.KeyValue
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UShort

class GuarantorAcceptanceSpec extends AnyFunSuite with Matchers with org.scalatest.LoneElement with NodeFixture:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  private final case class Devnet(spec: ChainSpec, authCodeHash: Hash)

  private def devnet(id: String, metadata: Int => Option[Array[Byte]] = _ => None): Devnet =
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val nullAuth = genesis.state.keyvals.map(_.value.toArray).find(_.length == 50).getOrElse(fail("no NULL Authorizer"))
    val kappaKey = Array[Byte](8) ++ new Array[Byte](30)
    val keyvals = genesis.state.keyvals.map { kv =>
      if !kv.key.toArray.sameElements(kappaKey) then kv
      else
        val v = kv.value.toArray.clone()
        val prefix = v.length % 336 // the set's length, before its 336-byte keys
        for i <- 0 until v.length / 336; m <- metadata(i) do System.arraycopy(m, 0, v, prefix + i * 336 + 208, m.length)
        KeyValue(kv.key, JamBytes(v))
    }
    Devnet(
      ChainSpec(
        id = id,
        config = ChainConfig.TINY,
        genesisHeaderBytes = Some(genesis.header.encode.toArray),
        explicitGenesisHash = None,
        genesisState = keyvals,
        bootnodes = Nil
      ),
      Hashing.blake2b256(nullAuth)
    )

  private def workPackage(node: JamNode, net: Devnet, anchor: Hash, slot: Long, root: Hash, beefy: Hash, payload: String) =
    val code = node.chain.stateView().accumulation.serviceAccounts
      .find(_.id == 0).map(_.data.service.codeHash).getOrElse(fail("no service 0"))
    WorkPackage(
      authCodeHost = ServiceId(0),
      authCodeHash = net.authCodeHash,
      context = Context(
        anchor = anchor,
        anchorSlot = Timeslot(slot.toInt),
        stateRoot = root,
        beefyRoot = beefy,
        lookupAnchor = anchor,
        lookupAnchorSlot = Timeslot(slot.toInt),
        lookupAnchorStateRoot = root,
        prerequisites = List.empty
      ),
      authorization = JamBytes.empty,
      authorizerConfig = JamBytes.empty,
      items = List(
        WorkItem(
          service = ServiceId(0),
          codeHash = code,
          payload = JamBytes(payload.getBytes("UTF-8")),
          refineGasLimit = Gas(100_000_000L),
          accumulateGasLimit = Gas(10_000_000L),
          importSegments = List.empty,
          extrinsic = List.empty,
          exportCount = UShort(0)
        )
      )
    )

  private def anchoredBeforeHead(node: JamNode, net: Devnet, payload: String): WorkPackage =
    val history = node.chain.stateView().beta.history
    val a = history(history.size - 2)
    workPackage(node, net, a.headerHash, a.slot, a.stateRoot, a.beefyRoot, payload)

  private def submitRpc(node: JamNode, core: Int, wp: WorkPackage): io.circe.Json =
    val b64 = java.util.Base64.getEncoder.encodeToString(wp.encode.toArray)
    val request = s"""{"jsonrpc":"2.0","id":1,"method":"submitWorkPackage","params":[$core,"$b64",[]]}"""
    node.rpcApi.handle(request, new RpcSession(_ => ())).getOrElse(fail("submitWorkPackage: no response"))

  private def accepted(response: io.circe.Json): Unit =
    withClue(response.noSpaces)(response.hcursor.downField("result").focus shouldBe Some(io.circe.Json.Null))

  private def awaitGuarantee(node: JamNode, millis: Long = 30000): Unit =
    val deadline = System.currentTimeMillis() + millis
    while node.pools.guaranteeCount == 0 && System.currentTimeMillis() < deadline do Thread.sleep(50)
    node.pools.guaranteeCount shouldBe 1

  private def awaitSync(a: JamNode, b: JamNode): Unit =
    val deadline = System.currentTimeMillis() + 30000
    while b.chain.best.hash != a.chain.best.hash && System.currentTimeMillis() < deadline do Thread.sleep(50)
    b.chain.best.hash shouldBe a.chain.best.hash

  private def address(node: JamNode) = new java.net.InetSocketAddress("127.0.0.1", node.listenPort)

  private def withNodes(prefix: String)(body: (NodeConfig => JamNode, String => java.nio.file.Path) => Unit)(using net: Devnet): Unit =
    val dirs = scala.collection.mutable.ListBuffer.empty[java.nio.file.Path]
    val nodes = scala.collection.mutable.ListBuffer.empty[JamNode]
    def dir(name: String) = { val d = tempDir(s"$prefix-$name"); dirs += d; d }
    def start(config: NodeConfig) = { val n = new JamNode(net.spec, config).start(); nodes += n; n }
    try body(start, dir)
    finally
      nodes.foreach(_.shutdown())
      dirs.foreach(cleanup)

  test("a package anchored at the guarantor's best head is guaranteed and included: the anchor's root is H_r of the next block") {
    given net: Devnet = devnet("accept-head-anchor")
    val telemetry = new TelemetryCollector
    try withNodes("jam-accept-head") { (start, dir) =>
      val author = start(NodeConfig(dir("a"), slotTicking = false))
      val guarantor = start(NodeConfig(dir("b"), slotTicking = false, telemetry = Some(telemetry.address)))
      val connected = System.currentTimeMillis() + 20000
      while telemetry.nodeInformation.isEmpty && System.currentTimeMillis() < connected do Thread.sleep(20)
      author.enableAuthoring(devKeys)
      guarantor.enableGuaranteeing(devKeys)
      author.connectPeer(address(guarantor))
      author.authorSlot(1).isDefined shouldBe true
      author.authorSlot(2).isDefined shouldBe true
      awaitSync(author, guarantor)

      val head = guarantor.chain.best
      val entry = guarantor.chain.stateView().beta.history.last
      entry.headerHash shouldBe head.hash
      val wp = workPackage(guarantor, net, head.hash, head.slot, head.stateRoot, entry.beefyRoot, "anchored at the head")
      accepted(submitRpc(guarantor, 0, wp))

      awaitGuarantee(author)
      author.authorSlot(3).isDefined shouldBe true
      author.chain.stateView().cores.reports(0).map(_.guarantee.report.packageSpec.hash) shouldBe Some(Hashing.blake2b256(wp.encode.toArray))

      val own = guarantor.identity.publicKeyBytes
      val submission = telemetry.await("90 work-package submission")(
        telemetry.all(90)(body => java.util.Arrays.equals(body.take(32), own) && body(32) == 0).headOption
      )
      val outline = telemetry.await("94 received")(telemetry.referring(94, submission.id).headOption)
      outline.u16(8) shouldBe 0L // core
      outline.bytes(14, 32) shouldBe Hashing.blake2b256(wp.encode.toArray).bytes.toArray
      outline.bytes(46, 32) shouldBe head.hash.bytes.toArray // anchor
      for d <- List(96, 97) do telemetry.await(s"$d")(telemetry.referring(d, submission.id).headOption)
      val authorized = telemetry.await("95 authorized")(telemetry.referring(95, submission.id).headOption)
      authorized.body.length shouldBe 8 + 16 + 8 + 16
      authorized.u64(8) should be > 0L // is-authorized gas
      val refined = telemetry.await("101 refined")(telemetry.referring(101, submission.id).headOption)
      refined.body(8) shouldBe wp.items.size.toByte
      refined.body.length shouldBe 8 + 1 + wp.items.size * (16 + 8 + 5 * 16)
      val built = telemetry.await("102 report built")(telemetry.referring(102, submission.id).headOption)
      val report = author.chain.stateView().cores.reports(0).get.guarantee.report
      built.bytes(8, 32) shouldBe Hashing.blake2b256(report.encode.toArray).bytes.toArray
      built.u32(40) shouldBe report.packageSpec.length.toLong
      built.bytes(44, 32) shouldBe report.packageSpec.erasureRoot.bytes.toArray
      val guarantee = telemetry.await("105 guarantee built")(telemetry.referring(105, submission.id).headOption)
      guarantee.bytes(8, 32) shouldBe built.bytes(8, 32)
      val sending = telemetry.await("106 sending to the author")(
        telemetry.referring(106, guarantee.id).find(e => java.util.Arrays.equals(e.bytes(8, 32), author.identity.publicKeyBytes))
      )
      telemetry.await("108 sent")(telemetry.referring(108, sending.id).headOption)
      telemetry.await("109 distributed")(telemetry.referring(109, submission.id).headOption)
      telemetry.referring(92, submission.id) shouldBe empty
    }
    finally telemetry.close()
  }

  test("a package the guarantor guaranteed lately is refused rather than refined again") {
    given net: Devnet = devnet("accept-repeat")
    withNodes("jam-accept-repeat") { (start, dir) =>
      val author = start(NodeConfig(dir("a"), slotTicking = false))
      val guarantor = start(NodeConfig(dir("b"), slotTicking = false))
      author.enableAuthoring(devKeys)
      guarantor.enableGuaranteeing(devKeys)
      author.connectPeer(address(guarantor))
      author.authorSlot(1).isDefined shouldBe true
      author.authorSlot(2).isDefined shouldBe true
      awaitSync(author, guarantor)

      val wp = anchoredBeforeHead(guarantor, net, "once is enough")
      accepted(submitRpc(guarantor, 0, wp))
      awaitGuarantee(author)
      val again = submitRpc(guarantor, 0, wp)
      withClue(again.noSpaces)(again.hcursor.downField("error").get[String]("message").toOption.get should include("already guaranteed"))
    }
  }

  test("a copy of a package that is still on its way to a guarantee is refused, and the package is guaranteed once") {
    given net: Devnet = devnet("accept-in-flight")
    withNodes("jam-accept-in-flight") { (start, dir) =>
      val author = start(NodeConfig(dir("a"), slotTicking = false))
      val guarantor = start(NodeConfig(dir("b"), slotTicking = false))
      author.enableAuthoring(devKeys)
      guarantor.enableGuaranteeing(devKeys)
      author.authorSlot(1).isDefined shouldBe true
      author.authorSlot(2).isDefined shouldBe true
      guarantor.chain.best.slot shouldBe 0L
      val service = guarantor.guarantorService.get
      def inFlight(n: Int): Unit =
        val deadline = System.currentTimeMillis() + 10000
        while service.packagesInFlight != n && System.currentTimeMillis() < deadline do Thread.sleep(10)
        service.packagesInFlight shouldBe n

      val wp = anchoredBeforeHead(author, net, "one at a time")
      accepted(submitRpc(guarantor, 0, wp))
      inFlight(1)
      val copy = submitRpc(guarantor, 0, wp)
      withClue(copy.noSpaces)(copy.hcursor.downField("error").get[String]("message").toOption.get should include("already on its way"))
      service.packagesInFlight shouldBe 1

      author.connectPeer(address(guarantor))
      awaitGuarantee(author)
      inFlight(0)
      val later = submitRpc(guarantor, 0, wp)
      withClue(later.noSpaces)(later.hcursor.downField("error").get[String]("message").toOption.get should include("already guaranteed"))
    }
  }

  test("a package that was refused is not held as in flight: the same package can be submitted again") {
    given net: Devnet = devnet("accept-in-flight-refused")
    val logger = org.slf4j.LoggerFactory.getLogger(classOf[GuarantorService]).asInstanceOf[ch.qos.logback.classic.Logger]
    val seen = new ch.qos.logback.core.read.ListAppender[ch.qos.logback.classic.spi.ILoggingEvent]()
    seen.start()
    logger.addAppender(seen)
    try
      withNodes("jam-accept-in-flight-refused") { (start, dir) =>
        val author = start(NodeConfig(dir("a"), slotTicking = false))
        val guarantor = start(NodeConfig(dir("b"), slotTicking = false))
        author.enableAuthoring(devKeys)
        guarantor.enableGuaranteeing(devKeys)
        author.connectPeer(address(guarantor))
        author.authorSlot(1).isDefined shouldBe true
        author.authorSlot(2).isDefined shouldBe true
        awaitSync(author, guarantor)
        val service = guarantor.guarantorService.get

        val a = guarantor.chain.stateView().beta.history.last
        val unauthorized = workPackage(guarantor, net, a.headerHash, a.slot, a.stateRoot, a.beefyRoot, "refused")
          .copy(authCodeHash = Hash(Array.fill[Byte](32)(7)))
        val name = Hashing.blake2b256(unauthorized.encode.toArray).toHex.take(18)
        def refusals = seen.list.toArray.toList.map(_.asInstanceOf[ch.qos.logback.classic.spi.ILoggingEvent].getFormattedMessage)
          .count(m => m.contains(name) && m.contains("refused before refine"))
        for attempt <- 1 to 2 do
          accepted(submitRpc(guarantor, 0, unauthorized))
          val deadline = System.currentTimeMillis() + 20000
          while (refusals < attempt || service.packagesInFlight != 0) && System.currentTimeMillis() < deadline do Thread.sleep(10)
          refusals shouldBe attempt
          service.packagesInFlight shouldBe 0
        guarantor.pools.guaranteeCount shouldBe 0
      }
    finally logger.detachAppender(seen)
  }

  test("a package with imports that its authorizer does not authorize is refused before any import is fetched") {
    given net: Devnet = devnet("accept-unauthorized-imports")
    val logger = org.slf4j.LoggerFactory.getLogger(classOf[GuarantorService]).asInstanceOf[ch.qos.logback.classic.Logger]
    val seen = new ch.qos.logback.core.read.ListAppender[ch.qos.logback.classic.spi.ILoggingEvent]()
    seen.start()
    logger.addAppender(seen)
    try
      withNodes("jam-accept-unauthorized") { (start, dir) =>
        val author = start(NodeConfig(dir("a"), slotTicking = false))
        val guarantor = start(NodeConfig(dir("b"), slotTicking = false))
        author.enableAuthoring(devKeys)
        guarantor.enableGuaranteeing(devKeys)
        author.connectPeer(address(guarantor))
        author.authorSlot(1).isDefined shouldBe true
        author.authorSlot(2).isDefined shouldBe true
        awaitSync(author, guarantor)

        val base = anchoredBeforeHead(guarantor, net, "unauthorized with imports")
        val imported = io.forge.jam.core.types.workitem.WorkItemImportSegment(Hash(Array.fill[Byte](32)(5)), UShort(0))
        val wp = base.copy(authCodeHost = ServiceId(99), items = base.items.map(_.copy(importSegments = List(imported))))
        val name = Hashing.blake2b256(wp.encode.toArray).toHex.take(18)
        accepted(submitRpc(guarantor, 0, wp))

        def refusal = seen.list.toArray.toList.map(_.asInstanceOf[ch.qos.logback.classic.spi.ILoggingEvent].getFormattedMessage)
          .find(m => m.contains(name) && m.contains("refused"))
        val deadline = System.currentTimeMillis() + 30000
        while refusal.isEmpty && System.currentTimeMillis() < deadline do Thread.sleep(50)
        refusal.getOrElse(fail("no refusal logged")) should include("not authorized")
      }
    finally logger.detachAppender(seen)
  }

  test("a package anchored at a block the guarantor has not imported yet is guaranteed once the block arrives") {
    given net: Devnet = devnet("accept-late-anchor")
    withNodes("jam-accept-late") { (start, dir) =>
      val author = start(NodeConfig(dir("a"), slotTicking = false))
      val guarantor = start(NodeConfig(dir("b"), slotTicking = false))
      author.enableAuthoring(devKeys)
      guarantor.enableGuaranteeing(devKeys)
      author.authorSlot(1).isDefined shouldBe true
      author.authorSlot(2).isDefined shouldBe true
      guarantor.chain.best.slot shouldBe 0L

      accepted(submitRpc(guarantor, 0, anchoredBeforeHead(author, net, "anchored ahead of the guarantor")))
      Thread.sleep(300)
      author.connectPeer(address(guarantor))

      awaitGuarantee(author)
    }
  }

  test("at a rotation boundary a guarantor of the rotation just ended guarantees, co-signed, timed in that rotation") {
    given net: Devnet = devnet("accept-rotation")
    withNodes("jam-accept-rotation") { (start, dir) =>
      val author = start(NodeConfig(dir("a"), slotTicking = false))
      author.enableAuthoring(devKeys)
      (1 to 3).foreach(s => author.authorSlot(s).isDefined shouldBe true)

      val view = author.chain.stateView()
      def onCore0(slot: Long) =
        GuarantorService.assignments(view, slot, net.spec.config)._1.zipWithIndex.collect { case (0, v) => v }
      val ended = onCore0(3L)
      ended.size shouldBe 3
      assume(onCore0(4L).intersect(ended).isEmpty, "the two rotations must give core 0 different guarantors")

      val first = start(NodeConfig(dir("g1"), slotTicking = false, ed25519Seed = Some(devKeys(ended(0)).ed25519Secret)))
      val second = start(NodeConfig(dir("g2"), slotTicking = false, ed25519Seed = Some(devKeys(ended(1)).ed25519Secret)))
      author.connectPeer(address(first))
      author.connectPeer(address(second))
      first.connectPeer(address(second))
      awaitSync(author, first)
      awaitSync(author, second)
      first.enableGuaranteeing(Seq(devKeys(ended(0))))
      second.enableGuaranteeing(Seq(devKeys(ended(1)), devKeys(ended(2))))

      val wp = anchoredBeforeHead(author, net, "sent at the boundary")
      val stream = author.connectPeer(address(first)).openStream(StreamKind.WorkPackageSubmission).get(10, TimeUnit.SECONDS)
      stream.send(Array[Byte](0, 0) ++ wp.encode.toArray)
      stream.finish()

      awaitGuarantee(author)
      author.authorSlot(4).isDefined shouldBe true
      val block = author.chain.decodeBlock(author.chain.blockStore.getBlock(author.chain.best.hash).get).toOption.get
      val guarantee = block.extrinsic.guarantees.loneElement
      guarantee.slot.toLong shouldBe 3L
      guarantee.signatures.map(_.validatorIndex.toInt).toSet.size should be >= 2
      guarantee.signatures.map(_.validatorIndex.toInt).toSet.subsetOf(ended.toSet) shouldBe true
      author.chain.stateView().cores.reports(0).isDefined shouldBe true
    }
  }

  test("a follower hands a package from its RPC to a guarantor of the core it was not connected to, dialed from κ's metadata") {
    val port = { val s = new java.net.DatagramSocket(0); try s.getLocalPort finally s.close() }
    val endpoint = new Array[Byte](18) // ::ffff:127.0.0.1, then the port little-endian
    endpoint(10) = 0xff.toByte; endpoint(11) = 0xff.toByte
    endpoint(12) = 127; endpoint(15) = 1
    endpoint(16) = (port & 0xff).toByte; endpoint(17) = ((port >> 8) & 0xff).toByte
    given net: Devnet = devnet("accept-follower", _ => Some(endpoint))
    withNodes("jam-accept-follower") { (start, dir) =>
      val follower = start(NodeConfig(dir("f"), slotTicking = false))
      val core0 = GuarantorService.assignments(follower.chain.stateView(), 1L, net.spec.config)._1.zipWithIndex.collect { case (0, v) => v }
      val guarantor = start(
        NodeConfig(dir("g"), listenPort = port, slotTicking = false, ed25519Seed = Some(devKeys(core0.head).ed25519Secret))
      )
      guarantor.enableAuthoring(devKeys)
      guarantor.enableGuaranteeing(devKeys)
      guarantor.authorSlot(1).isDefined shouldBe true
      guarantor.authorSlot(2).isDefined shouldBe true

      follower.chain.best.slot shouldBe 0L
      accepted(submitRpc(follower, 0, anchoredBeforeHead(guarantor, net, "through a follower")))
      awaitGuarantee(guarantor)
    }
  }
