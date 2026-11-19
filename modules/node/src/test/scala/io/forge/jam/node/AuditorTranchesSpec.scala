package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, ConcurrentLinkedQueue, CountDownLatch, ExecutorService, Executors, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.{CoreIndex, Ed25519Signature, Gas, Hash, Timeslot, ValidatorIndex}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, GuaranteeExtrinsic}
import io.forge.jam.core.types.work.PackageSpec
import io.forge.jam.core.types.workpackage.{AvailabilityAssignment, WorkReport}
import io.forge.jam.crypto.{BandersnatchVrf, Ed25519ZebraWrapper}
import io.forge.jam.network.{Loopback, MessageStream, PeerConnection, PeerStreamHandler, StreamKind}
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.vrfs.BandersnatchWrapper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

import AuditSelection.*

class AuditorTranchesSpec extends AnyFunSuite with Matchers with NodeFixture:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)
  private val TickMillis = 10L

  private final class FakeChain(real: ChainManager, block: Block, blockHash: Hash, report: WorkReport) extends ChainReader:
    @volatile var parentStateMissing = false

    @volatile var bestSlot: Option[Long] = None
    @volatile var finalizedSlot: Option[Long] = None
    def config: ChainConfig = real.config
    def best: ChainHead = bestSlot.fold(real.best)(s => real.best.copy(slot = s))
    def finalized: ChainHead = finalizedSlot.fold(real.finalized)(s => real.finalized.copy(slot = s))
    def nextBlockSlot: Long = real.nextBlockSlot
    def hasBlock(h: Hash): Boolean = h == blockHash || real.hasBlock(h)
    def block(h: Hash): Option[Block] = if h == blockHash then Some(block) else real.block(h)
    def slotOf(h: Hash): Option[Long] = if h == blockHash then Some(block.header.slot.toLong) else real.slotOf(h)
    def stateView(): TrieBackedJamState = real.stateView()
    def stateViewOf(head: ChainHead): TrieBackedJamState = real.stateViewOf(head)
    def stateViewAtRoot(root: Hash): TrieBackedJamState = real.stateViewAtRoot(root)
    def stateViewAt(h: Hash): Option[TrieBackedJamState] =
      if h != block.header.parent then real.stateViewAt(h)
      else if parentStateMissing then None
      else
        real.stateViewAt(h).map { v =>
          val guarantee = GuaranteeExtrinsic(report, Timeslot(UInt(0)), Nil)
          v.cores.reports = Some(AvailabilityAssignment(guarantee, 0L)) :: List.fill(config.coresCount - 1)(None)
          v
        }
    def stateAtRoot(root: Hash) = real.stateAtRoot(root)
    def readRawState(k: JamBytes): Option[JamBytes] = real.readRawState(k)
    private[node] def ancestorsOf(parent: Hash, parentPostRoot: Hash, slot: Long) = real.ancestorsOf(parent, parentPostRoot, slot)

  private final class Fixture(
      make: () => AuditorService,
      val chain: FakeChain,
      val store: InMemoryRoleStore,
      val block: Block,
      val blockHash: Hash,
      val report: Hash,
      val now: AtomicLong,
      ticks: AtomicInteger,
      hold: Hold,
      distribution: DistributionService,
      val step: AtomicLong
  ):
    @volatile var auditor: AuditorService = make()
    val items: Seq[(Int, Hash)] = Seq(0 -> report)

    def restart(): Unit =
      auditor.shutdown()
      auditor = make()

    def connect(keys: ValidatorKeySet, failFirst: Int = 0): Peer =
      val peer = new Peer
      val (toPeer, _) = Loopback.pair(new Array[Byte](32), keys.ed25519Public, _ => None, peer.handlers)
      distribution.trackConnection(new Flaky(toPeer, new AtomicInteger(failFirst), peer.opens))
      peer
    val y: Array[Byte] = BandersnatchVrf.getVrfOutput(block.header.entropySource.toArray).get

    def at(n: Int): Unit = now.set(n * TrancheSeconds * 1000L + 1000L)

    def settle(): Unit =
      val c = ticks.get
      eventually("an audit tick")(ticks.get > c + 2)
      drain()

    def drain(): Unit = auditor.judgesOf(report): Unit

    def audited: Boolean = auditor.isAudited(blockHash)

    def whileHeld(body: => Unit): Unit =
      val entered = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      hold.latches = Some((entered, release))
      try
        entered.await(20, TimeUnit.SECONDS) shouldBe true
        body
      finally
        hold.latches = None
        release.countDown()

    def track(): Unit = auditor.onImported(ChainHead(blockHash, block.header.slot.toLong, Hash.zero), block)

    def announcement(v: Int, n: Int, its: Seq[(Int, Hash)] = items): Announcement =
      Announcement(its, Ed25519ZebraWrapper.sign(devKeys(v).ed25519Secret, announcementMessage(n, its, blockHash)))

    def s0(v: Int): Array[Byte] =
      BandersnatchWrapper.ietfVrfSign(devKeys(v).bandersnatchSecret, initialInput(y), Array.emptyByteArray)

    def sn(v: Int, n: Int): Array[Byte] =
      BandersnatchWrapper.ietfVrfSign(devKeys(v).bandersnatchSecret, laterInput(y, report, n), Array.emptyByteArray)

    def yn(v: Int, n: Int): Array[Byte] = BandersnatchVrf.getVrfOutput(sn(v, n)).get

    def escalatingFor(n: Int, noShows: Int): Int =
      (2 to 5).find(v => peerEscalates(yn(v, n), devKeys.size, noShows)).getOrElse(fail("no validator escalates"))

    def announce0(v: Int, source: AnyRef = null): Boolean =
      auditor.receiveAnnouncement(devKeys(v).ed25519Public, encodeHead(blockHash, 0, announcement(v, 0)), s0(v), source)

    def judge(v: Int, valid: Boolean = true): Unit =
      auditor.receiveJudgment(AuditFlood.judgment(v, devKeys(v), report, valid))

  private final class Peer:
    val announcements = new ConcurrentLinkedQueue[(Array[Byte], Array[Byte])]()
    val judgments = new ConcurrentLinkedQueue[Array[Byte]]()
    val opens = new AtomicInteger()
    def received: (Int, Int) = (announcements.size, judgments.size)
    val handlers: Byte => Option[PeerStreamHandler] = kind =>
      if kind == StreamKind.AuditAnnouncement then
        Some((_: PeerConnection, stream: MessageStream) =>
          val parts = scala.collection.mutable.ListBuffer.empty[Array[Byte]]
          stream.onMessage { m =>
            parts += m
            if parts.size == 2 then
              announcements.add((parts(0), parts(1)))
              stream.finish()
          }
          ()
        )
      else if kind == StreamKind.JudgmentPublication then
        Some((_: PeerConnection, stream: MessageStream) =>
          stream.onMessage { m => judgments.add(m); stream.finish() }
          ()
        )
      else None

  private final class Flaky(inner: PeerConnection, failures: AtomicInteger, opens: AtomicInteger) extends PeerConnection:
    export inner.{initiator, openedAtMillis, peerKey, peerPublicKey, negotiatedAlpn, holdInbound, releaseInbound}
    export inner.{heldInboundBytes, pendingOutboundBytes, droppedOutboundMessages, close, isOpen, openStreamCount}
    export inner.remoteAddress
    def openStream(kind: Byte): CompletableFuture[? <: MessageStream] =
      opens.incrementAndGet()
      if failures.getAndDecrement() > 0 then CompletableFuture.failedFuture(new java.io.IOException("stream refused"))
      else inner.openStream(kind)

  private final class Hold:
    @volatile var latches: Option[(CountDownLatch, CountDownLatch)] = None
    def apply(): Unit = latches.foreach { (entered, release) =>
      entered.countDown()
      release.await(20, TimeUnit.SECONDS): Unit
    }

  private def eventually(what: String, seconds: Int = 20)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(2)
    if !cond then fail(s"timed out waiting for: $what")

  private def withAuditor(body: Fixture => Unit): Unit = withAuditorKeys(Nil, BandersnatchWrapper.ietfVrfSign)(body)

  private def withAuditorKeys(
      ownKeys: Seq[ValidatorKeySet],
      vrfSign: (Array[Byte], Array[Byte], Array[Byte]) => Array[Byte],
      audit: Option[WorkReport => Option[Boolean]] = None,
      egressOf: () => ExecutorService = () => Executors.newSingleThreadExecutor(),
      maxResendsPerTick: Int = AuditorService.MaxResendsPerTick
  )(body: Fixture => Unit): Unit =
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val spec = ChainSpec(
      id = "auditor-tranches",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dir = tempDir("jam-audit-tranches")
    var node: JamNode = null
    val threads = new NodeThreads
    val egress = egressOf()
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false))
      node.chain.initializeOrRestore(spec)
      val validators = node.chain.stateView().validators.current
      devKeys.indices.foreach(i => validators(i).ed25519.bytes.toSeq shouldBe devKeys(i).ed25519Public.toSeq)
      val authored = new BlockAuthor(node.chain, devKeys).tryAuthor(1).getOrElse(fail("slot 1 not authored"))
      val assurances = (0 until 5).toList.map(i =>
        AssuranceExtrinsic(authored.header.parent, JamBytes(Array[Byte](1)), ValidatorIndex(i), Ed25519Signature(new Array[Byte](64)))
      )
      val block = authored.copy(extrinsic = authored.extrinsic.copy(assurances = assurances))
      val blockHash = Hashing.blake2b256("audited block".getBytes("UTF-8"))
      val report = WorkReport(
        packageSpec = PackageSpec(Hashing.blake2b256("package".getBytes("UTF-8")), UInt(0), Hash.zero, UShort(0), Hash.zero, UShort(0)),
        context = Context(Hash.zero, Timeslot(UInt(0)), Hash.zero, Hash.zero, Hash.zero, Timeslot(UInt(0)), Hash.zero, Nil),
        coreIndex = CoreIndex(0),
        authorizerHash = Hash.zero,
        authGasUsed = Gas(0),
        authOutput = JamBytes.empty,
        segmentRootLookup = Nil,
        results = Nil
      )
      val chain = new FakeChain(node.chain, block, blockHash, report)
      val now = new AtomicLong(1000L)
      val ticks = new AtomicInteger()
      val hold = new Hold
      val step = new AtomicLong(0L)
      val clock: () => Long = () =>
        if Thread.currentThread.getName.startsWith("jam-audit") then
          ticks.incrementAndGet()
          hold()
          now.addAndGet(step.get)
        else now.get
      val store = new InMemoryRoleStore
      val make = () =>
        new AuditorService(
          chain,
          node.distribution,
          node.shards,
          node.pools,
          ownKeys,
          egress,
          trancheOrigin = (_, _) => 0L,
          clock = clock,
          tickMillis = TickMillis,
          store = store,
          threads = threads,
          vrfSign = vrfSign,
          audit = audit,
          maxResendsPerTick = maxResendsPerTick
        )
      val f = Fixture(make, chain, store, block, blockHash, Hashing.blake2b256(report.encode.toArray), now, ticks, hold, node.distribution, step)
      try body(f)
      finally f.auditor.shutdown()
    finally
      threads.close()
      egress.shutdownNow()
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)

  test("an empty tranche that has ended counts: a tranche-0 no-show nobody escalated for leaves the block audited from tranche 2") {
    withAuditor { f =>
      f.track()
      f.announce0(1) shouldBe true
      f.settle()
      f.auditor.announcersOf(f.report, 0) shouldBe Set(1)
      f.audited shouldBe false
      f.at(1); f.settle()
      f.audited shouldBe false
      f.at(2); f.settle()
      f.audited shouldBe true
      f.auditor.auditedStateConsistent shouldBe true
      val v = f.escalatingFor(1, noShows = 1)
      f.auditor.receiveAnnouncement(
        devKeys(v).ed25519Public,
        encodeHead(f.blockHash, 1, f.announcement(v, 1)),
        encodeLaterEvidence(Seq(f.sn(v, 1) -> Seq(NoShow(1, f.announcement(1, 0)))))
      )
      f.drain()
      f.auditor.announcersOf(f.report, 1) shouldBe Set(v)
      f.audited shouldBe false
      f.auditor.auditedStateConsistent shouldBe true
      f.judge(v); f.drain()
      f.audited shouldBe true
      f.judge(3, valid = false); f.drain()
      f.audited shouldBe false
      f.at(4); f.settle()
      f.audited shouldBe false
      f.auditor.auditedStateConsistent shouldBe true
    }
  }

  test("a block whose parent state is missing is not audited") {
    withAuditor { f =>
      f.chain.parentStateMissing = true
      f.track()
      f.audited shouldBe false
      f.at(3); f.settle()
      f.audited shouldBe false
    }
  }

  test("a block first tracked in tranche 3 waits for an empty tranche it watched from start to end") {
    withAuditor { f =>
      f.at(3)
      f.track()
      f.settle()
      f.audited shouldBe false
      f.at(4); f.settle()
      f.audited shouldBe false
      f.at(5); f.settle()
      f.audited shouldBe true
      f.auditor.auditedStateConsistent shouldBe true
    }
  }

  test("a tranche-0 announcement refused while the intake is full in all keeps tranche 0 from counting; tranche 1 still does") {
    withAuditor { f =>
      f.track()
      f.settle()
      f.whileHeld {
        f.auditor.receiveAnnouncement(devKeys(5).ed25519Public, Array[Byte](0), new Array[Byte](AuditorService.MaxQueuedBytes.toInt), "flood") shouldBe true
        f.announce0(1, source = "validator 1") shouldBe false
      }
      f.drain()
      f.auditor.announcersOf(f.report, 0) shouldBe Set.empty
      f.at(1); f.settle()
      f.audited shouldBe false
      f.auditor.auditedStateConsistent shouldBe true
      f.at(2); f.settle()
      f.audited shouldBe true
      f.auditor.auditedStateConsistent shouldBe true
    }
  }

  test("an announcement refused because its own source has too many waiting marks nothing lost") {
    withAuditor { f =>
      f.track()
      f.settle()
      val source = "validator 1"
      f.whileHeld {
        (1 to AuditorService.MaxQueuedMessages / 4).foreach { _ =>
          f.auditor.receiveAnnouncement(devKeys(1).ed25519Public, Array[Byte](0), Array.emptyByteArray, source) shouldBe true
        }
        f.announce0(1, source) shouldBe false
      }
      f.drain()
      f.auditor.announcersOf(f.report, 0) shouldBe Set.empty
      f.at(1); f.settle()
      f.audited shouldBe true
      f.auditor.auditedStateConsistent shouldBe true
    }
  }

  test("a replayed announcement is dropped with no second verification or store write") {
    withAuditor { f =>
      f.track(); f.settle()
      f.announce0(1) shouldBe true
      f.drain()
      f.auditor.announcersOf(f.report, 0) shouldBe Set(1)
      val stored = f.store.size
      (f.auditor.checks.signatures.get, f.auditor.checks.vrfs.get) shouldBe (1L, 1L)
      (1 to 5).foreach(_ => f.announce0(1) shouldBe true)
      f.drain()
      (f.auditor.checks.signatures.get, f.auditor.checks.vrfs.get) shouldBe (1L, 1L)
      f.store.size shouldBe stored
    }
  }

  test("an announcement with a bad signature costs no VRF check, and a fourth attempt is not verified at all") {
    withAuditor { f =>
      f.track(); f.settle()
      val forged = encodeHead(f.blockHash, 0, Announcement(f.items, new Array[Byte](64)))
      f.auditor.receiveAnnouncement(devKeys(2).ed25519Public, forged, f.s0(2)) shouldBe true
      f.drain()
      (f.auditor.checks.signatures.get, f.auditor.checks.vrfs.get) shouldBe (1L, 0L)
      (1 to 3).foreach(_ => f.auditor.receiveAnnouncement(devKeys(2).ed25519Public, forged, f.s0(2)))
      f.drain()
      (f.auditor.checks.signatures.get, f.auditor.checks.vrfs.get) shouldBe (3L, 0L)
      f.announce0(2) shouldBe true
      f.drain()
      f.auditor.checks.signatures.get shouldBe 3L
      f.auditor.announcersOf(f.report, 0) shouldBe Set.empty
      f.announce0(3) shouldBe true
      f.drain()
      f.auditor.announcersOf(f.report, 0) shouldBe Set(3)
    }
  }

  test("forged no-shows: a claim list longer than the validator set is refused unread; the first forged one ends the check") {
    withAuditor { f =>
      f.track(); f.settle()
      val v = 4
      val head = encodeHead(f.blockHash, 1, f.announcement(v, 1))
      def send(noShows: Seq[NoShow]): Unit =
        f.auditor.receiveAnnouncement(devKeys(v).ed25519Public, head, encodeLaterEvidence(Seq(f.sn(v, 1) -> noShows)))
        f.drain()
      val forged = NoShow(1, Announcement(f.items, new Array[Byte](64)))
      val genuine = (0 until 6).filter(_ != v).map(i => NoShow(i, f.announcement(i, 0)))
      send(forged +: Seq.tabulate(999)(i => genuine(i % genuine.size)))
      (f.auditor.checks.vrfs.get, f.auditor.checks.noShowSignatures.get) shouldBe (0L, 0L)
      send(forged +: genuine.filter(_.validator != 1))
      (f.auditor.checks.vrfs.get, f.auditor.checks.noShowSignatures.get) shouldBe (1L, 1L)
      f.auditor.announcersOf(f.report, 1) shouldBe Set.empty
    }
  }

  test("a later-tranche announcement needs exactly as many proven no-shows as its VRF output asks for") {
    withAuditor { f =>
      f.track(); f.settle()
      Seq(1, 2, 3).foreach(f.announce0(_))
      f.drain()
      f.auditor.announcersOf(f.report, 0) shouldBe Set(1, 2, 3)
      val v = 4
      val needed = noShowsNeeded(f.yn(v, 1), devKeys.size)
      needed should (be >= 1 and be <= 3)
      val head = encodeHead(f.blockHash, 1, f.announcement(v, 1))
      def send(count: Int): Unit =
        val noShows = Seq(1, 2, 3).take(count).map(i => NoShow(i, f.announcement(i, 0)))
        f.auditor.receiveAnnouncement(devKeys(v).ed25519Public, head, encodeLaterEvidence(Seq(f.sn(v, 1) -> noShows)))
        f.drain()
      send(needed - 1)
      f.auditor.announcersOf(f.report, 1) shouldBe Set.empty
      send(needed)
      f.auditor.announcersOf(f.report, 1) shouldBe Set(v)
      f.auditor.checks.noShowSignatures.get shouldBe 0L
    }
  }

  test("a later-tranche VRF signature is made once per (block, report, tranche), not at every tick") {
    val ours = devKeys(4)
    val later = new AtomicInteger()
    val signer: (Array[Byte], Array[Byte], Array[Byte]) => Array[Byte] = (secret, input, aux) =>
      if input.length == initialInput(new Array[Byte](32)).length then Array.emptyByteArray
      else
        later.incrementAndGet()
        BandersnatchWrapper.ietfVrfSign(secret, input, aux)
    withAuditorKeys(Seq(ours), signer) { f =>
      val y1 = BandersnatchVrf.getVrfOutput(f.sn(4, 1)).get
      val y2 = BandersnatchVrf.getVrfOutput(f.sn(4, 2)).get
      if escalates(y1, devKeys.size, 1) || escalates(y2, devKeys.size, 1) then
        cancel("validator 4 escalates for this block's fixed VRF output; nothing to count across ticks")
      f.track()
      f.announce0(1) shouldBe true // a tranche-0 no-show for tranche 1
      f.settle()
      later.get shouldBe 0
      f.at(1)
      (0 until 20).foreach(_ => f.settle())
      later.get shouldBe 1
      val w = Seq(2, 3, 5)
        .find(v => peerEscalates(f.yn(v, 1), devKeys.size, 1))
        .getOrElse(cancel("no other validator escalates in tranche 1"))
      f.auditor.receiveAnnouncement(
        devKeys(w).ed25519Public,
        encodeHead(f.blockHash, 1, f.announcement(w, 1)),
        encodeLaterEvidence(Seq(f.sn(w, 1) -> Seq(NoShow(1, f.announcement(1, 0)))))
      )
      f.drain()
      f.at(2)
      (0 until 20).foreach(_ => f.settle())
      later.get shouldBe 2
    }
  }


  private def withOwnJudgment(
      audits: AtomicInteger = new AtomicInteger(),
      egressOf: () => ExecutorService = () => Executors.newSingleThreadExecutor(),
      maxResendsPerTick: Int = AuditorService.MaxResendsPerTick
  )(body: Fixture => Unit): Unit =
    val audit: WorkReport => Option[Boolean] = _ => { audits.incrementAndGet(); Some(true) }
    withAuditorKeys(Seq(devKeys(1)), BandersnatchWrapper.ietfVrfSign, Some(audit), egressOf, maxResendsPerTick) { f =>
      f.track()
      f.settle()
      eventually("our positive judgment") { f.drain(); f.auditor.judgesOf(f.report)._1 == Set(1) }
      f.auditor.announcersOf(f.report, 0) shouldBe Set(1)
      body(f)
    }

  private def checkOurs(f: Fixture, peer: Peer): Unit =
    val (head, evidence) = peer.announcements.peek
    head.toSeq shouldBe encodeHead(f.blockHash, 0, f.announcement(1, 0)).toSeq
    BandersnatchVrf.ietfVrfVerify(devKeys(1).bandersnatchPublic, initialInput(f.y), Array.emptyByteArray, evidence)
      .isDefined shouldBe true
    peer.judgments.peek.toSeq shouldBe AuditFlood.judgment(1, devKeys(1), f.report, valid = true).toSeq

  test("our announcement and judgment reach a validator that connects after they were made, once while finality keeps up") {
    withOwnJudgment() { f =>
      val late = f.connect(devKeys(2))
      eventually("both to reach the validator that connected late")(late.received == (1, 1))
      checkOurs(f, late)
      (0 until 10).foreach(_ => f.settle())
      late.received shouldBe (1, 1)
      val other = f.connect(ValidatorKeySet.dev(9))
      (0 until 10).foreach(_ => f.settle())
      other.received shouldBe (0, 0)
    }
  }

  test("a send that failed is tried again after a back-off") {
    withOwnJudgment() { f =>
      val flaky = f.connect(devKeys(3), failFirst = 2)
      eventually("both sends to fail")(flaky.opens.get >= 2)
      (0 until 5).foreach(_ => f.settle())
      flaky.received shouldBe (0, 0)
      flaky.opens.get shouldBe 2 // not again before the back-off
      f.now.addAndGet(AuditorService.ResendBaseMillis)
      eventually("the retries to get through")(flaky.received == (1, 1))
      checkOurs(f, flaky)
    }
  }

  test("a send dropped from a full egress queue is tried again after a back-off") {
    val egress = WorkLimits.dropOldestExecutor("egress-spec", threads = 1, capacity = 1, r => {
      val t = new Thread(r, "egress-spec"); t.setDaemon(true); t
    })
    withOwnJudgment(egressOf = () => egress) { f =>
      val holding = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      egress.execute(() => { holding.countDown(); release.await() })
      val peer =
        try
          holding.await(5, TimeUnit.SECONDS) shouldBe true
          val late = f.connect(devKeys(2))
          eventually("the queued announcement to be dropped")(egress.dropped == 1L)
          (0 until 5).foreach(_ => f.settle())
          egress.dropped shouldBe 1L // not again before the back-off
          late
        finally release.countDown()
      eventually("the judgment that was kept")(peer.received == (0, 1))
      f.now.addAndGet(AuditorService.ResendBaseMillis)
      eventually("the dropped announcement to be sent again")(peer.received == (1, 1))
      checkOurs(f, peer)
    }
  }

  test("while finality lags and the budget is short, what was sent longest ago goes first: judgments are not starved") {
    withOwnJudgment(maxResendsPerTick = 1) { f =>
      val peer = f.connect(devKeys(2))
      eventually("the first sends")(peer.received == (1, 1))
      f.chain.bestSlot = Some(f.block.header.slot.toLong + AuditorService.FinalityLagSlots + 1)
      f.step.set(AuditorService.LagResendMillis)
      eventually("both to be sent again and again") {
        val (announcements, judgments) = peer.received
        announcements >= 4 && judgments >= 4
      }
      f.step.set(0L)
    }
  }

  test("while finality lags they go to every connected validator again at a short interval; once finalized, to nobody") {
    withOwnJudgment() { f =>
      val peer = f.connect(devKeys(2))
      eventually("the first sends")(peer.received == (1, 1))
      f.now.addAndGet(AuditorService.LagResendMillis)
      (0 until 5).foreach(_ => f.settle())
      peer.received shouldBe (1, 1) // finality keeps up: delivered once is enough
      f.chain.bestSlot = Some(f.block.header.slot.toLong + AuditorService.FinalityLagSlots + 1)
      eventually("a resend while finality lags")(peer.received == (2, 2))
      (0 until 5).foreach(_ => f.settle())
      peer.received shouldBe (2, 2)
      f.now.addAndGet(AuditorService.LagResendMillis)
      eventually("another resend")(peer.received == (3, 3))
      f.chain.finalizedSlot = Some(f.block.header.slot.toLong)
      f.settle()
      val sent = peer.received
      val after = f.connect(devKeys(3))
      f.now.addAndGet(2 * AuditorService.LagResendMillis)
      (0 until 10).foreach(_ => f.settle())
      peer.received shouldBe sent
      after.received shouldBe (0, 0)
    }
  }

  test("after a restart our judgment of an unfinalized block is sent again, without auditing it again") {
    val audits = new AtomicInteger()
    withOwnJudgment(audits) { f =>
      f.restart()
      f.track()
      f.settle()
      val peer = f.connect(devKeys(2))
      eventually("our announcement and judgment after the restart")(peer.received._1 >= 1 && peer.received._2 >= 1)
      checkOurs(f, peer)
      audits.get shouldBe 1
      f.auditor.auditedStateConsistent shouldBe true
    }
  }
