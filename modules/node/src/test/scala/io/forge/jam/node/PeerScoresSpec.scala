package io.forge.jam.node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PeerScoresSpec extends AnyFunSuite with Matchers:

  private def key(i: Int) = Array.fill[Byte](32)(i.toByte)

  test("repeated offences ban a peer for a while; occasional ones fade") {
    var now = 0L
    var bans = List.empty[Int]
    val scores = new PeerScores(onBan = k => bans ::= k(0).toInt, clock = () => now)
    scores.report(key(1), Offence.InvalidBlock)
    scores.banned(key(1)) shouldBe false
    scores.report(key(1), Offence.InvalidBlock)
    scores.banned(key(1)) shouldBe true
    bans shouldBe List(1)
    scores.banned(key(2)) shouldBe false
    scores.report(key(1), Offence.Malformed)
    bans shouldBe List(1)
    now += PeerScores.BanMillis
    scores.banned(key(1)) shouldBe false

    (1 to 200).foreach { _ =>
      now += PeerScores.HalfLifeMillis
      scores.report(key(3), Offence.Unanswered)
    }
    scores.banned(key(3)) shouldBe false
    scores.points(key(3)) should be < 10.5
    (1 to 20).foreach(_ => scores.report(key(4), Offence.Unanswered))
    scores.banned(key(4)) shouldBe true
  }

  test("a validator is never banned, but its points are kept and it is asked last") {
    val scores = new PeerScores(exempt = _(0) == 7, onBan = _ => fail("a validator was banned"))
    (1 to 10).foreach(_ => scores.report(key(7), Offence.BadSignature))
    scores.banned(key(7)) shouldBe false
    scores.points(key(7)) should be > 400.0
    scores.report(key(8), Offence.Unanswered)
    scores.leastFaultedFirst(List(7, 8, 9, 10))(key) shouldBe List(9, 10, 8, 7)
  }

  test("a ban ends as soon as the peer is exempt: it became a validator") {
    var validator = false
    val scores = new PeerScores(exempt = _ => validator)
    (1 to 2).foreach(_ => scores.report(key(1), Offence.InvalidBlock))
    scores.banned(key(1)) shouldBe true
    validator = true
    scores.banned(key(1)) shouldBe false
  }

  test("the table is bounded, and a peer serving a ban is not forgotten") {
    var now = 0L
    val scores = new PeerScores(clock = () => now, maxPeers = 8)
    (1 to 2).foreach(_ => scores.report(key(1), Offence.InvalidBlock))
    scores.banned(key(1)) shouldBe true
    (10 to 60).foreach(i => scores.report(key(i), Offence.Unanswered))
    scores.banned(key(1)) shouldBe true
    scores.points(key(10)) shouldBe 0.0 // forgotten
    scores.points(key(60)) should be > 0.0
  }

  test("the table stays bounded when every entry serves a ban: the ban that ends soonest is forgotten first") {
    var now = 0L
    val scores = new PeerScores(clock = () => now, maxPeers = 4)
    def ban(i: Int): Unit =
      (1 to 2).foreach(_ => scores.report(key(i), Offence.BadSignature))
      scores.banned(key(i)) shouldBe true
    (1 to 4).foreach { i => ban(i); now += 1000 }
    (5 to 40).foreach { i => ban(i); now += 1000 }
    scores.tableSize shouldBe 4
    (37 to 40).foreach(i => scores.banned(key(i)) shouldBe true)
    scores.banned(key(1)) shouldBe false // its ban ended soonest and was forgotten
    scores.report(key(50), Offence.Unanswered)
    now += 1000
    scores.report(key(51), Offence.Unanswered)
    scores.tableSize shouldBe 4
    scores.points(key(50)) shouldBe 0.0
    (38 to 40).foreach(i => scores.banned(key(i)) shouldBe true)
  }

  test("peers are ordered by their points read once each, not re-read while sorting") {
    var reads = 0
    var now = 0L
    val scores = new PeerScores(clock = () => { reads += 1; now })
    (0 until 100).foreach(i => (0 until i % 7).foreach(_ => scores.report(key(i), Offence.Unanswered)))
    reads = 0
    val ordered = scores.leastFaultedFirst((0 until 100).toList)(key)
    reads should be <= 1
    ordered.map(i => i % 7) shouldBe (0 until 100).toList.map(_ % 7).sorted
    ordered.filter(_ % 7 == 3) shouldBe (0 until 100).filter(_ % 7 == 3).toList // equal points keep their order
  }

class PeerFaultReportingSpec extends AnyFunSuite with Matchers with LoopbackFixture:
  import io.forge.jam.network.{Loopback, PeerStreamHandler, StreamKind}

  test("a peer that serves an invalid block is charged for it, and banned when it does so again") {
    withNodes(3, "peer-faults") { nodes =>
      val Seq(a, b, c) = nodes.toSeq
      author(Seq(a), 1 to 1)
      val genesis = a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
      val good = a.chain.blocksAscending(genesis, 1).head
      val bad = good.clone()
      bad(40) = (bad(40) ^ 1).toByte
      val serveBad: PeerStreamHandler = (_, stream) =>
        stream.onMessage { _ =>
          stream.send(bad)
          stream.finish()
        }
      val (_, bToA) = Loopback.pair(
        Array.fill[Byte](32)(0x21),
        b.identity.publicKeyBytes,
        kind => Option.when(kind == StreamKind.BlockRequest)(serveBad),
        _ => None
      )
      b.distribution.trackConnection(bToA)
      val peer = bToA.peerKey
      b.scores.points(peer) shouldBe 0.0
      val (_, bToC) = join(c, b, _ => _ => None)

      b.sync.fetchBlock(bToA, a.chain.best.hash)
      eventually("the invalid block is charged to the peer")(b.scores.points(peer) > 40)
      b.scores.banned(peer) shouldBe false
      b.chain.atGenesis shouldBe true
      b.distribution.peers.last shouldBe bToA
      b.distribution.peers.head shouldBe bToC

      b.sync.fetchBlock(bToA, a.chain.best.hash)
      eventually("the peer is banned")(b.scores.banned(peer))
    }
  }

  private def serveBytes(bytes: Array[Byte]): PeerStreamHandler = (_, stream) =>
    stream.onMessage { _ =>
      stream.send(bytes)
      stream.finish()
    }

  private def servingBlocks(node: JamNode): Byte => Option[PeerStreamHandler] =
    kind => Option.when(kind == StreamKind.BlockRequest)(node.sync.blockRequestHandler)

  test("a peer that serves a real header with another body is charged each time; the block is not lost") {
    withNodes(3, "peer-faults-body") { nodes =>
      val Seq(a, b, c) = nodes.toSeq
      author(Seq(a, c), 1 to 1)
      val genesis = a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
      val good = a.chain.blocksAscending(genesis, 1).head
      val block = a.chain.decodeBlock(good).toOption.get
      val hash = a.chain.headerHashOf(block)
      val extra = io.forge.jam.core.types.extrinsic.Preimage(
        io.forge.jam.core.primitives.ServiceId(0),
        io.forge.jam.core.JamBytes(Array[Byte](1, 2, 3))
      )
      val tampered = a.chain.encodeBlock(block.copy(extrinsic = block.extrinsic.copy(preimages = List(extra))))
      val (_, bToA) = Loopback.pair(
        Array.fill[Byte](32)(0x22),
        b.identity.publicKeyBytes,
        kind => Option.when(kind == StreamKind.BlockRequest)(serveBytes(tampered)),
        _ => None
      )
      val (_, bToC) = join(c, b, servingBlocks)
      val liar = bToA.peerKey
      val honest = bToC.peerKey

      b.sync.fetchBlock(bToA, hash)
      eventually("the altered body is charged to the peer")(b.scores.points(liar) > 0)
      b.scores.points(liar) should (be > 55.0 and be <= 60.0)
      b.scores.banned(liar) shouldBe false
      b.chain.atGenesis shouldBe true
      eventually("the first fetch is done")(!b.sync.fetchingBlock(hash))

      b.sync.fetchBlock(bToA, hash)
      eventually("the peer is banned")(b.scores.banned(liar))
      b.chain.atGenesis shouldBe true

      eventually("the second fetch is done")(!b.sync.fetchingBlock(hash))
      b.sync.fetchBlock(bToC, hash)
      eventually("the real block is imported")(b.chain.best.hash == hash)
      b.scores.points(honest) shouldBe 0.0
    }
  }

  test("a peer that serves a block we disregarded is not charged; one that serves an invalid block is") {
    withNodes(2, "peer-faults-disregarded") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 1)
      val hash = a.chain.best.hash
      b.chain.disregard(hash) shouldBe Left(ChainError.NotOnBestChain)
      val (_, bToA) = join(a, b, servingBlocks)
      val peer = bToA.peerKey

      b.sync.fetchBlock(bToA, hash)
      eventually("the fetch is done")(!b.sync.fetchingBlock(hash))
      b.scores.points(peer) shouldBe 0.0
      b.chain.atGenesis shouldBe true
      b.chain.importBlock(a.chain.blocksDescending(hash, 1).head) shouldBe Left(ChainError.Disregarded)

      b.chain.meta.markInvalid(hash)
      b.sync.fetchBlock(bToA, hash)
      eventually("the invalid block is charged to the peer")(b.scores.points(peer) > 40)
    }
  }

  test("a malformed guarantee and a malformed shard answer are each charged to their sender") {
    withNodes(2, "peer-faults-kinds") { nodes =>
      val Seq(a, b) = nodes.toSeq
      val garbage: PeerStreamHandler = (_, stream) =>
        stream.onMessage { _ =>
          stream.send(Array.fill[Byte](40)(1))
          stream.send(Array.fill[Byte](24)(2))
          stream.send(Array.fill[Byte](7)(3))
          stream.finish()
        }
      val (aToB, bToA) = Loopback.pair(
        a.identity.publicKeyBytes,
        b.identity.publicKeyBytes,
        _ => Some(garbage),
        kind => Option.when(kind == StreamKind.WorkReportDistribution)(b.distribution.workReportHandler)
      )
      val peer = bToA.peerKey

      val out = aToB.openStream(StreamKind.WorkReportDistribution).get()
      out.send(Array.fill[Byte](50)(9))
      out.finish()
      eventually("the malformed guarantee is charged")(b.scores.points(peer) >= 19)

      val before = b.scores.points(peer)
      b.shards.fetchShards(bToA, io.forge.jam.core.primitives.Hash(Array.fill[Byte](32)(5)), 0) shouldBe None
      eventually("the malformed shard answer is charged")(b.scores.points(peer) >= before + 19)
      b.scores.banned(peer) shouldBe false
    }
  }

  test("a block request, a state request, a preimage announcement and a judgment that cannot be read are each charged") {
    withNodes(2, "peer-faults-requests") { nodes =>
      val Seq(a, b) = nodes.toSeq
      val auditor = b.enableAuditing(Seq(devKeys(1)))
      val serves: Byte => Option[PeerStreamHandler] = {
        case StreamKind.BlockRequest         => Some(b.sync.blockRequestHandler)
        case StreamKind.StateRequest         => Some(b.sync.stateRequestHandler)
        case StreamKind.PreimageAnnouncement => Some(b.preimages.announceHandler)
        case StreamKind.JudgmentPublication  => Some(auditor.judgmentHandler)
        case _                               => None
      }
      val (aToB, bToA) = Loopback.pair(a.identity.publicKeyBytes, b.identity.publicKeyBytes, _ => None, serves)
      val peer = bToA.peerKey
      val unreadable = Seq(
        "block request" -> (StreamKind.BlockRequest, new Array[Byte](36)),
        "state request" -> (StreamKind.StateRequest, new Array[Byte](97)),
        "preimage announcement" -> (StreamKind.PreimageAnnouncement, new Array[Byte](3)),
        "judgment" -> (StreamKind.JudgmentPublication, new Array[Byte](5))
      )
      for (what, (kind, message)) <- unreadable do
        val before = b.scores.points(peer)
        aToB.openStream(kind).get().send(message)
        eventually(s"the unreadable $what to be charged")(b.scores.points(peer) >= before + 19)
    }
  }

  private val genesisFinal = SyncCodec.HashSlot(io.forge.jam.core.primitives.Hash.zero, 0)

  private def announce(conn: Loopback.Connection, headers: Seq[Array[Byte]]): Unit =
    val up = conn.openStream(StreamKind.BlockAnnouncement).get()
    up.send(SyncCodec.encodeHandshake(genesisFinal, Nil))
    headers.foreach(h => up.send(SyncCodec.encodeAnnouncement(h, genesisFinal)))

  private def madeUpHeader(): Array[Byte] =
    val bytes = new Array[Byte](120)
    new java.util.Random().nextBytes(bytes)
    io.forge.jam.core.LittleEndian.put(bytes, 96, 5L, 4)
    bytes

  private val resetsRequests: Byte => Option[PeerStreamHandler] =
    kind => Option.when(kind == StreamKind.BlockRequest)((_, stream) => stream.onMessage(_ => stream.reset()))

  test("a peer that resets the request for a block it announced is charged; one asked for a block it did not announce is not") {
    withNodes(1, "peer-faults-announced") { nodes =>
      val b = nodes.head
      val announcer = Array.fill[Byte](32)(0x31)
      val (announcerToB, _) = Loopback.pair(announcer, b.identity.publicKeyBytes, resetsRequests, servingAnnouncements(b))
      announce(announcerToB, Seq(madeUpHeader()))
      eventually("the reset to be charged", seconds = 10)(b.scores.points(announcer) > 4)
      b.scores.points(announcer) should be <= Offence.Unanswered.points.toDouble

      val asked = Array.fill[Byte](32)(0x32)
      val (_, bToAsked) = Loopback.pair(asked, b.identity.publicKeyBytes, resetsRequests, _ => None)
      val unknown = io.forge.jam.core.primitives.Hash(Array.fill[Byte](32)(0x33))
      b.sync.fetchBlock(bToAsked, unknown)
      eventually("the fetch to end")(!b.sync.fetchingBlock(unknown))
      b.scores.points(asked) shouldBe 0.0
    }
  }

  private def servingAnnouncements(node: JamNode): Byte => Option[PeerStreamHandler] =
    kind => Option.when(kind == StreamKind.BlockAnnouncement)(node.sync.blockAnnouncementHandler)

  test("a peer announcing many blocks it never serves holds one fetch, however many connections it has") {
    withNodes(2, "peer-faults-slots") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 1)
      val hash = a.chain.best.hash
      val silent: Byte => Option[PeerStreamHandler] =
        kind => Option.when(kind == StreamKind.BlockRequest)((_, stream) => stream.onMessage(_ => ()))
      val hog = Array.fill[Byte](32)(0x41)
      (1 to SyncService.MaxConcurrentFetches + 1).foreach { _ =>
        val (hogToB, _) = Loopback.pair(hog, b.identity.publicKeyBytes, silent, servingAnnouncements(b))
        announce(hogToB, Seq.fill(3)(madeUpHeader()))
      }
      Thread.sleep(300)
      val (aToB, _) = join(a, b, node => kind =>
        if node eq a then Option.when(kind == StreamKind.BlockRequest)(a.sync.blockRequestHandler)
        else servingAnnouncements(b)(kind)
      )
      announce(aToB, Seq(a.chain.headerBytes(hash).get))
      eventually("the honest peer's block to be imported", seconds = 8)(b.chain.best.hash == hash)
    }
  }

  test("an answer cut short because our own inbound limit refused it is not charged") {
    withNodes(1, "peer-faults-refused") { nodes =>
      val b = nodes.head
      val peer = Array.fill[Byte](32)(0x51)
      final class RefusedStream(val kind: Byte) extends io.forge.jam.network.MessageStream:
        private var receiver: Array[Byte] => Unit = _ => ()
        private var closed: () => Unit = () => ()
        def onMessage(f: Array[Byte] => Unit): io.forge.jam.network.MessageStream = { receiver = f; this }
        def onClosed(f: () => Unit): io.forge.jam.network.MessageStream = { closed = f; this }
        def send(message: Array[Byte]): Unit = ()
        def finish(): Unit =
          receiver(Array.fill[Byte](40)(1))
          closed()
        def close(): Unit = ()
        def reset(): Unit = ()
        def isOpen: Boolean = false
        def id: Long = 0L
        def pendingOutboundBytes: Long = 0L
        override def refusedHere: Boolean = true
      val (_, real) = Loopback.pair(peer, b.identity.publicKeyBytes, _ => None, _ => None)
      val conn = new io.forge.jam.network.PeerConnection:
        def initiator: Boolean = real.initiator
        def openedAtMillis: Long = real.openedAtMillis
        def peerKey: Array[Byte] = peer
        def peerPublicKey: io.forge.jam.core.primitives.Ed25519PublicKey = real.peerPublicKey
        def negotiatedAlpn: String = real.negotiatedAlpn
        def holdInbound(bytes: Long): Boolean = true
        def releaseInbound(bytes: Long): Unit = ()
        def heldInboundBytes: Long = 0L
        def pendingOutboundBytes: Long = 0L
        def droppedOutboundMessages: Long = 0L
        def openStream(kind: Byte) = java.util.concurrent.CompletableFuture.completedFuture(new RefusedStream(kind))
        def close(): Unit = ()
        def isOpen: Boolean = true
        def openStreamCount: Int = 0
        def remoteAddress: Option[java.net.SocketAddress] = None
      b.shards.fetchShards(conn, io.forge.jam.core.primitives.Hash(Array.fill[Byte](32)(5)), 0) shouldBe None
      b.scores.points(peer) shouldBe 0.0
    }
  }

  test("a peer the operator configured is never banned, whether its key was given or learned on connecting") {
    import io.forge.jam.core.codec.JamCodecs.encode
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val spec = ChainSpec(
      id = "peer-faults-configured",
      config = io.forge.jam.core.ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dirs = (0 until 3).map(i => tempDir(s"jam-configured-$i"))
    val Seq(a, b, c) = dirs.map(d => new JamNode(spec, NodeConfig(dataDir = d, slotTicking = false)).start())
    try
      def at(n: JamNode) = new java.net.InetSocketAddress("127.0.0.1", n.listenPort)
      b.addPeer(at(a), Some(a.identity.publicKeyBytes))
      b.addPeer(at(c)) // its key is learned when the dial succeeds
      val stranger = Array.fill[Byte](32)(0x61)
      Seq(a.identity.publicKeyBytes, c.identity.publicKeyBytes, stranger).foreach { k =>
        (1 to 3).foreach(_ => b.scores.report(k, Offence.InvalidBlock))
      }
      b.scores.banned(stranger) shouldBe true
      Seq(a, c).map(_.identity.publicKeyBytes).foreach { k =>
        b.scores.banned(k) shouldBe false
        b.scores.points(k) should be > 100.0 // still counted, and asked last
        b.scores.leastFaultedFirst(List(k, Array.fill[Byte](32)(0x62)))(identity).head(0) shouldBe 0x62.toByte
      }
    finally
      Seq(a, b, c).foreach(_.shutdown())
      dirs.foreach(cleanup)
  }

  test("the validators exempt from bans are those of the best head's state, across an epoch change while catching up") {
    import io.forge.jam.core.codec.JamCodecs.encode
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val joining = Array.fill[Byte](32)(0x77)
    val keyvals = genesis.state.keyvals.map { kv =>
      val k = kv.key.toArray
      if k(0) == 7 && k.drop(1).forall(_ == 0) then
        val v = kv.value.toArray.clone()
        System.arraycopy(joining, 0, v, v.length - 336 + 32, 32)
        kv.copy(value = io.forge.jam.core.JamBytes(v))
      else kv
    }
    val spec = ChainSpec(
      id = "peer-faults-head-validators",
      config = io.forge.jam.core.ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = keyvals,
      bootnodes = Nil
    )
    val dir = tempDir("jam-head-validators")
    val node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false, ed25519Seed = Some(devKeys(0).ed25519Secret)))
    try
      node.chain.initializeOrRestore(spec)
      (1 to 2).foreach(_ => node.scores.report(joining, Offence.InvalidBlock))
      node.scores.banned(joining) shouldBe true
      node.peers.update(1000L)
      author(Seq(node), 1 to 13) // slot 12 starts epoch 1
      node.chain.stateView().validators.nextEpoch.exists(_.ed25519.bytes.toArray.sameElements(joining)) shouldBe true
      node.peers.update(1000L)
      node.peers.isValidator(joining) shouldBe false
      node.scores.banned(joining) shouldBe false
    finally
      node.shutdown()
      cleanup(dir)
  }
