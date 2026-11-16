package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import io.forge.jam.network.{JamnpConfig, PeerConnection, JamnpNode, NodeIdentity, StreamKind}
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** What a peer's message costs beyond decoding it (a store read, a signature check, a state lookup) is done by a
  * worker pool, not by the network event loop the message arrived on: while the pool's threads are all taken the
  * message waits, and it is handled once one is free.
  */
class EventLoopWorkSpec extends AnyFunSuite with Matchers with NodeFixture:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  private def eventually(what: String, seconds: Int = 10)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(10)
    if !cond then fail(s"timed out waiting for: $what")

  private def withNode(f: (JamNode, ChainSpec) => Unit): Unit =
    if !Files.exists(genesisPath) then cancel("dev genesis not available")
    given Decoder[Genesis] = Genesis.decoder
    val genesis = decode[Genesis](Files.readString(genesisPath)).fold(e => fail(e.getMessage), identity)
    val spec = ChainSpec("event-loop-work", ChainConfig.TINY, Some(genesis.header.encode.toArray), None, genesis.state.keyvals, Nil)
    val dir = Files.createTempDirectory("jam-event-loop-work")
    var node: JamNode = null
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false)).start()
      f(node, spec)
    finally
      if node != null then node.shutdown()
      cleanup(dir)

  private def withPeer(node: JamNode, spec: ChainSpec)(f: PeerConnection => Unit): Unit =
    val peer = new JamnpNode(NodeIdentity.generate(), JamnpConfig(genesisHashPrefix = spec.alpnPrefix))
    try f(peer.connect(new InetSocketAddress("127.0.0.1", node.listenPort)).get(10, TimeUnit.SECONDS))
    finally peer.shutdown()

  /** Take every thread of `work`; the latch returned lets them go. */
  private def occupy(work: InboundWork, threads: Int): CountDownLatch =
    val release = new CountDownLatch(1)
    val started = new CountDownLatch(threads)
    (1 to threads).foreach { _ =>
      work.submit(null, 0L) { started.countDown(); release.await() } shouldBe true
    }
    started.await(5, TimeUnit.SECONDS) shouldBe true
    release

  /** A request on a fresh stream is not answered while `work` is busy, and its stream ends once it is free. */
  private def waitsFor(work: InboundWork, threads: Int, conn: PeerConnection, kind: Byte, request: Array[Byte]): Unit =
    val release = occupy(work, threads)
    try
      val ended = new CountDownLatch(1)
      val stream = conn.openStream(kind).get(10, TimeUnit.SECONDS)
      stream.onClosed(() => ended.countDown())
      stream.send(request)
      stream.finish()
      withClue(s"${StreamKind.name(kind)} answered while its pool was busy: ") {
        ended.await(400, TimeUnit.MILLISECONDS) shouldBe false
      }
      release.countDown()
      withClue(s"${StreamKind.name(kind)} not answered once its pool was free: ") {
        ended.await(10, TimeUnit.SECONDS) shouldBe true
      }
    finally release.countDown()

  test("shard requests (CE 137 to 140) are answered by the inbound pool") {
    withNode { (node, spec) =>
      withPeer(node, spec) { conn =>
        val unknown = ShardService.encodeShardRequest(Hashing.blake2b256("no such root".getBytes), 0)
        val segments = ShardService.encodeSegmentShardRequest(List((Hashing.blake2b256("no such root".getBytes), 0, List(0, 1))))
        waitsFor(node.inbound, InboundWork.Threads, conn, StreamKind.ShardDistribution, unknown)
        waitsFor(node.inbound, InboundWork.Threads, conn, StreamKind.AuditShardRequest, unknown)
        waitsFor(node.inbound, InboundWork.Threads, conn, StreamKind.SegmentShardRequest, segments)
        waitsFor(node.inbound, InboundWork.Threads, conn, StreamKind.SegmentShardRequestVerified, segments)
      }
    }
  }

  test("preimage announcements (CE 142) are checked by the inbound pool and requests (CE 143) answered by the serving pool") {
    withNode { (node, spec) =>
      withPeer(node, spec) { conn =>
        val hash = Hashing.blake2b256("no such preimage".getBytes)
        waitsFor(node.inbound, InboundWork.Threads, conn, StreamKind.PreimageAnnouncement, PreimageCodec.encodeAnnouncement(7L, hash, 16L))
        waitsFor(node.serving, InboundWork.ServingThreads, conn, StreamKind.PreimageRequest, hash.bytes.toArray)
      }
    }
  }

  test("GRANDPA justification (CE 130) and warp sync (CE 153) requests are answered by the serving pool") {
    withNode { (node, spec) =>
      node.enableGrandpa(None)
      withPeer(node, spec) { conn =>
        val block = Hashing.blake2b256("no such block".getBytes).bytes.toArray
        waitsFor(node.serving, InboundWork.ServingThreads, conn, StreamKind.GrandpaJustificationRequest, block)
        waitsFor(node.serving, InboundWork.ServingThreads, conn, StreamKind.WarpSyncRequest, Array[Byte](9, 0, 0, 0))
      }
    }
  }

  test("a judgment (CE 145) has its signature checked by the inbound pool") {
    withNode { (node, spec) =>
      val auditor = node.enableAuditing(Nil)
      withPeer(node, spec) { conn =>
        val release = occupy(node.inbound, InboundWork.Threads)
        try
          val stream = conn.openStream(StreamKind.JudgmentPublication).get(10, TimeUnit.SECONDS)
          stream.send(AuditFlood.judgment(2, devKeys(2), Hashing.blake2b256("some report".getBytes), valid = true))
          stream.finish()
          Thread.sleep(400)
          auditor.bufferSizes._3 shouldBe 0
          release.countDown()
          eventually("the judgment to be kept once its signature was checked")(auditor.bufferSizes._3 == 1)
        finally release.countDown()
      }
    }
  }

  test("block announcements (UP 0) are handled off the event loop, in order") {
    withNode { (node, spec) =>
      withPeer(node, spec) { conn =>
        val release = occupy(node.announcing, 1)
        try
          val genesis = SyncCodec.HashSlot(node.chain.finalized.hash, 0L)
          val stream = conn.openStream(StreamKind.BlockAnnouncement).get(10, TimeUnit.SECONDS)
          stream.send(SyncCodec.encodeHandshake(genesis, List(SyncCodec.HashSlot(Hash(Array.fill[Byte](32)(5)), 5L))))
          // A header is its parent's hash, then the rest: slot 9 at offset 96.
          val header = new Array[Byte](120)
          header(96) = 9
          stream.send(SyncCodec.encodeAnnouncement(header, genesis))
          Thread.sleep(400)
          node.sync.peerBestSlots shouldBe Nil
          release.countDown()
          eventually("the peer's announced head to be noted")(node.sync.peerBestSlots == List(9L))
        finally release.countDown()
      }
    }
  }

  test("a handshake naming more leaves than any chain has is refused") {
    withNode { (node, spec) =>
      withPeer(node, spec) { conn =>
        val ended = new CountDownLatch(1)
        val genesis = SyncCodec.HashSlot(node.chain.finalized.hash, 0L)
        val leaves = (1 to SyncService.MaxHandshakeLeaves + 1).toList.map(i => SyncCodec.HashSlot(Hashing.blake2b256(s"leaf $i".getBytes), 3L))
        val stream = conn.openStream(StreamKind.BlockAnnouncement).get(10, TimeUnit.SECONDS)
        stream.onClosed(() => ended.countDown())
        stream.send(SyncCodec.encodeHandshake(genesis, leaves))
        ended.await(10, TimeUnit.SECONDS) shouldBe true
        node.sync.peerBestSlots shouldBe Nil
      }
    }
  }
