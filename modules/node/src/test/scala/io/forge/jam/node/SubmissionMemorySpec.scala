package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

import io.forge.jam.core.ChainConfig
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend}
import io.forge.jam.network.{InboundLimits, JamnpConfig, JamnpConnection, JamnpNode, JamnpStream, NodeIdentity, StreamKind}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** A work-package stream's messages wait in memory until the whole submission has arrived. Until then they count
  * against the sender's connection, so one peer cannot fill the node with submissions nobody has agreed to take.
  */
class SubmissionMemorySpec extends AnyFunSuite with Matchers:

  private val KiB = 1024
  private val Budget = 1024L * KiB

  private def eventually(what: String, seconds: Int = 10)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(10)
    if !cond then fail(s"timed out waiting for: $what")

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  private final class Opened(val stream: JamnpStream):
    val ended = new CountDownLatch(1)
    stream.onClosed(() => ended.countDown())
    def wasReset: Boolean = ended.await(5, TimeUnit.SECONDS)
    def stillOpen: Boolean = !ended.await(300, TimeUnit.MILLISECONDS)

  /** A guarantor behind a network node whose connections may each hold `Budget` bytes. */
  private def withGuarantor(fromValidator: Boolean = true)(f: (JamnpNode, JamnpConnection) => Unit): Unit =
    val dir = Files.createTempDirectory("jam-submission-memory")
    val trieBackend = RocksDbTrieBackend.open(dir.resolve("trie"), trackGarbage = false)
    val blockStore = BlockStore.open(dir.resolve("blocks"))
    val egress = Executors.newSingleThreadExecutor()
    val guarantor = new GuarantorService(
      new ChainManager(ChainConfig.TINY, trieBackend, blockStore),
      new DistributionService(new ExtrinsicPools, coresCount = 1, egress),
      new ExtrinsicPools,
      Seq.empty,
      fromValidator = _ => fromValidator
    )
    val config = JamnpConfig(genesisHashPrefix = "deadbeef")
    val server = new JamnpNode(NodeIdentity.generate(), config.copy(inbound = InboundLimits(maxConnectionBytes = Budget)))
    val client = new JamnpNode(NodeIdentity.generate(), config)
    server
      .registerHandler(StreamKind.WorkPackageSubmission, guarantor.workPackageSubmissionHandler)
      .registerHandler(StreamKind.WorkPackageSharing, guarantor.workPackageSharingHandler)
      .registerHandler(StreamKind.WorkPackageBundleSubmission, guarantor.bundleSubmissionHandler)
    try
      server.start(new InetSocketAddress("127.0.0.1", 0))
      f(server, client.connect(new InetSocketAddress("127.0.0.1", server.boundPort)).get(10, TimeUnit.SECONDS))
    finally
      client.shutdown()
      server.shutdown()
      guarantor.shutdown()
      egress.shutdownNow()
      blockStore.close()
      trieBackend.close()
      cleanup(dir)

  private def open(conn: JamnpConnection, kind: Byte, messages: Array[Byte]*): Opened =
    val opened = new Opened(conn.openStream(kind).get(10, TimeUnit.SECONDS))
    messages.foreach(opened.stream.send)
    opened

  /** A second stream cannot add to what the first holds; when the first ends, its bytes are given back. */
  private def heldUntilTheStreamEnds(server: JamnpNode, conn: JamnpConnection, kind: Byte, messages: Array[Byte]*): Unit =
    val bytes = messages.map(_.length.toLong).sum
    val first = open(conn, kind, messages*)
    eventually(s"${StreamKind.name(kind)} messages to be counted")(server.heldInboundBytes == bytes)
    val second = open(conn, kind, messages*)
    withClue(s"${StreamKind.name(kind)}: a second submission over the connection's budget: ") {
      second.wasReset shouldBe true
    }
    first.stillOpen shouldBe true
    server.heldInboundBytes shouldBe bytes
    first.stream.finish()
    eventually(s"${StreamKind.name(kind)} bytes to be given back")(server.heldInboundBytes == 0L)

  test("CE 133: a submission's messages are held against its connection until it has arrived in full") {
    withGuarantor() { (server, conn) =>
      heldUntilTheStreamEnds(server, conn, StreamKind.WorkPackageSubmission, new Array[Byte](64), new Array[Byte](600 * KiB))
    }
  }

  test("CE 146: a bundle submission's messages are held against its connection until it has arrived in full") {
    withGuarantor() { (server, conn) =>
      heldUntilTheStreamEnds(
        server,
        conn,
        StreamKind.WorkPackageBundleSubmission,
        Array[Byte](0, 0, 0),
        new Array[Byte](64),
        new Array[Byte](600 * KiB)
      )
    }
  }

  test("CE 134: a shared package's first message is held against its connection until the bundle follows or the stream ends") {
    withGuarantor() { (server, conn) =>
      heldUntilTheStreamEnds(server, conn, StreamKind.WorkPackageSharing, new Array[Byte](600 * KiB))
    }
  }

  test("CE 134: a peer that is not a validator has its stream reset and nothing of it is kept") {
    withGuarantor(fromValidator = false) { (server, conn) =>
      val shared = open(conn, StreamKind.WorkPackageSharing, new Array[Byte](64))
      shared.wasReset shouldBe true
      server.heldInboundBytes shouldBe 0L
    }
  }
