package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{ConcurrentLinkedQueue, TimeUnit}

import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, LittleEndian}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block
import io.forge.jam.db.ShardStore
import io.forge.jam.network.{JamnpConfig, JamnpConnection, JamnpNode, JamnpStream, NodeIdentity, StreamKind}
import io.forge.jam.protocol.traces.TraceStep
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** A block request asks for no more blocks than can be missing, a block announced by several peers is fetched from
  * one of them at a time, and shard lookups ask several peers at once.
  */
class SyncRequestSizingSpec extends AnyFunSuite with Matchers:

  private given io.circe.Decoder[TraceStep] = TraceStep.decoder(using summon[io.circe.Decoder[Block]])
  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val tracesDir = baseDir.resolve("jam-conformance/fuzz-reports/0.8.0/traces")

  private def anyStep(): Option[TraceStep] =
    if !Files.isDirectory(tracesDir) then None
    else
      val dirs = Files.list(tracesDir).sorted().toArray.map(_.asInstanceOf[Path]).filter(Files.isDirectory(_))
      dirs.iterator
        .flatMap(d =>
          Files.list(d).sorted().toArray.map(_.asInstanceOf[Path]).iterator.filter(_.toString.endsWith(".json")).take(1)
        )
        .take(10)
        .map(p => decode[TraceStep](Files.readString(p)))
        .collectFirst { case Right(step) => step }

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  private def eventually(what: String, seconds: Int = 20)(cond: => Boolean): Unit =
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(seconds.toLong)
    while !cond && System.nanoTime() < deadline do Thread.sleep(20)
    assert(cond, s"timed out waiting for $what")

  /** A block request as the node sent it: the block asked from and the maximum number of blocks. */
  private final case class Asked(from: Hash, maxBlocks: Long)

  /** A bare peer that records the block requests it gets and holds each stream open until told what to do. */
  private final class Peer(alpn: String):
    val node = new JamnpNode(NodeIdentity.generate(), JamnpConfig(genesisHashPrefix = alpn))
    val asked = new ConcurrentLinkedQueue[Asked]()
    val held = new ConcurrentLinkedQueue[JamnpStream]()
    node.registerHandler(
      StreamKind.BlockRequest,
      (_: JamnpConnection, stream: JamnpStream) =>
        stream.onMessage { msg =>
          asked.add(Asked(Hash(java.util.Arrays.copyOfRange(msg, 0, 32)), LittleEndian.get(msg, 33, 4)))
          held.add(stream)
          ()
        }
        ()
    )
    private var up: JamnpStream = null
    def connect(port: Int, genesis: SyncCodec.HashSlot): Unit =
      val conn = node.connect(new InetSocketAddress("127.0.0.1", port)).get(10, TimeUnit.SECONDS)
      up = conn.openStream(StreamKind.BlockAnnouncement).get(10, TimeUnit.SECONDS)
      up.onMessage(_ => ())
      up.send(SyncCodec.encodeHandshake(genesis, Nil))
    def announce(header: Array[Byte], finalHead: SyncCodec.HashSlot): Unit =
      up.send(SyncCodec.encodeAnnouncement(header, finalHead))
    def refuseAll(): Unit =
      var s = held.poll()
      while s != null do
        s.reset()
        s = held.poll()

  /** A header-shaped message: parent hash first, the slot at offset 96, `tag` to tell headers apart. */
  private def header(parent: Hash, slot: Long, tag: Int): Array[Byte] =
    val h = new Array[Byte](120)
    System.arraycopy(parent.bytes.toArray, 0, h, 0, 32)
    System.arraycopy(LittleEndian.encode(slot, 4), 0, h, 96, 4)
    java.nio.ByteBuffer.wrap(h, 100, 4).putInt(tag)
    h

  private def withNode(test: (JamNode, ChainSpec) => Unit): Unit =
    val step = anyStep().getOrElse(cancel("no fuzz trace available (jam-conformance corpus not present)"))
    val spec = ChainSpec(
      id = "sync-sizing",
      config = ChainConfig.TINY,
      genesisHeaderBytes = None,
      explicitGenesisHash = Some(step.block.header.parent),
      genesisState = step.preState.keyvals,
      bootnodes = Nil
    )
    val dir = Files.createTempDirectory("jam-sync-sizing")
    var node: JamNode = null
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false)).start()
      test(node, spec)
    finally
      if node != null then node.shutdown()
      cleanup(dir)

  test("a block request asks for one block when the parent is held, and otherwise for the gap to our finalized slot") {
    withNode { (node, spec) =>
      val peer = new Peer(spec.alpnPrefix)
      try
        val fin = SyncCodec.HashSlot(node.chain.finalized.hash, node.chain.finalized.slot)
        peer.connect(node.listenPort, fin)
        val unknownParent = Hashing.blake2b256(Array[Byte](1, 2, 3))

        // Our finalized block is the parent: only the announced block is missing.
        val child = header(fin.hash, fin.slot + 1, tag = 1)
        peer.announce(child, fin)
        eventually("the request for the child")(peer.asked.size == 1)
        peer.asked.peek() shouldBe Asked(Hashing.blake2b256(child), 1)
        peer.refuseAll()

        // Unknown parent five slots above our finalized block: at most five blocks can be missing.
        val near = header(unknownParent, fin.slot + 5, tag = 2)
        peer.announce(near, fin)
        eventually("the request for the near block")(peer.asked.size == 2)
        peer.asked.toArray.toList.last shouldBe Asked(Hashing.blake2b256(near), 5)
        peer.refuseAll()

        // Far ahead: capped at the request limit.
        val far = header(unknownParent, fin.slot + 100000, tag = 3)
        peer.announce(far, fin)
        eventually("the request for the far block")(peer.asked.size == 3)
        peer.asked.toArray.toList.last shouldBe Asked(Hashing.blake2b256(far), SyncService.MaxBlocksPerRequest.toLong)
        peer.refuseAll()

        // At our finalized slot and unknown: it can never be on our chain, so nothing is asked.
        peer.announce(header(unknownParent, fin.slot, tag = 4), fin)
        Thread.sleep(1000)
        peer.asked.size shouldBe 3
      finally peer.node.shutdown()
    }
  }

  test("a block announced by three peers is fetched from one at a time; the next is asked only if that fails") {
    withNode { (node, spec) =>
      val peers = List.fill(3)(new Peer(spec.alpnPrefix))
      try
        val fin = SyncCodec.HashSlot(node.chain.finalized.hash, node.chain.finalized.slot)
        peers.foreach(_.connect(node.listenPort, fin))
        val block = header(Hashing.blake2b256(Array[Byte](9)), fin.slot + 3, tag = 7)
        val hash = Hashing.blake2b256(block)
        peers.foreach(_.announce(block, fin))

        def total: Int = peers.map(_.asked.size).sum
        eventually("the first request")(total == 1)
        Thread.sleep(1000) // the other two announcements are in by now
        total shouldBe 1
        node.sync.pendingFetchCount shouldBe 0

        // The peer asked refuses: the block is still missing, so the next peer that announced it is asked.
        peers.foreach(_.refuseAll())
        eventually("the second request")(total == 2)
        Thread.sleep(500)
        total shouldBe 2
        peers.foreach(_.refuseAll())
        eventually("the third request")(total == 3)
        peers.foreach(_.refuseAll())

        // All three failed: nothing is left waiting, and no peer was asked twice.
        eventually("the queue to drain")(node.sync.pendingFetchCount == 0)
        Thread.sleep(500)
        total shouldBe 3
        peers.foreach(p => p.asked.toArray.toList shouldBe List(Asked(hash, 3)))
      finally peers.foreach(_.node.shutdown())
    }
  }

  test("a shard lookup asks several peers at once and returns the first answer") {
    val dir = Files.createTempDirectory("jam-shard-first")
    val store = ShardStore.open(dir)
    val shards = new ShardService(store, ChainConfig.TINY)
    try
      // Eight peers, each taking 300 ms to answer; only the last has the shard. One at a time that is 2.4 s.
      val asked = new ConcurrentLinkedQueue[Int]()
      val started = System.nanoTime()
      val found = shards.firstFrom((1 to 8).toList, width = 4) { p =>
        asked.add(p)
        Thread.sleep(300)
        Option.when(p == 8)("shard")
      }
      val millis = (System.nanoTime() - started) / 1000000
      found shouldBe Some("shard")
      asked.size shouldBe 8
      millis should be < 1500L

      // The holder is in the first group: the peers behind it are never asked.
      asked.clear()
      shards.firstFrom((1 to 8).toList, width = 4)(p => { asked.add(p); Option.when(p == 2)("shard") }) shouldBe
        Some("shard")
      asked.toArray.toList.map(_.asInstanceOf[Int]).max should be <= 4

      // Nobody has it, and a peer that throws counts as not having it.
      shards.firstFrom((1 to 5).toList, width = 2)(p => if p == 3 then throw new RuntimeException("x") else None) shouldBe
        None
      shards.firstFrom(List.empty[Int])(_ => Some(1)) shouldBe None
    finally
      shards.shutdown()
      store.close()
      cleanup(dir)
  }
