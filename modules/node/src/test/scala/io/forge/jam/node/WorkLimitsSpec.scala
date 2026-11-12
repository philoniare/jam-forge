package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{
  CompletableFuture,
  ConcurrentLinkedQueue,
  CountDownLatch,
  Executors,
  RejectedExecutionException,
  TimeUnit
}
import java.util.concurrent.atomic.AtomicInteger

import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block
import io.forge.jam.network.{JamnpConfig, JamnpConnection, JamnpNode, JamnpStream, NodeIdentity, StreamKind}
import io.forge.jam.protocol.traces.TraceStep
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Queues a peer can fill stay bounded under a flood, and work that must not be lost still runs. */
class WorkLimitsSpec extends AnyFunSuite with Matchers:

  private def daemon(name: String): java.util.concurrent.ThreadFactory = r =>
    val t = new Thread(r, name); t.setDaemon(true); t

  private def eventually(what: String, seconds: Int = 20)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(10)
    if !cond then fail(s"timed out waiting for: $what")

  test("WorkGate: a flood of peer work is capped by count and bytes; the executor's own work all runs") {
    val exec = Executors.newSingleThreadScheduledExecutor(daemon("gate-spec"))
    try
      val gate = new WorkGate("spec", maxQueued = 64, maxBytes = 1000)
      val busy = new CountDownLatch(1)
      val started = new CountDownLatch(1)
      exec.execute { () => started.countDown(); busy.await() }
      started.await(5, TimeUnit.SECONDS) shouldBe true

      val ranPeer = new AtomicInteger()
      val ranOwn = new AtomicInteger()
      var admitted = 0
      var maxQueued = 0
      var maxBytes = 0L
      for i <- 1 to 100000 do
        // Every tenth submission is the service's own (a timer tick, an import callback): not gated.
        if i % 10 == 0 then exec.execute(() => { ranOwn.incrementAndGet(); () })
        if gate.tryExecute(exec, bytes = 10)(ranPeer.incrementAndGet()) then admitted += 1
        maxQueued = math.max(maxQueued, gate.queued)
        maxBytes = math.max(maxBytes, gate.queuedBytes)

      maxQueued should be <= 64
      maxBytes should be <= 1000L
      admitted shouldBe 64
      gate.dropped shouldBe (100000 - 64)

      busy.countDown()
      eventually("own and admitted work to run")(ranOwn.get() == 10000 && ranPeer.get() == 64)
      gate.queued shouldBe 0
      gate.queuedBytes shouldBe 0L
      // Room again once the queue has drained.
      gate.tryExecute(exec, bytes = 10)(ranPeer.incrementAndGet()) shouldBe true
    finally exec.shutdownNow()
  }

  test("WorkGate: one submission larger than the byte limit is admitted when nothing waits") {
    val exec = Executors.newSingleThreadExecutor(daemon("gate-spec"))
    try
      val gate = new WorkGate("spec", maxQueued = 4, maxBytes = 100)
      val busy = new CountDownLatch(1)
      exec.execute(() => busy.await())
      val ran = new AtomicInteger()
      gate.tryExecute(exec, bytes = 5000)(ran.incrementAndGet()) shouldBe true
      gate.tryExecute(exec, bytes = 1)(ran.incrementAndGet()) shouldBe false
      busy.countDown()
      eventually("the large task to run")(ran.get() == 1)
    finally exec.shutdownNow()
  }

  test("WorkGate: one source cannot fill the queue; another source is still admitted") {
    val exec = Executors.newSingleThreadExecutor(daemon("gate-spec"))
    try
      val gate = new WorkGate("spec", maxQueued = 64)
      gate.maxPerSource shouldBe 16
      val busy = new CountDownLatch(1)
      exec.execute(() => busy.await())
      val ran = new AtomicInteger()
      val flooder = new Object
      val honest = new Object
      (1 to 1000).count(_ => gate.tryExecute(exec, source = flooder)(ran.incrementAndGet())) shouldBe 16
      (1 to 4).count(_ => gate.tryExecute(exec, source = honest)(ran.incrementAndGet())) shouldBe 4
      busy.countDown()
      eventually("admitted work to run")(ran.get() == 20)
      // The flooder's share is free again once its work has run.
      gate.tryExecute(exec, source = flooder)(ran.incrementAndGet()) shouldBe true
    finally exec.shutdownNow()
  }

  test("egress executor: a flood keeps the queue at its capacity, drops the oldest and runs the newest") {
    val capacity = 128
    val egress = WorkLimits.dropOldestExecutor("egress-spec", threads = 1, capacity, daemon("egress-spec"))
    try
      val dist = new DistributionService(new ExtrinsicPools, coresCount = 1, egress)
      val firstOpened = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val opened = new ConcurrentLinkedQueue[Integer]()
      val total = 50000
      var maxQueued = 0
      for i <- 1 to total do
        dist.submitSend(
          () =>
            opened.add(i)
            firstOpened.countDown()
            // The first send holds the egress thread (a send does not wait there for its stream, so the open
            // itself is made to block); the rest fail at once.
            if i == 1 then release.await()
            CompletableFuture.failedFuture[JamnpStream](new RuntimeException("no peer")),
          StreamKind.AssuranceDistribution,
          Array[Byte](1)
        )
        if i == 1 then firstOpened.await(5, TimeUnit.SECONDS) shouldBe true
        maxQueued = math.max(maxQueued, egress.queued)

      maxQueued should be <= capacity
      egress.dropped shouldBe (total - 1 - capacity).toLong

      release.countDown()
      eventually("the kept sends to run")(opened.size == capacity + 1)
      // What ran: the send in flight and the newest `capacity` ones, in order.
      opened.toArray.toList shouldBe (1 +: ((total - capacity + 1) to total)).toList
    finally egress.shutdownNow()

    a[RejectedExecutionException] should be thrownBy egress.execute(() => ())
  }

  // --- sync fetch queue, end to end ------------------------------------------------------------------------------

  private given io.circe.Decoder[TraceStep] = TraceStep.decoder(using summon[io.circe.Decoder[Block]])
  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val tracesDir = baseDir.resolve("jam-conformance/fuzz-reports/0.8.0/traces")

  private def anyStep(): Option[TraceStep] =
    if !Files.isDirectory(tracesDir) then None
    else
      val dirs = Files.list(tracesDir).sorted().toArray.map(_.asInstanceOf[Path]).filter(Files.isDirectory(_))
      dirs.iterator
        .flatMap(d => Files.list(d).sorted().toArray.map(_.asInstanceOf[Path]).iterator.filter(_.toString.endsWith(".json")).take(1))
        .take(10)
        .map(p => decode[TraceStep](Files.readString(p)))
        .collectFirst { case Right(step) => step }

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  test("sync: a peer announcing thousands of unknown blocks leaves a handful of fetches queued, the newest among them") {
    val step = anyStep().getOrElse(cancel("no fuzz trace available (jam-conformance corpus not present)"))
    val spec = ChainSpec(
      id = "work-limits",
      config = ChainConfig.TINY,
      genesisHeaderBytes = None,
      explicitGenesisHash = Some(step.block.header.parent),
      genesisState = step.preState.keyvals,
      bootnodes = Nil
    )
    val dir = Files.createTempDirectory("jam-work-limits")
    var node: JamNode = null
    val peer = new JamnpNode(NodeIdentity.generate(), JamnpConfig(genesisHashPrefix = spec.alpnPrefix))
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false)).start()

      // The peer answers no block request until told to, so the node's import thread is held on the first fetch.
      val requested = new ConcurrentLinkedQueue[Hash]()
      val answer = new CountDownLatch(1)
      val held = new ConcurrentLinkedQueue[JamnpStream]()
      peer.registerHandler(
        StreamKind.BlockRequest,
        (_: JamnpConnection, stream: JamnpStream) =>
          stream.onMessage { msg =>
            requested.add(Hash(java.util.Arrays.copyOfRange(msg, 0, 32)))
            if answer.getCount == 0 then stream.reset() else held.add(stream)
            ()
          }
          ()
      )
      val conn = peer.connect(new InetSocketAddress("127.0.0.1", node.listenPort)).get(10, TimeUnit.SECONDS)
      val up = conn.openStream(StreamKind.BlockAnnouncement).get(10, TimeUnit.SECONDS)
      up.onMessage(_ => ())
      val genesis = SyncCodec.HashSlot(node.chain.finalized.hash, node.chain.finalized.slot)
      up.send(SyncCodec.encodeHandshake(genesis, Nil))

      val announcements = 5000
      def header(i: Int): Array[Byte] =
        val h = new Array[Byte](120)
        java.nio.ByteBuffer.wrap(h).putInt(i)
        h(96) = 1 // slot, little-endian
        h
      for i <- 1 to announcements do up.send(SyncCodec.encodeAnnouncement(header(i), genesis))

      eventually("the first fetch to reach the peer")(requested.size == 1)
      // All announcements are in once the last one's block is queued (they are handled in order).
      Thread.sleep(1500)
      node.sync.pendingFetchCount should be <= SyncService.MaxPendingFetchesPerPeer

      // Let every fetch fail fast; the queue drains and the node asked for only a handful of blocks.
      answer.countDown()
      held.forEach(_.reset())
      eventually("the fetch queue to drain", 60)(node.sync.pendingFetchCount == 0 && requested.size > 1)
      Thread.sleep(500)
      requested.size should be <= 1 + SyncService.MaxPendingFetchesPerPeer
      requested.toArray.toList.last shouldBe Hashing.blake2b256(header(announcements))
    finally
      peer.shutdown()
      if node != null then node.shutdown()
      cleanup(dir)
  }
