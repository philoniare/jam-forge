package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path}
import java.util.concurrent.{Executors, ExecutorService, LinkedBlockingQueue, TimeUnit}

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{CoreIndex, Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.work.{ExecutionResult, PackageSpec}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.workresult.{RefineLoad, WorkResult}
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend}
import io.forge.jam.network.{JamnpConfig, JamnpNode, NodeIdentity, StreamKind}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scodec.Codec
import scodec.bits.BitVector
import spire.math.{UInt, UShort}

class BundleServingSpec extends AnyFunSuite with Matchers:

  private val config = JamnpConfig(genesisHashPrefix = "deadbeef")

  private def await[A](q: LinkedBlockingQueue[A], seconds: Int = 10): A =
    val v = q.poll(seconds.toLong, TimeUnit.SECONDS)
    if v == null then fail("timed out waiting for message")
    v

  private def sampleReport(tag: Byte): WorkReport =
    WorkReport(
      packageSpec = PackageSpec(
        hash = Hashing.blake2b256(Array.fill[Byte](4)(tag)),
        length = UInt(0),
        erasureRoot = Hash.zero,
        exportsRoot = Hash.zero,
        exportsCount = UShort(0)
      ),
      context = Context(
        anchor = Hash.zero,
        stateRoot = Hash.zero,
        beefyRoot = Hash.zero,
        lookupAnchor = Hash.zero,
        lookupAnchorSlot = Timeslot(0),
        prerequisites = List.empty
      ),
      coreIndex = CoreIndex(0),
      authorizerHash = Hash.zero,
      authGasUsed = Gas(0),
      authOutput = JamBytes.empty,
      segmentRootLookup = List.empty,
      results = List(
        WorkResult(
          serviceId = ServiceId(0),
          codeHash = Hash.zero,
          payloadHash = Hash.zero,
          accumulateGas = Gas(0),
          result = ExecutionResult.Ok(JamBytes.empty),
          refineLoad = RefineLoad(Gas(0), UShort(0), UShort(0), UInt(0), UShort(0))
        )
      )
    )

  private def newEgress(): ExecutorService =
    Executors.newSingleThreadExecutor(r =>
      val t = new Thread(r, "jam-egress-test"); t.setDaemon(true); t
    )

  /** A `GuarantorService` backed by a scratch (no-genesis) `ChainManager`.
    * CE 147's held-bundle cache and handler (and CE 148's segment pool)
    * never touch chain state, so an un-imported chain is fine here —
    * mirrors how CE 136's rig above skips `JamNode`/genesis entirely.
    * Caller must delete `dir` when done. The `SegmentPool` is returned
    * alongside so CE 148 tests can `put` fixtures directly, mirroring how
    * `JamNode` constructs and injects it.
    */
  private def newGuarantor(
      maxSubmissionBytes: Long = GuarantorService.DefaultMaxSubmissionBytes
  ): (GuarantorService, SegmentPool, Path) =
    val dir = Files.createTempDirectory("jam-bundle-serving-spec")
    val trieBackend = RocksDbTrieBackend.open(dir.resolve("trie"))
    val blockStore = BlockStore.open(dir.resolve("blocks"))
    val chain = new ChainManager(ChainConfig.TINY, trieBackend, blockStore)
    val distribution = new DistributionService(new ExtrinsicPools, coresCount = 1, newEgress())
    val segments = new SegmentPool()
    val guarantor = new GuarantorService(
      chain,
      distribution,
      new ExtrinsicPools,
      Seq.empty,
      segments = segments,
      maxSubmissionBytes = maxSubmissionBytes
    )
    (guarantor, segments, dir)

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  // ===========================================================================
  // CE 136 — work-report request
  // ===========================================================================

  test("CE136: a recorded report is served back by its hash, decoding to the same value") {
    val egress = newEgress()
    val distribution = new DistributionService(new ExtrinsicPools, coresCount = 1, egress)
    val report = sampleReport(0x01)
    distribution.recordReport(report)
    val hash = Hashing.blake2b256(report.encode.toArray)

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.WorkReportRequest, distribution.reportRequestHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.WorkReportRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send(hash.bytes)
      stream.finish()

      val respBytes = await(responses)
      val decoded = summon[Codec[WorkReport]].decode(BitVector(respBytes)).require.value
      decoded shouldBe report
      await(closed) shouldBe true
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      egress.shutdownNow()
  }

  test("CE136: an unknown hash produces FIN with no response message") {
    val egress = newEgress()
    val distribution = new DistributionService(new ExtrinsicPools, coresCount = 1, egress)
    distribution.recordReport(sampleReport(0x02)) // populate the cache with something else

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.WorkReportRequest, distribution.reportRequestHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.WorkReportRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send(Hash.zero.bytes) // never recorded
      stream.finish()

      await(closed) shouldBe true
      responses.poll(500, TimeUnit.MILLISECONDS) shouldBe null
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      egress.shutdownNow()
  }

  test("CE136: a non-32-byte request does not crash the handler and still gets FIN") {
    val egress = newEgress()
    val distribution = new DistributionService(new ExtrinsicPools, coresCount = 1, egress)

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.WorkReportRequest, distribution.reportRequestHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.WorkReportRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send(Array[Byte](1, 2, 3)) // malformed: not a 32-byte hash
      stream.finish()

      await(closed) shouldBe true
      responses.poll(500, TimeUnit.MILLISECONDS) shouldBe null
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      egress.shutdownNow()
  }

  // ===========================================================================
  // CE 147 — bundle request
  // ===========================================================================

  test("CE147: a recorded bundle is served back by its erasure root, byte-identical") {
    val (guarantor, _, dir) = newGuarantor()
    val root = Hashing.blake2b256(Array.fill[Byte](4)(0x03))
    val bundleBytes = Array.tabulate[Byte](5000)(i => i.toByte) // larger than one shard
    guarantor.recordBundle(root, bundleBytes)

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.BundleRequest, guarantor.bundleRequestHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.BundleRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send(root.bytes.toArray)
      stream.finish()

      val respBytes = await(responses)
      respBytes shouldBe bundleBytes
      await(closed) shouldBe true
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      cleanup(dir)
  }

  test("CE147: an unknown erasure root produces FIN with no response message") {
    val (guarantor, _, dir) = newGuarantor()
    guarantor.recordBundle(Hashing.blake2b256(Array.fill[Byte](4)(0x04)), Array[Byte](1, 2, 3))

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.BundleRequest, guarantor.bundleRequestHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.BundleRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send(Hash.zero.bytes.toArray) // never recorded
      stream.finish()

      await(closed) shouldBe true
      responses.poll(500, TimeUnit.MILLISECONDS) shouldBe null
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      cleanup(dir)
  }

  test("CE147: a non-32-byte request does not crash the handler and still gets FIN") {
    val (guarantor, _, dir) = newGuarantor()

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.BundleRequest, guarantor.bundleRequestHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.BundleRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send(Array[Byte](1, 2, 3)) // malformed: not a 32-byte root
      stream.finish()

      await(closed) shouldBe true
      responses.poll(500, TimeUnit.MILLISECONDS) shouldBe null
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      cleanup(dir)
  }

  // ===========================================================================
  // CE 148 — segment request
  // ===========================================================================

  /** One request-encoding group: `segmentsRoot(32) ++ compactLen ++ [u16 LE index]`. */
  private def encodeSegmentRequest(groups: (Hash, Seq[Int])*): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream()
    groups.foreach { case (root, indices) =>
      out.write(root.bytes.toArray)
      out.write(io.forge.jam.core.scodec.JamCodecs.encodeCompactInteger(indices.length.toLong))
      indices.foreach(i => out.write(io.forge.jam.core.LittleEndian.encode(i.toLong, 2)))
    }
    out.toByteArray

  /** `count` fake but distinguishable 4104-byte (`Csegmentsize`) segments. */
  private def fakeSegments(count: Int): IndexedSeq[Array[Byte]] =
    (0 until count).map(i => Array.fill[Byte](4104)(i.toByte)).toIndexedSeq

  test("CE148: requested segments are served concatenated, followed by an empty proof list") {
    val (guarantor, pool, dir) = newGuarantor()
    val root = Hashing.blake2b256(Array.fill[Byte](4)(0x05))
    val segs = fakeSegments(3)
    pool.put(root, segs)

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.SegmentRequest, guarantor.segmentRequestHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.SegmentRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send(encodeSegmentRequest(root -> Seq(0, 2)))
      stream.finish()

      val msg1 = await(responses)
      msg1 shouldBe (segs(0) ++ segs(2))
      msg1.length shouldBe 8208

      val msg2 = await(responses)
      msg2 shouldBe io.forge.jam.core.scodec.JamCodecs.encodeCompactInteger(0)

      await(closed) shouldBe true
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      cleanup(dir)
  }

  test("CE148: an unknown segments-root produces FIN with no response message") {
    val (guarantor, pool, dir) = newGuarantor()
    pool.put(Hashing.blake2b256(Array.fill[Byte](4)(0x06)), fakeSegments(1))

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.SegmentRequest, guarantor.segmentRequestHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.SegmentRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send(encodeSegmentRequest(Hash.zero -> Seq(0))) // never recorded
      stream.finish()

      await(closed) shouldBe true
      responses.poll(500, TimeUnit.MILLISECONDS) shouldBe null
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      cleanup(dir)
  }

  test("CE148: an out-of-range index produces FIN with no response message") {
    val (guarantor, pool, dir) = newGuarantor()
    val root = Hashing.blake2b256(Array.fill[Byte](4)(0x07))
    pool.put(root, fakeSegments(3)) // valid indices are 0..2

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.SegmentRequest, guarantor.segmentRequestHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.SegmentRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send(encodeSegmentRequest(root -> Seq(5)))
      stream.finish()

      await(closed) shouldBe true
      responses.poll(500, TimeUnit.MILLISECONDS) shouldBe null
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      cleanup(dir)
  }

  test("CE148: a malformed request does not crash the handler and still gets FIN") {
    val (guarantor, _, dir) = newGuarantor()

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.SegmentRequest, guarantor.segmentRequestHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.SegmentRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send(Array[Byte](1, 2, 3)) // too short for even one full group
      stream.finish()

      await(closed) shouldBe true
      responses.poll(500, TimeUnit.MILLISECONDS) shouldBe null
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      cleanup(dir)
  }

  test("CE148: a two-group request concatenates segments from both roots in request order") {
    val (guarantor, pool, dir) = newGuarantor()
    val rootA = Hashing.blake2b256(Array.fill[Byte](4)(0x08))
    val rootB = Hashing.blake2b256(Array.fill[Byte](4)(0x09))
    val segsA = fakeSegments(3) // content bytes 0x00, 0x01, 0x02
    // Distinguish rootB's segments from rootA's so a wrong-order or wrong-root
    // concatenation would produce a different result than "A's picks then B's".
    val segsB = (0 until 2).map(i => Array.fill[Byte](4104)((0x10 + i).toByte)).toIndexedSeq
    pool.put(rootA, segsA)
    pool.put(rootB, segsB)

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.SegmentRequest, guarantor.segmentRequestHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.SegmentRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      // Group 1: rootA indices (1, 2). Group 2: rootB index (0).
      stream.send(encodeSegmentRequest(rootA -> Seq(1, 2), rootB -> Seq(0)))
      stream.finish()

      val msg1 = await(responses)
      msg1 shouldBe (segsA(1) ++ segsA(2) ++ segsB(0))
      msg1.length shouldBe 3 * 4104

      val msg2 = await(responses)
      msg2 shouldBe io.forge.jam.core.scodec.JamCodecs.encodeCompactInteger(0)

      await(closed) shouldBe true
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      cleanup(dir)
  }

  test("CE148: one unresolvable group among several aborts the whole request (no partial response)") {
    val (guarantor, pool, dir) = newGuarantor()
    val rootA = Hashing.blake2b256(Array.fill[Byte](4)(0x0a))
    val unknownRoot = Hashing.blake2b256(Array.fill[Byte](4)(0x0b)) // never pooled
    pool.put(rootA, fakeSegments(3))

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.SegmentRequest, guarantor.segmentRequestHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.SegmentRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send(encodeSegmentRequest(rootA -> Seq(0), unknownRoot -> Seq(0)))
      stream.finish()

      await(closed) shouldBe true
      responses.poll(500, TimeUnit.MILLISECONDS) shouldBe null
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      cleanup(dir)
  }

  // ===========================================================================
  // CE 146 — work-package bundle submission (accept from builders)
  // ===========================================================================
  test("CE146: a malformed msg1 (bad compact-length prefix) does not crash the handler and still gets FIN") {
    val (guarantor, _, dir) = newGuarantor()

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.WorkPackageBundleSubmission, guarantor.bundleSubmissionHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.WorkPackageBundleSubmission).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      // msg1: core index (2 bytes), then a compact-length prefix claiming
      // far more mapping entries than bytes actually follow.
      val badMsg1 = Array[Byte](0, 0, 0x7f.toByte) ++ Array.fill[Byte](5)(0) // len=0x7f, only 5 bytes follow
      stream.send(badMsg1)
      stream.send(Array.emptyByteArray) // msg2 (work package)
      stream.send(Array.emptyByteArray) // msg3 (extrinsics)
      stream.send(Array.emptyByteArray) // msg4 (segments)
      stream.send(Array.emptyByteArray) // msg5 (proofs)
      stream.finish()

      await(closed) shouldBe true
      responses.poll(500, TimeUnit.MILLISECONDS) shouldBe null
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      cleanup(dir)
  }

  test("CE146: a msg4 whose length isn't a multiple of Csegmentsize does not crash the handler and still gets FIN") {
    val (guarantor, _, dir) = newGuarantor()

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.WorkPackageBundleSubmission, guarantor.bundleSubmissionHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.WorkPackageBundleSubmission).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send(Array[Byte](0, 0, 0)) // msg1: core index 0, zero mappings
      stream.send(Array.emptyByteArray) // msg2 (work package)
      stream.send(Array.emptyByteArray) // msg3 (extrinsics)
      stream.send(Array.fill[Byte](100)(0)) // msg4: 100 bytes, not a multiple of 4104
      stream.send(Array.emptyByteArray) // msg5 (proofs)
      stream.finish()

      await(closed) shouldBe true
      responses.poll(500, TimeUnit.MILLISECONDS) shouldBe null
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      cleanup(dir)
  }

  test("CE146: cumulative submission bytes exceeding the cap aborts without waiting for the remaining messages") {
    val (guarantor, _, dir) = newGuarantor(maxSubmissionBytes = 100L)

    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    serverNode.registerHandler(StreamKind.WorkPackageBundleSubmission, guarantor.bundleSubmissionHandler)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.WorkPackageBundleSubmission).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))
      stream.send(Array.fill[Byte](200)(0))

      await(closed) shouldBe true
      responses.poll(500, TimeUnit.MILLISECONDS) shouldBe null
    finally
      clientNode.shutdown()
      serverNode.shutdown()
      cleanup(dir)
  }
