package io.forge.jam.node

import java.net.InetSocketAddress
import java.util.concurrent.{Executors, ExecutorService, LinkedBlockingQueue, TimeUnit}

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.{CoreIndex, Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.work.{ExecutionResult, PackageSpec}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.workresult.{RefineLoad, WorkResult}
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
