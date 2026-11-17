package io.forge.jam.node

import java.util.concurrent.{Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.{CoreIndex, Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.work.{ExecutionResult, PackageSpec}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.workresult.{RefineLoad, WorkResult}
import io.forge.jam.network.{Loopback, MessageStream, NodeIdentity, PeerConnection, PeerStreamHandler, StreamKind}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

class CustodyFetchSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private def silent(opened: AtomicInteger = new AtomicInteger): PeerStreamHandler =
    (_: PeerConnection, stream: MessageStream) =>
      opened.incrementAndGet()
      stream.onMessage(_ => ())

  private def serving(bytes: Array[Byte]): PeerStreamHandler =
    (_: PeerConnection, stream: MessageStream) =>
      stream.onMessage { _ =>
        stream.send(bytes)
        stream.finish()
      }

  private def peer(key: Array[Byte], handlers: Byte => Option[PeerStreamHandler]): PeerConnection =
    Loopback.pair(NodeIdentity.generate().publicKeyBytes, key, _ => None, handlers)._1

  private def report(erasureRoot: Hash, length: Int): WorkReport =
    WorkReport(
      packageSpec = PackageSpec(
        hash = Hashing.blake2b256(Array[Byte](3)),
        length = UInt(length),
        erasureRoot = erasureRoot,
        erasureShards = UShort(0),
        exportsRoot = Hash.zero,
        exportsCount = UShort(0)
      ),
      context = Context(
        anchor = Hash.zero,
        anchorSlot = Timeslot(0),
        stateRoot = Hash.zero,
        beefyRoot = Hash.zero,
        lookupAnchor = Hash.zero,
        lookupAnchorSlot = Timeslot(0),
        lookupAnchorStateRoot = Hash.zero,
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

  test("a pending report's custody is looked up once while a lookup is under way, and again only RetryMillis after it failed") {
    withNodes(1, "custody-lookups") { nodes =>
      val node = nodes.head
      val retryMillis = 1500L
      val assurer = new AssurerService(
        node.chain,
        node.distribution,
        node.pools,
        devKeys.take(1),
        node.shards,
        node.shardStore,
        retryMillis = retryMillis
      )
      try
        val opened = new AtomicInteger()
        val shardPeer = peer(devKeys(1).ed25519Public, kind => Option.when(kind == StreamKind.ShardDistribution)(silent(opened)))
        val root = Hashing.blake2b256("pending report".getBytes)
        val missing = Map(root -> Seq(3))

        val first = assurer.lookUp(missing, List(shardPeer))
        (2 to 10).foreach { _ =>
          assurer.lookUp(missing, List(shardPeer))
          assurer.lookupsQueuedOrRunning should be <= missing.size
        }
        Thread.sleep(300)
        opened.get shouldBe 1

        first.foreach(_.get(AssurerService.CustodyTimeoutMillis + 5000, TimeUnit.MILLISECONDS))
        assurer.lookUp(missing, List(shardPeer)).foreach(_.get(10, TimeUnit.SECONDS))
        opened.get shouldBe 1

        Thread.sleep(retryMillis)
        val retry = assurer.lookUp(missing, List(shardPeer))
        eventually("the retry")(opened.get == 2)
        retry.foreach(_.get(AssurerService.CustodyTimeoutMillis + 5000, TimeUnit.MILLISECONDS))

        assurer.lookUp(Map.empty, List(shardPeer))
        assurer.lookUp(missing, List(shardPeer))
        eventually("a lookup of a forgotten root")(opened.get == 3)
      finally assurer.shutdown()
    }
  }

  test("CE 147: a silent first peer leaves time for the others within the budget") {
    val bundle = Array.tabulate[Byte](3000)(i => (i * 11).toByte)
    val root = Hashing.blake2b256(bundle)
    val peers = List(
      peer(devKeys(1).ed25519Public, kind => Option.when(kind == StreamKind.BundleRequest)(silent())),
      peer(devKeys(2).ed25519Public, kind => Option.when(kind == StreamKind.BundleRequest)(serving(bundle))),
      peer(devKeys(3).ed25519Public, kind => Option.when(kind == StreamKind.BundleRequest)(silent()))
    )
    val budget = 3000L
    val started = System.currentTimeMillis()
    val fetched = AuditorService.fetchBundleDirect(root, peers, totalBudgetMs = budget, maxPeers = 3)
    fetched.exists(java.util.Arrays.equals(_, bundle)) shouldBe true
    (System.currentTimeMillis() - started) should be < budget
  }

  test("a bundle reconstruction against silent peers ends at its deadline as too few shards, and the audit is postponed") {
    withNodes(1, "reconstruct-deadline") { nodes =>
      val node = nodes.head
      val deadline = 1000L
      val shards = new ShardService(node.shardStore, node.chain.config, reconstructMillis = deadline)
      val egress = Executors.newSingleThreadExecutor()
      val peers = (1 to 2).map(v => peer(devKeys(v).ed25519Public, kind => Option.when(kind == StreamKind.AuditShardRequest)(silent())))
      peers.foreach(node.distribution.trackConnection)
      val root = Hashing.blake2b256("bundle nobody serves".getBytes)
      val auditor = new AuditorService(node.chain, node.distribution, shards, node.pools, devKeys.take(1), egress)
      try
        val started = System.currentTimeMillis()
        val rebuilt = shards.reconstructBundle(root, 1000, peers, _ => Seq(1))
        rebuilt.left.toOption.exists(_.startsWith(ShardService.TooFewShards)) shouldBe true
        (System.currentTimeMillis() - started) should be < deadline + 3000

        val auditStarted = System.currentTimeMillis()
        auditor.auditOutcome(report(root, 1000)) shouldBe None
        (System.currentTimeMillis() - auditStarted) should be < deadline + 3000
      finally
        auditor.shutdown()
        shards.shutdown()
        egress.shutdownNow()
        ()
    }
  }
