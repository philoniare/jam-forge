package io.forge.jam.node

import java.util.concurrent.{AbstractExecutorService, ConcurrentHashMap, Executors, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

import io.forge.jam.core.primitives.Ed25519PublicKey
import io.forge.jam.network.{Loopback, MessageStream, PeerConnection, PeerStreamHandler, StreamKind}
import io.forge.jam.node.GrandpaMessages.*

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** GRANDPA between nodes joined in memory: the six dev validators vote (CE 149), commit (CE 150) and serve
  * justifications (CE 130) with no QUIC endpoint. Five of the six make a supermajority.
  */
class GrandpaServiceSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private def handlers(services: Map[JamNode, GrandpaService])(node: JamNode): Byte => Option[PeerStreamHandler] =
    val g = services(node)
    Map(
      StreamKind.GrandpaVote -> g.voteHandler,
      StreamKind.GrandpaCommit -> g.commitHandler,
      StreamKind.GrandpaState -> g.stateHandler,
      StreamKind.GrandpaCatchUp -> g.catchUpHandler,
      StreamKind.GrandpaJustificationRequest -> g.justificationHandler,
      StreamKind.WarpSyncRequest -> g.warpSyncHandler,
      StreamKind.BlockAnnouncement -> node.sync.blockAnnouncementHandler,
      StreamKind.BlockRequest -> node.sync.blockRequestHandler
    ).get

  /** Every node holds blocks 1 to 3; node i votes as dev validator i when `votes(i)`, and observes otherwise. */
  private def network(count: Int, id: String, votes: Int => Boolean)(
      body: (IndexedSeq[JamNode], JamNode => Byte => Option[PeerStreamHandler]) => Unit
  ): Unit =
    withNodes(count, id) { nodes =>
      author(nodes, 1 to 3)
      val services = nodes.zipWithIndex.map((n, i) => n -> n.enableGrandpa(Option.when(votes(i))(devKeys(i)))).toMap
      body(nodes, handlers(services))
    }

  private def mesh(nodes: Seq[JamNode], handlers: JamNode => Byte => Option[PeerStreamHandler]): Unit =
    for i <- nodes.indices; j <- nodes.indices if i < j do join(nodes(i), nodes(j), handlers)

  test("six voters finalize the head, and an observer follows them by justification") {
    network(7, "grandpa-voters", votes = _ < 6) { (nodes, handlers) =>
      val voters = nodes.take(6)
      val observer = nodes(6)
      val head = voters.head.chain.best
      mesh(voters, handlers)
      eventually("every voter to finalize the head", seconds = 60)(voters.forall(_.chain.finalized.hash == head.hash))
      observer.chain.finalized.slot shouldBe 0L // joined to nobody yet

      // The observer learns of a peer's finality from UP 0, and asks that peer to prove it (CE 130).
      val (observerToVoter, _) = join(observer, voters.head, handlers)
      observer.sync.openAnnouncementStream(observerToVoter)
      eventually("the observer to finalize the head", seconds = 60)(observer.chain.finalized.hash == head.hash)
    }
  }

  test("four of six voters finalize nothing") {
    network(4, "grandpa-minority", votes = _ => true) { (nodes, handlers) =>
      mesh(nodes, handlers)
      Thread.sleep(8000) // several rounds
      nodes.foreach(_.chain.finalized.slot shouldBe 0L)
    }
  }

  private final class CountingEgress extends AbstractExecutorService:
    val queued = new AtomicInteger
    private val runner = Executors.newSingleThreadExecutor(r => { val t = new Thread(r, "egress-test"); t.setDaemon(true); t })
    def execute(r: Runnable): Unit = { queued.incrementAndGet(); runner.execute(r) }
    def shutdown(): Unit = runner.shutdown()
    def shutdownNow(): java.util.List[Runnable] = runner.shutdownNow()
    def isShutdown: Boolean = runner.isShutdown
    def isTerminated: Boolean = runner.isTerminated
    def awaitTermination(t: Long, u: TimeUnit): Boolean = runner.awaitTermination(t, u)

  test("a stalled round queues at most one entry per voter peer and a few forwards per tick, backing off, yet reaches every peer") {
    withNodes(1, "grandpa-stall") { nodes =>
      val node = nodes.head
      author(nodes, 1 to 3)
      val egress = new CountingEgress
      val distribution = new DistributionService(new ExtrinsicPools, coresCount = 1, egress)
      val received = new ConcurrentHashMap[Int, java.util.Set[Ed25519PublicKey]]()
      (1 to 5).foreach { i =>
        received.put(i, ConcurrentHashMap.newKeySet[Ed25519PublicKey]())
        val record: PeerStreamHandler = (_: PeerConnection, stream: MessageStream) =>
          stream.onMessage(msg => decodeVoteMessage(msg).foreach(m => received.get(i).add(m.message.signed.signerKey)))
        val (toPeer, _) = Loopback.pair(
          devKeys(0).ed25519Public,
          devKeys(i).ed25519Public,
          _ => None,
          kind => Option.when(kind == StreamKind.GrandpaVote)(record)
        )
        distribution.trackConnection(toPeer)
      }
      val voterPeers = 5
      val start = 1_000_000L
      val now = new AtomicLong(start)
      val g = new GrandpaService(node.chain, node.sync, distribution, Some(devKeys(0)), gossipMillis = 1000L, clock = () => now.get)
      try
        val validators = node.chain.stateView().validators.current.map(_.ed25519.bytes).toIndexedSeq
        val genesis = node.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
        g.start(genesis, validators, validators)
        g.resendNow()
        val best = node.chain.best
        val head = Vote(best.hash, best.stateRoot, best.slot)
        (1 to 3).foreach { i =>
          List(Prevote, Precommit).foreach { kind =>
            g.onVoterThread(_.onVote(VoteMessage(1L, 0L, SignedMessage(kind, sign(kind, head, 1L, 0L, devKeys(i)))))) shouldBe true
          }
        }
        def tick(): Int =
          val before = egress.queued.get
          val statesBefore = g.stateMessagesSent
          now.addAndGet(250)
          g.onVoterThread(_.step())
          g.resendNow()
          (egress.queued.get - before) - (g.stateMessagesSent - statesBefore).toInt
        while now.get - start < 5000 do tick()
        g.onVoterThread(_.ourVotes.size) shouldBe 2
        g.onVoterThread(_.roundVotes.size) shouldBe 8

        val stalls = mutable.ListBuffer.empty[Long]
        while now.get - start < 10 * 60 * 1000 do
          val entries = tick()
          entries should be <= voterPeers + GrandpaService.ForwardPeersPerStall
          if entries > 0 then stalls += now.get - start
        g.onVoterThread(_.round) shouldBe 1L
        stalls.take(7).toList shouldBe List(8000L, 12000L, 20000L, 36000L, 68000L, 132000L, 196000L)
        g.stateMessagesSent should be >= 100L * voterPeers
        g.stateMessagesSent should be <= 125L * voterPeers
        eventually("every peer to have been forwarded others' votes") {
          (1 to 5).forall(i => received.get(i).asScala.exists(k => k != devKeys(0).ed25519Key && k != devKeys(i).ed25519Key))
        }
      finally
        g.shutdown()
        egress.shutdownNow()
    }
  }

  test("a stalled voter sends a voter still in the previous round that round's votes, and nothing of its own round") {
    withNodes(1, "grandpa-behind") { nodes =>
      val node = nodes.head
      author(nodes, 1 to 3)
      val egress = Executors.newSingleThreadExecutor()
      val distribution = new DistributionService(new ExtrinsicPools, coresCount = 1, egress)
      val start = 1_000_000L
      val now = new AtomicLong(start)
      val g = new GrandpaService(node.chain, node.sync, distribution, Some(devKeys(0)), gossipMillis = 1000L, clock = () => now.get)
      val received = new ConcurrentHashMap[Int, java.util.List[Long]]()
      val toUs = (1 to 5).map { i =>
        received.put(i, java.util.Collections.synchronizedList(new java.util.ArrayList[Long]()))
        val record: PeerStreamHandler = (_: PeerConnection, stream: MessageStream) =>
          stream.onMessage(msg => decodeVoteMessage(msg).foreach(m => received.get(i).add(m.round): Unit))
        val (toPeer, fromPeer) = Loopback.pair(
          devKeys(0).ed25519Public,
          devKeys(i).ed25519Public,
          kind => Option.when(kind == StreamKind.GrandpaState)(g.stateHandler),
          kind => Option.when(kind == StreamKind.GrandpaVote)(record)
        )
        distribution.trackConnection(toPeer)
        i -> fromPeer
      }.toMap
      def tellRound(i: Int, round: Long): Unit =
        val stream = toUs(i).openStream(StreamKind.GrandpaState).get(5, TimeUnit.SECONDS)
        stream.send(encodeState(round, 0L, 0L))
        stream.finish()
      try
        val validators = node.chain.stateView().validators.current.map(_.ed25519.bytes).toIndexedSeq
        g.start(node.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get, validators, validators)
        val best = node.chain.best
        val head = Vote(best.hash, best.stateRoot, best.slot)
        def vote(round: Long, kind: Int, i: Int) =
          g.onVoterThread(_.onVote(VoteMessage(round, 0L, SignedMessage(kind, sign(kind, head, round, 0L, devKeys(i)))))) shouldBe true
        (1 to 5).foreach(i => List(Prevote, Precommit).foreach(vote(1L, _, i)))
        g.onVoterThread(_.round) shouldBe 2L
        (1 to 3).foreach(vote(2L, Prevote, _))
        tellRound(4, 1L)
        tellRound(5, 3L)
        g.resendNow() // round 2 starts its stall clock now
        eventually("our round 1 votes to go out")((1 to 5).forall(i => received.get(i).contains(1L)))
        Thread.sleep(200)
        (1 to 5).foreach(i => received.get(i).clear())

        now.addAndGet(9000) // past the first stall, 8 gossip periods in
        g.onVoterThread(_.step())
        g.resendNow()
        eventually("voter 4 to be sent round 1 again")(received.get(4).contains(1L))
        Thread.sleep(200)
        received.get(4).asScala.count(_ == 1L) shouldBe 12 // six prevotes, six precommits
        Seq(1, 2, 3, 5).foreach(i => received.get(i).asScala.count(_ == 1L) shouldBe 0)
        received.get(4).asScala.count(_ == 2L) shouldBe 1
        Seq(1, 2, 3).foreach(i => received.get(i).asScala.count(_ == 2L) should be >= 2)
      finally
        g.shutdown()
        egress.shutdownNow()
    }
  }

  test("a round stalls 8 gossip periods in, then after 4, 8, 16 ... periods up to 64, and anew with the next round") {
    val clock = new StallClock(1000L)
    def stallsWithin(from: Long, millis: Long): List[Long] =
      (from to from + millis by 250L).filter(clock.due).map(_ - from).toList
    clock.restart(0L)
    stallsWithin(0L, 300_000L) shouldBe List(8000L, 12000L, 20000L, 36000L, 68000L, 132000L, 196000L, 260000L)
    clock.restart(1_000_000L)
    stallsWithin(1_000_000L, 40_000L) shouldBe List(8000L, 12000L, 20000L, 36000L)
  }
