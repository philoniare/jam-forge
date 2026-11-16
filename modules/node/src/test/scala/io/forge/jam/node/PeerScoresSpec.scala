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
    val scores = new PeerScores(isValidator = _(0) == 7, onBan = _ => fail("a validator was banned"))
    (1 to 10).foreach(_ => scores.report(key(7), Offence.BadSignature))
    scores.banned(key(7)) shouldBe false
    scores.points(key(7)) should be > 400.0
    scores.report(key(8), Offence.Unanswered)
    scores.leastFaultedFirst(List(7, 8, 9, 10))(key) shouldBe List(9, 10, 8, 7)
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
      val (_, bToA) = join(a, b, _ => kind => Option.when(kind == StreamKind.BlockRequest)(serveBad))
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
