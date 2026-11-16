package io.forge.jam.node

import io.forge.jam.network.{PeerStreamHandler, StreamKind}
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
