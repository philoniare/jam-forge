package io.forge.jam.node

import io.forge.jam.network.{Loopback, PeerStreamHandler, StreamKind}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GrandpaKeepAliveSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  test("in a stalled round the voter sends its state every few gossip periods, though its vote resends back off") {
    withNodes(2, "grandpa-keepalive") { nodes =>
      val Seq(a, b) = nodes.toSeq
      val states = new java.util.concurrent.ConcurrentLinkedQueue[java.lang.Long]()
      val votes = new java.util.concurrent.ConcurrentLinkedQueue[java.lang.Long]()
      val listening: Byte => Option[PeerStreamHandler] = {
        case StreamKind.GrandpaState =>
          Some((_, stream) => stream.onMessage { _ => states.add(System.currentTimeMillis()); stream.finish() })
        case StreamKind.GrandpaVote =>
          Some((_, stream) => stream.onMessage(_ => votes.add(System.currentTimeMillis()): Unit))
        case _ => None
      }
      val (aToB, _) = Loopback.pair(a.identity.publicKeyBytes, b.identity.publicKeyBytes, _ => None, listening)
      a.distribution.trackConnection(aToB)
      val gossip = 100L
      val g = new GrandpaService(a.chain, a.sync, a.distribution, Some(devKeys(0)), gossipMillis = gossip)
      try
        val voters = a.chain.stateView().validators.current.map(_.ed25519.bytes.toArray).toIndexedSeq
        val started = System.currentTimeMillis()
        g.start(a.chain.genesisHash.get, voters, voters)
        Thread.sleep(45 * gossip)
        val at = states.toArray.toList.map(_.asInstanceOf[java.lang.Long].longValue).sorted
        at.size should be >= 6
        val gaps = (started :: at).zip(at).map((earlier, later) => later - earlier)
        withClue(s"gaps between state messages: $gaps ") {
          gaps.max should be < 15 * gossip
        }
        votes.size should be >= 1
      finally g.shutdown()
    }
  }
