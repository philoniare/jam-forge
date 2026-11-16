package io.forge.jam.node

import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import io.forge.jam.network.{Loopback, PeerStreamHandler, StreamKind}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Safrole ticket distribution between nodes joined in memory: CE 131 to the proxy, CE 132 from it. */
class TicketServiceSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private def handlers(node: JamNode): Byte => Option[PeerStreamHandler] =
    Map(
      StreamKind.TicketDistributionStep1 -> node.tickets.ticketHandler,
      StreamKind.TicketDistributionStep2 -> node.tickets.ticketHandler
    ).get

  /** TINY: three attempts per validator. */
  private val perValidator = 3

  private def generate(node: JamNode): Unit =
    node.tickets.setValidatorKeys(Seq(devKeys(0)))
    node.tickets.maybeGenerate(1L) shouldBe true
    node.pools.ticketCount shouldBe perValidator

  /** The CE 132 messages of `node`'s own tickets, taken off the wire of a peer that only records. */
  private def ticketMessages(node: JamNode): List[Array[Byte]] =
    val seen = new ConcurrentLinkedQueue[Array[Byte]]()
    val recorder: PeerStreamHandler = (_, stream) => stream.onMessage(m => seen.add(m): Unit)
    val (toRecorder, _) = Loopback.pair(
      node.identity.publicKeyBytes,
      devKeys(5).ed25519Public,
      _ => None,
      kind => Option.when(kind == StreamKind.TicketDistributionStep2)(recorder)
    )
    node.tickets.distributeTickets(List(toRecorder), _ => None)
    // The tickets it is its own proxy for are not sent here: they wait for its CE 132 schedule.
    val sent = perValidator - node.tickets.pendingForwardCount
    eventually("the tickets to be sent")(seen.size == sent)
    seen.asScala.toList

  /** Make `node` the proxy of every ticket that validator 0 is not its own proxy for. */
  private def asProxy(node: JamNode): Unit = node.tickets.setValidatorKeys(devKeys.drop(1))

  test("a validator's tickets reach their proxy on CE 131, which pools them and forwards them on CE 132") {
    withNodes(3, "tickets-flow") { nodes =>
      val Seq(a, b, c) = nodes: @unchecked
      generate(a)
      asProxy(b)
      val (aToB, _) = join(a, b, handlers)
      join(b, c, handlers)
      // B stands in for every proxy. A ticket whose proxy is A's own validator is not sent on CE 131: A forwards it.
      a.tickets.distributeTickets(List(aToB), _ => Some(aToB))
      val ownProxy = a.tickets.pendingForwardCount
      a.tickets.flushForwards()
      eventually("B to pool A's tickets")(b.pools.ticketCount == perValidator)
      eventually("B to queue the tickets it is proxy for")(b.tickets.pendingForwardCount == perValidator - ownProxy)
      c.pools.ticketCount shouldBe 0 // nothing is forwarded before its time

      b.tickets.flushForwards()
      eventually("C to pool the tickets B forwarded")(c.pools.ticketCount == perValidator - ownProxy)
      c.tickets.pendingForwardCount shouldBe 0 // a CE 132 ticket is not forwarded again
    }
  }

  test("a ticket is refused when its proof does not verify or its epoch is not the head's or the next") {
    withNodes(2, "tickets-refused") { nodes =>
      val Seq(a, b) = nodes: @unchecked
      generate(a)
      val genuine = ticketMessages(a).headOption.getOrElse(fail("no ticket was sent"))
      val (epoch, envelope) = TicketCodec.decode(genuine).fold(e => fail(e), identity)
      epoch shouldBe 0L

      val badProof = genuine.clone()
      badProof(genuine.length - 1) = (badProof(genuine.length - 1) ^ 1).toByte
      b.tickets.receive(badProof, asProxy = true, "test") shouldBe false
      b.tickets.receive(genuine.dropRight(1), asProxy = true, "test") shouldBe false
      b.tickets.receive(TicketCodec.encode(epoch + 2, envelope), asProxy = false, "test") shouldBe false
      // On CE 131 a ticket is taken by its proxy only, and B holds no validator key.
      b.tickets.receive(genuine, asProxy = true, "test") shouldBe false
      b.pools.ticketCount shouldBe 0
      b.tickets.pendingForwardCount shouldBe 0
      b.tickets.deferredCount shouldBe 0

      // The next epoch's cannot be checked yet: it is kept, not pooled.
      b.tickets.receive(TicketCodec.encode(epoch + 1, envelope), asProxy = false, "test") shouldBe true
      b.tickets.deferredCount shouldBe 1
      b.pools.ticketCount shouldBe 0
      // The same ticket on CE 132 is anyone's to pool.
      b.tickets.receive(genuine, asProxy = false, "test") shouldBe true
      b.pools.ticketCount shouldBe 1
      b.tickets.pendingForwardCount shouldBe 0
    }
  }

  test("a ticket sent to its proxy more than once is pooled and queued for forwarding once") {
    withNodes(2, "tickets-repeat") { nodes =>
      val Seq(a, b) = nodes: @unchecked
      generate(a)
      asProxy(b)
      val genuine = ticketMessages(a).headOption.getOrElse(fail("no ticket was sent"))
      (1 to 5).foreach(_ => b.tickets.receive(genuine, asProxy = true, "test"))
      b.pools.ticketCount shouldBe 1
      b.tickets.pendingForwardCount shouldBe 1
    }
  }
