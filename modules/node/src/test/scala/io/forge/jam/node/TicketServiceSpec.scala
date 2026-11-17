package io.forge.jam.node

import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import io.forge.jam.core.primitives.Hash
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

      b.tickets.receive(TicketCodec.encode(epoch + 1, envelope), asProxy = false, "test") shouldBe false
      b.tickets.deferredCount shouldBe 0
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

  private val perPeerBound = 2 * 4

  private def nextEpochTickets(a: JamNode, b: JamNode): (List[Array[Byte]], Array[Byte]) =
    author(Seq(a, b), 1 to 11)
    author(Seq(a), 12 to 12)
    val first = a.chain.encodeBlock(a.chain.block(a.chain.best.hash).getOrElse(fail("slot 12 block")))
    a.tickets.setValidatorKeys(devKeys)
    a.tickets.maybeGenerate(12L) shouldBe true
    val view = a.chain.stateView()
    val envs = a.pools.takeTickets(1L, TicketService.contestEntropy(view), Set.empty, Int.MaxValue, Int.MaxValue)
    envs.size shouldBe devKeys.size * perValidator
    (envs.map((_, env) => TicketCodec.encode(1L, env)), first)

  test("next-epoch tickets are kept up to a bound per sending validator; a flood from one keeps no other's out") {
    withNodes(2, "tickets-deferred-flood") { nodes =>
      val Seq(a, b) = nodes: @unchecked
      val (msgs, _) = nextEpochTickets(a, b)
      b.tickets.setValidatorKeys(devKeys) // the proxy of every ticket
      val one = Some(devKeys(1).ed25519Key)
      msgs.count(m => b.tickets.receive(m, asProxy = true, "one", one)) shouldBe perPeerBound
      b.tickets.deferredCount shouldBe perPeerBound
      msgs.take(perPeerBound).foreach(m => b.tickets.receive(m, asProxy = true, "one", one) shouldBe true)
      b.tickets.deferredCount shouldBe perPeerBound
      val two = Some(devKeys(2).ed25519Key)
      msgs.slice(perPeerBound, 2 * perPeerBound).foreach(m => b.tickets.receive(m, asProxy = true, "two", two) shouldBe true)
      b.tickets.deferredCount shouldBe 2 * perPeerBound
      b.tickets.receive(msgs.last, asProxy = true, "stranger", Some(ValidatorKeySet.dev(17).ed25519Key)) shouldBe false
      val bad = msgs.last.clone()
      bad(bad.length - 1) = (bad(bad.length - 1) ^ 1).toByte
      b.tickets.receive(bad, asProxy = false, "three", Some(devKeys(3).ed25519Key)) shouldBe false
      b.tickets.deferredCount shouldBe 2 * perPeerBound
      b.pools.ticketCount shouldBe 0
    }
  }

  test("deferred tickets are pooled and queued for forwarding once the epoch's first block is imported") {
    withNodes(2, "tickets-deferred-taken") { nodes =>
      val Seq(a, b) = nodes: @unchecked
      val (msgs, first) = nextEpochTickets(a, b)
      b.tickets.setValidatorKeys(devKeys)
      msgs.zipWithIndex.foreach((m, i) =>
        b.tickets.receive(m, asProxy = true, s"v$i", Some(devKeys(1 + i % 5).ed25519Key)) shouldBe true
      )
      b.tickets.deferredCount shouldBe msgs.size
      b.tickets.processDeferred() // still in epoch 0: nothing is due
      b.tickets.deferredCount shouldBe msgs.size
      b.chain.importBlock(first).isRight shouldBe true
      b.tickets.processDeferred()
      eventually("the deferred tickets to be pooled")(b.pools.ticketCount == msgs.size)
      b.tickets.deferredCount shouldBe 0
      b.tickets.pendingForwardCount shouldBe msgs.size
    }
  }

  test("the import listener hands deferred tickets to a worker and returns at once, however slow their checks") {
    withNodes(2, "tickets-deferred-slow") { nodes =>
      val Seq(a, b) = nodes: @unchecked
      val (msgs, first) = nextEpochTickets(a, b)
      val calls = new java.util.concurrent.atomic.AtomicInteger()
      val slow: TicketService.Verifier = (envs, _, _, _) =>
        if Thread.currentThread.getName.contains("deferred") then
          calls.incrementAndGet()
          Thread.sleep(1000)
        envs.map(e => io.forge.jam.crypto.BandersnatchVrf.ringVrfOutput(e.signature.toArray).map(Hash(_)))
      val threads = new NodeThreads
      val egress = java.util.concurrent.Executors.newSingleThreadExecutor()
      try
        val svc = new TicketService(
          b.chain,
          b.pools,
          b.distribution,
          egress,
          threads = threads,
          verifyTickets = slow,
          ringRootOf = _ => Some(io.forge.jam.core.JamBytes(new Array[Byte](144)))
        )
        svc.setValidatorKeys(devKeys)
        msgs.zipWithIndex.foreach((m, i) => svc.receive(m, asProxy = false, s"v$i", Some(devKeys(1 + i % 5).ed25519Key)) shouldBe true)
        svc.deferredCount shouldBe msgs.size
        b.chain.importBlock(first).isRight shouldBe true
        val t0 = System.nanoTime()
        svc.processDeferred()
        (System.nanoTime() - t0) / 1_000_000 should be < 500L
        eventually("the deferred tickets to be checked and pooled")(b.pools.ticketCount == msgs.size)
        calls.get shouldBe 1 // one batch
        svc.deferredCount shouldBe 0
      finally
        threads.close()
        egress.shutdownNow()
    }
  }
