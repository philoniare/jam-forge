package io.forge.jam.node

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.network.{Loopback, PeerStreamHandler, StreamKind}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Block sync between two nodes joined in memory: CE 128 requests and UP 0 announcements with no QUIC endpoint. */
class SyncLoopbackSpec extends AnyFunSuite with Matchers with NodeFixture:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  private def eventually(what: String, seconds: Int = 20)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(20)
    if !cond then fail(s"timed out waiting for: $what")

  private def handlers(node: JamNode): Byte => Option[PeerStreamHandler] =
    Map(
      StreamKind.BlockAnnouncement -> node.sync.blockAnnouncementHandler,
      StreamKind.BlockRequest -> node.sync.blockRequestHandler
    ).get

  /** Node A with `blocks` blocks and node B at genesis, B having dialed A. */
  private def withNodes(blocks: Int)(body: (JamNode, JamNode, Loopback.Connection, Loopback.Connection) => Unit): Unit =
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val spec = ChainSpec(
      id = "sync-loopback",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dirs = List(tempDir("jam-sync-loop-a"), tempDir("jam-sync-loop-b"))
    val nodes = dirs.map(d => new JamNode(spec, NodeConfig(dataDir = d, slotTicking = false)))
    try
      nodes.foreach(_.chain.initializeOrRestore(spec))
      val List(a, b) = nodes
      author(a, 1 to blocks)
      val (bToA, aToB) =
        Loopback.pair(b.identity.publicKeyBytes, a.identity.publicKeyBytes, handlers(b), handlers(a))
      body(a, b, bToA, aToB)
    finally
      nodes.foreach { n =>
        n.sync.shutdown()
        n.shutdownStorageOnly()
      }
      dirs.foreach(cleanup)

  private def author(node: JamNode, slots: Range): Unit =
    val author = new BlockAuthor(node.chain, devKeys)
    slots.foreach { slot =>
      val block = author.tryAuthor(slot.toLong).getOrElse(fail(s"author block $slot"))
      node.chain.importBlock(node.chain.encodeBlock(block)).isRight shouldBe true
    }

  test("a block is fetched with the ancestors the asker lacks") {
    withNodes(blocks = 3) { (a, b, bToA, _) =>
      b.sync.fetchBlock(bToA, a.chain.best.hash)
      eventually("B reaches A's head")(b.chain.best.hash == a.chain.best.hash)
      b.chain.best.stateRoot shouldBe a.chain.best.stateRoot
    }
  }

  test("the UP 0 handshake brings a node to its peer's head, and an announcement to the next block") {
    withNodes(blocks = 2) { (a, b, bToA, aToB) =>
      b.sync.openAnnouncementStream(bToA)
      eventually("A has B's handshake")(a.sync.peerFinal(aToB).isDefined)
      eventually("B reaches A's head")(b.chain.best.hash == a.chain.best.hash)
      author(a, 3 to 3)
      a.sync.announce(a.chain.headerBytes(a.chain.best.hash).getOrElse(fail("header of A's head")))
      eventually("B imports the announced block")(b.chain.best.hash == a.chain.best.hash)
      b.chain.best.slot shouldBe 3L
    }
  }
