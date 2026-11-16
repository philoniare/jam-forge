package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.Files

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Two nodes that each dialed the other keep one connection, the same one on both sides: the preferred
  * initiator's, unless the other side dialed long after it, which it does only when that connection never reached
  * it or has died on its side.
  */
class DuplicateConnectionSpec extends AnyFunSuite with Matchers with NodeFixture:

  private def eventually(what: String, seconds: Int = 10)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(20)
    if !cond then fail(s"timed out waiting for: $what")

  /** Two nodes, the first the preferred initiator of the pair. */
  private def withPair(f: (JamNode, JamNode) => Unit): Unit =
    if !Files.exists(genesisPath) then cancel("dev genesis not available")
    given Decoder[Genesis] = Genesis.decoder
    val genesis = decode[Genesis](Files.readString(genesisPath)).fold(e => fail(e.getMessage), identity)
    val spec = ChainSpec("duplicate-connections", ChainConfig.TINY, Some(genesis.header.encode.toArray), None, genesis.state.keyvals, Nil)
    val (k0, k1) = (ValidatorKeySet.dev(0), ValidatorKeySet.dev(1))
    val (first, second) =
      if Connectivity.isPreferredInitiator(k0.ed25519Public, k1.ed25519Public) then (k0, k1) else (k1, k0)
    val dirs = List("jam-dup-a", "jam-dup-b").map(Files.createTempDirectory(_))
    var nodes: List[JamNode] = Nil
    try
      nodes = dirs.zip(List(first, second)).map { (dir, keys) =>
        new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false, ed25519Seed = Some(keys.ed25519Secret))).start()
      }
      f(nodes(0), nodes(1))
    finally
      nodes.foreach(_.shutdown())
      dirs.foreach(d => Files.walk(d).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p)))

  private def address(node: JamNode) = new InetSocketAddress("127.0.0.1", node.listenPort)

  test("both sides dial at once: the preferred initiator's connection is the one kept") {
    withPair { (preferred, other) =>
      val mine = preferred.connectPeer(address(other))
      // The other side's dial is closed under it: it fails outright, or its connection ends at once.
      val theirs = scala.util.Try(other.connectPeer(address(preferred)))
      eventually("the other side's connection to be closed")(theirs.fold(_ => true, !_.isOpen))
      Thread.sleep(500)
      mine.isOpen shouldBe true
    }
  }

  test("the other side dials after the grace period: its connection replaces the preferred initiator's") {
    withPair { (preferred, other) =>
      val old = preferred.connectPeer(address(other))
      Thread.sleep(Connectivity.PreferredInitiatorGraceMillis + 300)
      old.isOpen shouldBe true
      // It would not dial if it still had the first connection: here it does, as a node does whose peer was
      // frozen and still believes in a connection that timed out long ago on this side.
      val fresh = other.connectPeer(address(preferred))
      eventually("the old connection to be closed")(!old.isOpen)
      Thread.sleep(500)
      fresh.isOpen shouldBe true
    }
  }
