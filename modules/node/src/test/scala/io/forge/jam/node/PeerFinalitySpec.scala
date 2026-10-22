package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The ancestor set used for lookup anchors, and following the finality that peers announce. */
class PeerFinalitySpec extends AnyFunSuite with Matchers:

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def spec(id: String): ChainSpec =
    if !Files.exists(genesisPath) then cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    given Decoder[Genesis] = Genesis.decoder
    val genesis = decode[Genesis](Files.readString(genesisPath)).getOrElse(fail("genesis.json undecodable"))
    ChainSpec(
      id = id,
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  private def withNode(s: ChainSpec)(f: JamNode => Unit): Unit =
    val dir = Files.createTempDirectory("jam-peer-finality")
    val node = new JamNode(s, NodeConfig(dataDir = dir, slotTicking = false))
    try
      node.chain.initializeOrRestore(s)
      f(node)
    finally
      node.shutdownStorageOnly()
      cleanup(dir)

  private def author(node: JamNode, slot: Long): Array[Byte] =
    val block = new BlockAuthor(node.chain, devKeys).tryAuthor(slot).getOrElse(fail(s"author slot $slot"))
    val bytes = node.chain.encodeBlock(block)
    node.chain.importBlock(bytes).isRight shouldBe true
    bytes

  test("GP: the ancestor set holds the headers of the last L slots with their posterior state roots") {
    val s = spec("ancestry-devnet")
    withNode(s) { node =>
      (1L to 30L).foreach(author(node, _))
      val best = node.chain.best
      val ancestry = node.chain.ancestorsOf(best.hash, best.stateRoot, 31L)
      // L = 24 in the tiny config: slots 7 to 30, newest first.
      ancestry.map(_.slot) shouldBe (30L to 7L by -1L).toList
      ancestry.head.headerHash shouldBe best.hash
      ancestry.head.postStateRoot shouldBe Some(best.stateRoot)
      ancestry.flatMap(_.postStateRoot).distinct.size shouldBe 24
    }
  }

  test("follows the block a quorum of peers announces as finalized, off its own best chain") {
    val s = spec("peer-finality-devnet")
    withNode(s) { a =>
      withNode(s) { b =>
        val block1 = author(a, 1)
        b.chain.importBlock(block1).isRight shouldBe true
        // A's best chain: 1, 2, 3, 4. B's: 1, 5 (lighter, so A keeps it on a side branch).
        (2L to 4L).foreach(author(a, _))
        val b5 = author(b, 5)
        a.chain.importBlock(b5).isLeft shouldBe true
        val target = b.chain.best

        // Fewer peers than needed: nothing changes.
        a.chain.followPeerFinality(List.fill(3)((target.hash, target.slot)), needed = 4) shouldBe Right(None)
        a.chain.best.slot shouldBe 4L

        a.chain.followPeerFinality(List.fill(4)((target.hash, target.slot)), needed = 4).isRight shouldBe true
        a.chain.best.hash shouldBe target.hash
        a.chain.finalized.hash shouldBe target.hash

      }
    }
  }

  test("asks for the agreed finalized block when it is not stored yet") {
    val s = spec("peer-finality-missing")
    withNode(s) { a =>
      withNode(s) { b =>
        author(b, 1)
        val target = b.chain.best
        a.chain.followPeerFinality(List.fill(4)((target.hash, target.slot)), needed = 4) shouldBe Left(target.hash)
      }
    }
  }
