package io.forge.jam.node

import java.nio.file.Files

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The ancestor set used for lookup anchors, and following the finality that peers announce. */
class PeerFinalitySpec extends AnyFunSuite with Matchers with NodeFixture:

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
        (3L to 5L).foreach(author(a, _))
        val b2 = author(b, 2)
        a.chain.importBlock(b2).isLeft shouldBe true
        val target = b.chain.best

        // Fewer peers than needed: nothing changes.
        a.chain.followPeerFinality(List.fill(3)((target.hash, target.slot)), needed = 4) shouldBe Right(None)
        a.chain.best.slot shouldBe 5L

        a.chain.followPeerFinality(List.fill(4)((target.hash, target.slot)), needed = 4).isRight shouldBe true
        a.chain.best.hash shouldBe target.hash
        a.chain.finalized.hash shouldBe target.hash

      }
    }
  }

  test("a block we hold is taken at its own slot: an ancestor of the finalized block never moves finality back") {
    val s = spec("peer-finality-claimed-slot")
    withNode(s) { a =>
      val hashes = (1L to 6L).map { slot => author(a, slot); a.chain.best.hash }
      a.chain.followPeerFinality(List((hashes(3), 4L)), needed = 1).isRight shouldBe true
      a.chain.finalized.hash shouldBe hashes(3)

      for (hash, claimed) <- Seq((hashes(0), 0xffffffffL), (hashes(2), 5L), (hashes(3), 99L)) do
        a.chain.followPeerFinality(List((hash, claimed)), needed = 1) shouldBe Right(None)
        a.chain.finalized.hash shouldBe hashes(3)

      a.chain.followPeerFinality(List((hashes(4), 0xffffffffL), (hashes(5), 1L)), needed = 1).isRight shouldBe true
      a.chain.finalized.hash shouldBe hashes(5)
    }
  }

  test("a block we do not hold does not stand in the way of one we hold, and is asked for only when none is agreed") {
    val s = spec("peer-finality-unknown")
    withNode(s) { a =>
      val hashes = (1L to 3L).map { slot => author(a, slot); a.chain.best.hash }
      val unknown = io.forge.jam.core.primitives.Hash(Array.fill[Byte](32)(0x5a))
      val other = io.forge.jam.core.primitives.Hash(Array.fill[Byte](32)(0x6b))

      a.chain.followPeerFinality(List((unknown, 0xffffffffL), (hashes(1), 2L)), needed = 1).isRight shouldBe true
      a.chain.finalized.hash shouldBe hashes(1)

      a.chain.followPeerFinality(List((unknown, 9L)), needed = 1) shouldBe Left(unknown)
      a.chain.followPeerFinality(List((unknown, 9L)), needed = 1, beingFetched = _ == unknown) shouldBe Right(None)
      a.chain.followPeerFinality(List((unknown, 9L), (other, 9L)), needed = 1, beingFetched = _ == unknown) shouldBe Left(other)
      a.chain.followPeerFinality(List((unknown, 9L), (other, 9L), (other, 9L)), needed = 2) shouldBe Left(other)
      a.chain.finalized.hash shouldBe hashes(1)
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
