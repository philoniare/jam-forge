package io.forge.jam.node

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.ServiceId
import io.forge.jam.core.types.extrinsic.Preimage
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BlockBodyMismatchSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private def altered(node: JamNode, bytes: Array[Byte]): Array[Byte] =
    val block = node.chain.decodeBlock(bytes).toOption.get
    val extra = Preimage(ServiceId(0), JamBytes(Array[Byte](1, 2, 3)))
    val tampered = node.chain.encodeBlock(block.copy(extrinsic = block.extrinsic.copy(preimages = List(extra))))
    node.chain.headerHashOf(node.chain.decodeBlock(tampered).toOption.get) shouldBe node.chain.headerHashOf(block)
    tampered

  private def firstBlock(node: JamNode): Array[Byte] =
    val genesis = node.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
    node.chain.blocksAscending(genesis, 1).head

  test("a real header with an altered body is refused, and the real block still imports afterwards") {
    withNodes(2, "body-mismatch") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 1)
      val genesis = a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
      val good = a.chain.blocksAscending(genesis, 1).head
      val block = a.chain.decodeBlock(good).toOption.get
      val extra = Preimage(ServiceId(0), JamBytes(Array[Byte](1, 2, 3)))
      val tampered = a.chain.encodeBlock(block.copy(extrinsic = block.extrinsic.copy(preimages = List(extra))))
      a.chain.headerHashOf(a.chain.decodeBlock(tampered).toOption.get) shouldBe a.chain.headerHashOf(block)

      b.chain.importBlock(tampered).isLeft shouldBe true
      b.chain.atGenesis shouldBe true
      withClue("the real block after the tampered one: ") {
        b.chain.importBlock(good).map(_.hash) shouldBe Right(a.chain.best.hash)
      }
    }
  }

  test("on a side branch an altered body is refused and nothing is stored; the real block is then stored and adopted") {
    withNodes(2, "body-mismatch-side") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a, b), 1 to 1)
      val x = new BlockAuthor(a.chain, devKeys).tryAuthor(2).getOrElse(fail("author 2"))
      val xBytes = a.chain.encodeBlock(x)
      a.chain.importBlock(xBytes).isRight shouldBe true
      val y = new BlockAuthor(b.chain, devKeys).tryAuthor(3).getOrElse(fail("author 3"))
      b.chain.importBlock(b.chain.encodeBlock(y)).isRight shouldBe true
      val bBest = b.chain.best
      val xHash = a.chain.headerHashOf(x)

      b.chain.importBlock(altered(a, xBytes)) shouldBe Left(ChainError.BodyMismatch)
      b.chain.hasBlock(xHash) shouldBe false
      b.chain.best shouldBe bBest

      b.chain.importBlock(xBytes) match
        case Left(_: ChainError.StoredOnSideBranch) => ()
        case other                                 => fail(s"expected X on a side branch, got $other")
      b.chain.hasBlock(xHash) shouldBe true
      b.chain.importBlock(altered(a, xBytes)) shouldBe Left(ChainError.BodyMismatch)
      b.chain.blockStore.getBlock(xHash).map(_.toSeq) shouldBe Some(xBytes.toSeq)

      val x2 = new BlockAuthor(a.chain, devKeys).tryAuthor(4).getOrElse(fail("author 4"))
      val x2Bytes = a.chain.encodeBlock(x2)
      a.chain.importBlock(x2Bytes).isRight shouldBe true
      b.chain.importBlock(x2Bytes).map(_.hash) shouldBe Right(a.chain.best.hash)
      b.chain.best.stateRoot shouldBe a.chain.best.stateRoot
    }
  }

  test("a refused altered body leaves no mark: after a restart the real block imports") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val spec = ChainSpec(
      id = "body-mismatch-restart",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dirA = tempDir("jam-body-mismatch-a")
    val dirB = tempDir("jam-body-mismatch-b")
    def node(dir: java.nio.file.Path, i: Int) =
      new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false, ed25519Seed = Some(devKeys(i).ed25519Secret)))
    val a = node(dirA, 0)
    var b = node(dirB, 1)
    try
      a.chain.initializeOrRestore(spec)
      b.chain.initializeOrRestore(spec)
      author(Seq(a), 1 to 1)
      val good = firstBlock(a)
      b.chain.importBlock(altered(a, good)) shouldBe Left(ChainError.BodyMismatch)
      b.shutdown()
      b = node(dirB, 1)
      b.chain.initializeOrRestore(spec)
      b.chain.atGenesis shouldBe true
      b.chain.importBlock(good).map(_.hash) shouldBe Right(a.chain.best.hash)
    finally
      a.shutdown()
      b.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("a block that is really invalid (an altered header) is still marked, and refused as such afterwards") {
    withNodes(2, "body-mismatch-invalid") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 1)
      val good = firstBlock(a)
      val bad = good.clone()
      bad(40) = (bad(40) ^ 1).toByte
      b.chain.importBlock(bad) match
        case Left(_: ChainError.InvalidBlock) => ()
        case other                           => fail(s"expected InvalidBlock, got $other")
      b.chain.importBlock(bad) shouldBe Left(ChainError.PreviouslyInvalid)
      b.chain.importBlock(good).map(_.hash) shouldBe Right(a.chain.best.hash)
    }
  }
