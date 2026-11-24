package io.forge.jam.node

import java.nio.file.Path

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FailedCommitSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private final class InjectedFault extends RuntimeException("injected fault after the state save")

  private val first = JamBytes(new Array[Byte](31))
  private val last = JamBytes(Array.fill[Byte](31)(0xff.toByte))

  private def pairs(node: JamNode, h: Hash) = node.chain.stateAt(h).get.range(first, last, Int.MaxValue)._2

  private def spec(id: String): ChainSpec =
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    ChainSpec(
      id = id,
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

  private def node(spec: ChainSpec, dir: Path, i: Int): JamNode =
    val n = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false, ed25519Seed = Some(devKeys(i).ed25519Secret)))
    n.chain.initializeOrRestore(spec)
    n

  private def authorOn(on: JamNode, slot: Long): Array[Byte] =
    val block = new BlockAuthor(on.chain, devKeys).tryAuthor(slot).getOrElse(fail(s"author block $slot"))
    val bytes = on.chain.encodeBlock(block)
    on.chain.importBlock(bytes).isRight shouldBe true
    bytes

  private def hashOf(n: JamNode, bytes: Array[Byte]): Hash = n.chain.headerHashOf(n.chain.decodeBlock(bytes).toOption.get)

  private def sameStates(ref: JamNode, b: JamNode, blocks: Seq[Hash]): Unit =
    blocks.foreach { h =>
      b.chain.postStateRoot(h) shouldBe ref.chain.postStateRoot(h)
      pairs(b, h) shouldBe pairs(ref, h)
    }

  private def failThenRetry(a: JamNode, b: JamNode, n: Int): Seq[Hash] =
    author(Seq(a, b), 1 to n)
    val before = b.chain.best
    before.hash shouldBe a.chain.best.hash
    val earlier = a.chain.blocksAscending(a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get, 100)
      .map(hashOf(a, _))

    val next = authorOn(a, n + 1L)
    b.chain.faultAfterStateSave = () => throw new InjectedFault
    an[InjectedFault] should be thrownBy b.chain.importBlock(next)
    b.chain.faultAfterStateSave = () => ()

    b.chain.best shouldBe before
    b.chain.leaves.map(_.hash) shouldBe List(before.hash)
    b.chain.postStateRoot(hashOf(a, next)) shouldBe None
    sameStates(a, b, earlier)

    b.chain.importBlock(next) shouldBe Right(a.chain.best)
    b.chain.best.stateRoot shouldBe a.chain.best.stateRoot
    (n + 2 to n + 3).foreach { slot =>
      val bytes = authorOn(a, slot.toLong)
      b.chain.importBlock(bytes).isRight shouldBe true
      b.chain.best shouldBe a.chain.best
    }
    val all = earlier ++ Seq(hashOf(a, next)) ++
      a.chain.blocksAscending(hashOf(a, next), 10).map(hashOf(a, _))
    all.size shouldBe n + 3
    sameStates(a, b, all)
    all

  test("an import that throws after the state save leaves the chain as it was; the block imports on a retry") {
    withNodes(2, "failed-commit") { nodes =>
      val Seq(a, b) = nodes.toSeq
      failThenRetry(a, b, n = 5)
    }
  }

  test("after a failed import and a restart the node has the same chain and keeps importing") {
    val s = spec("failed-commit-restart")
    val dirA = tempDir("jam-failed-commit-a")
    val dirB = tempDir("jam-failed-commit-b")
    val a = node(s, dirA, 0)
    var b = node(s, dirB, 1)
    try
      val blocks = failThenRetry(a, b, n = 4)
      val bestBefore = b.chain.best
      b.shutdown()
      b = node(s, dirB, 1)
      b.chain.best shouldBe bestBefore
      sameStates(a, b, blocks)
      (9 to 11).foreach { slot =>
        b.chain.importBlock(authorOn(a, slot.toLong)).isRight shouldBe true
        b.chain.best shouldBe a.chain.best
      }
      sameStates(a, b, a.chain.blocksAscending(bestBefore.hash, 10).map(hashOf(a, _)))
    finally
      a.shutdown()
      b.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("a reorg whose replay throws partway leaves the stored best chain; more imports and the reorg itself work") {
    withNodes(3, "failed-reorg") { nodes =>
      val Seq(x, y, b) = nodes.toSeq
      // The branch is executed when it is adopted, as a branch is whose blocks could not be executed as they
      // arrived. (SideExecutionSpec has the failure of an execution on arrival.)
      b.chain.executeSideBlocks = false
      author(Seq(x, y, b), 1 to 2)
      val shared = x.chain.blocksAscending(x.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get, 10)
        .map(hashOf(x, _))
      val x4 = authorOn(x, 4)
      b.chain.importBlock(x4).isRight shouldBe true
      val y3 = authorOn(y, 3)
      val y5 = authorOn(y, 5)
      val y8 = authorOn(y, 8)

      b.chain.importBlock(y3).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      val before = b.chain.best
      before.hash shouldBe hashOf(x, x4)
      var saves = 0
      b.chain.faultAfterStateSave = () =>
        saves += 1
        if saves == 2 then throw new InjectedFault
      an[InjectedFault] should be thrownBy b.chain.importBlock(y5)
      saves shouldBe 2
      b.chain.faultAfterStateSave = () => ()

      b.chain.best shouldBe before
      b.chain.leaves.map(_.hash).toSet shouldBe Set(hashOf(x, x4), hashOf(y, y3))
      b.chain.postStateRoot(hashOf(y, y3)) shouldBe None
      b.chain.postStateRoot(hashOf(y, y5)) shouldBe None
      sameStates(x, b, shared :+ hashOf(x, x4))

      val x7 = authorOn(x, 7)
      b.chain.importBlock(x7) shouldBe Right(x.chain.best)
      sameStates(x, b, shared ++ Seq(hashOf(x, x4), hashOf(x, x7)))
      b.chain.importBlock(y5).left.toOption.get shouldBe a[ChainError.StoredOnSideBranch]
      b.chain.importBlock(y8) shouldBe Right(y.chain.best)
      b.chain.best shouldBe y.chain.best
      sameStates(y, b, shared ++ Seq(y3, y5, y8).map(hashOf(y, _)))
      b.chain.leaves.map(_.hash).toSet shouldBe Set(hashOf(y, y8), hashOf(x, x7))
    }
  }
