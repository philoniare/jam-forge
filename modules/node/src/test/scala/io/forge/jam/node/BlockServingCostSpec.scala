package io.forge.jam.node

import io.forge.jam.core.primitives.Hash
import io.forge.jam.db.BlockStore
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BlockServingCostSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private val Length = 160

  private def withChain(id: String)(body: (ChainManager, IndexedSeq[Hash]) => Unit): Unit =
    withNodes(1, id) { nodes =>
      val chain = nodes.head.chain
      author(nodes, 1 to Length)
      val genesis = chain.blockStore.getHead(BlockStore.GenesisHead).get
      val byHeight = Iterator.iterate(Option(chain.best.hash))(_.flatMap(chain.parentHash)).take(Length + 1).flatten.toIndexedSeq.reverse
      byHeight.head shouldBe genesis
      byHeight.zipWithIndex.foreach((h, i) => chain.heightOf(h) shouldBe Some(i.toLong))
      chain.finalize(byHeight(Length - 2)).isRight shouldBe true
      body(chain, byHeight)
    }

  private def links[A](chain: ChainManager)(body: => A): (A, Long) =
    val before = chain.parentLookups
    val result = body
    (result, chain.parentLookups - before)

  private def hashesOf(chain: ChainManager, blocks: List[Array[Byte]]): List[Hash] =
    blocks.map(b => chain.decodeBlock(b).fold(e => fail(s"served block does not decode: $e"), identity)).map(chain.headerHashOf)

  test("blocks after an early one are served without walking the chain for each") {
    withChain("serving-cost-ascending") { (chain, byHeight) =>
      val (served, followed) = links(chain)(chain.blocksAscending(byHeight(0), 128))
      hashesOf(chain, served) shouldBe byHeight.slice(1, 129).toList
      followed should be <= 4L

      val (tail, toHead) = links(chain)(chain.blocksAscending(byHeight(Length - 40), 256))
      hashesOf(chain, tail) shouldBe byHeight.drop(Length - 39).toList
      toHead should be <= 8L
    }
  }

  test("whether a block can still be finalized, and its ancestor at a height, take one lookup below finality") {
    withChain("serving-cost-ancestor") { (chain, byHeight) =>
      val (finalizable, followed) = links(chain)((1 until Length).forall(h => chain.canBeFinalized(byHeight(h))))
      finalizable shouldBe true
      followed should be <= 4L

      val (ancestors, more) = links(chain) {
        for from <- Seq(Length, Length - 2, Length - 3, 90, 1); to <- Seq(0, 1, 50, from) if to <= from
        yield (from, to, chain.ancestorAtHeight(byHeight(from), to.toLong))
      }
      for (from, to, got) <- ancestors do withClue(s"ancestor of $from at $to: ")(got shouldBe Some(byHeight(to)))
      more should be <= 20L
      chain.ancestorAtHeight(byHeight(10), 11L) shouldBe None
    }
  }

  test("without the index the answers are the same, by the walk") {
    withChain("serving-cost-no-index") { (chain, byHeight) =>
      val indexed = hashesOf(chain, chain.blocksAscending(byHeight(3), 100))
      for h <- 0 to Length do chain.meta.dropFinalizedAt(h.toLong)
      val (walked, followed) = links(chain)(hashesOf(chain, chain.blocksAscending(byHeight(3), 100)))
      walked shouldBe indexed
      walked shouldBe byHeight.slice(4, 104).toList
      followed should be > 5000L
      for (from, to) <- Seq((Length, 0), (120, 7), (Length - 2, 1)) do
        chain.ancestorAtHeight(byHeight(from), to.toLong) shouldBe Some(byHeight(to))
      (1 until Length).forall(h => chain.canBeFinalized(byHeight(h))) shouldBe true
    }
  }

  test("a block on a branch that finality passed by is not on the finalized chain, whatever the index holds at its height") {
    withNodes(2, "serving-cost-side") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 2 to 31)
      val other = new BlockAuthor(b.chain, devKeys).tryAuthor(1L).getOrElse(fail("author the side block"))
      val side = b.chain.headerHashOf(other)
      a.chain.importBlock(b.chain.encodeBlock(other))
      a.chain.hasBlock(side) shouldBe true
      val main = Iterator.iterate(Option(a.chain.best.hash))(_.flatMap(a.chain.parentHash)).take(31).flatten.toIndexedSeq.reverse
      a.chain.finalize(main(20)).isRight shouldBe true
      a.chain.heightOf(side) shouldBe Some(1L)
      side should not be main(1)
      a.chain.canBeFinalized(side) shouldBe false
      a.chain.canBeFinalized(main(1)) shouldBe true
      a.chain.ancestorAtHeight(side, 0L) shouldBe Some(main(0))
      hashesOf(a.chain, a.chain.blocksAscending(main(0), 5)) shouldBe main.slice(1, 6).toList
      a.chain.blocksAscending(side, 5) shouldBe Nil
    }
  }
