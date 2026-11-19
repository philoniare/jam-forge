package io.forge.jam.node

import java.nio.file.Path
import java.util.concurrent.ConcurrentLinkedQueue

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BatchImportSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private final class InjectedFault extends RuntimeException("injected fault")

  private def spec(id: String): ChainSpec =
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    ChainSpec(id, ChainConfig.TINY, Some(genesis.header.encode.toArray), None, genesis.state.keyvals, Nil)

  private def node(spec: ChainSpec, dir: Path, i: Int): JamNode =
    val n = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false, ed25519Seed = Some(devKeys(i).ed25519Secret)))
    n.chain.initializeOrRestore(spec)
    n

  private def authorOn(on: JamNode, slots: Range): Vector[Array[Byte]] =
    val author = new BlockAuthor(on.chain, devKeys)
    slots.toVector.map { slot =>
      val bytes = on.chain.encodeBlock(author.tryAuthor(slot.toLong).getOrElse(fail(s"author block $slot")))
      on.chain.importBlock(bytes).isRight shouldBe true
      bytes
    }

  private def hashOf(n: JamNode, bytes: Array[Byte]): Hash = n.chain.headerHashOf(n.chain.decodeBlock(bytes).toOption.get)

  private def commits(n: JamNode): (Long, Long) =
    val t = n.chain.timings.snapshot
    (t.walSync.count, t.metaWrite.count)

  private def watchImports(n: JamNode): ConcurrentLinkedQueue[(Long, Long)] =
    val seen = new ConcurrentLinkedQueue[(Long, Long)]()
    n.chain.onImported((head, _) => { seen.add((head.slot, n.chain.best.slot)); () })
    seen

  test("a batch is one commit: one WAL sync and one metadata write for all its blocks") {
    withNodes(2, "batch-import") { nodes =>
      val Seq(a, b) = nodes.toSeq
      val blocks = authorOn(a, 1 to 6)
      val seen = watchImports(b)
      val (syncs, writes) = commits(b)
      val imported = b.chain.timings.snapshot.blockImport.count

      val results = b.chain.importBatch(blocks)
      results.map(_.map(_.slot)) shouldBe (1L to 6L).map(Right(_))
      b.chain.best shouldBe a.chain.best
      commits(b) shouldBe (syncs + 1, writes + 1)
      b.chain.timings.snapshot.blockImport.count shouldBe imported + 6
      blocks.foreach(bytes => b.chain.postStateRoot(hashOf(a, bytes)) shouldBe a.chain.postStateRoot(hashOf(a, bytes)))

      eventually("the import events")(seen.size == 6)
      seen.toArray.toList shouldBe (1L to 6L).map(slot => (slot, 6L)).toList
    }
  }

  test("importing one block is a batch of one") {
    withNodes(2, "batch-of-one") { nodes =>
      val Seq(a, b) = nodes.toSeq
      val blocks = authorOn(a, 1 to 2)
      val (syncs, writes) = commits(b)
      b.chain.importBlock(blocks(0)).map(_.slot) shouldBe Right(1L)
      b.chain.importBatch(Vector(blocks(0))) shouldBe Vector(Left(ChainError.AlreadyImported))
      b.chain.importBatch(Vector(blocks(1))).map(_.map(_.slot)) shouldBe Vector(Right(2L))
      b.chain.importBatch(Vector.empty) shouldBe Vector.empty
      commits(b)._1 should be >= syncs + 2
      commits(b)._2 should be >= writes + 2
    }
  }

  test("a batch stops when its time is up, after at least one block; the rest is the caller's to go on with") {
    withNodes(2, "batch-budget") { nodes =>
      val Seq(a, b) = nodes.toSeq
      val blocks = authorOn(a, 1 to 4)
      val first = b.chain.importBatch(blocks, budgetNanos = 0L)
      first.map(_.map(_.slot)) shouldBe Vector(Right(1L))
      b.chain.best.slot shouldBe 1L
      val rest = b.chain.importBatch(blocks.drop(1), budgetNanos = Long.MaxValue)
      rest.map(_.map(_.slot)) shouldBe Vector(Right(2L), Right(3L), Right(4L))
      b.chain.best shouldBe a.chain.best
    }
  }

  test("an invalid block in a batch does not undo the blocks before it, and those after it have no parent") {
    withNodes(2, "batch-invalid") { nodes =>
      val Seq(a, b) = nodes.toSeq
      val blocks = authorOn(a, 1 to 4)
      val third = a.chain.decodeBlock(blocks(2)).toOption.get
      val seal = third.header.seal.toArray
      seal(0) = (seal(0) ^ 1).toByte
      val forged = a.chain.encodeBlock(third.copy(header = third.header.copy(seal = JamBytes(seal))))

      val results = b.chain.importBatch(Vector(blocks(0), blocks(1), forged, blocks(3)))
      results(0).map(_.slot) shouldBe Right(1L)
      results(1).map(_.slot) shouldBe Right(2L)
      results(2).left.toOption.get shouldBe an[ChainError.InvalidBlock]
      results(3).left.toOption.get shouldBe an[ChainError.UnknownParent]
      b.chain.best.hash shouldBe hashOf(a, blocks(1))
      b.chain.importBatch(blocks.drop(2)).map(_.map(_.slot)) shouldBe Vector(Right(3L), Right(4L))
      b.chain.best shouldBe a.chain.best
    }
  }

  private def failThenRetry(a: JamNode, b: JamNode, arm: JamNode => Unit): (Vector[Array[Byte]], ChainHead) =
    val seen = watchImports(b)
    val earlier = authorOn(a, 1 to 2)
    b.chain.importBatch(earlier).forall(_.isRight) shouldBe true
    val before = b.chain.best
    eventually("the events of the earlier blocks")(seen.size == 2)
    seen.clear()
    val blocks = authorOn(a, 3 to 7)

    arm(b)
    an[InjectedFault] should be thrownBy b.chain.importBatch(blocks)
    b.chain.faultBeforeMetaCommit = () => ()
    b.chain.faultAfterStateSave = () => ()

    b.chain.best shouldBe before
    b.chain.leaves.map(_.hash) shouldBe List(before.hash)
    blocks.foreach(bytes => b.chain.postStateRoot(hashOf(a, bytes)) shouldBe None)
    Thread.sleep(300)
    seen.size shouldBe 0
    (blocks, before)

  test("a batch that fails at its commit leaves the chain as it was; the same blocks import on a retry") {
    withNodes(2, "batch-failed-commit") { nodes =>
      val Seq(a, b) = nodes.toSeq
      val (blocks, _) = failThenRetry(a, b, _.chain.faultBeforeMetaCommit = () => throw new InjectedFault)
      b.chain.importBatch(blocks).map(_.map(_.slot)) shouldBe (3L to 7L).map(Right(_))
      b.chain.best shouldBe a.chain.best
    }
  }

  test("a batch that fails at its third block leaves none of it; the same blocks import on a retry") {
    withNodes(2, "batch-failed-block") { nodes =>
      val Seq(a, b) = nodes.toSeq
      var saves = 0
      val (blocks, _) = failThenRetry(
        a,
        b,
        _.chain.faultAfterStateSave = () => {
          saves += 1
          if saves == 3 then throw new InjectedFault
        }
      )
      saves shouldBe 3
      b.chain.importBatch(blocks).map(_.map(_.slot)) shouldBe (3L to 7L).map(Right(_))
      b.chain.best shouldBe a.chain.best
    }
  }

  test("after a batch that failed at its commit and a restart, the node is where it was and imports the batch") {
    val s = spec("batch-failed-restart")
    val dirA = tempDir("jam-batch-restart-a")
    val dirB = tempDir("jam-batch-restart-b")
    val a = node(s, dirA, 0)
    var b = node(s, dirB, 1)
    try
      val (blocks, before) = failThenRetry(a, b, _.chain.faultBeforeMetaCommit = () => throw new InjectedFault)
      b.shutdown()
      b = node(s, dirB, 1)
      b.chain.best shouldBe before
      blocks.foreach(bytes => b.chain.postStateRoot(hashOf(a, bytes)) shouldBe None)
      b.chain.importBatch(blocks).map(_.map(_.slot)) shouldBe (3L to 7L).map(Right(_))
      b.chain.best shouldBe a.chain.best
      b.shutdown()
      b = node(s, dirB, 1)
      b.chain.best shouldBe a.chain.best
    finally
      a.shutdown()
      b.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }
