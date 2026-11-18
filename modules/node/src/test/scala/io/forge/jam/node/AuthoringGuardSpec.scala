package io.forge.jam.node

import java.util.concurrent.atomic.AtomicLong

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AuthoringGuardSpec extends AnyFunSuite with Matchers with NodeFixture:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  private def eventually(what: String, seconds: Int = 20)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(10)
    if !cond then fail(s"timed out waiting for: $what")

  private final class Node(val node: JamNode):
    private val seenSlot = new AtomicLong(-1L)
    node.chain.onImported((head, _) => seenSlot.set(head.slot))

    def seen(slot: Long): Unit = eventually(s"the import listeners to see slot $slot")(seenSlot.get >= slot)

  private def withNode(id: String)(body: (() => Node) => Unit): Unit =
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val spec = ChainSpec(id, ChainConfig.TINY, Some(genesis.header.encode.toArray), None, genesis.state.keyvals, Nil)
    val dir = tempDir(s"jam-$id")
    var open: Option[JamNode] = None
    def start(): Node =
      open.foreach(_.shutdownStorageOnly())
      val n = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false))
      open = Some(n)
      n.chain.initializeOrRestore(spec)
      n.enableAuthoring(devKeys)
      new Node(n)
    try body(() => start())
    finally
      open.foreach(_.shutdownStorageOnly())
      cleanup(dir)

  test("a slot a block was sealed for is not authored again, in the same run or after a restart, even if the block was lost") {
    withNode("authoring-restart") { start =>
      val first = start().node
      first.authorSlot(1).map(_.slot) shouldBe Some(1L)
      first.blockAuthor.getOrElse(fail("no author")).tryAuthor(2).isDefined shouldBe true
      first.chain.best.slot shouldBe 1L
      first.authorSlot(2) shouldBe None
      first.chain.meta.lastAuthoredSlot shouldBe Some(2L)

      val again = start().node
      again.chain.best.slot shouldBe 1L
      again.authorSlot(2) shouldBe None
      again.authorSlot(3).map(_.slot) shouldBe Some(3L)
      again.chain.meta.lastAuthoredSlot shouldBe Some(3L)
      again.authoringStopped shouldBe None
    }
  }

  test("a block on the best chain sealed with our key that this node did not author stops its authoring") {
    withNode("authoring-twin") { start =>
      val n = start()
      val node = n.node
      node.authorSlot(1).isDefined shouldBe true
      val twin = new BlockAuthor(node.chain, devKeys)
      val theirs = twin.tryAuthor(2).getOrElse(fail("the twin does not author slot 2"))
      node.chain.importBlock(node.chain.encodeBlock(theirs)).isRight shouldBe true
      n.seen(2)
      node.authoringStopped.getOrElse(fail("authoring goes on")) should include("slot 2")
      node.authorSlot(3) shouldBe None
      node.chain.best.slot shouldBe 2L
    }
  }

  test("another process's block for a slot we sealed, kept on a side branch, stops authoring too") {
    withNode("authoring-twin-side") { start =>
      val n = start()
      val node = n.node
      node.authorSlot(1).isDefined shouldBe true
      val theirs = new BlockAuthor(node.chain, devKeys).tryAuthor(2).getOrElse(fail("the twin does not author slot 2"))
      val ours = node.authorSlot(2).getOrElse(fail("slot 2 not authored"))
      node.chain.headerHashOf(theirs) should not be ours.hash
      node.chain.importBlock(node.chain.encodeBlock(theirs)).isRight shouldBe false
      eventually("authoring to stop")(node.authoringStopped.isDefined)
      node.authoringStopped.get should include("slot 2")
      node.authorSlot(3) shouldBe None
    }
  }

  test("blocks with our key from before this run started authoring do not stop it, nor do our own") {
    withNode("authoring-history") { start =>
      val n = start()
      val node = n.node
      val earlier = new BlockAuthor(node.chain, devKeys)
      for slot <- 1 to 2 do
        val block = earlier.tryAuthor(slot.toLong).getOrElse(fail(s"slot $slot"))
        node.chain.importBlock(node.chain.encodeBlock(block)).isRight shouldBe true
      for slot <- 3 to 6 do node.authorSlot(slot.toLong).map(_.slot) shouldBe Some(slot.toLong)
      n.seen(6)
      node.authoringStopped shouldBe None
      node.authorSlot(7).map(_.slot) shouldBe Some(7L)
    }
  }
