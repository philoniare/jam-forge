package io.forge.jam.node

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import io.forge.jam.network.{PeerStreamHandler, StreamKind}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CheckpointSyncSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private val first = JamBytes(new Array[Byte](31))
  private val last = JamBytes(Array.fill[Byte](31)(0xff.toByte))

  private def serving(node: JamNode): Byte => Option[PeerStreamHandler] =
    case StreamKind.BlockRequest => Some(node.sync.blockRequestHandler)
    case StreamKind.StateRequest => Some(node.sync.stateRequestHandler)
    case _                       => None

  private def pairs(node: JamNode, h: Hash) = node.chain.stateAt(h).get.range(first, last, Int.MaxValue)._2

  test("a node joins at a checkpoint, follows the chain from there, finalizes, prunes and restarts") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val spec = ChainSpec(
      id = "checkpoint-sync",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dirA = tempDir("jam-checkpoint-a")
    val dirB = tempDir("jam-checkpoint-b")
    def node(dir: java.nio.file.Path, i: Int) =
      new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false, ed25519Seed = Some(devKeys(i).ed25519Secret)))
    val a = node(dirA, 0)
    var b = node(dirB, 1)
    try
      a.chain.initializeOrRestore(spec)
      b.chain.initializeOrRestore(spec)
      author(Seq(a), 1 to 40)
      val chainOfA = a.chain.blocksAscending(a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get, 100)
      chainOfA.size shouldBe 40
      val blocks = chainOfA.map(bytes => a.chain.decodeBlock(bytes).toOption.get)
      val checkpoint = blocks(29) // slot 30
      val start = blocks(28)
      val (_, bToA) = join(a, b, serving)

      b.chain.atGenesis shouldBe true
      b.sync.hold()
      val job = new CheckpointSync(b.chain, b.sync, a.chain.headerHashOf(checkpoint), () => List(bToA))
      val head = job.attempt().fold(why => fail(why), identity)

      head.hash shouldBe a.chain.headerHashOf(start)
      head.slot shouldBe start.header.slot.toLong
      b.chain.best shouldBe head
      b.chain.finalized.hash shouldBe head.hash
      b.chain.atGenesis shouldBe false
      pairs(b, head.hash) shouldBe pairs(a, head.hash)
      b.chain.hasBlock(a.chain.headerHashOf(blocks(27))) shouldBe true // an ancestor: header only
      b.chain.stateAt(a.chain.headerHashOf(blocks(27))) shouldBe None
      b.chain.blocksDescending(head.hash, 10).size shouldBe 1 // older bodies are not kept, so not served

      chainOfA.drop(29).foreach(bytes => b.chain.importBlock(bytes).isRight shouldBe true)
      b.chain.best.hash shouldBe a.chain.best.hash
      b.chain.best.stateRoot shouldBe a.chain.best.stateRoot

      b.chain.importBlock(chainOfA(10)).isLeft shouldBe true

      b.chain.finalizeAtDepth(2).isDefined shouldBe true
      b.chain.finalized.slot shouldBe 38L

      b.chain.startAt(chainOfA(35), a.chain.best.stateRoot, Nil).isLeft shouldBe true
      val bestBefore = b.chain.best
      b.shutdown()
      b = node(dirB, 1)
      b.chain.initializeOrRestore(spec)
      b.chain.best shouldBe bestBefore
      b.chain.atGenesis shouldBe false
      author(Seq(a), 41 to 44)
      a.chain.blocksAscending(bestBefore.hash, 10).foreach(bytes => b.chain.importBlock(bytes).isRight shouldBe true)
      b.chain.best.stateRoot shouldBe a.chain.best.stateRoot
      pairs(b, b.chain.best.hash) shouldBe pairs(a, a.chain.best.hash)
    finally
      a.shutdown()
      b.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("a checkpoint no peer has leaves the node at genesis, and the first block as checkpoint means genesis") {
    withNodes(2, "checkpoint-missing") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 5)
      val (_, bToA) = join(a, b, serving)
      val unknown = Hash(Array.fill[Byte](32)(0x42))
      new CheckpointSync(b.chain, b.sync, unknown, () => List(bToA)).attempt().isLeft shouldBe true
      new CheckpointSync(b.chain, b.sync, unknown, () => Nil).attempt().isLeft shouldBe true
      b.chain.atGenesis shouldBe true

      val genesisHash = a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
      val firstBlock = a.chain.decodeBlock(a.chain.blocksAscending(genesisHash, 1).head).toOption.get
      new CheckpointSync(b.chain, b.sync, a.chain.headerHashOf(firstBlock), () => List(bToA)).attempt().isRight shouldBe true
      b.chain.atGenesis shouldBe true
    }
  }
