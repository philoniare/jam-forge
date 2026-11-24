package io.forge.jam.node

import java.nio.file.Path
import java.util.concurrent.{CountDownLatch, TimeUnit}

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend}
import io.forge.jam.network.{PeerConnection, PeerStreamHandler, StreamKind}
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StateSweepSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private val first = JamBytes(new Array[Byte](31))
  private val last = JamBytes(Array.fill[Byte](31)(0xff.toByte))

  private def spec(id: String, genesis: Genesis): ChainSpec =
    ChainSpec(
      id = id,
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

  private def newNode(sp: ChainSpec, dir: Path, i: Int, intervalMillis: Long = 0L): JamNode =
    val node = new JamNode(
      sp,
      NodeConfig(
        dataDir = dir,
        slotTicking = false,
        ed25519Seed = Some(devKeys(i).ed25519Secret),
        stateSweepIntervalMillis = intervalMillis
      )
    )
    node.chain.initializeOrRestore(sp)
    node

  private def withTwo(id: String, intervalB: Long = 0L)(body: (ChainSpec, JamNode, () => JamNode, Path) => Unit): Unit =
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val sp = spec(id, genesis)
    val (dirA, dirB) = (tempDir(s"jam-$id-a"), tempDir(s"jam-$id-b"))
    val a = newNode(sp, dirA, 0)
    var b: JamNode = null
    try body(sp, a, () => { if b == null then b = newNode(sp, dirB, 1, intervalB); b }, dirB)
    finally
      a.shutdown()
      if b != null then b.shutdown()
      cleanup(dirA)
      cleanup(dirB)

  private def authorOn(node: JamNode, slot: Long): ChainHead =
    val block = new BlockAuthor(node.chain, devKeys).tryAuthor(slot).getOrElse(fail(s"author slot $slot"))
    node.chain.importBlock(node.chain.encodeBlock(block)).fold(e => fail(s"import of slot $slot: $e"), identity)

  private def serving(node: JamNode): Byte => Option[PeerStreamHandler] =
    case StreamKind.BlockRequest => Some(node.sync.blockRequestHandler)
    case StreamKind.StateRequest => Some(node.sync.stateRequestHandler)
    case _                       => None

  private def pairs(node: JamNode, h: Hash) = node.chain.stateAt(h).get.range(first, last, Int.MaxValue)._2

  private def keptBlocks(node: JamNode): List[Hash] =
    val floor = node.chain.pruneFloorHeight
    Iterator
      .iterate(Option(node.chain.best.hash))(_.flatMap(node.chain.parentHash))
      .takeWhile(_.exists(h => node.chain.meta.height(h).exists(_ >= floor)))
      .flatten
      .toList

  private def completed(node: JamNode): RocksDbTrieBackend.SweepStats =
    node.chain.awaitStatePruning()
    node.chain.lastStateSweep match
      case Some(RocksDbTrieBackend.SweepOutcome.Completed(stats)) => stats
      case other                                                  => fail(s"expected a completed sweep, got $other")

  private def cutShort(b: JamNode, bToA: PeerConnection, head: ChainHead, answers: Int, gate: () => Unit = () => ()) =
    var asked = 0
    b.chain.downloadState(
      head.hash,
      head.slot,
      head.stateRoot,
      List(bToA),
      (conn, start) =>
        asked += 1
        if asked == 2 then gate()
        if asked > answers then None else StateDownload.request(head.hash, maxBytes = 2048)(conn, start)
    )

  test("the states of executed blocks a reorg left are kept while they can be adopted again, and go once finality passes them; every kept state stays whole") {
    withTwo("sweep-branch") { (_, a, makeB, _) =>
      val b = makeB()
      val block1 = new BlockAuthor(a.chain, devKeys).tryAuthor(1).getOrElse(fail("author block 1"))
      val block1Bytes = a.chain.encodeBlock(block1)
      a.chain.importBlock(block1Bytes).isRight shouldBe true
      b.chain.importBlock(block1Bytes).left.toOption shouldBe None
      val own = List(authorOn(a, 2), authorOn(a, 3))
      val branch = (4 to 6).map { slot =>
        val blk = new BlockAuthor(b.chain, devKeys).tryAuthor(slot.toLong).get
        b.chain.importBlock(b.chain.encodeBlock(blk)).isRight shouldBe true
        b.chain.encodeBlock(blk)
      }
      branch.foreach(a.chain.importBlock(_))
      a.chain.best.hash shouldBe b.chain.best.hash
      // The blocks A left are off the best chain with their states: the chain could come back to them.
      own.foreach(h => a.chain.postStateRoot(h.hash) shouldBe None)
      own.foreach(h => a.chain.executedStateRoot(h.hash) shouldBe Some(h.stateRoot))
      own.foreach(h => a.chain.stateBackendForTesting.readNode(h.stateRoot) shouldBe defined)
      a.chain.meta.sweepWanted shouldBe None

      // Finality passes them: their states are released, and the sweep this asks for deletes them.
      (7 to 9).foreach { slot =>
        authorOn(a, slot.toLong)
        a.chain.finalizeAtDepth(2)
      }
      a.chain.finalized.slot should be >= 6L
      own.foreach(h => a.chain.executedStateRoot(h.hash) shouldBe None)
      val stats = completed(a)
      info(stats.toString)
      stats.nodesDeleted should be > 0L
      a.chain.meta.sweepWanted shouldBe None
      own.foreach(h => a.chain.stateBackendForTesting.readNode(h.stateRoot) shouldBe None)

      (10 to 45).foreach { slot =>
        authorOn(a, slot.toLong)
        a.chain.finalizeAtDepth(2)
      }
      a.chain.pruneFloorHeight should be > 1L
      a.chain.awaitStatePruning()
      a.chain.meta.sweepWanted shouldBe None
      val kept = keptBlocks(a)
      kept.size should be > 20
      kept.foreach(h => noException should be thrownBy pairs(a, h))
      a.chain.blocksAscending(b.chain.best.hash, 100).foreach(bytes => b.chain.importBlock(bytes).isRight shouldBe true)
      b.chain.best.hash shouldBe a.chain.best.hash
      kept.foreach(h => pairs(a, h) shouldBe pairs(b, h))
      authorOn(a, 46).slot shouldBe 46L
    }
  }

  test("what a download cut short saved goes, and the genesis state stays whole") {
    withTwo("sweep-aborted-download") { (_, a, makeB, _) =>
      (1 to 6).foreach(slot => authorOn(a, slot.toLong))
      val b = makeB()
      val (_, bToA) = join(a, b, serving)
      val genesisHash = b.chain.best.hash
      val genesisPairs = pairs(b, genesisHash)
      val (nodesBefore, valuesBefore) = b.chain.storedStateCounts
      cutShort(b, bToA, a.chain.best, answers = 4).isLeft shouldBe true
      val stats = completed(b)
      stats.nodesDeleted should be > 0L
      b.chain.storedStateCounts shouldBe ((nodesBefore, valuesBefore))
      pairs(b, genesisHash) shouldBe genesisPairs
      b.chain.meta.sweepWanted shouldBe None
    }
  }

  test("after a checkpoint start the genesis state goes, the start state stays whole, and the download journalled nothing") {
    withTwo("sweep-checkpoint") { (_, a, makeB, _) =>
      (1 to 20).foreach(slot => authorOn(a, slot.toLong))
      val b = makeB()
      val (_, bToA) = join(a, b, serving)
      val genesisRoot = b.chain.best.stateRoot
      val chainOfA = a.chain.blocksAscending(a.chain.blockStore.getHead(BlockStore.GenesisHead).get, 100)
      val start = a.chain.headerHashOf(a.chain.decodeBlock(chainOfA(14)).toOption.get)
      val root = a.chain.postStateRoot(start).get
      b.chain.downloadState(start, 15L, root, List(bToA), StateDownload.request(start, maxBytes = 2048)).isRight shouldBe true
      b.chain.stateBackendForTesting.journalOf(start) shouldBe Nil
      b.chain.startAt(chainOfA(14), root, headersOf(a.chain, chainOfA.slice(5, 14).reverse)).isRight shouldBe true
      completed(b).nodesDeleted should be > 0L
      b.chain.stateBackendForTesting.readNode(genesisRoot) shouldBe None
      pairs(b, start) shouldBe pairs(a, start)
      b.chain.meta.sweepWanted shouldBe None
      chainOfA.drop(15).foreach(bytes => b.chain.importBlock(bytes).isRight shouldBe true)
      b.chain.best.stateRoot shouldBe a.chain.best.stateRoot
    }
  }

  test("no sweep runs while a download is in progress; the request stays and is taken up after it") {
    withTwo("sweep-during-download") { (_, a, makeB, _) =>
      (1 to 6).foreach(slot => authorOn(a, slot.toLong))
      val b = makeB()
      val (_, bToA) = join(a, b, serving)
      val inside = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val download = new Thread(() =>
        cutShort(b, bToA, a.chain.best, answers = 3, gate = () => { inside.countDown(); release.await(); () }): Unit
      )
      download.start()
      inside.await(30, TimeUnit.SECONDS) shouldBe true
      val counts = b.chain.storedStateCounts
      b.chain.requestStateSweep()
      b.chain.awaitStatePruning()
      b.chain.lastStateSweep shouldBe None
      b.chain.storedStateCounts shouldBe counts
      b.chain.meta.sweepWanted shouldBe defined
      release.countDown()
      download.join(30000)
      completed(b).nodesDeleted should be > 0L
      b.chain.meta.sweepWanted shouldBe None
    }
  }

  test("a sweep request outlives a restart and is taken up at start-up") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val sp = spec("sweep-restart", genesis)
    val (dirA, dirB) = (tempDir("jam-sweep-restart-a"), tempDir("jam-sweep-restart-b"))
    val a = newNode(sp, dirA, 0)
    var b = newNode(sp, dirB, 1, intervalMillis = 3_600_000L)
    try
      (1 to 6).foreach(slot => authorOn(a, slot.toLong))
      val (_, bToA) = join(a, b, serving)
      val counts = b.chain.storedStateCounts
      cutShort(b, bToA, a.chain.best, answers = 4).isLeft shouldBe true
      b.chain.awaitStatePruning()
      b.chain.lastStateSweep shouldBe None // within the interval
      b.chain.meta.sweepWanted shouldBe defined
      b.chain.storedStateCounts._1 should be > counts._1
      b.shutdown()
      b = newNode(sp, dirB, 1, intervalMillis = 3_600_000L)
      completed(b).nodesDeleted should be > 0L
      b.chain.storedStateCounts shouldBe counts
      b.chain.meta.sweepWanted shouldBe None
    finally
      a.shutdown()
      b.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }
