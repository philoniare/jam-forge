package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.types.block.Block
import io.forge.jam.protocol.traces.TraceStep
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** End-to-end block sync between two networked nodes using a real fuzz-trace
  * block: node A treats the trace's pre-state as a checkpoint genesis and
  * imports the trace block (full STF incl. seal verification); node B starts
  * from the same genesis, connects over JAMNP-S, learns A's leaf from the
  * UP 0 handshake, fetches the block via CE 128 and imports it.
  */
class TwoNodeSyncSpec extends AnyFunSuite with Matchers:
  private given io.circe.Decoder[TraceStep] =
    TraceStep.decoder(using summon[io.circe.Decoder[Block]])

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val tracesDir =
    baseDir.resolve("jam-conformance/fuzz-reports/0.8.0/traces")

  /** Find a trace step whose import mutates state (post != pre → the block is
    * expected to import successfully).
    */
  private def findImportableStep(): Option[TraceStep] =
    if !Files.isDirectory(tracesDir) then return None
    val dirs = Files.list(tracesDir).sorted().toArray.map(_.asInstanceOf[Path])
    dirs.iterator
      .filter(Files.isDirectory(_))
      .flatMap { dir =>
        Files
          .list(dir)
          .sorted()
          .toArray
          .map(_.asInstanceOf[Path])
          .iterator
          .filter(_.toString.endsWith(".json"))
          .take(3)
      }
      .take(30)
      .map(p => decode[TraceStep](Files.readString(p)))
      .collectFirst {
        case Right(step) if step.postState.stateRoot != step.preState.stateRoot => step
      }

  private def tempDir(prefix: String): Path = Files.createTempDirectory(prefix)

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  test("node B syncs a real trace block from node A over JAMNP") {
    val step = findImportableStep().getOrElse(
      cancel("no importable fuzz trace available (jam-conformance corpus not present)")
    )

    val spec = ChainSpec(
      id = "trace-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = None,
      // Checkpoint genesis: the trace block's parent hash.
      explicitGenesisHash = Some(step.block.header.parent),
      genesisState = step.preState.keyvals,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-node-a")
    val dirB = tempDir("jam-node-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false)).start()
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false)).start()

      nodeA.chain.best.stateRoot shouldBe step.preState.stateRoot
      nodeB.chain.best.stateRoot shouldBe step.preState.stateRoot

      // Node A imports the trace block locally (checkpoint import).
      val blockBytes = nodeA.chain.encodeBlock(step.block)
      val headA = nodeA.chain.importBlock(blockBytes)
      headA.isRight shouldBe true
      nodeA.chain.best.stateRoot shouldBe step.postState.stateRoot

      // Node B connects; the UP 0 handshake carries A's leaf, B pulls the
      // block via CE 128 and imports it.
      nodeB.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeA.listenPort))

      val deadline = System.currentTimeMillis() + 30000
      while nodeB.chain.best.stateRoot != step.postState.stateRoot &&
        System.currentTimeMillis() < deadline
      do Thread.sleep(100)

      nodeB.chain.best.hash shouldBe nodeA.chain.best.hash
      nodeB.chain.best.stateRoot shouldBe step.postState.stateRoot
      nodeB.chain.hasBlock(nodeA.chain.best.hash) shouldBe true
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("node B's SyncService records node A's announced Final from the UP0 handshake") {
    val step = findImportableStep().getOrElse(
      cancel("no importable fuzz trace available (jam-conformance corpus not present)")
    )

    val spec = ChainSpec(
      id = "trace-devnet-final",
      config = ChainConfig.TINY,
      genesisHeaderBytes = None,
      explicitGenesisHash = Some(step.block.header.parent),
      genesisState = step.preState.keyvals,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-node-final-a")
    val dirB = tempDir("jam-node-final-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false)).start()
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false)).start()

      // Node A imports and directly finalizes the trace block (bypassing
      // depth-based auto finality) so its announced Final is deterministic.
      val blockBytes = nodeA.chain.encodeBlock(step.block)
      val headA = nodeA.chain.importBlock(blockBytes).getOrElse(fail("node A import"))
      nodeA.chain.finalize(headA.hash).isRight shouldBe true
      nodeA.chain.finalized.hash shouldBe headA.hash

      // Node B connects after A's Final has advanced; the UP0 handshake
      // response carries A's Final, which SyncService must record per
      // connection.
      val conn = nodeB.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeA.listenPort))

      val deadline = System.currentTimeMillis() + 15000
      while nodeB.sync.peerFinal(conn).isEmpty && System.currentTimeMillis() < deadline
      do Thread.sleep(50)

      nodeB.sync.peerFinal(conn) shouldBe Some(SyncCodec.HashSlot(headA.hash, headA.slot))
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }
