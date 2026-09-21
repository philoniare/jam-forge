package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.types.block.Block
import io.forge.jam.network.StreamKind
import io.forge.jam.protocol.traces.TraceStep
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** End-to-end CE 129 (state request) between two networked nodes: node A is
  * seeded with a real fuzz-trace pre-state as its genesis; node B connects,
  * opens a CE 129 stream, requests the full key range of A's genesis state,
  * and the response's key/value pairs are checked against A's own state
  * reads (`chain.readRawState`), matching the `TwoNodeSyncSpec` devnet
  * pattern.
  */
class StateRangeSpec extends AnyFunSuite with Matchers:
  private given io.circe.Decoder[TraceStep] =
    TraceStep.decoder(using summon[io.circe.Decoder[Block]])

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val tracesDir =
    baseDir.resolve("jam-conformance/fuzz-reports/0.7.2/traces")

  /** Find a trace step whose pre-state has a handful of keyvals to query. */
  private def findStepWithState(): Option[TraceStep] =
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
        case Right(step) if step.preState.keyvals.sizeIs >= 3 => step
      }

  private def tempDir(prefix: String): Path = Files.createTempDirectory(prefix)

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  test("node B fetches node A's genesis state range over CE 129") {
    val step = findStepWithState().getOrElse(
      cancel("no fuzz trace with genesis state available (jam-conformance corpus not present)")
    )

    val spec = ChainSpec(
      id = "state-range-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = None,
      explicitGenesisHash = Some(step.block.header.parent),
      genesisState = step.preState.keyvals,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-node-a-state")
    val dirB = tempDir("jam-node-b-state")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false)).start()
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false)).start()

      nodeA.chain.best.stateRoot shouldBe step.preState.stateRoot
      val expectedKeys = step.preState.keyvals.map(_.key).distinct.sorted
      val expected = expectedKeys.map(k => (k, nodeA.chain.readRawState(k).getOrElse(
        fail(s"node A is missing genesis key ${k.toHex}")
      )))

      val connB = nodeB.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeA.listenPort))

      val stream = connB.openStream(StreamKind.StateRequest).get(10, TimeUnit.SECONDS)
      val received = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[java.lang.Boolean]()
      stream.onMessage(received.offer(_))
      stream.onClosed(() => closed.offer(true))

      val request = SyncCodec.encodeStateRequest(
        headerHash = nodeA.chain.best.hash,
        start = JamBytes.zeros(31),
        end = JamBytes.fill(31)(0xff.toByte),
        maxSize = Int.MaxValue.toLong
      )
      stream.send(request)
      stream.finish()

      closed.poll(20, TimeUnit.SECONDS) should not be null

      val messages = mutable.ListBuffer.empty[Array[Byte]]
      var next = received.poll()
      while next != null do
        messages += next
        next = received.poll()

      messages.size shouldBe 2 // boundary-node message, then pairs message
      val boundaryNodes = SyncCodec.decodeBoundaryNodes(messages(0))
      val pairs = SyncCodec.decodeStatePairs(messages(1))

      boundaryNodes should not be empty
      pairs.map(_._1) shouldBe expectedKeys
      pairs shouldBe expected
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }
