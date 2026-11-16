package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, LittleEndian}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import io.forge.jam.network.{JamnpConfig, JamnpNode, JamnpStream, NodeIdentity, StreamKind}
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** A validator that is still catching up with its peers holds its duties: what it would author, guarantee or assure
  * is built on a head the network has moved past.
  */
class SyncGateSpec extends AnyFunSuite with Matchers:

  private val second = 1000L

  test("a node with no peers, or level with them, is not held") {
    val gate = new SyncGate(lagSlots = 2, patienceMillis = 30 * second)
    gate.holding(ourBest = 10, peerBests = Nil, nowMillis = 0) shouldBe false
    gate.holding(ourBest = 10, peerBests = List(10, 11, 12), nowMillis = 0) shouldBe false
    gate.holding(ourBest = 10, peerBests = List(3, 4), nowMillis = 0) shouldBe false
  }

  test("a node is held while most of its peers are more than the lag ahead") {
    val gate = new SyncGate(lagSlots = 2, patienceMillis = 30 * second)
    gate.holding(ourBest = 10, peerBests = List(40), nowMillis = 0) shouldBe true
    gate.holding(ourBest = 10, peerBests = List(40, 41, 12), nowMillis = 0) shouldBe true
    // One peer claiming a far head does not hold a node whose other peers are level with it.
    gate.holding(ourBest = 10, peerBests = List(1000, 11, 10), nowMillis = 0) shouldBe false
    gate.holding(ourBest = 10, peerBests = List(1000, 11), nowMillis = 0) shouldBe false
  }

  test("the hold lasts only while blocks keep arriving, and ends once the node has caught up") {
    val gate = new SyncGate(lagSlots = 2, patienceMillis = 30 * second)
    gate.holding(10, List(100), nowMillis = 0) shouldBe true
    gate.holding(10, List(100), nowMillis = 29 * second) shouldBe true
    // Nothing arrived for the whole patience window: the claim is not backed by blocks.
    gate.holding(10, List(100), nowMillis = 31 * second) shouldBe false
    // Blocks arrive again: held again, for as long as they keep coming.
    gate.holding(11, List(100), nowMillis = 32 * second) shouldBe true
    gate.holding(60, List(100), nowMillis = 61 * second) shouldBe true
    gate.holding(98, List(100), nowMillis = 62 * second) shouldBe false
  }

  test("a slot tick that arrives after its slot has passed is not acted on, and duties wait until the time given") {
    val gate = new SyncGate(lagSlots = 2, patienceMillis = 30 * second)
    gate.onTime(slot = 100, clockSlot = 100, resumeAtMillis = 5 * second) shouldBe true
    gate.holding(ourBest = 10, peerBests = Nil, nowMillis = 1 * second) shouldBe false

    // The process was frozen: the tick for slot 101 is delivered ten slots late.
    gate.onTime(slot = 101, clockSlot = 111, resumeAtMillis = 12 * second) shouldBe false
    // The peer heads noted before the pause are as old as our own: level with them, or with none, we still wait.
    gate.holding(ourBest = 10, peerBests = Nil, nowMillis = 2 * second) shouldBe true
    gate.holding(ourBest = 10, peerBests = List(10, 11), nowMillis = 11 * second) shouldBe true
    // An on-time tick in between does not end the wait.
    gate.onTime(slot = 112, clockSlot = 112, resumeAtMillis = 6 * second) shouldBe true
    gate.holding(ourBest = 10, peerBests = List(10, 11), nowMillis = 11 * second) shouldBe true
    gate.holding(ourBest = 10, peerBests = List(10, 11), nowMillis = 12 * second) shouldBe false
    // From then on the peers' heads decide again.
    gate.holding(ourBest = 10, peerBests = List(40), nowMillis = 13 * second) shouldBe true
  }

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")
  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  private def eventually(what: String, seconds: Int = 20)(cond: => Boolean): Unit =
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(seconds.toLong)
    while !cond && System.nanoTime() < deadline do Thread.sleep(20)
    assert(cond, s"timed out waiting for $what")

  private def header(parent: Hash, slot: Long): Array[Byte] =
    val h = new Array[Byte](120)
    System.arraycopy(parent.bytes.toArray, 0, h, 0, 32)
    System.arraycopy(LittleEndian.encode(slot, 4), 0, h, 96, 4)
    h

  test("a validator does not author while its peer announces a head far past its own, and authors again once it leaves") {
    if !Files.exists(genesisPath) then cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    given Decoder[Genesis] = Genesis.decoder
    val genesis = decode[Genesis](Files.readString(genesisPath)).fold(e => fail(e.toString), identity)
    val spec = ChainSpec(
      id = "sync-gate-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dir: Path = Files.createTempDirectory("jam-sync-gate")
    var node: JamNode = null
    val peer = new JamnpNode(NodeIdentity.generate(), JamnpConfig(genesisHashPrefix = spec.alpnPrefix))
    peer.registerHandler(StreamKind.BlockRequest, (_, stream: JamnpStream) => stream.onMessage(_ => ()))
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false)).start()
      node.enableAuthoring(devKeys)
      node.authorSlot(1).map(_.slot) shouldBe Some(1L)

      val fin = SyncCodec.HashSlot(node.chain.finalized.hash, node.chain.finalized.slot)
      val conn = peer.connect(new InetSocketAddress("127.0.0.1", node.listenPort)).get(10, TimeUnit.SECONDS)
      val up = conn.openStream(StreamKind.BlockAnnouncement).get(10, TimeUnit.SECONDS)
      up.onMessage(_ => ())
      up.send(SyncCodec.encodeHandshake(fin, Nil))
      up.send(SyncCodec.encodeAnnouncement(header(Hashing.blake2b256(Array[Byte](7)), 50), fin))
      eventually("the peer's head to be noted")(node.sync.peerBestSlots.contains(50L))

      node.authorSlot(2) shouldBe None
      node.chain.best.slot shouldBe 1L

      peer.shutdown()
      eventually("the peer to be gone")(node.sync.peerBestSlots.isEmpty)
      node.authorSlot(3).map(_.slot) shouldBe Some(3L)
    finally
      peer.shutdown()
      if node != null then node.shutdown()
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
  }

  test("a validator back from a pause skips the slot ticks it missed and resumes authoring within two slots") {
    if !Files.exists(genesisPath) then cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    given Decoder[Genesis] = Genesis.decoder
    val genesis = decode[Genesis](Files.readString(genesisPath)).fold(e => fail(e.toString), identity)
    val spec = ChainSpec("resume-devnet", ChainConfig.TINY, Some(genesis.header.encode.toArray), None, genesis.state.keyvals, Nil)
    val dir: Path = Files.createTempDirectory("jam-resume-gate")
    var node: JamNode = null
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false)).start()
      node.enableAuthoring(devKeys)
      val clock = node.slotClock.currentSlot
      // In step with the clock: authored.
      node.slotTick(clock - 20, clock - 20)
      node.chain.best.slot shouldBe clock - 20

      // The tick for the next slot is delivered only now, 19 slots late: its slot is over, nothing is authored for it.
      node.slotTick(clock - 19, clock)
      node.chain.best.slot shouldBe clock - 20
      // The clock's own slot is ticked on time straight after, but what the node knows of its peers is as old
      // as the pause: it waits.
      node.slotTick(clock, clock)
      node.chain.best.slot shouldBe clock - 20

      // Within two slots it authors again.
      eventually("authoring to resume", seconds = 3 * spec.config.slotDuration) {
        val now = node.slotClock.currentSlot
        node.slotTick(now, now)
        node.chain.best.slot > clock - 20
      }
    finally
      if node != null then node.shutdown()
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
  }
