package io.forge.jam.node

import java.io.DataInputStream
import java.net.{DatagramSocket, InetSocketAddress, ServerSocket}
import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, JamBytes, LittleEndian}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.tickets.{RingVrfSignatureSize, TicketEnvelope}
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UByte

/** Ticket pool selection: epoch-bound, ascending, and only tickets the
  * accumulator will retain.
  */
class TicketPoolSpec extends AnyFunSuite with Matchers:

  private def id(b: Int): Hash = Hash(Array.fill(32)(b.toByte))
  private def env: TicketEnvelope = TicketEnvelope(UByte(0), JamBytes(new Array[Byte](RingVrfSignatureSize)))
  private val eta = id(0xee)

  test("only tickets entered for the block's epoch are offered, ascending") {
    val pools = new ExtrinsicPools
    pools.addTicket(id(0x30), env, 1, eta)
    pools.addTicket(id(0x10), env, 2, eta)
    pools.addTicket(id(0x20), env, 2, eta)
    pools.takeTickets(2, eta, Set.empty, 3, 12).map(_._1) shouldBe List(id(0x10), id(0x20))
    pools.takeTickets(1, eta, Set.empty, 3, 12).map(_._1) shouldBe List(id(0x30))
  }

  test("GP: with a saturated accumulator only ids below its E-th smallest are offered (n ⊆ γ'_a)") {
    val pools = new ExtrinsicPools
    val accumulated = Set(0x20, 0x40).map(b => JamBytes(id(b).bytes.toArray))
    Seq(0x10, 0x30, 0x50).foreach(b => pools.addTicket(id(b), env, 0, eta))
    // E = 2: 0x10 ranks first and pushes 0x40 out; 0x30 would then rank third of {10, 20, 30}.
    pools.takeTickets(0, eta, accumulated, 5, 2).map(_._1) shouldBe List(id(0x10))
    // E = 3: {10, 20, 30} survive; 0x50 would rank fifth.
    pools.takeTickets(0, eta, accumulated, 5, 3).map(_._1) shouldBe List(id(0x10), id(0x30))
    pools.takeTickets(0, eta, accumulated, 5, 4).map(_._1) shouldBe List(id(0x10), id(0x30))
    pools.takeTickets(0, eta, accumulated, 5, 12).map(_._1) shouldBe List(id(0x10), id(0x30), id(0x50))
  }

  test("tickets verified against another fork's η_2 are not offered") {
    val pools = new ExtrinsicPools
    pools.addTicket(id(0x10), env, 2, eta)
    pools.addTicket(id(0x20), env, 2, id(0xdd))
    pools.takeTickets(2, eta, Set.empty, 3, 12).map(_._1) shouldBe List(id(0x10))
    pools.takeTickets(2, id(0xdd), Set.empty, 3, 12).map(_._1) shouldBe List(id(0x20))
  }

  test("pruning drops earlier epochs") {
    val pools = new ExtrinsicPools
    pools.addTicket(id(1), env, 3, eta)
    pools.addTicket(id(2), env, 4, eta)
    pools.pruneTicketsBefore(4)
    pools.ticketCount shouldBe 1
    pools.takeTickets(4, eta, Set.empty, 3, 12).map(_._1) shouldBe List(id(2))
  }

/** Node start-up and networking resilience: data directory creation, the
  * committed devnet spec, redialing lost peers, and JIP-3 telemetry.
  */
class NodeResilienceSpec extends AnyFunSuite with Matchers:

  private val baseDir =
    sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")
  private val interopSpecPath = baseDir.resolve("scripts/interop/jam-forge-devnet-spec.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def specFor(id: String, genesis: Genesis): ChainSpec =
    ChainSpec(
      id = id,
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

  private def tempDir(p: String): Path = Files.createTempDirectory(p)
  private def cleanup(dir: Path): Unit =
    if Files.exists(dir) then
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  private def freeUdpPort(): Int =
    val s = new DatagramSocket(0)
    try s.getLocalPort
    finally s.close()

  private def eventually(timeoutMillis: Long)(cond: => Boolean): Boolean =
    val deadline = System.currentTimeMillis() + timeoutMillis
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(100)
    cond

  test("a node creates a missing data directory") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val root = tempDir("jam-datadir")
    val dataDir = root.resolve("does/not/exist")
    var node: JamNode = null
    try
      node = new JamNode(specFor("datadir-devnet", genesis), NodeConfig(dataDir = dataDir, slotTicking = false))
      node.chain.initializeOrRestore(node.spec) shouldBe true
      Files.isDirectory(dataDir.resolve("state")) shouldBe true
    finally
      if node != null then node.shutdownStorageOnly()
      cleanup(root)
  }

  test("the committed devnet spec loads and a validator authors on it") {
    if !Files.exists(interopSpecPath) then cancel(s"$interopSpecPath not present")
    val spec = ChainSpec.load(interopSpecPath).fold(err => fail(err), identity)
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    spec.genesisHeaderHash shouldBe Hash(
      io.forge.jam.core.Hashing.blake2b256(genesis.header.encode.toArray).bytes.toArray
    )
    val dir = tempDir("jam-interop-spec")
    var node: JamNode = null
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false))
      node.chain.initializeOrRestore(spec)
      node.enableAuthoring((0 until 6).map(ValidatorKeySet.dev))
      node.authorSlot(1).isDefined shouldBe true
    finally
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)
  }

  test("a bootnode that is down at start is dialed again until it comes up") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val spec = specFor("redial-devnet", genesis)
    val portA = freeUdpPort()
    val seedA = Array.fill(32)(7.toByte)
    val keyA = io.forge.jam.network.NodeIdentity.fromSeed(seedA).publicKeyBytes
    val dirA = tempDir("jam-redial-a")
    val dirB = tempDir("jam-redial-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeB = new JamNode(
        spec.copy(bootnodes = List(Bootnode(JamNode.hex(keyA), "127.0.0.1", portA))),
        NodeConfig(dataDir = dirB, slotTicking = false, redialMillis = 200L)
      ).start()
      nodeB.distribution.peers shouldBe empty

      nodeA = new JamNode(
        spec,
        NodeConfig(dataDir = dirA, listenPort = portA, ed25519Seed = Some(seedA), slotTicking = false)
      ).start()
      withClue("B reconnects to A once A is up: ") {
        eventually(20000)(nodeB.distribution.peers.exists(c => java.util.Arrays.equals(c.peerKey, keyA))) shouldBe true
      }
    finally
      if nodeB != null then nodeB.shutdown()
      if nodeA != null then nodeA.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("JIP-3: the node sends its information, then best-block events") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val spec = specFor("telemetry-devnet", genesis)
    val server = new ServerSocket(0)
    server.setSoTimeout(20000)
    val dir = tempDir("jam-telemetry")
    var node: JamNode = null
    try
      node = new JamNode(
        spec,
        NodeConfig(
          dataDir = dir,
          slotTicking = false,
          telemetry = Some(new InetSocketAddress("127.0.0.1", server.getLocalPort))
        )
      ).start()
      val socket = server.accept()
      socket.setSoTimeout(20000)
      val in = new DataInputStream(socket.getInputStream)
      def readMessage(): Array[Byte] =
        val size = new Array[Byte](4)
        in.readFully(size)
        val content = new Array[Byte](LittleEndian.get(size, 0, 4).toInt)
        in.readFully(content)
        content

      val info = readMessage()
      info(0) shouldBe 1.toByte
      val params = io.forge.jam.protocol.accumulation.ConstantsBlob.build(spec.config)
      info.slice(1, 1 + params.length) shouldBe params
      info.slice(1 + params.length, 33 + params.length) shouldBe spec.genesisHeaderHash.bytes.toArray

      node.enableAuthoring((0 until 6).map(ValidatorKeySet.dev))
      val head = node.authorSlot(1).getOrElse(fail("author slot 1"))

      // Events: 40 authoring, 42 authored, then 11 best block changed for slot 1.
      val events = Iterator.continually(readMessage()).take(10).find(_(8) == 11.toByte).getOrElse(fail("no event 11"))
      LittleEndian.get(events, 9, 4) shouldBe 1L
      events.slice(13, 45) shouldBe head.hash.bytes.toArray
      socket.close()
    finally
      if node != null then node.shutdown()
      server.close()
      cleanup(dir)
  }
