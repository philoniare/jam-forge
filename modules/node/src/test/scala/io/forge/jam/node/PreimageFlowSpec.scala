package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Hash, ServiceId}
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.types.extrinsic.Preimage
import io.forge.jam.network.StreamKind
import io.forge.jam.protocol.accumulation.StateKey
import io.forge.jam.protocol.traces.{Genesis, KeyValue}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PreimageFlowSpec extends AnyFunSuite with Matchers:
  test("CE142 announcement is serviceId(4) ++ hash(32) ++ length(4) = 40 bytes") {
    val h = Hash(Array.tabulate[Byte](32)(_.toByte))
    val bytes = PreimageCodec.encodeAnnouncement(7L, h, 1000L)
    bytes.length shouldBe 40
    val Right((sid, hash, len)) = PreimageCodec.decodeAnnouncement(bytes): @unchecked
    sid shouldBe 7L; hash shouldBe h; len shouldBe 1000L
  }
  test("announcement decode rejects wrong length") {
    PreimageCodec.decodeAnnouncement(new Array[Byte](39)).isLeft shouldBe true
  }

  private val baseDir =
    sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath =
    baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def tempDir(prefix: String): Path = Files.createTempDirectory(prefix)

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  test("CE142/143: node A announces a pooled preimage and node B fetches + pools it") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )

    val serviceId = 0L
    val blob = Array.tabulate[Byte](64)(i => i.toByte)
    val hash = Hashing.blake2b256(blob)

    val infoKey = StateKey.computePreimageInfoStateKey(serviceId, blob.length, JamBytes(hash.bytes))
    val solicitedKv = KeyValue(infoKey, StateKey.encodePreimageInfoValue(Nil))

    val specA = ChainSpec(
      id = "preimage-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val specB = specA.copy(genesisState = genesis.state.keyvals :+ solicitedKv)

    val dirA = tempDir("jam-pi-a")
    val dirB = tempDir("jam-pi-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(specA, NodeConfig(dataDir = dirA, slotTicking = false)).start()
      nodeB = new JamNode(specB, NodeConfig(dataDir = dirB, slotTicking = false)).start()

      nodeA.pools.addPreimage(Preimage(requester = ServiceId(serviceId.toInt), blob = JamBytes(blob)))

      val connAtoB = nodeA.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeB.listenPort))
      nodeA.preimages.announce(serviceId, blob, List(connAtoB))

      val deadline = System.currentTimeMillis() + 15000
      while nodeB.pools.takePreimages().isEmpty && System.currentTimeMillis() < deadline do
        Thread.sleep(50)

      val pooled = nodeB.pools.takePreimages()
      pooled should not be empty
      Hashing.blake2b256(pooled.head.blob.toArray) shouldBe hash
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("CE143: node A serves a preimage it only knows about from an imported block") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )

    val serviceId = 0L
    val blob = Array.tabulate[Byte](48)(i => (i * 3).toByte)
    val hash = Hashing.blake2b256(blob)

    val infoKey = StateKey.computePreimageInfoStateKey(serviceId, blob.length, JamBytes(hash.bytes))
    val solicitedKv = KeyValue(infoKey, StateKey.encodePreimageInfoValue(Nil))

    val specA = ChainSpec(
      id = "preimage-import-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals :+ solicitedKv,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-pi-import-a")
    val dirB = tempDir("jam-pi-import-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(specA, NodeConfig(dataDir = dirA, slotTicking = false)).start()
      nodeB = new JamNode(specA, NodeConfig(dataDir = dirB, slotTicking = false)).start()

      nodeA.enableAuthoring((0 until 6).map(ValidatorKeySet.dev))
      nodeA.pools.addPreimage(Preimage(requester = ServiceId(serviceId.toInt), blob = JamBytes(blob)))

      val genesisSlot = genesis.header.slot.value.toLong
      val head = nodeA.authorSlot(genesisSlot + 1)
      head.isDefined shouldBe true

      val imported = nodeA.chain
        .decodeBlock(nodeA.chain.blockStore.getBlock(nodeA.chain.best.hash).get)
        .toOption
        .get
      imported.extrinsic.preimages.exists(p => Hashing.blake2b256(p.blob.toArray) == hash) shouldBe true
      nodeA.pools.findPreimage(hash) shouldBe empty

      val connBtoA = nodeB.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeA.listenPort))

      var response: Array[Byte] = null
      val deadline = System.currentTimeMillis() + 15000
      while response == null && System.currentTimeMillis() < deadline do
        val requestStream = connBtoA.openStream(StreamKind.PreimageRequest).get(10, TimeUnit.SECONDS)
        val collected = new LinkedBlockingQueue[Array[Byte]]()
        requestStream.onMessage(collected.put)
        requestStream.send(hash.bytes)
        requestStream.finish()
        response = collected.poll(2, TimeUnit.SECONDS)
        if response == null then Thread.sleep(200)

      response should not be null
      Hashing.blake2b256(response) shouldBe hash
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }
