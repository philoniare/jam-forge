package io.forge.jam.node

import java.nio.file.Files

import io.forge.jam.core.JamBytes
import io.forge.jam.network.NodeIdentity
import io.forge.jam.protocol.traces.StateMerklization
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SlotClockSpec extends AnyFunSuite with Matchers:

  test("slot arithmetic against a fake clock") {
    var now = 1000_000_000_000L // ms
    val era = 999_999_000L // seconds → era start at 999_999_000_000 ms
    val clock = new SlotClock(eraStartSeconds = era, slotSeconds = 6, timeSource = () => now)

    // 1_000_000_000_000 - 999_999_000_000 = 1_000_000 ms = 166 slots + 4s
    clock.currentSlot shouldBe 166L
    clock.millisUntilSlot(167) shouldBe 2000L
    clock.slotStartMillis(167) shouldBe era * 1000L + 167 * 6000L

    now += 2000L
    clock.currentSlot shouldBe 167L
    clock.millisUntilSlot(167) shouldBe 0L
  }

  test("pre-era times clamp to slot 0") {
    val clock = new SlotClock(timeSource = () => 0L)
    clock.currentSlot shouldBe 0L
  }

class ChainSpecSpec extends AnyFunSuite with Matchers:

  test("parses a chain spec and derives the ALPN prefix") {
    val good =
      """{
        |  "id": "dev",
        |  "config": "tiny",
        |  "genesis_header_hash": "0xabcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789",
        |  "genesis_state": [
        |    {"key": "0x01000000000000000000000000000000000000000000000000000000000000", "value": "0x11"}
        |  ],
        |  "bootnodes": []
        |}""".stripMargin

    val spec = ChainSpec.fromJson(good).toOption.get
    spec.id shouldBe "dev"
    spec.config.validatorCount shouldBe 6
    spec.alpnPrefix shouldBe "abcdef01"
    spec.genesisState.size shouldBe 1
  }

  test("bootnode parsing") {
    val hex = "ab" * 32
    val bn = Bootnode.parse(s"$hex@127.0.0.1:40000")
    bn.ed25519Hex shouldBe hex
    bn.host shouldBe "127.0.0.1"
    bn.port shouldBe 40000
  }

class NodeIdentitySeedSpec extends AnyFunSuite with Matchers:

  test("identity from a fixed seed is deterministic and TLS-usable") {
    val seed = Array.tabulate[Byte](32)(i => (i + 1).toByte)
    val id1 = NodeIdentity.fromSeed(seed)
    val id2 = NodeIdentity.fromSeed(seed)
    id1.publicKeyBytes shouldBe id2.publicKeyBytes
    id1.altName shouldBe id2.altName
    id1.certificate.verify(id1.keyPair.getPublic)
    NodeIdentity.peerPublicKey(id1.certificate) shouldBe id1.publicKeyBytes
  }

class GenesisBootstrapSpec extends AnyFunSuite with Matchers:

  test("genesis initialization pins the merklized state root and survives restart") {
    val dir = Files.createTempDirectory("jam-node-genesis")
    val keyvals = List(
      io.forge.jam.protocol.traces.KeyValue(
        JamBytes.fromHexUnsafe("0x01000000000000000000000000000000000000000000000000000000000000"),
        JamBytes.fromHexUnsafe("0x11223344")
      ),
      io.forge.jam.protocol.traces.KeyValue(
        JamBytes.fromHexUnsafe("0x02000000000000000000000000000000000000000000000000000000000000"),
        JamBytes.fromHexUnsafe("0x55")
      )
    )
    val expectedRoot = StateMerklization.stateMerklize(keyvals)

    val spec = ChainSpec(
      id = "dev",
      config = io.forge.jam.core.ChainConfig.TINY,
      genesisHeaderBytes = None,
      explicitGenesisHash =
        Some(io.forge.jam.core.primitives.Hash(Array.fill[Byte](32)(9))),
      genesisState = keyvals,
      bootnodes = Nil
    )

    try
      val node1 = new JamNode(spec, NodeConfig(dataDir = dir))
      node1.chain.initializeOrRestore(spec) shouldBe true
      node1.chain.best.stateRoot shouldBe expectedRoot
      node1.shutdownStorageOnly()

      val node2 = new JamNode(spec, NodeConfig(dataDir = dir))
      node2.chain.initializeOrRestore(spec) shouldBe false // restored
      node2.chain.best.stateRoot shouldBe expectedRoot
      node2.shutdownStorageOnly()
    finally
      Files
        .walk(dir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(p => Files.deleteIfExists(p))
  }
