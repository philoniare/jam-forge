package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.network.StreamKind
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UShort

/** The real guarantor path over the network: a builder submits a work
  * package via CE 146 (bundle submission); the guarantor node runs the actual
  * in-core pipeline — the genesis NULL Authorizer executes for is-authorized
  * and the bootstrap service's refine code runs in the PVM — then signs and
  * distributes the guaranteed report via CE 135, and the author includes it
  * on-chain. The same path from a CE 133 submission, on through assurance and
  * accumulation, is covered by `DevnetAuditorSpec`.
  */
class DevnetGuarantorSpec extends AnyFunSuite with Matchers:

  private val baseDir =
    sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath =
    baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def tempDir(p: String): Path = Files.createTempDirectory(p)
  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  test("CE146 bundle submission -> real is-authorized + refine -> CE135 -> on-chain inclusion") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )

    val nullAuthPreimage = genesis.state.keyvals
      .map(_.value.toArray)
      .find(_.length == 50)
      .getOrElse(fail("NULL Authorizer preimage not found in genesis"))
    val authCodeHash = Hashing.blake2b256(nullAuthPreimage)

    val spec = ChainSpec(
      id = "guarantor-devnet-ce146",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-guar146-a")
    val dirB = tempDir("jam-guar146-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false)).start()
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false)).start()
      nodeA.enableAuthoring(devKeys)
      nodeB.enableGuaranteeing(devKeys)

      val connAtoB =
        nodeA.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeB.listenPort))

      nodeA.authorSlot(1).isDefined shouldBe true
      nodeA.authorSlot(2).isDefined shouldBe true
      awaitSync(nodeA, nodeB)

      val view = nodeB.chain.stateView()
      val history = view.beta.history
      val anchor = history(history.size - 2)
      val serviceCodeHash = view.accumulation.serviceAccounts
        .find(_.id == 0)
        .map(_.data.service.codeHash)
        .getOrElse(fail("service 0 missing"))

      val wp = WorkPackage(
        authCodeHost = ServiceId(0),
        authCodeHash = authCodeHash,
        context = Context(
          anchor = anchor.headerHash,
          anchorSlot = Timeslot(anchor.slot.toInt),
          stateRoot = anchor.stateRoot,
          beefyRoot = anchor.beefyRoot,
          lookupAnchor = anchor.headerHash,
          lookupAnchorSlot = Timeslot(anchor.slot.toInt),
          lookupAnchorStateRoot = anchor.stateRoot,
          prerequisites = List.empty
        ),
        authorization = JamBytes.empty,
        authorizerConfig = JamBytes.empty,
        items = List(
          WorkItem(
            service = ServiceId(0),
            codeHash = serviceCodeHash,
            payload = JamBytes("hello ce146".getBytes("UTF-8")),
            refineGasLimit = Gas(100_000_000L),
            accumulateGasLimit = Gas(10_000_000L),
            importSegments = List.empty,
            extrinsic = List.empty,
            exportCount = UShort(0)
          )
        )
      )

      // Builder (node A) submits the full bundle via CE 146: five messages
      // then FIN. No cross-package imports here, so the segments-root
      // mapping (msg1's tail), extrinsics (msg3), segments (msg4) and
      // justifications (msg5) are all empty.
      val stream =
        connAtoB.openStream(StreamKind.WorkPackageBundleSubmission).get(10, TimeUnit.SECONDS)
      val msg1 = Array[Byte](0, 0) ++ io.forge.jam.core.codec.JamCodecs.encodeCompactInteger(0)
      stream.send(msg1) // core index 0, zero segment-root mappings
      stream.send(wp.encode.toArray) // msg2: work package
      stream.send(Array.emptyByteArray) // msg3: extrinsics
      stream.send(Array.emptyByteArray) // msg4: imported segments
      stream.send(Array.emptyByteArray) // msg5: import proofs
      stream.finish()

      val poolDeadline = System.currentTimeMillis() + 30000
      while nodeA.pools.guaranteeCount == 0 && System.currentTimeMillis() < poolDeadline do
        Thread.sleep(50)
      nodeA.pools.guaranteeCount shouldBe 1

      nodeA.authorSlot(3).isDefined shouldBe true
      val block =
        nodeA.chain.decodeBlock(nodeA.chain.blockStore.getBlock(nodeA.chain.best.hash).get).toOption.get
      block.extrinsic.guarantees.size shouldBe 1

      val includedReport = block.extrinsic.guarantees.head.report
      includedReport.packageSpec.hash shouldBe
        Hashing.blake2b256(wp.encode.toArray)
      includedReport.coreIndex.toInt shouldBe 0
      includedReport.packageSpec.erasureRoot.bytes.toArray should not be new Array[Byte](32)

      nodeA.chain.stateView().cores.reports(0).isDefined shouldBe true
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }

  private def awaitSync(a: JamNode, b: JamNode): Unit =
    val deadline = System.currentTimeMillis() + 30000
    while b.chain.best.hash != a.chain.best.hash && System.currentTimeMillis() < deadline do
      Thread.sleep(100)
    b.chain.best.hash shouldBe a.chain.best.hash
