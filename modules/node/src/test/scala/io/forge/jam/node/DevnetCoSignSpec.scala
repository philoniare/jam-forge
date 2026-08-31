package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Gas, ServiceId, Timeslot}
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.network.StreamKind
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

/** CE 134 co-signing: two guarantor nodes each hold exactly ONE validator key
  * assigned to the target core, so neither can credential a report alone
  * (>= 2 signatures required). The receiving guarantor shares the bundle via
  * CE 134; the peer re-executes it in the PVM and returns its signature; the
  * combined 2-credential guarantee lands on-chain.
  */
class DevnetCoSignSpec extends AnyFunSuite with Matchers:

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

  test("two single-key guarantors co-sign a report via CE 134") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val nullAuthPreimage = genesis.state.keyvals
      .map(_.value.toArray)
      .find(_.length == 50)
      .getOrElse(fail("NULL Authorizer preimage not found in genesis"))
    val authCodeHash = Hashing.blake2b256(nullAuthPreimage)

    val spec = ChainSpec(
      id = "cosign-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

    val dirs = Seq("jam-cosign-a", "jam-cosign-b", "jam-cosign-c").map(tempDir)
    var nodes: Seq[JamNode] = Nil
    try
      nodes = dirs.map(d => new JamNode(spec, NodeConfig(dataDir = d, slotTicking = false)).start())
      val Seq(nodeA, nodeB, nodeC) = nodes
      nodeA.enableAuthoring(devKeys)

      // Mesh: A announces to B and C; B reaches C for CE 134.
      nodeA.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeB.listenPort))
      nodeA.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeC.listenPort))
      nodeB.connectPeer(new java.net.InetSocketAddress("127.0.0.1", nodeC.listenPort))

      nodeA.authorSlot(1).isDefined shouldBe true
      nodeA.authorSlot(2).isDefined shouldBe true
      awaitSync(nodeA, nodeB)
      awaitSync(nodeA, nodeC)

      // The report will be signed for slot 3: find the validators assigned
      // to core 0 then and give one key each to B and C.
      val view = nodeB.chain.stateView()
      val assignments =
        GuarantorService.coreAssignments(view.entropy.pool(2), 3L, spec.config)
      val core0Validators = assignments.zipWithIndex.collect { case (0, idx) => idx }
      core0Validators.size should be >= 2
      nodeB.enableGuaranteeing(Seq(devKeys(core0Validators(0))))
      nodeC.enableGuaranteeing(Seq(devKeys(core0Validators(1))))

      val anchor = view.beta.history(view.beta.history.size - 2)
      val serviceCodeHash = view.accumulation.serviceAccounts
        .find(_.id == 0).map(_.data.service.codeHash).getOrElse(fail("no service 0"))
      val wp = WorkPackage(
        authCodeHost = ServiceId(0),
        authCodeHash = authCodeHash,
        context = Context(
          anchor = anchor.headerHash,
          stateRoot = anchor.stateRoot,
          beefyRoot = anchor.beefyRoot,
          lookupAnchor = anchor.headerHash,
          lookupAnchorSlot = Timeslot(UInt(view.timeslot.toInt)),
          prerequisites = List.empty
        ),
        authorization = JamBytes.empty,
        authorizerConfig = JamBytes.empty,
        items = List(
          WorkItem(
            service = ServiceId(0),
            codeHash = serviceCodeHash,
            payload = JamBytes("co-sign me".getBytes("UTF-8")),
            refineGasLimit = Gas(100_000_000L),
            accumulateGasLimit = Gas(10_000_000L),
            importSegments = List.empty,
            extrinsic = List.empty,
            exportCount = UShort(0)
          )
        )
      )

      // Builder submits to B only; B cannot credential alone.
      val connBuilderToB = nodeA.connectPeer(
        new java.net.InetSocketAddress("127.0.0.1", nodeB.listenPort)
      )
      val stream =
        connBuilderToB.openStream(StreamKind.WorkPackageSubmission).get(10, TimeUnit.SECONDS)
      stream.send(Array[Byte](0, 0) ++ wp.encode.toArray)
      stream.finish()

      // B refines, obtains C's CE 134 signature, and distributes CE 135 to A.
      val poolDeadline = System.currentTimeMillis() + 60000
      while nodeA.pools.guaranteeCount == 0 && System.currentTimeMillis() < poolDeadline do
        Thread.sleep(50)
      nodeA.pools.guaranteeCount shouldBe 1

      nodeA.authorSlot(3).isDefined shouldBe true
      val block =
        nodeA.chain.decodeBlock(nodeA.chain.blockStore.getBlock(nodeA.chain.best.hash).get).toOption.get
      block.extrinsic.guarantees.size shouldBe 1
      val guarantee = block.extrinsic.guarantees.head
      guarantee.signatures.size shouldBe 2
      guarantee.signatures.map(_.validatorIndex.value.toInt).toSet shouldBe
        Set(core0Validators(0), core0Validators(1))
      nodeA.chain.stateView().cores.reports(0).isDefined shouldBe true
    finally
      nodes.foreach(n => if n != null then n.shutdown())
      dirs.foreach(cleanup)
  }

  private def awaitSync(a: JamNode, b: JamNode): Unit =
    val deadline = System.currentTimeMillis() + 30000
    while b.chain.best.hash != a.chain.best.hash && System.currentTimeMillis() < deadline do
      Thread.sleep(100)
    b.chain.best.hash shouldBe a.chain.best.hash
