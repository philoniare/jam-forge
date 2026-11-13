package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.{Gas, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.{WorkItem, WorkItemImportSegment}
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.network.StreamKind
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UShort

/** A package that imports a segment another package exported, named by that package's hash: the guarantor
  * resolves it through recent history, gets the segment and its proof, refines with it, and the report carries the
  * segment-root lookup that the chain accepts.
  */
class DevnetImportSpec extends AnyFunSuite with Matchers:

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")
  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  test("an import of an earlier package's export, by package hash, is resolved, fetched, verified and reported") {
    given Decoder[Genesis] = Genesis.decoder
    val genesis = Option.when(Files.exists(genesisPath))(decode[Genesis](Files.readString(genesisPath)).toOption).flatten
      .getOrElse(cancel("dev genesis not available"))
    val authCodeHash = Hashing.blake2b256(
      genesis.state.keyvals.map(_.value.toArray).find(_.length == 50).getOrElse(fail("no NULL authorizer"))
    )
    val spec = ChainSpec("import-devnet", ChainConfig.TINY, Some(genesis.header.encode.toArray), None, genesis.state.keyvals, Nil)
    val dirs = Seq("jam-import-a", "jam-import-b").map(Files.createTempDirectory(_))
    var nodes: Seq[JamNode] = Nil
    try
      nodes = dirs.map(d => new JamNode(spec, NodeConfig(d, slotTicking = false)).start())
      val Seq(a, b) = nodes
      a.enableAuthoring(devKeys)
      b.enableGuaranteeing(devKeys)
      val conn = a.connectPeer(new java.net.InetSocketAddress("127.0.0.1", b.listenPort))
      a.authorSlot(1).isDefined shouldBe true
      a.authorSlot(2).isDefined shouldBe true
      awaitSync(a, b)

      val view = b.chain.stateView()
      val codeHash = view.accumulation.serviceAccounts.find(_.id == 0).map(_.data.service.codeHash).get
      def pkg(payload: String, exports: Int, imports: List[WorkItemImportSegment]) =
        val history = b.chain.stateView().beta.history
        val anchorBlock = history(history.size - 2) // the newest entry's state root is filled in by the next block
        WorkPackage(
          ServiceId(0),
          authCodeHash,
          Context(
            anchorBlock.headerHash,
            Timeslot(anchorBlock.slot.toInt),
            anchorBlock.stateRoot,
            anchorBlock.beefyRoot,
            anchorBlock.headerHash,
            Timeslot(anchorBlock.slot.toInt),
            anchorBlock.stateRoot,
            Nil
          ),
          JamBytes.empty,
          JamBytes.empty,
          List(
            WorkItem(
              ServiceId(0),
              codeHash,
              JamBytes(payload.getBytes("UTF-8")),
              Gas(100_000_000L),
              Gas(10_000_000L),
              imports,
              Nil,
              UShort(exports)
            )
          )
        )
      def submit(wp: WorkPackage): Unit =
        val s = conn.openStream(StreamKind.WorkPackageSubmission).get(10, TimeUnit.SECONDS)
        s.send(Array[Byte](0, 0) ++ wp.encode.toArray)
        s.send(Array.emptyByteArray)
        s.finish()
      def awaitGuarantees(n: Int): Unit =
        val deadline = System.currentTimeMillis() + 30000
        while a.pools.guaranteeCount < n && System.currentTimeMillis() < deadline do Thread.sleep(50)
        a.pools.guaranteeCount shouldBe n

      // Package 1 declares one export; slot 3 includes its report, which puts it in recent history.
      val exporter = pkg("export", exports = 1, imports = Nil)
      submit(exporter)
      awaitGuarantees(1)
      val included = a.authorSlot(3).getOrElse(fail("slot 3 not authored"))
      val report1 = a.chain.block(included.hash).get.extrinsic.guarantees.head.report
      val exporterHash = report1.packageSpec.hash
      exporterHash shouldBe Hashing.blake2b256(exporter.encode.toArray)
      awaitSync(a, b)
      b.chain.stateView().beta.history.flatMap(_.reported).map(_.hash) should contain(exporterHash)

      // Package 2 imports segment 0 of package 1, by package hash.
      val importer = pkg("import", exports = 0, imports = List(WorkItemImportSegment(exporterHash, UShort(0x8000))))
      submit(importer)
      // Core 0 is engaged until package 1's report is available; the guarantee waits in A's pool meanwhile.
      awaitGuarantees(1) // package 1's left the pool when included; this is package 2's
      val report2 = a.pools.pendingGuarantees.head.report
      report2.segmentRootLookup.map(l => l.workPackageHash -> l.segmentTreeRoot) shouldBe
        List(exporterHash -> report1.packageSpec.exportsRoot)
      report2.results.head.refineLoad.imports.toInt shouldBe 1

      // Authoring while core 0 is still engaged (nobody assures here), with a guarantee for a package already on
      // chain back in the pool: the block goes in without either, the one that can never go in leaves the pool, and
      // the one that may go in once the core frees stays.
      val onChain = a.chain.block(included.hash).get.extrinsic.guarantees.head
      a.pools.addGuarantee(onChain)
      a.pools.guaranteeCount shouldBe 2
      val s4 = a.authorSlot(4).getOrElse(fail("slot 4 not authored"))
      a.chain.block(s4.hash).get.extrinsic.guarantees shouldBe Nil
      a.pools.pendingGuarantees.map(_.report.packageSpec.hash) shouldBe List(report2.packageSpec.hash)
    finally
      nodes.foreach(n => if n != null then n.shutdown())
      dirs.foreach(cleanup)
  }

  private def awaitSync(a: JamNode, b: JamNode): Unit =
    val deadline = System.currentTimeMillis() + 30000
    while b.chain.best.hash != a.chain.best.hash && System.currentTimeMillis() < deadline do Thread.sleep(100)
    b.chain.best.hash shouldBe a.chain.best.hash
