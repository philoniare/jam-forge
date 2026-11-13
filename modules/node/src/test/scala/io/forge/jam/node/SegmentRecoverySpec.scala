package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.db.ShardStore
import io.forge.jam.network.{JamnpConfig, JamnpNode, NodeIdentity, StreamKind}
import io.forge.jam.protocol.refine.{AvailabilitySpecifier, DaShards, ImportResolver}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Imported segments rebuilt from the availability layer: peers each hold one validator's custody unit and serve its
  * segment shards on CE 140; a node holding none rebuilds the segment and its page's paged proof from R of them,
  * derives its justification and checks it against the segments-root.
  */
class SegmentRecoverySpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private val netConfig = JamnpConfig(genesisHashPrefix = "deadbeef")

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  test("segments and their justifications are rebuilt from CE 140 shards of R peers, and verify against the segments-root") {
    val exports = 70 // two pages of paged proofs
    val exported = (0 until exports).map(i => Array.fill[Byte](Csegmentsize.toInt)((i * 3 + 1).toByte))
    val bundle = Array.fill[Byte](1000)(9)
    val root = AvailabilitySpecifier.segmentsRoot(exported)
    val all = DaShards.buildAll(bundle, exported, config).fold(e => fail(e), identity)
    val erasureRoot = io.forge.jam.core.primitives.Hash(
      io.forge.jam.core.merkle.BinaryMerkle.Tree(all.map(_.leaf)).root
    )
    val holders = Seq(1, 3, 4) // more than R = 2
    val dirs = (holders :+ -1).map(i => Files.createTempDirectory(s"jam-seg-recovery-$i"))
    val stores = dirs.map(d => ShardStore.open(d))
    val services = stores.map(s => new ShardService(s, config))
    holders.zipWithIndex.foreach((v, k) => stores(k).put(erasureRoot, v, all(v).encode))
    val servers = holders.map(_ => new JamnpNode(NodeIdentity.generate(), netConfig))
    val client = new JamnpNode(NodeIdentity.generate(), netConfig)
    try
      servers.zipWithIndex.foreach { (n, k) =>
        n.registerHandler(StreamKind.SegmentShardRequestVerified, services(k).segmentShardHandler(verified = true))
        n.start(new InetSocketAddress("127.0.0.1", 0))
      }
      val conns = servers.map(n => client.connect(new InetSocketAddress("127.0.0.1", n.boundPort)).get(10, TimeUnit.SECONDS))
      val holdsOf = conns.zip(holders).toMap
      val total = exports + ImportResolver.pagedProofCount(exports)
      val wanted = List(2, 66)
      val pages = wanted.map(i => exports + i / ImportResolver.PageSize).distinct
      val rebuilt = services.last
        .reconstructSegments(erasureRoot, total, wanted ++ pages, conns, c => Seq(holdsOf(c)))
        .fold(e => fail(e), identity)
      val byIndex = (wanted ++ pages).zip(rebuilt).toMap
      for i <- wanted do
        byIndex(i).toList shouldBe exported(i).toList
        val j0 = ImportResolver
          .justificationFromPagedProof(byIndex(exports + i / ImportResolver.PageSize), i, exports)
          .getOrElse(fail(s"no justification for $i"))
        ImportResolver.verify(root, i, byIndex(i), j0) shouldBe true

      // With fewer than R holders reachable there is nothing to rebuild.
      services.last.reconstructSegments(erasureRoot, total, wanted, conns.take(1), c => Seq(holdsOf(c))).isLeft shouldBe true
    finally
      client.shutdown()
      servers.foreach(_.shutdown())
      services.foreach(_.shutdown())
      stores.foreach(_.close())
      dirs.foreach(cleanup)
  }
