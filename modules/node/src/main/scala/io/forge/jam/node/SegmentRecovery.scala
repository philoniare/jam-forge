package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.network.JamnpConnection
import io.forge.jam.protocol.refine.{DaShards, ImportResolver}

/** What is needed to rebuild a package's exported segments from the availability layer, by segments-root: the
  * report's erasure root, how many segments it exported and its core (which fixes who holds which shard). Fed with
  * the guarantees of imported blocks; the oldest entries go first.
  */
final class ExportIndex(maxEntries: Int = ExportIndex.Capacity):
  private val byRoot = java.util.Collections.synchronizedMap(
    new java.util.LinkedHashMap[Hash, ExportIndex.Entry](256, 0.75f, false) {
      override def removeEldestEntry(e: java.util.Map.Entry[Hash, ExportIndex.Entry]) = size > maxEntries
    }
  )

  /** Every report's bundle length by erasure root: what rebuilding its bundle from shards needs. */
  private val bundleLengths = java.util.Collections.synchronizedMap(
    new java.util.LinkedHashMap[Hash, Integer](256, 0.75f, false) {
      override def removeEldestEntry(e: java.util.Map.Entry[Hash, Integer]) = size > maxEntries
    }
  )

  def record(guarantees: Iterable[GuaranteeExtrinsic]): Unit =
    guarantees.foreach { g =>
      val spec = g.report.packageSpec
      bundleLengths.put(spec.erasureRoot, Integer.valueOf(spec.length.toLong.toInt))
      if spec.exportsCount.toInt > 0 then
        byRoot.put(spec.exportsRoot, ExportIndex.Entry(spec.erasureRoot, spec.exportsCount.toInt, g.report.coreIndex.toInt))
    }

  def get(segmentsRoot: Hash): Option[ExportIndex.Entry] = Option(byRoot.get(segmentsRoot))

  /** The length of the bundle a recorded report's erasure root commits to. */
  def bundleLength(erasureRoot: Hash): Option[Int] = Option(bundleLengths.get(erasureRoot)).map(_.intValue)

object ExportIndex:
  final case class Entry(erasureRoot: Hash, exports: Int, core: Int)

  /** Reports remembered; imports may name exports up to the lookup-anchor age back. */
  val Capacity: Int = 16384

/** Rebuilds imported segments from assurers' erasure-coded shards (CE 139/140) when neither we nor any guarantor
  * holds them: for each segment, its own chunks and its page's paged-proof segment are reconstructed from R verified
  * shards, its justification is derived from the paged proof, and the segment is used only if it verifies against its
  * segments-root. Blocks on the network: runs on I/O threads.
  */
final class SegmentRecovery(
    chain: ChainManager,
    shards: ShardService,
    exports: ExportIndex,
    peers: () => Iterable[JamnpConnection]
) extends LazyLogging:

  /** Segments `indices` of `segmentsRoot` with their proofs, verified against the root; None when they cannot be
    * rebuilt. With `localOnly` only the shards in our own custody are used: a guarantor holds every validator's
    * shards of the reports it guaranteed, so it can serve their segments long after its segment cache let them go,
    * without asking the network on a peer's behalf.
    */
  def recover(
      segmentsRoot: Hash,
      indices: Seq[Int],
      localOnly: Boolean = false
  ): Option[Map[Int, (Array[Byte], List[Array[Byte]])]] =
    exports.get(segmentsRoot) match
      case None =>
        logger.debug(s"no known report exported ${segmentsRoot.toHex.take(18)}; cannot rebuild its segments")
        None
      case Some(entry) =>
        val pages = indices.map(_ / ImportResolver.PageSize).distinct.map(entry.exports + _)
        val wanted = (indices ++ pages).distinct.toList
        val total = entry.exports + ImportResolver.pagedProofCount(entry.exports)
        // Read only when peers are asked: which shards each one holds.
        lazy val sets =
          val view = chain.stateView()
          List(view.validators.current, view.validators.previous)
        val rebuilt = shards.reconstructSegments(
          entry.erasureRoot,
          total,
          wanted,
          if localOnly then Nil else peers(),
          conn =>
            sets
              .flatMap(_.zipWithIndex.collect { case (vk, v) if vk.ed25519 == conn.peerPublicKey => v })
              .map(DaShards.shardIndexFor(entry.core, _, chain.config))
        )
        rebuilt match
          case Left(err) =>
            logger.warn(s"cannot rebuild segments of ${segmentsRoot.toHex.take(18)}: $err")
            None
          case Right(segs) =>
            val byIndex = wanted.zip(segs).toMap
            val out = indices.map { i =>
              val seg = byIndex(i)
              val proof = ImportResolver
                .justificationFromPagedProof(byIndex(entry.exports + i / ImportResolver.PageSize), i, entry.exports)
                .filter(ImportResolver.verify(segmentsRoot, i, seg, _))
              i -> proof.map(seg -> _)
            }
            if out.forall(_._2.isDefined) then Some(out.map((i, sp) => i -> sp.get).toMap)
            else
              logger.warn(s"segments rebuilt for ${segmentsRoot.toHex.take(18)} do not verify against it")
              None
