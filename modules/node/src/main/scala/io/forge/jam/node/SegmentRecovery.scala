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
final class ExportIndex(maxEntries: Int = ExportIndex.Capacity, store: RoleStore = RoleStore.none):
  private val byRoot = new ExportIndex.Remembered[ExportIndex.Entry](
    "role:export:",
    maxEntries,
    store,
    e => java.nio.ByteBuffer.allocate(40).put(e.erasureRoot.bytes.toArray).putInt(e.exports).putInt(e.core).array(),
    b => ExportIndex.Entry(ExportIndex.hash(b), b.getInt, b.getInt)
  )

  /** Every report's bundle length by erasure root: what rebuilding its bundle from shards needs. */
  private val bundleLengths = new ExportIndex.Remembered[Int](
    "role:bundle:",
    maxEntries,
    store,
    length => java.nio.ByteBuffer.allocate(4).putInt(length).array(),
    _.getInt
  )

  def record(guarantees: Iterable[GuaranteeExtrinsic]): Unit =
    val specs = guarantees.toList.map(g => g.report.packageSpec -> g.report.coreIndex.toInt)
    bundleLengths.putAll(specs.map((spec, _) => spec.erasureRoot -> spec.length.toLong.toInt))
    byRoot.putAll(specs.collect {
      case (spec, core) if spec.exportsCount.toInt > 0 =>
        spec.exportsRoot -> ExportIndex.Entry(spec.erasureRoot, spec.exportsCount.toInt, core)
    })

  def get(segmentsRoot: Hash): Option[ExportIndex.Entry] = byRoot.get(segmentsRoot)

  /** The length of the bundle a recorded report's erasure root commits to. */
  def bundleLength(erasureRoot: Hash): Option[Int] = bundleLengths.get(erasureRoot)

object ExportIndex:
  final case class Entry(erasureRoot: Hash, exports: Int, core: Int)

  /** Reports remembered; imports may name exports up to the lookup-anchor age back. */
  val Capacity: Int = 16384

  private def hash(b: java.nio.ByteBuffer): Hash =
    val bytes = new Array[Byte](32)
    b.get(bytes)
    Hash(bytes)

  private final class Remembered[V](
      prefix: String,
      capacity: Int,
      store: RoleStore,
      encode: V => Array[Byte],
      decode: java.nio.ByteBuffer => V
  ):
    private val entries = new java.util.LinkedHashMap[Hash, (Long, V)](256, 0.75f, false)
    private var nextSeq = 0L

    store
      .scan(prefix)
      .map { (_, bytes) =>
        val b = java.nio.ByteBuffer.wrap(bytes)
        (b.getLong, hash(b), decode(b))
      }
      .sortBy(_._1)
      .foreach { (seq, key, value) =>
        entries.put(key, (seq, value))
        nextSeq = seq + 1
      }

    private def name(key: Hash): String = prefix + key.toHex

    def get(key: Hash): Option[V] = synchronized(Option(entries.get(key)).map(_._2))

    def putAll(items: Iterable[(Hash, V)]): Unit = synchronized {
      val puts = scala.collection.mutable.LinkedHashMap.empty[String, Array[Byte]]
      val deletes = scala.collection.mutable.LinkedHashSet.empty[String]
      items.foreach { (key, value) =>
        val known = entries.get(key)
        if known == null || known._2 != value then
          val seq = if known == null then nextSeq else known._1
          if known == null then nextSeq += 1
          entries.put(key, (seq, value))
          val body = encode(value)
          puts.put(
            name(key),
            java.nio.ByteBuffer.allocate(40 + body.length).putLong(seq).put(key.bytes.toArray).put(body).array()
          )
          deletes -= name(key)
          while entries.size > capacity do
            val eldest = entries.keySet.iterator.next()
            entries.remove(eldest)
            puts -= name(eldest)
            deletes += name(eldest)
      }
      if puts.nonEmpty || deletes.nonEmpty then store.write(puts, deletes)
    }

/** Rebuilds imported segments from assurers' erasure-coded shards (CE 139/140) when neither we nor any guarantor
  * holds them: for each segment, its own chunks and its page's paged-proof segment are reconstructed from R verified
  * shards, its justification is derived from the paged proof, and the segment is used only if it verifies against its
  * segments-root. Blocks on the network: runs on I/O threads.
  */
final class SegmentRecovery(
    chain: ChainReader,
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
