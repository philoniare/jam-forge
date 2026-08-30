package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes, constants}
import io.forge.jam.core.primitives.{Ed25519Signature, Hash, ValidatorIndex}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.crypto.Ed25519ZebraWrapper
import io.forge.jam.network.StreamKind

/** Assurer role: after each imported block, acquires and verifies DA custody
  * for every pending report, then signs a per-validator
  * availability bitfield of the cores whose shards it actually holds,
  * anchored at the new head, and pools/distributes the assurances (CE 141).
  */
final class AssurerService(
    chain: ChainManager,
    distribution: DistributionService,
    pools: ExtrinsicPools,
    validatorKeys: Seq[ValidatorKeySet],
    shards: ShardService,
    shardStore: io.forge.jam.db.ShardStore
) extends LazyLogging:

  private val ed25519ByPublic: Map[Seq[Byte], ValidatorKeySet] =
    validatorKeys.map(k => k.ed25519Public.toSeq -> k).toMap

  /** Import listener: acquire custody, then assure held cores. */
  def onImported(head: ChainManager#Head, block: Block): Unit =
    val view = chain.stateView()
    val reports = view.cores.reports
    if !reports.exists(_.isDefined) then return

    val activeSet = view.validators.current
    val heldIndexes = activeSet.zipWithIndex.flatMap { case (vk, idx) =>
      ed25519ByPublic.get(vk.ed25519.bytes.toArray.toSeq).map(keys => idx -> keys)
    }
    if heldIndexes.isEmpty then return

    // Custody per (core, validator index): local store, else CE 137 pull.
    val custody: Map[(Int, Int), Boolean] =
      (for
        (r, core) <- reports.zipWithIndex
        assignment <- r.toList
        (idx, _) <- heldIndexes
      yield
        val root = assignment.report.packageSpec.erasureRoot
        (core, idx) -> acquireCustody(root, idx)
      ).toMap

    var produced = 0
    heldIndexes.foreach { case (idx, keys) =>
      val bitfield = new Array[Byte]((chain.config.coresCount + 7) / 8)
      var any = false
      reports.zipWithIndex.foreach { case (r, core) =>
        if r.isDefined && custody.getOrElse((core, idx), false) then
          bitfield(core / 8) = (bitfield(core / 8) | (1 << (core % 8))).toByte
          any = true
      }
      if any then
        val dataHash = Hashing.blake2b256(head.hash.bytes ++ bitfield)
        val message = constants.JAM_AVAILABLE_BYTES ++ dataHash.bytes
        val assurance = AssuranceExtrinsic(
          anchor = head.hash,
          bitfield = JamBytes(bitfield),
          validatorIndex = ValidatorIndex(idx),
          signature =
            Ed25519Signature(Ed25519ZebraWrapper.sign(keys.ed25519Secret, message))
        )
        pools.addAssurance(assurance)
        distribution.distributeAssuranceToAll(assurance)
        produced += 1
    }
    if produced > 0 then
      logger.debug(
        s"assured ${reports.count(_.isDefined)} pending core(s) with $produced key(s) " +
          s"at anchor ${head.hash.toHex.take(18)}"
      )

  /** True when verified shards for (erasure-root, validator index) are in
    * custody, pulling them from peers via CE 137 when absent.
    */
  private def acquireCustody(erasureRoot: Hash, validatorIndex: Int): Boolean =
    if shardStore.has(erasureRoot, validatorIndex) then true
    else
      distribution.peers.exists { conn =>
        shards
          .fetchShards(conn, StreamKind.ShardDistribution, erasureRoot, validatorIndex)
          .map { s =>
            shardStore.put(erasureRoot, validatorIndex, s.encode)
            true
          }
          .getOrElse(false)
      } || {
        logger.warn(
          s"no custody for shard $validatorIndex of ${erasureRoot.toHex.take(18)}; withholding assurance"
        )
        false
      }
