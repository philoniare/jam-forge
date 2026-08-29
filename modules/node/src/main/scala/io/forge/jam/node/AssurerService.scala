package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes, constants}
import io.forge.jam.core.primitives.{Ed25519Signature, ValidatorIndex}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.crypto.Ed25519ZebraWrapper

/** Assurer role: after each imported block, signs an availability bitfield of
  * the cores with pending reports for every held validator key, anchored at
  * the new head, then pools and distributes the assurances via CE 141.
  */
final class AssurerService(
    chain: ChainManager,
    distribution: DistributionService,
    pools: ExtrinsicPools,
    validatorKeys: Seq[ValidatorKeySet]
) extends LazyLogging:

  private val ed25519ByPublic: Map[Seq[Byte], ValidatorKeySet] =
    validatorKeys.map(k => k.ed25519Public.toSeq -> k).toMap

  /** Import listener: assure pending cores anchored at the new head. */
  def onImported(head: ChainManager#Head, block: Block): Unit =
    val view = chain.stateView()
    val reports = view.cores.reports
    if !reports.exists(_.isDefined) then return

    // Bitfield over cores, LSB-first within each byte.
    val bitfield = new Array[Byte]((chain.config.coresCount + 7) / 8)
    reports.zipWithIndex.foreach { case (r, core) =>
      if r.isDefined then
        bitfield(core / 8) = (bitfield(core / 8) | (1 << (core % 8))).toByte
    }

    val dataHash = Hashing.blake2b256(head.hash.bytes ++ bitfield)
    val message = constants.JAM_AVAILABLE_BYTES ++ dataHash.bytes

    val activeSet = view.validators.current
    var produced = 0
    activeSet.zipWithIndex.foreach { case (vk, idx) =>
      ed25519ByPublic.get(vk.ed25519.bytes.toArray.toSeq).foreach { keys =>
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
    }
    if produced > 0 then
      logger.debug(
        s"assured ${reports.count(_.isDefined)} pending core(s) with $produced key(s) " +
          s"at anchor ${head.hash.toHex.take(18)}"
      )
