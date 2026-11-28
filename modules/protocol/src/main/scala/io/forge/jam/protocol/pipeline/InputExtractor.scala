package io.forge.jam.protocol.pipeline

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.vrfs.BandersnatchWrapper
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.header.Header
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.history.ReportedWorkPackage
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.assurance.AssuranceTypes.*
import io.forge.jam.protocol.accumulation.AccumulationInput
import io.forge.jam.protocol.history.HistoryTypes.*
import io.forge.jam.protocol.authorization.AuthorizationTypes.*
import io.forge.jam.protocol.preimage.PreimageTypes.*
import io.forge.jam.protocol.statistics.StatisticsTypes.*
import io.forge.jam.protocol.dispute.DisputeTypes.*

/** Extracts STF inputs from block and state.
  */
object InputExtractor:
  /** Extract SafroleInput from block. The entropy source in the header is a VRF
    * signature from which we extract the output.
    */

  def extractSafroleInput(block: Block): SafroleInput =
    extractSafroleInput(block, entropyOutput(block.header))

  /** As [[extractSafroleInput]], with the header's entropy output already worked out. */
  def extractSafroleInput(block: Block, entropy: Hash): SafroleInput =
    SafroleInput(slot = block.header.slot.toLong, entropy = entropy, extrinsic = block.extrinsic.tickets)

  /** The output of the header's entropy source, a 96-byte Bandersnatch IETF VRF signature; zeroes when it has none. */
  def entropyOutput(header: Header): Hash = entropyOutput(header.entropySource.toArray)

  def entropyOutput(entropySource: Array[Byte]): Hash =
    BandersnatchWrapper.ensureLibraryLoaded()
    val vrfOutput =
      try Option(BandersnatchWrapper.getIetfVrfOutput(entropySource)).filter(_.length == 32)
      catch case _: RuntimeException => None
    vrfOutput.fold(Hash(new Array[Byte](32)))(Hash(_))

  /** Extract DisputeInput from block.
    */
  def extractDisputeInput(block: Block): DisputeInput =
    DisputeInput(disputes = block.extrinsic.disputes)

  /** Extract AssuranceInput from block.
    */
  def extractAssuranceInput(block: Block): AssuranceInput =
    AssuranceInput(
      assurances = block.extrinsic.assurances,
      slot = block.header.slot.toLong,
      parent = block.header.parent
    )

  /** Extract AccumulationInput from available reports and slot.
    */
  def extractAccumulationInput(
      availableReports: List[WorkReport],
      slot: Long
  ): AccumulationInput =
    AccumulationInput(
      slot = slot,
      reports = availableReports
    )

  /** Extract HistoricalInput from block and accumulate root.
    */
  def extractHistoryInput(block: Block, accumulateRoot: Hash, headerHash: Hash): HistoricalInput =
    val workPackages = block.extrinsic.guarantees
      .map { guarantee =>
        ReportedWorkPackage(
          hash = guarantee.report.packageSpec.hash,
          exportsRoot =
            guarantee.report.packageSpec.exportsRoot // Segment root is the exports root
        )
      }
      .sortBy(wp => JamBytes(wp.hash.bytes))

    HistoricalInput(
      headerHash = headerHash,
      parentStateRoot = block.header.parentStateRoot,
      accumulateRoot = accumulateRoot,
      slot = block.header.slot.toLong,
      workPackages = workPackages
    )

  /** Extract AuthInput from block.
    */
  def extractAuthInput(block: Block): AuthInput =
    // Consumed authorizations come from guarantees
    val auths = block.extrinsic.guarantees.map { guarantee =>
      Auth(
        core = guarantee.report.coreIndex,
        authHash = guarantee.report.authorizerHash
      )
    }
    AuthInput(
      slot = block.header.slot.toLong,
      auths = auths
    )

  /** Extract PreimageInput from block.
    */
  def extractPreimageInput(block: Block, slot: Long): PreimageInput =
    PreimageInput(
      preimages = block.extrinsic.preimages,
      slot = slot
    )

  /** Extract StatInput from block.
    */
  def extractStatInput(block: Block): StatInput =
    StatInput(
      slot = block.header.slot.toLong,
      authorIndex = block.header.authorIndex.toInt.toLong,
      extrinsic = StatExtrinsic(
        tickets = block.extrinsic.tickets,
        preimages = block.extrinsic.preimages,
        guarantees = block.extrinsic.guarantees,
        assurances = block.extrinsic.assurances,
        disputes = block.extrinsic.disputes
      )
    )


/** Companion for [[BlockImporter]]. */
