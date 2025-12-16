package io.forge.jam.core.scodec

import scodec.*
import scodec.bits.*
import scodec.codecs.*
import spire.math.UByte
import io.forge.jam.core.primitives.*
import io.forge.jam.core.types.tickets.{TicketMark, TicketEnvelope}
import io.forge.jam.core.types.epoch.{ValidatorKey, EpochMark, EpochValidatorKey}
import io.forge.jam.core.types.service.{ServiceInfo, ServiceData, ServiceAccount}
import io.forge.jam.core.types.history.{ReportedWorkPackage, HistoricalBeta, HistoricalMmr, HistoricalBetaContainer}

/**
 * Protocol type codecs using scodec for core JAM protocol structures.
 *
 * This object provides scodec Codec instances for protocol types that were
 * previously encoded/decoded using JamEncoder/JamDecoder. These codecs
 * produce identical binary output for compatibility.
 *
 * == Included Types ==
 * - ValidatorKey: Full validator key (336 bytes)
 * - TicketMark: Ticket identifier (33 bytes)
 * - TicketEnvelope: Ticket with signature (785 bytes)
 * - EpochMark: Epoch marker (config-dependent size)
 * - EpochValidatorKey: Short validator key (64 bytes)
 * - ServiceInfo: Service metadata (89 bytes)
 * - HistoricalBetaContainer: History container (variable size)
 * - CoreStatistics-related types via compact integer encoding
 */
object ProtocolCodecs:

  // ============================================================================
  // Constants
  // ============================================================================

  /** ValidatorKey total size: bandersnatch (32) + ed25519 (32) + bls (144) + metadata (128) */
  val ValidatorKeySize: Int = 336

  /** Metadata size within ValidatorKey */
  val MetadataSize: Int = 128

  /** Ring VRF signature size in bytes */
  val RingVrfSignatureSize: Int = 784

  /** TicketEnvelope size: 1 byte attempt + 784 bytes signature */
  val TicketEnvelopeSize: Int = 1 + RingVrfSignatureSize

  /** TicketMark size: 32 bytes id + 1 byte attempt */
  val TicketMarkSize: Int = Hash.Size + 1

  /** EpochValidatorKey size: 32 + 32 = 64 bytes */
  val EpochValidatorKeySize: Int = BandersnatchPublicKey.Size + Ed25519PublicKey.Size

  /** ServiceInfo fixed size: 89 bytes */
  val ServiceInfoSize: Int = 89

  /** ReportedWorkPackage size: 64 bytes (2 x 32-byte hashes) */
  val ReportedWorkPackageSize: Int = Hash.Size * 2

  // ============================================================================
  // ValidatorKey Codec (336 bytes)
  // ============================================================================

  /**
   * Use ValidatorKey codec from its companion object (epoch.scala).
   * Note: Not a 'given' to avoid infinite loop - just import the given from epoch.scala instead.
   */
  val validatorKeyCodec: Codec[ValidatorKey] = summon[Codec[ValidatorKey]]

  // ============================================================================
  // TicketMark Codec (33 bytes)
  // ============================================================================

  /**
   * Use TicketMark codec from JamCodecs.
   * Note: Not a 'given' - just use JamCodecs.ticketMarkCodec directly.
   */
  val ticketMarkCodec: Codec[TicketMark] = JamCodecs.ticketMarkCodec

  // ============================================================================
  // TicketEnvelope Codec (785 bytes)
  // ============================================================================

  /**
   * Codec for TicketEnvelope (785 bytes fixed size).
   *
   * Binary format:
   * - attempt: 1 byte (UByte)
   * - signature: 784 bytes (Ring VRF signature)
   */
  /** Use TicketEnvelope codec from its companion object (tickets.scala) */
  given ticketEnvelopeCodec: Codec[TicketEnvelope] = summon[Codec[TicketEnvelope]]

  // ============================================================================
  // EpochValidatorKey Codec (64 bytes)
  // ============================================================================

  /**
   * Codec for EpochValidatorKey (64 bytes fixed size).
   *
   * Binary format:
   * - bandersnatch: 32 bytes
   * - ed25519: 32 bytes
   */
  given epochValidatorKeyCodec: Codec[EpochValidatorKey] =
    (JamCodecs.bandersnatchPublicKeyCodec :: JamCodecs.ed25519PublicKeyCodec).xmap(
      { case (bandersnatch, ed25519) =>
        EpochValidatorKey(bandersnatch, ed25519)
      },
      evk => (evk.bandersnatch, evk.ed25519)
    )

  // ============================================================================
  // EpochMark Codec (config-dependent size)
  // ============================================================================

  /**
   * Create a codec for EpochMark with a specific validator count.
   *
   * Binary format:
   * - entropy: 32 bytes
   * - ticketsEntropy: 32 bytes
   * - validators: validatorCount * 64 bytes (no length prefix)
   *
   * @param validatorCount The number of validators (from ChainConfig)
   */
  def epochMarkCodec(validatorCount: Int): Codec[EpochMark] =
    (JamCodecs.hashCodec ::
     JamCodecs.hashCodec ::
     JamCodecs.fixedSizeList(epochValidatorKeyCodec, validatorCount)).xmap(
      { case (entropy, ticketsEntropy, validators) =>
        EpochMark(entropy, ticketsEntropy, validators)
      },
      em => (em.entropy, em.ticketsEntropy, em.validators)
    )

  // ============================================================================
  // ServiceInfo Codec (89 bytes)
  // ============================================================================

  /**
   * Use ServiceInfo codec from its companion object (service.scala).
   * Note: Not a 'given' to avoid infinite loop - just import the given from service.scala instead.
   */
  val serviceInfoCodec: Codec[ServiceInfo] = summon[Codec[ServiceInfo]]

  // ============================================================================
  // ServiceData Codec
  // ============================================================================

  /**
   * Codec for ServiceData (wraps ServiceInfo).
   */
  given serviceDataCodec: Codec[ServiceData] =
    serviceInfoCodec.xmap(
      si => ServiceData(si),
      sd => sd.service
    )

  // ============================================================================
  // ServiceAccount Codec
  // ============================================================================

  /**
   * Codec for ServiceAccount.
   *
   * Binary format:
   * - id: 4 bytes (u32 LE)
   * - data: ServiceData (89 bytes)
   */
  given serviceAccountCodec: Codec[ServiceAccount] =
    (uint32L :: serviceDataCodec).xmap(
      { case (id, data) =>
        ServiceAccount(id & 0xFFFFFFFFL, data)
      },
      sa => (sa.id & 0xFFFFFFFFL, sa.data)
    )

  // ============================================================================
  // ReportedWorkPackage Codec (64 bytes)
  // ============================================================================

  /**
   * Codec for ReportedWorkPackage (64 bytes fixed size).
   *
   * Binary format:
   * - hash: 32 bytes
   * - exportsRoot: 32 bytes
   */
  given reportedWorkPackageCodec: Codec[ReportedWorkPackage] =
    (JamCodecs.hashCodec :: JamCodecs.hashCodec).xmap(
      { case (hash, exportsRoot) =>
        ReportedWorkPackage(hash, exportsRoot)
      },
      rwp => (rwp.hash, rwp.exportsRoot)
    )

  // ============================================================================
  // HistoricalBeta Codec (variable size)
  // ============================================================================

  /**
   * Codec for HistoricalBeta.
   *
   * Binary format:
   * - headerHash: 32 bytes
   * - beefyRoot: 32 bytes
   * - stateRoot: 32 bytes
   * - reported: compact length prefix + N * 64 bytes
   */
  given historicalBetaCodec: Codec[HistoricalBeta] =
    (JamCodecs.hashCodec ::
     JamCodecs.hashCodec ::
     JamCodecs.hashCodec ::
     JamCodecs.compactPrefixedList(reportedWorkPackageCodec)).xmap(
      { case (headerHash, beefyRoot, stateRoot, reported) =>
        HistoricalBeta(headerHash, beefyRoot, stateRoot, reported)
      },
      hb => (hb.headerHash, hb.beefyRoot, hb.stateRoot, hb.reported.sortBy(_.hash.toHex))
    )

  // ============================================================================
  // HistoricalMmr Codec (variable size)
  // ============================================================================

  /**
   * Codec for HistoricalMmr (MMR peaks as list of optional hashes).
   *
   * Binary format:
   * - peaks: compact length prefix + N * optional hash (1 or 33 bytes each)
   */
  given historicalMmrCodec: Codec[HistoricalMmr] =
    JamCodecs.compactPrefixedList(JamCodecs.optionCodec(JamCodecs.hashCodec)).xmap(
      peaks => HistoricalMmr(peaks),
      hmr => hmr.peaks
    )

  // ============================================================================
  // HistoricalBetaContainer Codec (variable size)
  // ============================================================================

  /**
   * Codec for HistoricalBetaContainer.
   *
   * Binary format:
   * - history: compact length prefix + N * HistoricalBeta
   * - mmr: HistoricalMmr
   */
  given historicalBetaContainerCodec: Codec[HistoricalBetaContainer] =
    (JamCodecs.compactPrefixedList(historicalBetaCodec) :: historicalMmrCodec).xmap(
      { case (history, mmr) =>
        HistoricalBetaContainer(history, mmr)
      },
      hbc => (hbc.history, hbc.mmr)
    )

  // ============================================================================
  // CoreStatisticsRecord Codec (variable size - compact integers)
  // ============================================================================

  /**
   * Codec for CoreStatisticsRecord.
   *
   * Represents per-core statistics with all fields as compact integers:
   * - dataSize: compact integer
   * - assuranceCount: compact integer
   * - importsCount: compact integer
   * - extrinsicsCount: compact integer
   * - extrinsicsSize: compact integer
   * - exportsCount: compact integer
   * - packageSize: compact integer
   * - gasUsed: compact integer
   */
  final case class CoreStatisticsRecord(
    dataSize: Long,
    assuranceCount: Long,
    importsCount: Long,
    extrinsicsCount: Long,
    extrinsicsSize: Long,
    exportsCount: Long,
    packageSize: Long,
    gasUsed: Long
  )

  given coreStatisticsRecordCodec: Codec[CoreStatisticsRecord] =
    (JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger).xmap(
      { case (dataSize, assuranceCount, importsCount, extrinsicsCount, extrinsicsSize, exportsCount, packageSize, gasUsed) =>
        CoreStatisticsRecord(dataSize, assuranceCount, importsCount, extrinsicsCount, extrinsicsSize, exportsCount, packageSize, gasUsed)
      },
      csr => (csr.dataSize, csr.assuranceCount, csr.importsCount, csr.extrinsicsCount, csr.extrinsicsSize, csr.exportsCount, csr.packageSize, csr.gasUsed)
    )

  // ============================================================================
  // ServiceStatisticsEntry Codec (variable size)
  // ============================================================================

  /**
   * Data container for service statistics entry.
   *
   * Binary format:
   * - serviceId: 4 bytes (u32 LE)
   * - preimagesCount: compact integer
   * - preimagesSize: compact integer
   * - refinesCount: compact integer
   * - refinesGas: compact integer
   * - importsCount: compact integer
   * - extrinsicsCount: compact integer
   * - extrinsicsSize: compact integer
   * - exportsCount: compact integer
   * - accumulatesCount: compact integer
   * - accumulatesGas: compact integer
   * - transfersCount: compact integer
   * - transfersGas: compact integer
   */
  final case class ServiceStatisticsEntryData(
    serviceId: Long,
    preimagesCount: Long,
    preimagesSize: Long,
    refinesCount: Long,
    refinesGas: Long,
    importsCount: Long,
    extrinsicsCount: Long,
    extrinsicsSize: Long,
    exportsCount: Long,
    accumulatesCount: Long,
    accumulatesGas: Long,
    transfersCount: Long,
    transfersGas: Long
  )

  given serviceStatisticsEntryCodec: Codec[ServiceStatisticsEntryData] =
    (uint32L ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger ::
     JamCodecs.compactInteger).xmap(
      { case (serviceId, pc, ps, rc, rg, ic, xc, xs, ec, ac, ag, tc, tg) =>
        ServiceStatisticsEntryData(serviceId & 0xFFFFFFFFL, pc, ps, rc, rg, ic, xc, xs, ec, ac, ag, tc, tg)
      },
      e => (e.serviceId & 0xFFFFFFFFL, e.preimagesCount, e.preimagesSize, e.refinesCount, e.refinesGas,
            e.importsCount, e.extrinsicsCount, e.extrinsicsSize, e.exportsCount,
            e.accumulatesCount, e.accumulatesGas, e.transfersCount, e.transfersGas)
    )

  // ============================================================================
  // Helper Methods for Config-Parameterized Codecs
  // ============================================================================

  /**
   * Create a codec for a fixed-size list of validators.
   *
   * @param count The number of validators (from ChainConfig.validatorCount)
   */
  def validatorListCodec(count: Int): Codec[List[ValidatorKey]] =
    JamCodecs.fixedSizeList(validatorKeyCodec, count)
