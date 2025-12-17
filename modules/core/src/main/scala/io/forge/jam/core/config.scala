package io.forge.jam.core

/**
 * Chain configuration parameters for different network configurations.
 */
final case class ChainConfig(
  /** Number of validators in the network */
  validatorCount: Int,
  /** Number of cores available for parallel execution */
  coresCount: Int,
  /** Preimage expunge period */
  preimageExpungePeriod: Int,
  /** Slot duration in seconds */
  slotDuration: Int,
  /** Epoch duration in slots */
  epochLength: Int,
  /** Contest duration in slots */
  contestDuration: Int,
  /** Number of tickets each validator can submit per epoch */
  ticketsPerValidator: Int,
  /** Maximum tickets allowed per extrinsic */
  maxTicketsPerExtrinsic: Int,
  /** Rotation period for validators */
  rotationPeriod: Int,
  /** Number of erasure coding pieces per segment */
  numEcPiecesPerSegment: Int,
  /** Maximum gas allowed per block */
  maxBlockGas: Long,
  /** Maximum gas allowed for refinement */
  maxRefineGas: Long,
  /** Minimum public service index */
  minPublicServiceIndex: Long = 1L << 16,
  /** Assurance timeout period in slots */
  assuranceTimeoutPeriod: Int = 5,
  /** Maximum age of lookup anchor in slots */
  maxLookupAnchorAge: Long = 14_000L,
  /** Maximum number of dependencies per work report */
  maxDependencies: Int = 8,
  /** Maximum accumulation gas per work report */
  reportAccGas: Long = 10_000_000L,
  /** Ticket cutoff phase within epoch (tickets rejected when phase >= ticketCutoff) */
  ticketCutoff: Int = 10,
  /** Maximum block history for accumulation */
  maxBlockHistory: Int = 8,
  /** Authorization queue size */
  authQueueSize: Int = 80,
  /** Minimum balance required for a service account */
  serviceMinBalance: Long = 100L,
  /** Additional minimum balance per state item */
  additionalMinBalancePerStateItem: Long = 10L,
  /** Additional minimum balance per state byte */
  additionalMinBalancePerStateByte: Long = 1L
):
  /** Votes per verdict (2/3 * validators + 1 for supermajority) */
  val votesPerVerdict: Int = (2 * validatorCount / 3) + 1

  /** Supermajority threshold: strictly more than 2/3 of validators */
  val superMajority: Int = (2 * validatorCount) / 3

  /** One-third threshold */
  val oneThird: Int = validatorCount / 3

object ChainConfig:
  /**
   * Tiny configuration for fast testing with 6 validators.
   */
  val TINY: ChainConfig = ChainConfig(
    validatorCount = 6,
    coresCount = 2,
    preimageExpungePeriod = 32,
    slotDuration = 6,
    epochLength = 12,
    contestDuration = 10,
    ticketsPerValidator = 3,
    maxTicketsPerExtrinsic = 3,
    rotationPeriod = 4,
    numEcPiecesPerSegment = 1026,
    maxBlockGas = 20_000_000L,
    maxRefineGas = 1_000_000_000L,
    reportAccGas = 10_000_000L,
    ticketCutoff = 10,
    maxBlockHistory = 8,
    authQueueSize = 80,
    serviceMinBalance = 100L,
    additionalMinBalancePerStateItem = 10L,
    additionalMinBalancePerStateByte = 1L
  )

  /**
   * Full production configuration with 1023 validators.
   */
  val FULL: ChainConfig = ChainConfig(
    validatorCount = 1023,
    coresCount = 341,
    preimageExpungePeriod = 19200,
    slotDuration = 6,
    epochLength = 600,
    contestDuration = 500,
    ticketsPerValidator = 2,
    maxTicketsPerExtrinsic = 16,
    rotationPeriod = 10,
    numEcPiecesPerSegment = 6,
    maxBlockGas = 35_000_000L,
    maxRefineGas = 5_000_000_000L,
    reportAccGas = 10_000_000L,
    ticketCutoff = 500,
    maxBlockHistory = 8,
    authQueueSize = 80,
    serviceMinBalance = 100L,
    additionalMinBalancePerStateItem = 10L,
    additionalMinBalancePerStateByte = 1L
  )
