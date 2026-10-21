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
  maxLookupAnchorAge: Long = 14_400L,
  /** Maximum number of dependencies per work report */
  maxDependencies: Int = 8,
  /** Maximum number of work items/results per work report */
  maxWorkItems: Int = 16,
  /**
   * Erasure-coding piece size in bytes: `2 * ecOriginalShards(validatorCount)`.
   */
  ecPieceSize: Int = 684,
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

  def isValidValidatorCount(z: Int): Boolean =
    z >= 6 && z % 3 == 0 && z <= 3 * coresCount

object ChainConfig:
  def ecPieceSizeFor(validatorCount: Int, segmentSize: Int = 4104): Int =
    val bound = validatorCount / 3 + 2 // d ranges over N_bound = {0 .. bound-1}
    var d = bound - 1
    while d > 0 && segmentSize % (2 * d) != 0 do d -= 1
    2 * d

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
    additionalMinBalancePerStateByte = 1L,
    maxLookupAnchorAge = 24L,
    ecPieceSize = 6
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
    maxBlockGas = 3_500_000_000L,
    maxRefineGas = 5_000_000_000L,
    reportAccGas = 10_000_000L,
    ticketCutoff = 500,
    maxBlockHistory = 8,
    authQueueSize = 80,
    serviceMinBalance = 100L,
    additionalMinBalancePerStateItem = 10L,
    additionalMinBalancePerStateByte = 1L
  )

  /**
   * Fuzz configuration from davxy/jam-conformance#215: `FULL` with shorter periods and fewer cores, so a fuzz
   * session crosses many epochs and the validator set can be resized by `designate`.
   *
   * Genesis has `V = 3C = 255` validators. Designates may resize the set to any size in
   * `{6, 9, ..., 255}` (GP 0.8.0 §safrole `eq:valcount`), so code must size validator lists from the
   * state (`|kappa|`, `|lambda|`), not from `validatorCount`.
   */
  val FUZZ: ChainConfig = FULL.copy(
    validatorCount = 255,
    coresCount = 85,
    preimageExpungePeriod = 900,
    epochLength = 60,
    contestDuration = 50,
    maxTicketsPerExtrinsic = 4,
    rotationPeriod = 5,
    maxBlockGas = 875_000_000L,
    maxRefineGas = 1_250_000_000L,
    maxLookupAnchorAge = 600L,
    ticketCutoff = 50,
    ecPieceSize = ecPieceSizeFor(255),
    numEcPiecesPerSegment = 4104 / ecPieceSizeFor(255)
  )

  /** Spec names accepted by `JAM_FUZZ_SPEC`, `--spec` and chain-spec files. */
  val byName: Map[String, ChainConfig] = Map("tiny" -> TINY, "full" -> FULL, "fuzz" -> FUZZ)

  /** Parse a spec name (case-insensitive, surrounding whitespace ignored). */
  def fromName(name: String): Either[String, ChainConfig] =
    byName.get(name.trim.toLowerCase).toRight(
      s"unknown spec '${name.trim}' (expected ${byName.keys.toList.sorted.mkString("|")})"
    )

  /** The spec name of `config`, or "custom" for a config not in [[byName]]. */
  def nameOf(config: ChainConfig): String =
    byName.collectFirst { case (n, c) if c == config => n }.getOrElse("custom")
