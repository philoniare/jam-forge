package io.forge.jam.core.scodec

import scodec.*
import scodec.bits.*
import scodec.codecs.*
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.*

/**
 * Config-aware codec factories for JAM protocol types.
 *
 * This object provides codec factories that produce codecs parameterized by
 * chain configuration values (validator count, cores count, epoch length, etc.).
 *
 * The generated codecs are compatible with the existing encoding methods in
 * FullJamState and produce identical binary output.
 *
 * == Usage Patterns ==
 *
 * For fixed-size state components:
 * {{{
 * val entropyCodec = ConfigCodecs.entropyPoolCodec
 * val encoded = entropyCodec.encode(entropyPool).require
 * }}}
 *
 * For config-parameterized components:
 * {{{
 * val config = ChainConfig.TINY
 * val validatorCodec = ConfigCodecs.validatorListCodec(config.validatorCount)
 * val queuesCodec = ConfigCodecs.authQueuesCodec(config.coresCount, config.authQueueSize)
 * }}}
 */
object ConfigCodecs:

  // ============================================================================
  // Constants
  // ============================================================================

  /** ValidatorKey size: bandersnatch (32) + ed25519 (32) + bls (144) + metadata (128) */
  val ValidatorKeySize: Int = 336

  /** Metadata size within ValidatorKey */
  val MetadataSize: Int = 128

  /** Entropy pool size (always 4 hashes) */
  val EntropyPoolSize: Int = 4

  // ============================================================================
  // ValidatorKey Data Type and Codec
  // ============================================================================

  /**
   * Data container for ValidatorKey codec (mirrors epoch.ValidatorKey).
   *
   * This type is used within ConfigCodecs to avoid circular dependencies
   * with the protocol module where the actual ValidatorKey lives.
   */
  final case class ValidatorKeyData(
    bandersnatch: BandersnatchPublicKey,
    ed25519: Ed25519PublicKey,
    bls: BlsPublicKey,
    metadata: JamBytes
  )

  /**
   * Codec for a single ValidatorKey.
   *
   * Binary format (336 bytes total):
   * - bandersnatch: 32 bytes
   * - ed25519: 32 bytes
   * - bls: 144 bytes
   * - metadata: 128 bytes
   */
  val validatorKeyCodec: Codec[ValidatorKeyData] =
    (JamCodecs.bandersnatchPublicKeyCodec ::
     JamCodecs.ed25519PublicKeyCodec ::
     JamCodecs.blsPublicKeyCodec ::
     fixedSizeBytes(MetadataSize.toLong, bytes)).xmap(
      { case (bandersnatch, ed25519, bls, metadataBytes) =>
        ValidatorKeyData(bandersnatch, ed25519, bls, JamBytes(metadataBytes.toArray))
      },
      vk => (vk.bandersnatch, vk.ed25519, vk.bls, ByteVector(vk.metadata.toArray))
    )

  // ============================================================================
  // Config-Aware Validator List Codec
  // ============================================================================

  /**
   * Create a codec for a fixed-size list of validators.
   *
   * This matches the encoding in FullJamState.encodeValidatorList:
   * - No length prefix
   * - Each validator encoded sequentially (336 bytes each)
   *
   * @param count The number of validators (from ChainConfig.validatorCount)
   * @return A codec for List[ValidatorKeyData] of exactly `count` elements
   */
  def validatorListCodec(count: Int): Codec[List[ValidatorKeyData]] =
    JamCodecs.fixedSizeList(validatorKeyCodec, count)

  // ============================================================================
  // Config-Aware Entropy Pool Codec
  // ============================================================================

  /**
   * Codec for entropy pool (exactly 4 hashes).
   *
   * This matches the encoding in FullJamState.encodeEntropyPool:
   * - No length prefix
   * - Exactly 4 hashes of 32 bytes each = 128 bytes total
   *
   * The entropy pool size is fixed (not config-dependent).
   */
  val entropyPoolCodec: Codec[List[Hash]] =
    JamCodecs.fixedSizeList(JamCodecs.hashCodec, EntropyPoolSize)

  // ============================================================================
  // Config-Aware Auth Queues Codec
  // ============================================================================

  /**
   * Create a codec for authorization queues.
   *
   * This matches the encoding in FullJamState.encodeAuthQueues:
   * - No length prefix
   * - Fixed-size array: coresCount * queueSize * 32 bytes
   * - Each queue has exactly `queueSize` hashes
   *
   * @param coresCount The number of cores (from ChainConfig.coresCount)
   * @param queueSize The size of each queue (from ChainConfig.authQueueSize, typically 80)
   * @return A codec for List[List[Hash]] representing auth queues
   */
  def authQueuesCodec(coresCount: Int, queueSize: Int): Codec[List[List[Hash]]] =
    // Inner codec: fixed-size list of hashes per queue
    val queueCodec: Codec[List[Hash]] = JamCodecs.fixedSizeList(JamCodecs.hashCodec, queueSize)
    // Outer codec: fixed-size list of queues (one per core)
    JamCodecs.fixedSizeList(queueCodec, coresCount)

  // ============================================================================
  // Config-Aware Auth Pools Codec
  // ============================================================================

  /**
   * Create a codec for authorization pools.
   *
   * This matches the encoding in FullJamState.encodeAuthPools:
   * - For each core: compact length prefix + N x 32-byte hashes
   * - Variable-length inner lists (unlike auth queues which are fixed-size)
   *
   * @param coresCount The number of cores (from ChainConfig.coresCount)
   * @return A codec for List[List[Hash]] representing auth pools
   */
  def authPoolsCodec(coresCount: Int): Codec[List[List[Hash]]] =
    // Inner codec: compact-prefixed list of hashes per pool
    val poolCodec: Codec[List[Hash]] = JamCodecs.compactPrefixedList(JamCodecs.hashCodec)
    // Outer codec: fixed-size list of pools (one per core)
    JamCodecs.fixedSizeList(poolCodec, coresCount)

  // ============================================================================
  // Config-Aware TicketsOrKeys Codec (delegates to JamCodecs)
  // ============================================================================

  /**
   * Create a codec for TicketsOrKeys (safrole gamma_s state).
   *
   * This delegates to JamCodecs.ticketsOrKeysCodec which handles:
   * - Discriminator byte (0 for Tickets, 1 for Keys)
   * - Fixed-size list of epochLength elements
   *
   * @param epochLength The epoch length (from ChainConfig.epochLength)
   * @return A codec for TicketsOrKeys with exactly `epochLength` elements
   */
  def ticketsOrKeysCodec(epochLength: Int): Codec[JamCodecs.TicketsOrKeys] =
    JamCodecs.ticketsOrKeysCodec(epochLength)

  // ============================================================================
  // Helper Methods for Full State Codec Construction
  // ============================================================================

  /**
   * Create codecs for all config-dependent state components at once.
   *
   * This is a convenience method for creating all necessary codecs from a
   * single ChainConfig instance.
   *
   * @param config The chain configuration
   * @return A ConfigCodecSet containing all parameterized codecs
   */
  def fromConfig(config: ChainConfig): ConfigCodecSet =
    ConfigCodecSet(
      validatorList = validatorListCodec(config.validatorCount),
      entropyPool = entropyPoolCodec,
      authQueues = authQueuesCodec(config.coresCount, config.authQueueSize),
      authPools = authPoolsCodec(config.coresCount),
      ticketsOrKeys = ticketsOrKeysCodec(config.epochLength)
    )

  /**
   * Container for all config-parameterized codecs.
   *
   * This allows passing around a complete set of codecs for a given configuration.
   */
  final case class ConfigCodecSet(
    validatorList: Codec[List[ValidatorKeyData]],
    entropyPool: Codec[List[Hash]],
    authQueues: Codec[List[List[Hash]]],
    authPools: Codec[List[List[Hash]]],
    ticketsOrKeys: Codec[JamCodecs.TicketsOrKeys]
  )
