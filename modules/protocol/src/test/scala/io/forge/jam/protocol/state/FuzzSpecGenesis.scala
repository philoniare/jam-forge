package io.forge.jam.protocol.state

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.codec.codecs.uint32L
import io.forge.jam.core.primitives.{BandersnatchPublicKey, Hash}
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrieStore}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys
import io.forge.jam.protocol.statistics.StatisticsTypes.StatCount
import io.forge.jam.protocol.traces.{KeyValue, RawState, StateKeys, StateMerklization}
import io.forge.jam.protocol.report.ReportTypes.CoreStatisticsRecord
import io.forge.jam.crypto.BandersnatchVrf

/**
 * Re-sizes a tiny genesis state for another chain config (e.g. [[ChainConfig.FUZZ]]): every item whose size
 * depends on V, C or E is rebuilt, everything else (services, recent history, entropy, judgments) is kept.
 */
object FuzzSpecGenesis:

  /** `validators` become kappa, lambda and gamma_P; `staging` (default: the same) becomes iota. */
  def build(
      tiny: RawState,
      config: ChainConfig,
      validators: List[ValidatorKey],
      staging: List[ValidatorKey] = Nil
  ): RawState =
    val iota = if staging.isEmpty then validators else staging
    for set <- List(validators, iota) do
      require(config.isValidValidatorCount(set.size), s"${set.size} validators is not a valid set size for $config")
    val store = new StateTrieStore(new InMemoryTrieBackend)
    val trie = store.at(store.bootstrap(tiny.keyvals.map(kv => (kv.key, kv.value))))
    val src = new TrieBackedJamState(trie, ChainConfig.TINY, new ServiceStorageView(trie), Some(store))
    val (authPools, authQueues, privileges, entropy) = (src.authPools, src.authQueues, src.privileges, src.entropyPool)
    val serviceStatistics = src.serviceStatistics

    // Drop the re-sized items first so `dst` never decodes their tiny encodings with the new sizes.
    val resized = List(
      StateKeys.CORE_AUTHORIZATION_POOL, StateKeys.AUTHORIZATION_QUEUE, StateKeys.SAFROLE_STATE,
      StateKeys.VALIDATOR_QUEUE, StateKeys.CURRENT_VALIDATORS, StateKeys.PREVIOUS_VALIDATORS, StateKeys.REPORTS,
      StateKeys.PRIVILEGED_SERVICES, StateKeys.ACTIVITY_STATISTICS, StateKeys.ACCUMULATION_QUEUE,
      StateKeys.ACCUMULATION_HISTORY
    ).map(StateKeys.simpleKey)
    trie.update(resized.map(k => (k, None)))
    val dst = new TrieBackedJamState(trie, config, new ServiceStorageView(trie), Some(store))

    def spread[A](items: List[A]): List[A] = List.tabulate(config.coresCount)(c => items(c % items.size))

    dst.kappa = validators
    dst.lambda = validators
    dst.iota = iota
    dst.gammaK = validators
    dst.gammaZ = BandersnatchVrf
      .generateRingRoot(validators.map(_.bandersnatch), config.validatorCount)
      .getOrElse(throw new IllegalStateException("ring root"))
    dst.gammaS = TicketsOrKeys.Keys(fallbackKeys(entropy(2), validators, config.epochLength))
    dst.gammaA = List.empty
    dst.authPools = spread(authPools)
    dst.authQueues = spread(authQueues)
    // These setters skip a value equal to what a missing key reads as, which here is exactly the value we want,
    // so set a placeholder first to force the write.
    dst.coreReports = Nil
    dst.coreReports = List.fill(config.coresCount)(None)
    dst.coreStatistics = Nil
    dst.coreStatistics = List.fill(config.coresCount)(CoreStatisticsRecord.zero)
    dst.readyQueue = Nil
    dst.readyQueue = List.fill(config.epochLength)(List.empty)
    dst.accumulated = Nil
    dst.accumulated = List.fill(config.epochLength)(List.empty)
    dst.privileges = privileges.copy(assign = spread(privileges.assign))
    dst.serviceStatistics = serviceStatistics
    dst.statsCurrent = List.fill(validators.size)(StatCount.zero)
    dst.statsLast = List.fill(validators.size)(StatCount.zero)
    dst.commit(trie)

    val keyvals = tiny.keyvals.map(kv =>
      KeyValue(kv.key, trie.read(kv.key).getOrElse(throw new IllegalStateException(s"${kv.key.toHex} not written")))
    )
    RawState(StateMerklization.stateMerklize(keyvals), keyvals)

  /** F(eta_2, kappa) (GP 0.8.0 §safrole eq:fallbackkeysequence). */
  private def fallbackKeys(entropy: Hash, validators: List[ValidatorKey], epochLength: Int): List[BandersnatchPublicKey] =
    List.tabulate(epochLength) { i =>
      val h = Hashing.blake2b256(entropy.bytes ++ uint32L.encode(i.toLong))
      validators((uint32L.decodeUnsafe(h.bytes) % validators.size).toInt).bandersnatch
    }

  /** A validator key with the dev Bandersnatch and Ed25519 keys and zeroed BLS key and metadata. */
  def devValidatorKey(bandersnatch: Array[Byte], ed25519: Array[Byte]): ValidatorKey =
    ValidatorKey(
      BandersnatchPublicKey(bandersnatch),
      io.forge.jam.core.primitives.Ed25519PublicKey(ed25519),
      io.forge.jam.core.primitives.BlsPublicKey(new Array[Byte](io.forge.jam.core.primitives.BlsPublicKey.Size)),
      JamBytes.zeros(ValidatorKey.MetadataSize)
    )
