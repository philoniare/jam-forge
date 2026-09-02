package io.forge.jam.protocol.traces

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Hash, Ed25519PublicKey}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.types.workpackage.AvailabilityAssignment
import io.forge.jam.core.types.history.HistoricalBetaContainer
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.dispute.DisputeTypes.Psi
import io.forge.jam.protocol.accumulation.{
  AccumulationServiceItem,
  Privileges,
  AccumulationReadyRecord
}
import io.forge.jam.protocol.report.ReportTypes.{
  CoreStatisticsRecord,
  ServiceStatisticsEntry
}
import io.forge.jam.protocol.statistics.StatisticsTypes.StatCount

/** Unified JAM state container holding all state components.
  */
final case class FullJamState(
    // tau - Current timeslot
    timeslot: Long,

    // eta - Entropy pool (4 x 32-byte hashes)
    entropyPool: List[Hash],

    // kappa - Current validators
    currentValidators: List[ValidatorKey],

    // lambda - Previous validators
    previousValidators: List[ValidatorKey],

    // iota - Validator queue (pending validators)
    validatorQueue: List[ValidatorKey],

    // gamma - Safrole state
    safroleGammaK: List[ValidatorKey], // gamma_k - next epoch validators
    safroleGammaZ: JamBytes, // gamma_z - ring root
    safroleGammaS: TicketsOrKeys, // gamma_s - sealing sequence
    safroleGammaA: List[TicketMark], // gamma_a - ticket accumulator

    // phi_c - Core authorization pools (per core, variable-size inner lists)
    authPools: List[List[Hash]] = List.empty,

    // phi - Authorization queues (per core, fixed-size 80 inner lists)
    authQueues: List[List[Hash]] = List.empty,

    // beta - Recent block history
    recentHistory: HistoricalBetaContainer = HistoricalBetaContainer(),

    // rho - Pending work reports (availability assignments per core)
    reports: List[Option[AvailabilityAssignment]] = List.empty,

    // psi - Judgements (disputes resolution state)
    judgements: Psi = Psi.empty,

    // chi - Privileged services configuration
    privilegedServices: Privileges =
      Privileges(0, List.empty, 0, 0, List.empty),

    // Ready queue for accumulation (epoch-length ring buffer)
    accumulationQueue: List[List[AccumulationReadyRecord]] = List.empty,

    // Accumulated hashes history (epoch-length ring buffer)
    accumulationHistory: List[List[JamBytes]] = List.empty,

    // delta - Service accounts with full data
    serviceAccounts: List[AccumulationServiceItem] = List.empty,

    // pi - Service statistics (per block, fresh each block)
    serviceStatistics: List[ServiceStatisticsEntry] = List.empty,

    // alpha_c - Core statistics (per core)
    coreStatistics: List[CoreStatisticsRecord] = List.empty,

    // alpha_v^curr - Current epoch validator statistics
    activityStatsCurrent: List[StatCount] = List.empty,

    // alpha_v^last - Last epoch validator statistics
    activityStatsLast: List[StatCount] = List.empty,

    // Post-dispute offenders
    postOffenders: List[Ed25519PublicKey] = List.empty,

    // Last accumulation outputs (service commitments)
    lastAccumulationOutputs: List[(Long, JamBytes)] = List.empty,

    // Raw keyvals for all state components (for pass-through when unchanged)
    otherKeyvals: List[KeyValue] = List.empty,

    // Original keyvals by full key (for preserving exact bytes when unchanged)
    originalKeyvals: Map[JamBytes, KeyValue] = Map.empty,

    // Raw service data by state key (for preimages, storage, etc.)
    rawServiceDataByStateKey: Map[JamBytes, JamBytes] = Map.empty
)
