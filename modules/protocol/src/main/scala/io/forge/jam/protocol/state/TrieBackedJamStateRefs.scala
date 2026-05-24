package io.forge.jam.protocol.state

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.types.workpackage.AvailabilityAssignment
import io.forge.jam.protocol.accumulation.{
  AccumulationReadyRecord,
  AccumulationServiceItem,
  Privileges
}
import io.forge.jam.protocol.report.ReportTypes.CoreStatisticsRecord
import io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys
import io.forge.jam.protocol.statistics.StatisticsTypes.StatCount


final class ValidatorsRef(private val s: TrieBackedJamState):
  def current: List[ValidatorKey] = s.kappa
  def current_=(v: List[ValidatorKey]): Unit = s.kappa = v

  def previous: List[ValidatorKey] = s.lambda
  def previous_=(v: List[ValidatorKey]): Unit = s.lambda = v

  def queue: List[ValidatorKey] = s.iota
  def queue_=(v: List[ValidatorKey]): Unit = s.iota = v

  def nextEpoch: List[ValidatorKey] = s.gammaK
  def nextEpoch_=(v: List[ValidatorKey]): Unit = s.gammaK = v

final class EntropyRef(private val s: TrieBackedJamState):
  def pool: List[Hash] = s.entropyPool
  def pool_=(v: List[Hash]): Unit = s.entropyPool = v

  def firstAsBytes: JamBytes = JamBytes(s.entropyPool.head.bytes)

final class GammaRef(private val s: TrieBackedJamState):
  def z: JamBytes = s.gammaZ
  def z_=(v: JamBytes): Unit = s.gammaZ = v

  def st: TicketsOrKeys = s.gammaS
  def st_=(v: TicketsOrKeys): Unit = s.gammaS = v

  def a: List[TicketMark] = s.gammaA
  def a_=(v: List[TicketMark]): Unit = s.gammaA = v

final class CoresRef(private val s: TrieBackedJamState):
  def reports: List[Option[AvailabilityAssignment]] = s.coreReports
  def reports_=(v: List[Option[AvailabilityAssignment]]): Unit = s.coreReports = v

  def statistics: List[CoreStatisticsRecord] = s.coreStatistics
  def statistics_=(v: List[CoreStatisticsRecord]): Unit = s.coreStatistics = v

final class AccumulationRef(private val s: TrieBackedJamState):
  def readyQueue: List[List[AccumulationReadyRecord]] = s.readyQueue
  def readyQueue_=(v: List[List[AccumulationReadyRecord]]): Unit =
    s.readyQueue = v

  def accumulated: List[List[JamBytes]] = s.accumulated
  def accumulated_=(v: List[List[JamBytes]]): Unit = s.accumulated = v

  def privileges: Privileges = s.privileges
  def privileges_=(v: Privileges): Unit = s.privileges = v

  def serviceAccounts: List[AccumulationServiceItem] = s.serviceAccounts
  def serviceAccounts_=(v: List[AccumulationServiceItem]): Unit =
    s.serviceAccounts = v

final class StatisticsRef(private val s: TrieBackedJamState):
  def current: List[StatCount] = s.statsCurrent
  def current_=(v: List[StatCount]): Unit = s.statsCurrent = v

  def last: List[StatCount] = s.statsLast
  def last_=(v: List[StatCount]): Unit = s.statsLast = v
