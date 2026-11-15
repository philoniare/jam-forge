package io.forge.jam.protocol.state

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.protocol.accumulation.{AccountSource, AccumulationState, ServiceAccount}
import io.forge.jam.protocol.assurance.AssuranceTypes.AssuranceState
import io.forge.jam.protocol.authorization.AuthorizationTypes.AuthState
import io.forge.jam.protocol.dispute.DisputeTypes.DisputeState
import io.forge.jam.protocol.history.HistoryTypes.HistoricalState
import io.forge.jam.protocol.report.ReportTypes.ReportState
import io.forge.jam.protocol.safrole.SafroleTypes.SafroleState
import io.forge.jam.protocol.statistics.StatisticsTypes.StatState


object TrieBackedJamStateBridges:


  object SafroleBridge:
    def extract(view: TrieBackedJamState): SafroleState =
      SafroleState(
        tau = view.timeslot,
        eta = view.entropy.pool,
        lambda = view.validators.previous,
        kappa = view.validators.current,
        gammaK = view.validators.nextEpoch,
        iota = view.validators.queue,
        gammaA = view.gamma.a,
        gammaS = view.gamma.st,
        gammaZ = view.gamma.z,
        postOffenders = view.postOffenders
      )

    def apply(view: TrieBackedJamState, post: SafroleState): Unit =
      view.timeslot = post.tau
      view.entropy.pool = post.eta
      view.validators.previous = post.lambda
      view.validators.current = post.kappa
      view.validators.nextEpoch = post.gammaK
      view.validators.queue = post.iota
      view.gamma.a = post.gammaA
      view.gamma.st = post.gammaS
      view.gamma.z = post.gammaZ
      view.postOffenders = post.postOffenders


  object DisputeBridge:
    def extract(view: TrieBackedJamState): DisputeState =
      DisputeState(
        psi = view.psi,
        rho = view.cores.reports,
        tau = view.timeslot,
        kappa = view.validators.current,
        lambda = view.validators.previous
      )

    def apply(view: TrieBackedJamState, post: DisputeState): Unit =
      view.psi = post.psi
      view.cores.reports = post.rho


  object AssuranceBridge:
    def extract(view: TrieBackedJamState): AssuranceState =
      AssuranceState(
        availAssignments = view.cores.reports,
        currValidators = view.validators.current,
        postValidators = view.validators.current
      )

    def apply(view: TrieBackedJamState, post: AssuranceState): Unit =
      view.cores.reports = post.availAssignments


  final case class ReportPreState(state: ReportState, serviceInfo: Long => Option[ServiceInfo])

  object ReportBridge:
    def extract(view: TrieBackedJamState): ReportPreState =
      ReportPreState(stateOf(view), view.accumulation.serviceInfo)

    private def stateOf(view: TrieBackedJamState): ReportState =
      val readyQueuePkgHashes = view.accumulation.readyQueue.flatten.map(r =>
        Hash(r.report.packageSpec.hash.bytes)
      ).toSet
      val accumulatedPkgHashes = view.accumulation.accumulated.flatten.map(jb =>
        Hash(jb.toArray)
      ).toSet
      val pendingReportPkgHashes = view.cores.reports.flatten.map(aa =>
        Hash(aa.report.packageSpec.hash.bytes)
      ).toSet

      ReportState(
        availAssignments = view.cores.reports,
        currValidators = view.validators.current,
        prevValidators = view.validators.previous,
        entropy = view.entropy.pool,
        offenders = view.psi.offenders.map(k => Hash(k.bytes)),
        recentBlocks = view.beta,
        authPools = view.authPools,
        accounts = Nil,
        coresStatistics = view.cores.statistics,
        servicesStatistics = view.serviceStatistics,
        readyQueuePackageHashes = readyQueuePkgHashes,
        accumulatedPackageHashes = accumulatedPkgHashes ++ pendingReportPkgHashes
      )

    def apply(view: TrieBackedJamState, post: ReportState): Unit =
      view.cores.reports = post.availAssignments
      view.cores.statistics = post.coresStatistics
      view.serviceStatistics = post.servicesStatistics


  object AuthorizationBridge:
    def extract(view: TrieBackedJamState): AuthState =
      AuthState(
        authPools = view.authPools,
        authQueues = view.authQueues
      )

    def apply(view: TrieBackedJamState, post: AuthState): Unit =
      view.authPools = post.authPools
      view.authQueues = post.authQueues


  object HistoryBridge:
    def extract(view: TrieBackedJamState): HistoricalState =
      HistoricalState(beta = view.beta)

    def apply(view: TrieBackedJamState, post: HistoricalState): Unit =
      view.beta = post.beta


  object StatisticsBridge:
    def extract(view: TrieBackedJamState): StatState =
      StatState(
        valsCurrStats = view.statistics.current,
        valsLastStats = view.statistics.last,
        slot = view.timeslot,
        currValidators = view.validators.current,
        prevValidators = view.validators.previous
      )

    def apply(view: TrieBackedJamState, post: StatState): Unit =
      view.statistics.current = post.valsCurrStats
      view.statistics.last = post.valsLastStats


  final case class AccumulationPreState(
      state: AccumulationState,
      initStagingSet: List[JamBytes],
      initAuthQueues: List[List[Hash]],
      accounts: AccountSource
  )

  object AccumulationBridge:
    def extract(view: TrieBackedJamState): AccumulationPreState =
      val initStagingSet: List[JamBytes] =
        view.validators.queue.map(_.toJamBytes).toList
      val initAuthQueues: List[List[Hash]] = view.authQueues

      val state = AccumulationState(
        slot = view.timeslot,
        entropy = view.entropy.firstAsBytes,
        readyQueue = view.accumulation.readyQueue,
        accumulated = view.accumulation.accumulated,
        privileges = view.accumulation.privileges,
        statistics = List.empty,
        accounts = Nil
      )
      AccumulationPreState(state, initStagingSet, initAuthQueues, accountSource(view))

    def accountSource(view: TrieBackedJamState): AccountSource =
      new AccountSource:
        private val read = new java.util.concurrent.ConcurrentHashMap[Long, Option[ServiceAccount]]()
        private val changedBefore = view.accumulation.accountChanges
        private lazy val allIds = view.accumulation.serviceIds
        def account(id: Long): Option[ServiceAccount] =
          read.computeIfAbsent(
            id,
            _ => changedBefore.getOrElse(id, view.accumulation.priorServiceInfo(id)).map(info =>
              ServiceAccount(
                info = info,
                storage = Map.empty,
                preimages = Map.empty,
                preimageRequests = Map.empty,
                lastAccumulated = info.lastAccumulationSlot
              )
            )
          )
        def ids: Iterable[Long] = allIds

    def apply(
        view: TrieBackedJamState,
        postState: AccumulationState,
        removedAccounts: Set[Long],
        postStagingSet: List[JamBytes],
        initStagingSet: List[JamBytes],
        postAuthQueues: List[List[Hash]],
        initAuthQueues: List[List[Hash]]
    ): Unit =
      val stagingChanged =
        !(postStagingSet eq initStagingSet) && postStagingSet != initStagingSet
      if stagingChanged then
        val newQueue: IndexedSeq[ValidatorKey] =
          postStagingSet.map(ValidatorKey.fromJamBytes).toIndexedSeq
        view.validators.queue = newQueue

      val authChanged =
        !(postAuthQueues eq initAuthQueues) && postAuthQueues != initAuthQueues
      if authChanged then view.authQueues = postAuthQueues

      view.accumulation.readyQueue = postState.readyQueue
      view.accumulation.accumulated = postState.accumulated
      view.accumulation.privileges = postState.privileges
      postState.accounts.foreach(item => view.accumulation.setServiceInfo(item.id, item.data.service))
      removedAccounts.foreach(view.accumulation.removeService)
