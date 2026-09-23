package io.forge.jam.protocol.accumulation

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing, StfResult}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.protocol.state.{ServiceStorageView, TrieBackedJamState}
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.AccumulationBridge

import scala.collection.mutable

import io.forge.jam.core.types.epoch.ValidatorKey

final case class AccumulationStfResult(
    state: AccumulationState,
    stagingSet: List[JamBytes],
    authQueues: List[List[JamBytes]],
    output: AccumulationOutput
)

/** Accumulation State Transition Function.
  */
object AccumulationTransition:

  def stfView(
      input: AccumulationInput,
      view: TrieBackedJamState,
      prevSlot: Long,
      sharedExecutor: Option[AccumulationExecutor] = None
  ): AccumulationOutput =
    val pre = AccumulationBridge.extract(view)

    val result =
      stfInternal(
        input,
        pre.state,
        pre.initStagingSet,
        pre.initAuthQueues,
        view.config,
        prevSlot,
        sharedExecutor
      )

    AccumulationBridge.apply(
      view,
      result.state,
      result.stagingSet,
      pre.initStagingSet,
      result.authQueues,
      pre.initAuthQueues
    )
    result.output

  def stfInternal(
      input: AccumulationInput,
      preState: AccumulationState,
      initStagingSet: List[JamBytes],
      initAuthQueues: List[List[JamBytes]],
      config: ChainConfig,
      prevSlot: Long,
      sharedExecutor: Option[AccumulationExecutor] = None
  ): AccumulationStfResult =
    val m = (input.slot % config.epochLength).toInt
    val deltaT = Math.max(input.slot - prevSlot, 1L)
    val partition = partitionAndQueueReports(input.reports, preState, m, config)

    // 8. Execute PVM for accumulated reports (respecting gas budget)
    val run = runOuterAccumulation(
      immediateReports = partition.immediateReports,
      readyToAccumulate = partition.readyToAccumulate,
      preState = preState,
      initStagingSet = initStagingSet,
      initAuthQueues = initAuthQueues,
      slot = input.slot,
      config = config,
      sharedExecutor = sharedExecutor
    )

    val outerResult = run.outerResult
    val commitments = outerResult.commitments

    // 9. Rebuild the ready queue, then rotate the accumulated sliding window —
    //    both edited with the gas-bounded actually-accumulated set (ACC-003).
    val finalReadyQueue = rebuildReadyQueue(
      workingReadyQueue = partition.workingReadyQueue,
      editedNewRecords = partition.editedNewRecords,
      actuallyAccumulated = run.actuallyAccumulated,
      m = m,
      deltaT = deltaT,
      config = config
    )

    val newAccumulatedArray =
      rotateAccumulated(preState.accumulated, run.actuallyAccumulated, config)

    // 10-13. Statistics, lastAccumulationSlot, posterior privileges, post state.
    val assembled = assemblePostState(
      outerResult = outerResult,
      reportsToAccumulate = run.reportsToAccumulate,
      finalReadyQueue = finalReadyQueue,
      newAccumulatedArray = newAccumulatedArray,
      preState = preState,
      slot = input.slot
    )

    // 14. Compute commitment root from yields
    val outputHash = KeccakCommitmentMerkle.computeCommitmentRoot(commitments)

    // 15. Convert commitments to list format for state storage (key 0x10)
    val commitmentsList =
      commitments.toList.sortBy(c => (c.serviceIndex, c.hash)).map { c =>
        (c.serviceIndex, c.hash)
      }

    // 16. Extract post staging set and auth queues from posterior state
    val postStagingSet = outerResult.postState.stagingSet.toList

    // Auth queues: use the posterior state's auth queues
    val postAuthQueues = outerResult.postState.authQueue.map(_.toList).toList

    AccumulationStfResult(
      assembled.state,
      postStagingSet,
      postAuthQueues,
      StfResult.success(
        AccumulationOutputData(
          outputHash,
          assembled.accumulationStats,
          commitmentsList,
          assembled.accumulationTransferCounts
        )
      )
    )

  private final case class QueuedReportPartition(
      immediateReports: List[WorkReport],
      readyToAccumulate: List[WorkReport],
      workingReadyQueue: Vector[List[AccumulationReadyRecord]],
      editedNewRecords: List[AccumulationReadyRecord]
  )

  private def partitionAndQueueReports(
      reports: List[WorkReport],
      preState: AccumulationState,
      m: Int,
      config: ChainConfig
  ): QueuedReportPartition =
    // 1. Collect all historically accumulated hashes (for dependency checking)
    val historicallyAccumulated = mutable.Set.from(preState.accumulated.flatten)

    // 2. Partition new reports into immediate vs queued
    val (immediateReports, queuedReports) = reports.partition { report =>
      report.context.prerequisites.isEmpty && report.segmentRootLookup.isEmpty
    }

    // 3. Track newly accumulated package hashes this block
    val newAccumulated = mutable.Set.empty[JamBytes]

    // Add immediate reports to accumulated set
    immediateReports.foreach { report =>
      val hash = JamBytes(report.packageSpec.hash.bytes.toArray)
      newAccumulated += hash
      historicallyAccumulated += hash
    }

    val accumulatedSnapshot: Set[JamBytes] = historicallyAccumulated.toSet

    // 4. Build working copy of ready queue with edited dependencies.
    val workingReadyQueue: Vector[List[AccumulationReadyRecord]] =
      preState.readyQueue.indices.iterator.map { slotIdx =>
        editReadyQueueRecords(
          preState.readyQueue(slotIdx),
          accumulatedSnapshot
        )
      }.toVector

    // Build new queued records
    val newRecords = queuedReports.map { report =>
      val prereqs = report.context.prerequisites
      val segmentDeps = report.segmentRootLookup.map(_.workPackageHash)
      val allDeps = (prereqs ++ segmentDeps).filter(h =>
        !accumulatedSnapshot.contains(JamBytes(h.bytes))
      )
      AccumulationReadyRecord(report, allDeps)
    }

    // Edit new records to remove already-accumulated dependencies
    val editedNewRecords =
      editReadyQueueRecords(newRecords, accumulatedSnapshot)

    // 5. Extract accumulatable reports from ready queue
    val epochLen = config.epochLength
    val reorderedSlots = (m until epochLen) ++ (0 until m)
    val existingQueuedWithSlots = reorderedSlots.flatMap { slotIdx =>
      workingReadyQueue(slotIdx).map(record => (slotIdx, record))
    }.toList
    // Add new records at the end
    val allQueuedWithSlots =
      existingQueuedWithSlots ++ editedNewRecords.map(r => (m, r))

    val (readyToAccumulate, _) =
      extractAccumulatableWithSlots(
        allQueuedWithSlots,
        accumulatedSnapshot
      )

    // 6. Add ready-to-accumulate reports to accumulated set
    readyToAccumulate.foreach { report =>
      val hash = JamBytes(report.packageSpec.hash.bytes.toArray)
      newAccumulated += hash
      historicallyAccumulated += hash
    }

    QueuedReportPartition(
      immediateReports = immediateReports,
      readyToAccumulate = readyToAccumulate,
      workingReadyQueue = workingReadyQueue,
      editedNewRecords = editedNewRecords
    )

  /** What step 8 produces: the raw outer-accumulation result plus the two
    * gas-BOUNDED views of it every later step is computed against.
    */
  private final case class AccumulationRun(
      outerResult: OuterAccumulationResult,
      reportsToAccumulate: List[WorkReport],
      actuallyAccumulated: Set[JamBytes]
  )

  private def runOuterAccumulation(
      immediateReports: List[WorkReport],
      readyToAccumulate: List[WorkReport],
      preState: AccumulationState,
      initStagingSet: List[JamBytes],
      initAuthQueues: List[List[JamBytes]],
      slot: Long,
      config: ChainConfig,
      sharedExecutor: Option[AccumulationExecutor]
  ): AccumulationRun =
    val allToAccumulate = immediateReports ++ readyToAccumulate
    val partialState = preState.toPartialState(initStagingSet, initAuthQueues)

    // Calculate total gas budget
    val sumPrivilegedGas = partialState.alwaysAccers.values.sum
    val minTotalGas = config.reportAccGas * config.coresCount + sumPrivilegedGas
    val totalGasLimit = Math.max(config.maxBlockGas, minTotalGas)

    // Use shared executor if provided (for module cache reuse), otherwise create new
    val executor = sharedExecutor.getOrElse(new AccumulationExecutor(config))

    // Execute outer accumulation with recursive deferred transfer processing
    val outerResult = outerAccumulate(
      partialState = partialState,
      transfers = List.empty,
      workReports = allToAccumulate,
      alwaysAccers = partialState.alwaysAccers.toMap,
      gasLimit = totalGasLimit,
      timeslot = slot,
      entropy = preState.entropy,
      executor = executor,
      config = config
    )

    // Determine which reports were actually accumulated (based on reportsAccumulated count)
    val reportsToAccumulate =
      allToAccumulate.take(outerResult.reportsAccumulated)

    // Rebuild actuallyAccumulated to only include reports that will actually be accumulated
    val actuallyAccumulated: Set[JamBytes] =
      reportsToAccumulate.iterator
        .map(report => JamBytes(report.packageSpec.hash.bytes.toArray))
        .toSet

    AccumulationRun(
      outerResult = outerResult,
      reportsToAccumulate = reportsToAccumulate,
      actuallyAccumulated = actuallyAccumulated
    )

  private def rebuildReadyQueue(
      workingReadyQueue: Vector[List[AccumulationReadyRecord]],
      editedNewRecords: List[AccumulationReadyRecord],
      actuallyAccumulated: Set[JamBytes],
      m: Int,
      deltaT: Long,
      config: ChainConfig
  ): List[List[AccumulationReadyRecord]] =
    // Rebuild ready queue
    val finalReadyQueue = (0 until config.epochLength).map { idx =>
      val i =
        ((m - idx) % config.epochLength + config.epochLength) % config.epochLength
      if i == 0 then
        editReadyQueueRecords(editedNewRecords, actuallyAccumulated)
      else if i >= 1 && i < deltaT then
        // Slots that wrapped around - clear them
        List.empty[AccumulationReadyRecord]
      else
        editReadyQueueRecords(workingReadyQueue(idx), actuallyAccumulated)
    }.toList

    require(
      finalReadyQueue.forall { slotRecords =>
        slotRecords.forall { record =>
          val recordHash =
            JamBytes(record.report.packageSpec.hash.bytes.toArray)
          !actuallyAccumulated.contains(recordHash) &&
          record.dependencies.forall(h =>
            !actuallyAccumulated.contains(JamBytes(h.bytes))
          )
        }
      },
      "Ready-queue post-condition violated: an accumulated hash leaked into the final queue"
    )

    finalReadyQueue

  private def rotateAccumulated(
      priorAccumulated: List[List[JamBytes]],
      actuallyAccumulated: Set[JamBytes],
      config: ChainConfig
  ): List[List[JamBytes]] =
    val newAccumulatedList = actuallyAccumulated.toList.sorted
    (0 until config.epochLength).map { idx =>
      if idx == config.epochLength - 1 then
        // New items at last position
        newAccumulatedList
      else
        // Shift left by 1
        priorAccumulated.lift(idx + 1).getOrElse(List.empty)
    }.toList

  /** The posterior state plus the per-service accumulation stats the STF output
    * also reports.
    */
  private final case class AssembledPostState(
      state: AccumulationState,
      accumulationStats: Map[Long, (Long, Int)],
      accumulationTransferCounts: Map[Long, Int]
  )

  private def assemblePostState(
      outerResult: OuterAccumulationResult,
      reportsToAccumulate: List[WorkReport],
      finalReadyQueue: List[List[AccumulationReadyRecord]],
      newAccumulatedArray: List[List[JamBytes]],
      preState: AccumulationState,
      slot: Long
  ): AssembledPostState =
    val newPartialState = outerResult.postState
    val gasUsedPerService = outerResult.gasUsedMap

    // 10. Update statistics
    val workItemsPerService = countWorkItemsPerService(reportsToAccumulate)
    val transferCountsPerService = outerResult.transferCountMap
    val newStatistics = updateStatistics(
      gasUsedPerService,
      workItemsPerService,
      transferCountsPerService
    )

    // 11. Build accumulation stats for fresh service statistics computation.
    val statsServiceIds =
      gasUsedPerService.keySet ++ workItemsPerService.keySet ++
        transferCountsPerService.keySet
    val accumulationStats: Map[Long, (Long, Int)] = statsServiceIds.iterator
      .map { serviceId =>
        serviceId -> (
          gasUsedPerService.getOrElse(serviceId, 0L),
          workItemsPerService.getOrElse(serviceId, 0)
        )
      }
      .filter { case (sid, (gas, count)) =>
        gas > 0 || count > 0 || transferCountsPerService.getOrElse(sid, 0) > 0
      }
      .toMap
    val accumulationTransferCounts: Map[Long, Int] =
      accumulationStats.keysIterator
        .map(sid => sid -> transferCountsPerService.getOrElse(sid, 0))
        .filter(_._2 > 0)
        .toMap

    // 12. Update lastAccumulationSlot for all services in accumulationStats
    for (serviceId, _) <- accumulationStats do
      newPartialState.accounts.get(serviceId).foreach { account =>
        newPartialState.accounts = newPartialState.accounts.updated(
          serviceId,
          account.copy(
            info = account.info.copy(lastAccumulationSlot = slot)
          )
        )
      }

    // 13. Use privileges from the posterior state after all batches
    val finalManager = outerResult.postState.manager
    val finalDelegator = outerResult.postState.delegator
    val finalRegistrar = outerResult.postState.registrar
    val finalAssigners = outerResult.postState.assigners.toList
    val finalAlwaysAccers = outerResult.postState.alwaysAccers.toMap

    val finalState = AccumulationState(
      slot = slot,
      entropy = JamBytes(preState.entropy.toArray),
      readyQueue = finalReadyQueue,
      accumulated = newAccumulatedArray,
      privileges = Privileges(
        bless = finalManager,
        assign = finalAssigners,
        designate = finalDelegator,
        register = finalRegistrar,
        alwaysAcc =
          finalAlwaysAccers.toList.sortBy(_._1).map { case (id, gas) =>
            AlwaysAccItem(id, gas)
          }
      ),
      statistics = newStatistics,
      accounts = newPartialState.toAccumulationServiceItems(),
      rawServiceDataByStateKey = newPartialState.rawServiceDataByStateKey,
      rawServiceAccountsByStateKey =
        newPartialState.rawServiceAccountsByStateKey
    )

    AssembledPostState(
      state = finalState,
      accumulationStats = accumulationStats,
      accumulationTransferCounts = accumulationTransferCounts
    )

  /** Edit ready queue records by removing accumulated reports and pruning
    * dependencies.
    */
  private def editReadyQueueRecords(
      records: List[AccumulationReadyRecord],
      accumulatedHashes: Set[JamBytes]
  ): List[AccumulationReadyRecord] =
    records
      .filter { record =>
        val reportHash = JamBytes(record.report.packageSpec.hash.bytes.toArray)
        !accumulatedHashes.contains(reportHash)
      }
      .map { record =>
        AccumulationReadyRecord(
          report = record.report,
          dependencies = record.dependencies
            .filter(h => !accumulatedHashes.contains(JamBytes(h.bytes)))
        )
      }

  /** Extract accumulatable reports while preserving slot information.
    */
  private def extractAccumulatableWithSlots(
      queueWithSlots: List[(Int, AccumulationReadyRecord)],
      initiallyAccumulated: Set[JamBytes]
  ): (List[WorkReport], List[(Int, AccumulationReadyRecord)]) =
    val accumulated = mutable.Set.from(initiallyAccumulated)
    val result = mutable.ListBuffer.empty[WorkReport]
    var remaining = queueWithSlots

    var continue = true
    while continue do
      val (ready, notReady) = remaining.partition { case (_, record) =>
        record.dependencies.forall(h => accumulated.contains(JamBytes(h.bytes)))
      }
      if ready.isEmpty then continue = false
      else
        ready.foreach { case (_, record) =>
          result += record.report
          accumulated += JamBytes(record.report.packageSpec.hash.bytes.toArray)
        }
        remaining = notReady

    (result.toList, remaining)

  /** Result of outer accumulation.
    */
  case class OuterAccumulationResult(
      reportsAccumulated: Int,
      postState: PartialState,
      gasUsedMap: Map[Long, Long],
      commitments: Set[Commitment],
      privilegeSnapshots: Map[Long, PrivilegeSnapshot] = Map.empty,
      transferCountMap: Map[Long, Int] = Map.empty
  )

  /** Snapshot of privilege state values at a point in time. Also includes
    * staging set and auth queues for final state computation.
    */
  case class PrivilegeSnapshot(
      manager: Long,
      delegator: Long,
      registrar: Long,
      assigners: List[Long],
      alwaysAccers: Map[Long, Long],
      stagingSet: List[JamBytes] = List.empty,
      authQueues: List[List[JamBytes]] = List.empty
  )

  private def mergeBy[K, V](a: Map[K, V], b: Map[K, V])(
      combine: (V, V) => V
  ): Map[K, V] =
    b.foldLeft(a) { case (acc, (k, v)) =>
      acc.updated(k, acc.get(k).map(combine(_, v)).getOrElse(v))
    }

  /** Outer accumulation function. Recursively processes work reports and
    * deferred transfers.
    */
  private[accumulation] def outerAccumulate(
      partialState: PartialState,
      transfers: List[DeferredTransfer],
      workReports: List[WorkReport],
      alwaysAccers: Map[Long, Long],
      gasLimit: Long,
      timeslot: Long,
      entropy: JamBytes,
      executor: AccumulationExecutor,
      config: ChainConfig
  ): OuterAccumulationResult =
    // Count how many reports can fit in gas budget
    var i = 0
    var sumGasRequired = transfers.map(_.gasLimit).sum + alwaysAccers.values.sum

    val reportIterator = workReports.iterator
    var continue = true
    while reportIterator.hasNext && continue do
      val report = reportIterator.next()
      var canAccumulate = true
      for result <- report.results if canAccumulate do
        if result.accumulateGas.toLong + sumGasRequired > gasLimit then
          canAccumulate = false
        else sumGasRequired += result.accumulateGas.toLong
      if canAccumulate then i += 1
      else continue = false

    val n = i + transfers.size + alwaysAccers.size

    if n == 0 then
      return OuterAccumulationResult(
        reportsAccumulated = 0,
        postState = partialState,
        gasUsedMap = Map.empty,
        commitments = Set.empty,
        privilegeSnapshots = Map.empty
      )

    // Execute parallel accumulation for this batch
    val parallelResult = executeAccumulation(
      partialState = partialState,
      reports = workReports.take(i),
      deferredTransfers = transfers,
      alwaysAccers = alwaysAccers,
      timeslot = timeslot,
      entropy = entropy,
      executor = executor
    )

    val parallelGasUsed = parallelResult.gasUsedMap.values.sum
    val transfersGas = parallelResult.deferredTransfers.map(_.gasLimit).sum

    // Recursively process remaining reports with new deferred transfers
    val remainingReports = workReports.drop(i)
    val newTransfers = parallelResult.deferredTransfers
    val stateForRecursion = parallelResult.postState

    // Recursive call if there are new transfers or remaining reports
    val outerResult = outerAccumulate(
      partialState = stateForRecursion,
      transfers = newTransfers,
      workReports = remainingReports,
      alwaysAccers =
        Map.empty, // Always-accumulate services only processed in first iteration
      gasLimit = gasLimit + transfersGas - parallelGasUsed,
      timeslot = timeslot,
      entropy = entropy,
      executor = executor,
      config = config
    )

    // Merge results
    val mergedGasUsed =
      mergeBy(parallelResult.gasUsedMap, outerResult.gasUsedMap)(_ + _)

    val thisRoundTransferCounts: Map[Long, Int] =
      transfers.groupBy(_.destination).view.mapValues(_.size).toMap
    val mergedTransferCounts =
      mergeBy(thisRoundTransferCounts, outerResult.transferCountMap)(_ + _)

    // Merge privilege snapshots:
    // - For privilege fields (manager, delegator, registrar, assigners, alwaysAccers):
    //   FIRST batch takes precedence
    // - For stagingSet and authQueues: LAST update wins
    val mergedSnapshots =
      mergeBy(parallelResult.privilegeSnapshots, outerResult.privilegeSnapshots) {
        (p, o) =>
          // Both batches have this service - merge field by field
          // For privileges, use first batch (parallel); for stagingSet/authQueues, use last update (outer if non-empty)
          val mergedStagingSet =
            if o.stagingSet.nonEmpty then o.stagingSet else p.stagingSet
          val mergedAuthQueues =
            if o.authQueues.nonEmpty && o.authQueues.exists(_.nonEmpty) then
              o.authQueues
            else p.authQueues
          PrivilegeSnapshot(
            manager = p.manager,
            delegator = p.delegator,
            registrar = p.registrar,
            assigners = p.assigners,
            alwaysAccers = p.alwaysAccers,
            stagingSet = mergedStagingSet,
            authQueues = mergedAuthQueues
          )
      }

    OuterAccumulationResult(
      reportsAccumulated = i + outerResult.reportsAccumulated,
      postState = outerResult.postState,
      gasUsedMap = mergedGasUsed,
      commitments = parallelResult.commitments ++ outerResult.commitments,
      privilegeSnapshots = mergedSnapshots,
      transferCountMap = mergedTransferCounts
    )

  /** Result of parallel accumulation execution.
    */
  case class AccumulationExecResult(
      postState: PartialState,
      gasUsedMap: Map[Long, Long],
      commitments: Set[Commitment],
      deferredTransfers: List[DeferredTransfer] = List.empty,
      privilegeSnapshots: Map[Long, PrivilegeSnapshot] = Map.empty
  )

  private def executeAccumulation(
      partialState: PartialState,
      reports: List[WorkReport],
      deferredTransfers: List[DeferredTransfer],
      alwaysAccers: Map[Long, Long],
      timeslot: Long,
      entropy: JamBytes,
      executor: AccumulationExecutor
  ): AccumulationExecResult =
    val initialState = partialState.deepCopy()

    val serviceOperands = groupOperandsByService(reports, deferredTransfers)

    // Collect all services to accumulate (work items + always-accers + transfer destinations)
    val servicesToAccumulate = mutable.Set.empty[Long]
    servicesToAccumulate ++= serviceOperands.keys
    servicesToAccumulate ++= alwaysAccers.keys

    if servicesToAccumulate.isEmpty && deferredTransfers.isEmpty then
      return AccumulationExecResult(
        partialState,
        Map.empty,
        Set.empty,
        List.empty
      )

    // Execute services sequentially (for now - can be parallelized later)
    val sortedServices = servicesToAccumulate.toList.sorted
    val run = runServices(
      sortedServices = sortedServices,
      serviceOperands = serviceOperands,
      alwaysAccers = alwaysAccers,
      initialState = initialState,
      timeslot = timeslot,
      entropy = entropy,
      executor = executor
    )

    val finalState = initialState

    val privileges = reconcilePrivileges(partialState, run.privilegeSnapshots)
    finalState.manager = privileges.manager
    finalState.delegator = privileges.delegator
    finalState.registrar = privileges.registrar
    finalState.assigners.clear()
    finalState.assigners ++= privileges.assigners
    finalState.alwaysAccers.clear()
    finalState.alwaysAccers ++= privileges.alwaysAccers
    // Update stagingSet from delegator's post-state (only when it changed)
    privileges.stagingSet.foreach { ss =>
      finalState.stagingSet.clear()
      finalState.stagingSet ++= ss
    }
    finalState.authQueue.clear()
    finalState.authQueue ++= privileges.authQueues.map(q =>
      mutable.ListBuffer.from(q)
    )

    // Process preimage integrations on the final merged state
    val stateAfterPreimages =
      if run.provisions.nonEmpty then
        preimageIntegration(
          run.provisions,
          finalState,
          timeslot,
          executor.storageView
        )
      else finalState
    AccumulationExecResult(
      stateAfterPreimages,
      run.gasUsedMap,
      run.commitments,
      run.deferredTransfers,
      run.privilegeSnapshots
    )

  private def groupOperandsByService(
      reports: List[WorkReport],
      deferredTransfers: List[DeferredTransfer]
  ): mutable.Map[Long, mutable.ListBuffer[AccumulationOperand]] =
    val serviceOperands =
      mutable.Map.empty[Long, mutable.ListBuffer[AccumulationOperand]]

    // Add deferred transfers as operands (v0.7.1 - transfers processed in accumulate)
    for transfer <- deferredTransfers do
      serviceOperands.getOrElseUpdate(
        transfer.destination,
        mutable.ListBuffer.empty
      ) +=
        AccumulationOperand.Transfer(transfer)

    for report <- reports do
      for result <- report.results do
        val operand = OperandTuple(
          packageHash = JamBytes(report.packageSpec.hash.bytes.toArray),
          segmentRoot = JamBytes(report.packageSpec.exportsRoot.bytes.toArray),
          authorizerHash = JamBytes(report.authorizerHash.bytes.toArray),
          payloadHash = JamBytes(result.payloadHash.bytes.toArray),
          gasLimit = result.accumulateGas.toLong,
          authTrace = report.authOutput,
          result = result.result
        )
        serviceOperands.getOrElseUpdate(
          result.serviceId.value.toLong,
          mutable.ListBuffer.empty
        ) +=
          AccumulationOperand.WorkItem(operand)

    serviceOperands

  /** Everything the per-service execution loop accumulates across one batch.
    */
  private final case class ServiceRunResults(
      gasUsedMap: Map[Long, Long],
      privilegeSnapshots: Map[Long, PrivilegeSnapshot],
      commitments: Set[Commitment],
      deferredTransfers: List[DeferredTransfer],
      provisions: Set[(Long, JamBytes)]
  )

  private def runServices(
      sortedServices: List[Long],
      serviceOperands: mutable.Map[Long, mutable.ListBuffer[AccumulationOperand]],
      alwaysAccers: Map[Long, Long],
      initialState: PartialState,
      timeslot: Long,
      entropy: JamBytes,
      executor: AccumulationExecutor
  ): ServiceRunResults =
    val gasUsedMap = mutable.Map.empty[Long, Long]
    val commitments = mutable.Set.empty[Commitment]
    val newDeferredTransfers = mutable.ListBuffer.empty[DeferredTransfer]
    val allProvisions = mutable.Set.empty[(Long, JamBytes)]

    // Track privilege snapshots
    val privilegeSnapshots = mutable.Map.empty[Long, PrivilegeSnapshot]

    // Collect account changes from all services for merging
    val allAccountChanges = new AccountChanges()

    for serviceId <- sortedServices do
      val operands =
        serviceOperands.getOrElse(serviceId, mutable.ListBuffer.empty).toList
      val alwaysAccGas = alwaysAccers.getOrElse(serviceId, 0L)
      var workItemGas = 0L
      var transferGas = 0L
      operands.foreach {
        case AccumulationOperand.WorkItem(op) => workItemGas += op.gasLimit
        case AccumulationOperand.Transfer(t)  => transferGas += t.gasLimit
        case _                                => ()
      }
      val totalGasLimit = workItemGas + alwaysAccGas + transferGas

      val execResult = executor.executeService(
        partialState = initialState,
        timeslot = timeslot,
        serviceId = serviceId,
        gasLimit = totalGasLimit,
        entropy = entropy,
        operands = operands
      )

      // Compute changes this service made
      val serviceChanges =
        computeServiceChanges(serviceId, initialState, execResult.postState)

      // Merge changes
      allAccountChanges.checkAndMerge(serviceChanges)

      val prevGas = gasUsedMap.getOrElse(serviceId, 0L)
      val newGas = prevGas + execResult.gasUsed
      gasUsedMap(serviceId) = newGas

      // Capture privilege snapshot including staging set and auth queues
      privilegeSnapshots(serviceId) = PrivilegeSnapshot(
        manager = execResult.postState.manager,
        delegator = execResult.postState.delegator,
        registrar = execResult.postState.registrar,
        assigners = execResult.postState.assigners.toList,
        alwaysAccers = execResult.postState.alwaysAccers.toMap,
        stagingSet = execResult.postState.stagingSet.toList,
        authQueues = execResult.postState.authQueue.map(_.toList).toList
      )

      // Collect yield/commitment if present
      execResult.yieldHash.foreach(hash =>
        commitments += Commitment(serviceId, hash)
      )

      // Collect new deferred transfers
      newDeferredTransfers ++= execResult.deferredTransfers

      // Collect provisions
      allProvisions ++= execResult.provisions

    // Apply all merged account changes to the initial state
    allAccountChanges.applyTo(initialState)

    ServiceRunResults(
      gasUsedMap = gasUsedMap.toMap,
      privilegeSnapshots = privilegeSnapshots.toMap,
      commitments = commitments.toSet,
      deferredTransfers = newDeferredTransfers.toList,
      provisions = allProvisions.toSet
    )

  private final case class PrivilegeResolution(
      manager: Long,
      delegator: Long,
      registrar: Long,
      assigners: List[Long],
      alwaysAccers: Map[Long, Long],
      stagingSet: Option[List[JamBytes]],
      authQueues: List[List[JamBytes]]
  )

  private def reconcilePrivileges(
      prior: PartialState,
      snapshots: Map[Long, PrivilegeSnapshot]
  ): PrivilegeResolution =
    // R(o, a, b) = b if a == o else a — `o` the pre-state value, `a` the
    // manager's post value, `b` the owning service's post value.
    def r(orig: Long, managerPost: Long, ownerPost: Long): Long =
      if managerPost == orig then ownerPost else managerPost

    val origManager = prior.manager
    val origDelegator = prior.delegator
    val origRegistrar = prior.registrar
    val origAssigners = prior.assigners.toList

    val managerSnapshot = snapshots.get(origManager)
    val managerPostManager =
      managerSnapshot.map(_.manager).getOrElse(origManager)
    val managerPostDelegator =
      managerSnapshot.map(_.delegator).getOrElse(origDelegator)
    val managerPostRegistrar =
      managerSnapshot.map(_.registrar).getOrElse(origRegistrar)
    val managerPostAssigners =
      managerSnapshot.map(_.assigners).getOrElse(origAssigners)
    val managerPostAlwaysAccers = managerSnapshot
      .map(_.alwaysAccers)
      .getOrElse(prior.alwaysAccers.toMap)

    val delegatorSnapshot = snapshots.get(origDelegator)
    val delegatorPostDelegator =
      delegatorSnapshot.map(_.delegator).getOrElse(origDelegator)

    val registrarSnapshot = snapshots.get(origRegistrar)
    val registrarPostRegistrar =
      registrarSnapshot.map(_.registrar).getOrElse(origRegistrar)

    val newAssigners = origAssigners.zipWithIndex.map {
      case (origAssigner, c) =>
        val managerPostAssigner =
          managerPostAssigners.lift(c).getOrElse(origAssigner)
        val assignerSnapshot = snapshots.get(origAssigner)
        val assignerPostAssigner =
          assignerSnapshot.flatMap(_.assigners.lift(c)).getOrElse(origAssigner)
        r(origAssigner, managerPostAssigner, assignerPostAssigner)
    }

    // Update auth queues: for each core c, the new auth queue comes from the original assigner's post-state
    val newAuthQueues = origAssigners.zipWithIndex.map {
      case (origAssigner, coreIndex) =>
        val assignerSnapshot = snapshots.get(origAssigner)
        // Get the auth queue for this specific core from the assigner's post-state
        assignerSnapshot.flatMap(_.authQueues.lift(coreIndex)).getOrElse {
          // If no change, use the original auth queue for this core
          prior.authQueue
            .lift(coreIndex)
            .map(_.toList)
            .getOrElse(List.empty)
        }
    }

    PrivilegeResolution(
      manager = managerPostManager,
      delegator = r(origDelegator, managerPostDelegator, delegatorPostDelegator),
      registrar = r(origRegistrar, managerPostRegistrar, registrarPostRegistrar),
      assigners = newAssigners,
      alwaysAccers = managerPostAlwaysAccers,
      stagingSet = delegatorSnapshot.map(_.stagingSet).filter(_.nonEmpty),
      authQueues = newAuthQueues
    )

  /** Compute changes a service made to state.
    */
  private def computeServiceChanges(
      serviceId: Long,
      initialState: PartialState,
      postState: PartialState
  ): AccountChanges =
    val changes = new AccountChanges()

    def accountChanged(init: Option[ServiceAccount], post: ServiceAccount): Boolean =
      init.isEmpty || (!(init.get eq post) && init.get != post)

    // Check for changes in the service's own account
    postState.accounts.get(serviceId).foreach { postAccount =>
      if accountChanged(initialState.accounts.get(serviceId), postAccount) then
        changes.accountUpdates(serviceId) = postAccount
    }

    // Check for changes in other accounts
    for (id, postAccount) <- postState.accounts if id != serviceId do
      if accountChanged(initialState.accounts.get(id), postAccount) then
        if !changes.accountUpdates.contains(id) then
          changes.accountUpdates(id) = postAccount

    // Check for removed accounts (accounts that existed in initial but not in post)
    for (id, _) <- initialState.accounts do
      if !postState.accounts.contains(id) then changes.removedAccounts += id

    for (key, value) <- postState.rawServiceDataByStateKey do
      val initValue = initialState.rawServiceDataByStateKey.get(key)
      if initValue.isEmpty || initValue.get != value then
        changes.rawServiceDataUpdates(key) = value

    // Check for removed rawServiceData keys
    for (key, _) <- initialState.rawServiceDataByStateKey do
      if !postState.rawServiceDataByStateKey.contains(key) then
        changes.rawServiceDataRemovals += key

    changes

  /** Preimage integration function.
    */
  private def preimageIntegration(
      provisions: Set[(Long, JamBytes)],
      state: PartialState,
      timeslot: Long,
      view: Option[ServiceStorageView] = None
  ): PartialState =
    for (serviceId, preimage) <- provisions do
      state.accounts.get(serviceId).foreach { account =>
        // Hash the preimage
        val preimageHash = Hashing.blake2b256(preimage.toArray)
        val preimageHashAsHash = Hash(preimageHash.bytes.toArray)
        val preimageHashBytes = JamBytes(preimageHash.bytes.toArray)
        val length = preimage.length

        // Look up the preimage info entry
        val preimageKey = PreimageKey(preimageHashAsHash, length)
        val infoStateKey = StateKey.computePreimageInfoStateKey(
          serviceId,
          length,
          preimageHashBytes
        )
        val request: Option[PreimageRequest] =
          account.preimageRequests.get(preimageKey).orElse {
            val rawInfoData = view match
              case Some(v) => v.readTrie(infoStateKey)
              case None    => state.rawServiceDataByStateKey.get(infoStateKey)
            rawInfoData.map { raw =>
              PreimageRequest(StateKey.decodePreimageInfoValue(raw))
            }
          }
        request.foreach { info =>
          if info.requestedAt.isEmpty then
            // Update preimage info with current timeslot
            state.accounts = state.accounts.updated(
              serviceId,
              account.copy(
                preimageRequests = account.preimageRequests
                  .updated(preimageKey, PreimageRequest(List(timeslot))),
                preimages =
                  account.preimages.updated(preimageHashAsHash, preimage)
              )
            )

            val infoValue =
              StateKey.encodePreimageInfoValue(List(timeslot))
            val blobStateKey = StateKey.computeServiceDataStateKey(
              serviceId,
              0xfffffffeL,
              preimageHashBytes
            )
            view match
              case Some(v) =>
                v.putByStateKey(infoStateKey, infoValue)
                v.putByStateKey(blobStateKey, preimage)
              case None =>
                state.rawServiceDataByStateKey = state.rawServiceDataByStateKey
                  .updated(infoStateKey, infoValue)
                  .updated(blobStateKey, preimage)
        }
      }
    state

  private def updateStatistics(
      gasUsedPerService: Map[Long, Long],
      workItemsPerService: Map[Long, Int],
      transferCountsPerService: Map[Long, Int]
  ): List[ServiceStatisticsEntry] =
    // Build fresh statistics from only this slot's activity
    val statsMap = mutable.Map.empty[Long, ServiceStatisticsEntry]

    // Collect all services that had accumulation activity
    val allServiceIds = gasUsedPerService.keySet ++ workItemsPerService.keySet ++
      transferCountsPerService.keySet

    for serviceId <- allServiceIds do
      val accGasUsed = gasUsedPerService.getOrElse(serviceId, 0L)
      val workItems = workItemsPerService.getOrElse(serviceId, 0)
      val transferCount = transferCountsPerService.getOrElse(serviceId, 0)

      // Only include services that actually did something
      if accGasUsed > 0 || workItems > 0 || transferCount > 0 then
        statsMap(serviceId) = ServiceStatisticsEntry(
          id = serviceId,
          record = ServiceActivityRecord(
            accumulateCount = workItems,
            accumulateTransferCount = transferCount,
            accumulateGasUsed = accGasUsed
          )
        )

    statsMap.values.toList.sortBy(_.id)

  /** Count work items per service from accumulated reports.
    */
  private def countWorkItemsPerService(
      reports: List[WorkReport]
  ): Map[Long, Int] =
    reports
      .flatMap(_.results.map(_.serviceId.value.toLong))
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toMap
