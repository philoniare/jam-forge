package io.forge.jam.protocol.accumulation

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing, StfResult}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.protocol.state.JamState
import org.bouncycastle.jcajce.provider.digest.Keccak

import scala.collection.mutable
import java.nio.{ByteBuffer, ByteOrder}

import io.forge.jam.core.types.epoch.ValidatorKey

/**
 * Accumulation State Transition Function.
 */
object AccumulationTransition:

  /**
   * Execute the Accumulation STF using unified JamState.
   *
   * Reads: tau, entropy.pool[0], accumulation (readyQueue, accumulated, privileges, serviceAccounts), validators.queue, authQueues
   * Writes: accumulation (readyQueue, accumulated, privileges, serviceAccounts), validators.queue, authQueues
   *
   * @param input The accumulation input containing slot and reports
   * @param state The unified JamState
   * @param config The chain configuration
   * @param prevSlot The previous block's slot (τ) - used for ready queue clearing per Gray Paper
   * @return Tuple of (updated JamState, AccumulationOutput)
   */
  def stf(
    input: AccumulationInput,
    state: JamState,
    config: ChainConfig,
    prevSlot: Long
  ): (JamState, AccumulationOutput) =
    val entropyBytes = state.entropy.firstAsBytes

    // Convert validator queue to JamBytes for PartialState initialization
    val initStagingSet: List[JamBytes] = state.validators.queue.map(vk => vk.toJamBytes)

    // Convert auth queues to JamBytes for PartialState initialization
    val initAuthQueues: List[List[JamBytes]] = state.authQueues.map(_.map(h => JamBytes(h.bytes)))

    // Convert JamState fields to AccumulationState for existing logic
    val preState = AccumulationState(
      slot = state.tau,
      entropy = entropyBytes,
      readyQueue = state.accumulation.readyQueue,
      accumulated = state.accumulation.accumulated,
      privileges = state.accumulation.privileges,
      statistics = List.empty, // Not used in accumulation input
      accounts = state.accumulation.serviceAccounts,
      rawServiceDataByStateKey = mutable.Map.from(state.rawServiceDataByStateKey)
    )

    val (postState, postStagingSet, postAuthQueues, output) =
      stfInternal(input, preState, initStagingSet, initAuthQueues, config, prevSlot)

    val stagingSetChanged = postStagingSet.map(_.toHex) != initStagingSet.map(_.toHex)

    // Convert staging set back to ValidatorKey list only if it changed
    val newValidatorQueue: List[ValidatorKey] =
      if stagingSetChanged then postStagingSet.map(ValidatorKey.fromJamBytes)
      else state.validators.queue

    // Check if auth queues actually changed
    val authQueuesChanged = postAuthQueues.map(_.map(_.toHex)) != initAuthQueues.map(_.map(_.toHex))

    // Convert auth queues back to Hash list only if changed
    val newAuthQueues: List[List[Hash]] =
      if authQueuesChanged then postAuthQueues.map(_.map(jb => Hash(jb.toArray)))
      else state.authQueues

    // Update JamState with results
    val updatedState = state.copy(
      validators = state.validators.copy(queue = newValidatorQueue),
      authQueues = newAuthQueues,
      accumulation = state.accumulation.copy(
        readyQueue = postState.readyQueue,
        accumulated = postState.accumulated,
        privileges = postState.privileges,
        serviceAccounts = postState.accounts
      ),
      rawServiceDataByStateKey = postState.rawServiceDataByStateKey.toMap
    )

    (updatedState, output)

  /**
   * Internal Accumulation STF implementation using AccumulationState.
   *
   * @param input The accumulation input containing slot and reports
   * @param preState The pre-transition state
   * @param initStagingSet Initial staging set (validator queue) as list of 336-byte JamBytes
   * @param initAuthQueues Initial authorization queues per core as list of lists of 32-byte hashes
   * @param config The accumulation configuration
   * @param prevSlot The previous block's slot
   * @return Tuple of (post-transition state, post staging set, post auth queues, output)
   */
  def stfInternal(
    input: AccumulationInput,
    preState: AccumulationState,
    initStagingSet: List[JamBytes],
    initAuthQueues: List[List[JamBytes]],
    config: ChainConfig,
    prevSlot: Long
  ): (AccumulationState, List[JamBytes], List[List[JamBytes]], AccumulationOutput) =
    val m = (input.slot % config.epochLength).toInt
    val deltaT = Math.max((input.slot - prevSlot).toInt, 1)

    // 1. Collect all historically accumulated hashes (for dependency checking)
    val historicallyAccumulated = mutable.Set.from(preState.accumulated.flatten)

    // 2. Partition new reports into immediate vs queued
    val (immediateReports, queuedReports) = input.reports.partition { report =>
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

    // 4. Build working copy of ready queue with edited dependencies
    val workingReadyQueue = preState.readyQueue.indices.map { slotIdx =>
      val oldRecords = preState.readyQueue(slotIdx)
      editReadyQueueRecords(oldRecords, historicallyAccumulated.toSet)
    }.toList.to(mutable.ListBuffer)

    // Build new queued records
    val newRecords = queuedReports.map { report =>
      val prereqs = report.context.prerequisites
      val segmentDeps = report.segmentRootLookup.map(_.workPackageHash)
      val allDeps = (prereqs ++ segmentDeps).filter(h => !historicallyAccumulated.contains(JamBytes(h.bytes)))
      AccumulationReadyRecord(report, allDeps)
    }

    // Edit new records to remove already-accumulated dependencies
    val editedNewRecords = editReadyQueueRecords(newRecords, historicallyAccumulated.toSet)

    // 5. Extract accumulatable reports from ready queue
    val epochLen = config.epochLength
    val reorderedSlots = (m until epochLen) ++ (0 until m)
    val existingQueuedWithSlots = reorderedSlots.flatMap { slotIdx =>
      workingReadyQueue(slotIdx).map(record => (slotIdx, record))
    }.toList
    // Add new records at the end
    val allQueuedWithSlots = existingQueuedWithSlots ++ editedNewRecords.map(r => (m, r))

    val (readyToAccumulate, stillQueuedWithSlots) = extractAccumulatableWithSlots(
      allQueuedWithSlots,
      historicallyAccumulated.toSet
    )

    // 6. Add ready-to-accumulate reports to accumulated set
    readyToAccumulate.foreach { report =>
      val hash = JamBytes(report.packageSpec.hash.bytes.toArray)
      newAccumulated += hash
      historicallyAccumulated += hash
    }

    // 7. Rebuild ready queue with remaining records
    val accumulatedThisBlock = newAccumulated.toSet

    val newQueuedReportsNotAccumulated = stillQueuedWithSlots
      .filter {
        case (slot, record) =>
          slot == m && newRecords.exists(nr =>
            JamBytes(nr.report.packageSpec.hash.bytes.toArray) ==
              JamBytes(record.report.packageSpec.hash.bytes.toArray)
          )
      }
      .map(_._2)

    // Edit dependencies to remove accumulated hashes
    val editedNewQueuedReports = editReadyQueueRecords(newQueuedReportsNotAccumulated, accumulatedThisBlock)

    val finalReadyQueue = (0 until config.epochLength).map { idx =>
      val i = ((m - idx) % config.epochLength + config.epochLength) % config.epochLength
      if i == 0 then
        // Current slot: ONLY new queued reports from this block, with dependencies edited
        editedNewQueuedReports
      else if i >= 1 && i < deltaT then
        // Slots that wrapped around - clear them
        List.empty[AccumulationReadyRecord]
      else
        // Other slots: keep remaining items that weren't accumulated, with dependencies edited
        val records = stillQueuedWithSlots.filter(_._1 == idx).map(_._2).toList
        editReadyQueueRecords(records, accumulatedThisBlock)
    }.toList

    // 8. Execute PVM for accumulated reports (respecting gas budget)
    val allToAccumulate = immediateReports ++ readyToAccumulate
    val partialState = preState.toPartialState(initStagingSet, initAuthQueues)

    // Calculate total gas budget
    val sumPrivilegedGas = partialState.alwaysAccers.values.sum
    val minTotalGas = config.reportAccGas * config.coresCount + sumPrivilegedGas
    val totalGasLimit = Math.max(config.maxBlockGas, minTotalGas)

    // Create executor (now directly uses PVM, no strategy pattern needed)
    val executor = new AccumulationExecutor(config)

    // Execute outer accumulation with recursive deferred transfer processing
    val outerResult = outerAccumulate(
      partialState = partialState,
      transfers = List.empty,
      workReports = allToAccumulate,
      alwaysAccers = partialState.alwaysAccers.toMap,
      gasLimit = totalGasLimit,
      timeslot = input.slot,
      entropy = preState.entropy,
      executor = executor,
      config = config
    )

    // Determine which reports were actually accumulated (based on reportsAccumulated count)
    val reportsToAccumulate = allToAccumulate.take(outerResult.reportsAccumulated)

    // Rebuild actuallyAccumulated to only include reports that will actually be accumulated
    val actuallyAccumulated = mutable.Set.empty[JamBytes]
    reportsToAccumulate.foreach(report => actuallyAccumulated += JamBytes(report.packageSpec.hash.bytes.toArray))

    val newPartialState = outerResult.postState
    val gasUsedPerService = outerResult.gasUsedMap
    val commitments = outerResult.commitments

    // 9. Rotate accumulated array (sliding window)
    val newAccumulatedList = actuallyAccumulated.toList.sortBy(_.toHex)
    val newAccumulatedArray = (0 until config.epochLength).map { idx =>
      if idx == config.epochLength - 1 then
        // New items at last position
        newAccumulatedList
      else
        // Shift left by 1
        preState.accumulated.lift(idx + 1).getOrElse(List.empty)
    }.toList

    // 10. Update statistics
    val workItemsPerService = countWorkItemsPerService(reportsToAccumulate)
    val transferStatsPerService: Map[Long, (Long, Long)] = Map.empty // TODO: compute from accumulation results
    val newStatistics = updateStatistics(
      gasUsedPerService,
      workItemsPerService,
      transferStatsPerService
    )

    // 11. Build accumulation stats for fresh service statistics computation
    val accumulationStats: Map[Long, (Long, Int)] = gasUsedPerService
      .map {
        case (serviceId, gasUsed) =>
          val count = workItemsPerService.getOrElse(serviceId, 0)
          serviceId -> (gasUsed, count)
      }
      .filter { case (_, (gas, count)) => gas > 0 || count > 0 }

    // 12. Update lastAccumulationSlot for all services in accumulationStats
    for (serviceId, _) <- accumulationStats do
      newPartialState.accounts.get(serviceId).foreach { account =>
        newPartialState.accounts(serviceId) = account.copy(
          info = account.info.copy(lastAccumulationSlot = input.slot)
        )
      }

    // 13. Use privileges from the posterior state after all batches
    val finalManager = outerResult.postState.manager
    val finalDelegator = outerResult.postState.delegator
    val finalRegistrar = outerResult.postState.registrar
    val finalAssigners = outerResult.postState.assigners.toList
    val finalAlwaysAccers = outerResult.postState.alwaysAccers.toMap


    val finalState = AccumulationState(
      slot = input.slot,
      entropy = JamBytes(preState.entropy.toArray),
      readyQueue = finalReadyQueue,
      accumulated = newAccumulatedArray,
      privileges = Privileges(
        bless = finalManager,
        assign = finalAssigners,
        designate = finalDelegator,
        register = finalRegistrar,
        alwaysAcc = finalAlwaysAccers.toList.sortBy(_._1).map {
          case (id, gas) =>
            AlwaysAccItem(id, gas)
        }
      ),
      statistics = newStatistics,
      accounts = newPartialState.toAccumulationServiceItems(),
      rawServiceDataByStateKey = newPartialState.rawServiceDataByStateKey,
      rawServiceAccountsByStateKey = newPartialState.rawServiceAccountsByStateKey
    )

    // 14. Compute commitment root from yields
    val outputHash = computeCommitmentRoot(commitments)

    // 15. Get transfer stats from outerResult
    val transferStats = outerResult.transferStatsMap

    // 16. Convert commitments to list format for state storage (key 0x10)
    val commitmentsList = commitments.toList.sortBy(c => (c.serviceIndex, c.hash.toHex)).map { c =>
      (c.serviceIndex, c.hash)
    }

    // 17. Extract post staging set and auth queues from posterior state
    val postStagingSet = outerResult.postState.stagingSet.toList

    // Auth queues: use the posterior state's auth queues
    val postAuthQueues = outerResult.postState.authQueue.map(_.toList).toList

    (
      finalState,
      postStagingSet,
      postAuthQueues,
      StfResult.success(AccumulationOutputData(outputHash, accumulationStats, transferStats, commitmentsList))
    )

  /**
   * Edit ready queue records by removing accumulated reports and pruning dependencies.
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
          dependencies = record.dependencies.filter(h => !accumulatedHashes.contains(JamBytes(h.bytes)))
        )
      }

  /**
   * Extract accumulatable reports while preserving slot information.
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
      val (ready, notReady) = remaining.partition {
        case (_, record) =>
          record.dependencies.forall(h => accumulated.contains(JamBytes(h.bytes)))
      }
      if ready.isEmpty then
        continue = false
      else
        ready.foreach {
          case (_, record) =>
            result += record.report
            accumulated += JamBytes(record.report.packageSpec.hash.bytes.toArray)
        }
        remaining = notReady

    (result.toList, remaining)

  /**
   * Result of outer accumulation.
   */
  case class OuterAccumulationResult(
    reportsAccumulated: Int,
    postState: PartialState,
    gasUsedMap: Map[Long, Long],
    commitments: Set[Commitment],
    privilegeSnapshots: Map[Long, PrivilegeSnapshot] = Map.empty,
    transferStatsMap: Map[Long, (Long, Long)] = Map.empty // serviceId -> (count, gasUsed)
  )

  /**
   * Snapshot of privilege state values at a point in time.
   * Also includes staging set and auth queues for final state computation.
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

  /**
   * Outer accumulation function.
   * Recursively processes work reports and deferred transfers.
   */
  private def outerAccumulate(
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
    var sumGasRequired = 0L

    val reportIterator = workReports.iterator
    var continue = true
    while reportIterator.hasNext && continue do
      val report = reportIterator.next()
      var canAccumulate = true
      for result <- report.results if canAccumulate do
        if result.accumulateGas.toLong + sumGasRequired > gasLimit then
          canAccumulate = false
        else
          sumGasRequired += result.accumulateGas.toLong
      if canAccumulate then
        i += 1
      else
        continue = false

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
    val transfersGas = transfers.map(_.gasLimit).sum

    // Recursively process remaining reports with new deferred transfers
    val remainingReports = workReports.drop(i)
    val newTransfers = parallelResult.deferredTransfers
    val stateForRecursion = parallelResult.postState

    // Recursive call if there are new transfers or remaining reports
    val outerResult = outerAccumulate(
      partialState = stateForRecursion,
      transfers = newTransfers,
      workReports = remainingReports,
      alwaysAccers = Map.empty, // Always-accumulate services only processed in first iteration
      gasLimit = gasLimit + transfersGas - parallelGasUsed,
      timeslot = timeslot,
      entropy = entropy,
      executor = executor,
      config = config
    )

    // Merge results
    val mergedGasUsed = (parallelResult.gasUsedMap.keys ++ outerResult.gasUsedMap.keys).toSet.map { serviceId =>
      serviceId -> (parallelResult.gasUsedMap.getOrElse(serviceId, 0L) + outerResult.gasUsedMap.getOrElse(
        serviceId,
        0L
      ))
    }.toMap

    // Merge privilege snapshots:
    // - For privilege fields (manager, delegator, registrar, assigners, alwaysAccers):
    //   FIRST batch takes precedence
    // - For stagingSet and authQueues: LAST update wins
    val allServiceIds = (parallelResult.privilegeSnapshots.keys ++ outerResult.privilegeSnapshots.keys).toSet
    val mergedSnapshots = allServiceIds.map { serviceId =>
      val parallel = parallelResult.privilegeSnapshots.get(serviceId)
      val outer = outerResult.privilegeSnapshots.get(serviceId)

      (parallel, outer) match
        case (Some(p), Some(o)) =>
          // Both batches have this service - merge field by field
          // For privileges, use first batch (parallel); for stagingSet/authQueues, use last update (outer if non-empty)
          val mergedStagingSet = if o.stagingSet.nonEmpty then o.stagingSet else p.stagingSet
          val mergedAuthQueues = if o.authQueues.nonEmpty && o.authQueues.exists(_.nonEmpty) then o.authQueues else p.authQueues
          serviceId -> PrivilegeSnapshot(
            manager = p.manager,
            delegator = p.delegator,
            registrar = p.registrar,
            assigners = p.assigners,
            alwaysAccers = p.alwaysAccers,
            stagingSet = mergedStagingSet,
            authQueues = mergedAuthQueues
          )
        case (Some(p), None) => serviceId -> p
        case (None, Some(o)) => serviceId -> o
        case (None, None) => throw new RuntimeException(s"Unexpected: service $serviceId in allServiceIds but not in any snapshot")
    }.toMap

    // Merge transfer stats
    val mergedTransferStats =
      (parallelResult.transferStatsMap.keys ++ outerResult.transferStatsMap.keys).toSet.map { serviceId =>
        val (c1, g1) = parallelResult.transferStatsMap.getOrElse(serviceId, (0L, 0L))
        val (c2, g2) = outerResult.transferStatsMap.getOrElse(serviceId, (0L, 0L))
        serviceId -> (c1 + c2, g1 + g2)
      }.toMap

    OuterAccumulationResult(
      reportsAccumulated = i + outerResult.reportsAccumulated,
      postState = outerResult.postState,
      gasUsedMap = mergedGasUsed,
      commitments = parallelResult.commitments ++ outerResult.commitments,
      privilegeSnapshots = mergedSnapshots,
      transferStatsMap = mergedTransferStats
    )

  /**
   * Result of parallel accumulation execution.
   */
  case class AccumulationExecResult(
    postState: PartialState,
    gasUsedMap: Map[Long, Long],
    commitments: Set[Commitment],
    deferredTransfers: List[DeferredTransfer] = List.empty,
    privilegeSnapshots: Map[Long, PrivilegeSnapshot] = Map.empty,
    transferStatsMap: Map[Long, (Long, Long)] = Map.empty // serviceId -> (count, gasUsed)
  )

  /**
   * Execute PVM accumulation for all reports.
   * In v0.7.0, deferred transfers are processed separately via on_transfer (PC=10),
   * not mixed with work items in accumulate (PC=5).
   */
  private def executeAccumulation(
    partialState: PartialState,
    reports: List[WorkReport],
    deferredTransfers: List[DeferredTransfer],
    alwaysAccers: Map[Long, Long],
    timeslot: Long,
    entropy: JamBytes,
    executor: AccumulationExecutor
  ): AccumulationExecResult =
    val gasUsedMap = mutable.Map.empty[Long, Long]
    val commitments = mutable.Set.empty[Commitment]
    val newDeferredTransfers = mutable.ListBuffer.empty[DeferredTransfer]
    val allProvisions = mutable.Set.empty[(Long, JamBytes)]
    val initialState = partialState.deepCopy()
    val transferStatsMap = mutable.Map.empty[Long, (Long, Long)] // serviceId -> (count, gasUsed)

    // Group work items AND transfers by service (v0.7.1 - unified accumulate entry point)
    val serviceOperands = mutable.Map.empty[Long, mutable.ListBuffer[AccumulationOperand]]

    // Add deferred transfers as operands (v0.7.1 - transfers processed in accumulate)
    for transfer <- deferredTransfers do
      serviceOperands.getOrElseUpdate(transfer.destination, mutable.ListBuffer.empty) +=
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
        serviceOperands.getOrElseUpdate(result.serviceId.value.toLong, mutable.ListBuffer.empty) +=
          AccumulationOperand.WorkItem(operand)

    // Collect all services to accumulate (work items + always-accers + transfer destinations)
    val servicesToAccumulate = mutable.Set.empty[Long]
    servicesToAccumulate ++= serviceOperands.keys
    servicesToAccumulate ++= alwaysAccers.keys

    if servicesToAccumulate.isEmpty && deferredTransfers.isEmpty then
      return AccumulationExecResult(partialState, Map.empty, Set.empty, List.empty)

    // Track privilege snapshots
    val privilegeSnapshots = mutable.Map.empty[Long, PrivilegeSnapshot]

    // Collect account changes from all services for merging
    val allAccountChanges = new AccountChanges()

    // Execute services sequentially (for now - can be parallelized later)
    val sortedServices = servicesToAccumulate.toList.sorted

    for serviceId <- sortedServices do
      val operands = serviceOperands.getOrElse(serviceId, mutable.ListBuffer.empty).toList
      val alwaysAccGas = alwaysAccers.getOrElse(serviceId, 0L)
      val workItemGas = operands.collect { case AccumulationOperand.WorkItem(op) => op.gasLimit }.sum
      val transferGas = operands.collect { case AccumulationOperand.Transfer(t) => t.gasLimit }.sum
      val totalGasLimit = workItemGas + alwaysAccGas + transferGas

      val serviceInitialState = initialState.deepCopy()

      val execResult = executor.executeService(
        partialState = serviceInitialState,
        timeslot = timeslot,
        serviceId = serviceId,
        gasLimit = totalGasLimit,
        entropy = entropy,
        operands = operands
      )

      // Compute changes this service made
      val serviceChanges = computeServiceChanges(serviceId, initialState, execResult.postState)

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
      execResult.yieldHash.foreach(hash => commitments += Commitment(serviceId, hash))

      // Collect new deferred transfers
      newDeferredTransfers ++= execResult.deferredTransfers

      // Collect provisions
      allProvisions ++= execResult.provisions

    // Apply all merged account changes to the initial state
    val finalState = initialState.deepCopy()
    allAccountChanges.applyTo(finalState)
    
    val origManager = partialState.manager
    val origDelegator = partialState.delegator
    val origRegistrar = partialState.registrar
    val origAssigners = partialState.assigners.toList

    val managerSnapshot = privilegeSnapshots.get(origManager)
    val managerPostManager = managerSnapshot.map(_.manager).getOrElse(origManager)
    val managerPostDelegator = managerSnapshot.map(_.delegator).getOrElse(origDelegator)
    val managerPostRegistrar = managerSnapshot.map(_.registrar).getOrElse(origRegistrar)
    val managerPostAssigners = managerSnapshot.map(_.assigners).getOrElse(origAssigners)
    val managerPostAlwaysAccers = managerSnapshot.map(_.alwaysAccers).getOrElse(partialState.alwaysAccers.toMap)

    val delegatorSnapshot = privilegeSnapshots.get(origDelegator)
    val delegatorPostDelegator = delegatorSnapshot.map(_.delegator).getOrElse(origDelegator)

    val registrarSnapshot = privilegeSnapshots.get(origRegistrar)
    val registrarPostRegistrar = registrarSnapshot.map(_.registrar).getOrElse(origRegistrar)

    // Apply R function: R(o, a, b) = b if a == o else a
    finalState.manager = managerPostManager
    finalState.delegator = if managerPostDelegator == origDelegator then delegatorPostDelegator else managerPostDelegator
    finalState.registrar = if managerPostRegistrar == origRegistrar then registrarPostRegistrar else managerPostRegistrar
    finalState.assigners.clear()
    finalState.assigners ++= origAssigners.zipWithIndex.map { case (origAssigner, c) =>
      val managerPostAssigner = managerPostAssigners.lift(c).getOrElse(origAssigner)
      val assignerSnapshot = privilegeSnapshots.get(origAssigner)
      val assignerPostAssigner = assignerSnapshot.flatMap(_.assigners.lift(c)).getOrElse(origAssigner)
      if managerPostAssigner == origAssigner then assignerPostAssigner else managerPostAssigner
    }
    finalState.alwaysAccers.clear()
    finalState.alwaysAccers ++= managerPostAlwaysAccers

    // Also update stagingSet from delegator's post-state
    val delegatorStagingSet = delegatorSnapshot.map(_.stagingSet).filter(_.nonEmpty)
    delegatorStagingSet.foreach { ss =>
      finalState.stagingSet.clear()
      finalState.stagingSet ++= ss
    }

    // Update auth queues: for each core c, the new auth queue comes from the original assigner's post-state
    val newAuthQueues = origAssigners.zipWithIndex.map { case (origAssigner, coreIndex) =>
      val assignerSnapshot = privilegeSnapshots.get(origAssigner)
      // Get the auth queue for this specific core from the assigner's post-state
      assignerSnapshot.flatMap(_.authQueues.lift(coreIndex)).getOrElse {
        // If no change, use the original auth queue for this core
        partialState.authQueue.lift(coreIndex).map(_.toList).getOrElse(List.empty)
      }
    }
    finalState.authQueue.clear()
    finalState.authQueue ++= newAuthQueues.map(q => mutable.ListBuffer.from(q))

    // Process preimage integrations on the final merged state
    val stateAfterPreimages = if allProvisions.nonEmpty then
      preimageIntegration(allProvisions.toSet, finalState, timeslot)
    else
      finalState
    AccumulationExecResult(
      stateAfterPreimages,
      gasUsedMap.toMap,
      commitments.toSet,
      newDeferredTransfers.toList,
      privilegeSnapshots.toMap,
      transferStatsMap.toMap
    )

  /**
   * Compute changes a service made to state.
   */
  private def computeServiceChanges(
    serviceId: Long,
    initialState: PartialState,
    postState: PartialState
  ): AccountChanges =
    val changes = new AccountChanges()

    // Check for changes in the service's own account
    postState.accounts.get(serviceId).foreach { postAccount =>
      val initAccount = initialState.accounts.get(serviceId)
      if initAccount.isEmpty || initAccount.get != postAccount then
        changes.accountUpdates(serviceId) = postAccount
    }

    // Check for changes in other accounts
    for (id, postAccount) <- postState.accounts if id != serviceId do
      val initAccount = initialState.accounts.get(id)
      if initAccount.isEmpty || initAccount.get != postAccount then
        if !changes.accountUpdates.contains(id) then
          changes.accountUpdates(id) = postAccount

    // Check for removed accounts (accounts that existed in initial but not in post)
    for (id, _) <- initialState.accounts do
      if !postState.accounts.contains(id) then
        changes.removedAccounts += id

    // Check for privilege changes
    if postState.manager != initialState.manager then
      changes.managerChange = Some(postState.manager)
    if postState.delegator != initialState.delegator then
      changes.delegatorChange = Some(postState.delegator)
    if postState.registrar != initialState.registrar then
      changes.registrarChange = Some(postState.registrar)
    if postState.assigners.toList != initialState.assigners.toList then
      changes.assignersChange = Some(postState.assigners.toList)
    if postState.alwaysAccers.toMap != initialState.alwaysAccers.toMap then
      changes.alwaysAccersChange = Some(postState.alwaysAccers.toMap)

    // Check for staging set and auth queue changes
    if postState.stagingSet.toList != initialState.stagingSet.toList then
      changes.stagingSetChange = Some(postState.stagingSet.toList)
    if postState.authQueue.map(_.toList).toList != initialState.authQueue.map(_.toList).toList then
      changes.authQueueChange = Some(postState.authQueue.toList)

    // Check for rawServiceData changes (added or updated keys)
    for (key, value) <- postState.rawServiceDataByStateKey do
      val initValue = initialState.rawServiceDataByStateKey.get(key)
      if initValue.isEmpty || initValue.get != value then
        changes.rawServiceDataUpdates(key) = value

    // Check for removed rawServiceData keys
    for (key, _) <- initialState.rawServiceDataByStateKey do
      if !postState.rawServiceDataByStateKey.contains(key) then
        changes.rawServiceDataRemovals += key

    changes

  /**
   * Preimage integration function.
   */
  private def preimageIntegration(
    provisions: Set[(Long, JamBytes)],
    state: PartialState,
    timeslot: Long
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
        account.preimageRequests.get(preimageKey).foreach { info =>
          if info.requestedAt.isEmpty then
            // Update preimage info with current timeslot
            account.preimageRequests(preimageKey) = PreimageRequest(List(timeslot))
            // Store the preimage blob
            account.preimages(preimageHashAsHash) = preimage

            // Update raw state data
            val infoStateKey = StateKey.computePreimageInfoStateKey(serviceId, length, preimageHashBytes)
            state.rawServiceDataByStateKey(infoStateKey) = StateKey.encodePreimageInfoValue(List(timeslot))

            val blobStateKey = StateKey.computeServiceDataStateKey(serviceId, 0xfffffffeL, preimageHashBytes)
            state.rawServiceDataByStateKey(blobStateKey) = preimage
        }
      }
    state

  private def updateStatistics(
    gasUsedPerService: Map[Long, Long],
    workItemsPerService: Map[Long, Int],
    transferStatsPerService: Map[Long, (Long, Long)]
  ): List[ServiceStatisticsEntry] =
    // Build fresh statistics from only this slot's activity
    val statsMap = mutable.Map.empty[Long, ServiceStatisticsEntry]

    // Collect all services that had activity (accumulation or transfers)
    val allServiceIds = gasUsedPerService.keys ++ transferStatsPerService.keys

    for serviceId <- allServiceIds do
      val accGasUsed = gasUsedPerService.getOrElse(serviceId, 0L)
      val workItems = workItemsPerService.getOrElse(serviceId, 0)
      val (transferCount, _) = transferStatsPerService.getOrElse(serviceId, (0L, 0L))

      // Only include services that actually did something
      if accGasUsed > 0 || workItems > 0 || transferCount > 0 then
        statsMap(serviceId) = ServiceStatisticsEntry(
          id = serviceId,
          record = ServiceActivityRecord(
            accumulateCount = workItems,
            accumulateGasUsed = accGasUsed
          )
        )

    statsMap.values.toList.sortBy(_.id)

  /**
   * Count work items per service from accumulated reports.
   */
  private def countWorkItemsPerService(reports: List[WorkReport]): Map[Long, Int] =
    reports.flatMap(_.results.map(_.serviceId.value.toLong))
      .groupBy(identity)
      .view.mapValues(_.size)
      .toMap

  /**
   * Compute the Keccak Merkle root of service commitments.
   */
  private def computeCommitmentRoot(commitments: Set[Commitment]): JamBytes =
    if commitments.isEmpty then
      return JamBytes(new Array[Byte](32))

    // Sort by service index, then by hash for deterministic ordering
    val sortedCommitments = commitments.toList.sortBy(c => (c.serviceIndex, c.hash.toHex))
    val nodes = sortedCommitments.map { commitment =>
      val buffer = ByteBuffer.allocate(4 + 32).order(ByteOrder.LITTLE_ENDIAN)
      buffer.putInt(commitment.serviceIndex.toInt)
      buffer.put(commitment.hash.toArray)
      buffer.array()
    }

    // Binary Merkle tree with Keccak-256
    JamBytes(binaryMerklize(nodes))

  /**
   * Well-balanced binary Merkle function.
   */
  private def binaryMerklize(leaves: List[Array[Byte]]): Array[Byte] =
    leaves match
      case Nil => new Array[Byte](32)
      case head :: Nil => keccak256(head)
      case _ =>
        binaryMerklizeHelper(leaves) match
          case MerklizeResult.Leaf(data) => keccak256(data)
          case MerklizeResult.Hash(hash) => hash

  /**
   * Merkle result can be either a leaf (unhashed data) or a hash.
   */
  private enum MerklizeResult:
    case Leaf(data: Array[Byte])
    case Hash(hash: Array[Byte])

    def toByteArray: Array[Byte] = this match
      case Leaf(data) => data
      case Hash(hash) => hash

  /**
   * Helper for well-balanced binary Merkle tree.
   */
  private def binaryMerklizeHelper(nodes: List[Array[Byte]]): MerklizeResult =
    nodes match
      case Nil => MerklizeResult.Hash(new Array[Byte](32))
      case head :: Nil => MerklizeResult.Leaf(head)
      case _ =>
        val mid = (nodes.size + 1) / 2 // roundup of half
        val left = nodes.take(mid)
        val right = nodes.drop(mid)
        val leftResult = binaryMerklizeHelper(left)
        val rightResult = binaryMerklizeHelper(right)
        // Hash with "node" prefix as per GP E.1.1
        MerklizeResult.Hash(
          keccakHashWithPrefix(
            "node".getBytes,
            leftResult.toByteArray,
            rightResult.toByteArray
          )
        )

  private def keccak256(data: Array[Byte]): Array[Byte] =
    val digest = new Keccak.Digest256()
    digest.update(data, 0, data.length)
    digest.digest()

  private def keccakHashWithPrefix(prefix: Array[Byte], left: Array[Byte], right: Array[Byte]): Array[Byte] =
    val digest = new Keccak.Digest256()
    digest.update(prefix, 0, prefix.length)
    digest.update(left, 0, left.length)
    digest.update(right, 0, right.length)
    digest.digest()

/**
 * Account changes tracker for merging parallel service executions.
 */
class AccountChanges:
  val accountUpdates: mutable.Map[Long, ServiceAccount] = mutable.Map.empty
  val removedAccounts: mutable.Set[Long] = mutable.Set.empty
  var managerChange: Option[Long] = None
  var delegatorChange: Option[Long] = None
  var registrarChange: Option[Long] = None
  var assignersChange: Option[List[Long]] = None
  var alwaysAccersChange: Option[Map[Long, Long]] = None
  // Storage data changes (for WRITE host call updates)
  val rawServiceDataUpdates: mutable.Map[JamBytes, JamBytes] = mutable.Map.empty
  val rawServiceDataRemovals: mutable.Set[JamBytes] = mutable.Set.empty
  // Staging set and auth queue changes (from designate and assign host calls)
  var stagingSetChange: Option[List[JamBytes]] = None
  var authQueueChange: Option[List[mutable.ListBuffer[JamBytes]]] = None

  def checkAndMerge(other: AccountChanges): Unit =
    // Merge account updates
    for (id, account) <- other.accountUpdates do
      if !accountUpdates.contains(id) then
        accountUpdates(id) = account

    // Merge removed accounts
    removedAccounts ++= other.removedAccounts

    // Merge privilege changes
    if other.managerChange.isDefined && managerChange.isEmpty then
      managerChange = other.managerChange
    if other.delegatorChange.isDefined && delegatorChange.isEmpty then
      delegatorChange = other.delegatorChange
    if other.registrarChange.isDefined && registrarChange.isEmpty then
      registrarChange = other.registrarChange
    if other.assignersChange.isDefined && assignersChange.isEmpty then
      assignersChange = other.assignersChange
    if other.alwaysAccersChange.isDefined && alwaysAccersChange.isEmpty then
      alwaysAccersChange = other.alwaysAccersChange

    // Merge staging set and auth queue changes (first write wins)
    if other.stagingSetChange.isDefined && stagingSetChange.isEmpty then
      stagingSetChange = other.stagingSetChange
    if other.authQueueChange.isDefined && authQueueChange.isEmpty then
      authQueueChange = other.authQueueChange

    // Merge rawServiceData updates (first write wins for conflicts)
    for (key, value) <- other.rawServiceDataUpdates do
      if !rawServiceDataUpdates.contains(key) then
        rawServiceDataUpdates(key) = value
    rawServiceDataRemovals ++= other.rawServiceDataRemovals

  def applyTo(state: PartialState): Unit =
    for id <- removedAccounts do
      state.accounts.remove(id)

      val serviceIdBytes = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(id.toInt).array()
      val keysToRemove = state.rawServiceDataByStateKey.keys.filter { key =>
        key.length >= 8 &&
        key.toArray(0) == serviceIdBytes(0) &&
        key.toArray(2) == serviceIdBytes(1) &&
        key.toArray(4) == serviceIdBytes(2) &&
        key.toArray(6) == serviceIdBytes(3)
      }.toList
      keysToRemove.foreach(state.rawServiceDataByStateKey.remove)

      // Also remove the service account key from rawServiceAccountsByStateKey
      val serviceAccountKey = StateKey.computeServiceAccountKey(id)
      state.rawServiceAccountsByStateKey.remove(serviceAccountKey)

    // Apply account updates
    for (id, account) <- accountUpdates do
      state.accounts(id) = account

    // Apply privilege changes
    managerChange.foreach(state.manager = _)
    delegatorChange.foreach(state.delegator = _)
    registrarChange.foreach(state.registrar = _)
    assignersChange.foreach { assigners =>
      state.assigners.clear()
      state.assigners ++= assigners
    }
    alwaysAccersChange.foreach { alwaysAccers =>
      state.alwaysAccers.clear()
      state.alwaysAccers ++= alwaysAccers
    }

    // Apply staging set and auth queue changes
    stagingSetChange.foreach { stagingSet =>
      state.stagingSet.clear()
      state.stagingSet ++= stagingSet
    }
    authQueueChange.foreach { authQueue =>
      state.authQueue.clear()
      state.authQueue ++= authQueue
    }

    // Apply rawServiceData changes
    for key <- rawServiceDataRemovals do
      state.rawServiceDataByStateKey.remove(key)
    for (key, value) <- rawServiceDataUpdates do
      state.rawServiceDataByStateKey(key) = value
