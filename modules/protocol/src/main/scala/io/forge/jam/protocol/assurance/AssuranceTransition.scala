package io.forge.jam.protocol.assurance

import io.forge.jam.core.{ChainConfig, Hashing, constants, StfResult, ValidationHelpers}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.protocol.assurance.AssuranceTypes.*
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.AssuranceBridge
import io.forge.jam.crypto.Ed25519

/**
 * Assurances State Transition Function.
 *
 * Processes availability assurances from validators, tracking which work reports
 * have achieved sufficient attestations (2/3 supermajority) for availability confirmation.
 *
 * Key operations:
 * - Verify Ed25519 signatures on assurance extrinsics
 * - Track availability attestations per core via bitfield processing
 * - Remove stale reports that have timed out
 * - Confirm availability when supermajority is reached
 */
object AssuranceTransition:

  /**
   * Check if a bit is set in a byte array at the given position.
   */
  private def isBitSet(bytes: Array[Byte], position: Int): Boolean =
    val byteIndex = position / 8
    val bitIndex = position % 8
    if byteIndex >= bytes.length then false
    else (bytes(byteIndex).toInt & (1 << bitIndex)) != 0

  /**
   * Verify Ed25519 signature on an assurance extrinsic.
   *
   * The signature message is: "jam_available" + blake2b(anchor + bitfield)
   */
  private def verifyAssuranceSignature(
    assurance: AssuranceExtrinsic,
    validatorKey: ValidatorKey
  ): Boolean =
    // First create combined data and hash it
    val serializedData = assurance.anchor.bytes ++ assurance.bitfield.toArray
    val dataHash = Hashing.blake2b256(serializedData)

    // Create final message by prepending context
    val signatureMessage = constants.JAM_AVAILABLE_BYTES ++ dataHash.bytes

    // Verify using centralized Ed25519 module
    Ed25519.verify(validatorKey.ed25519, signatureMessage, assurance.signature)

  /**
   * Check if a report has timed out.
   */
  private def isReportTimedOut(timeout: Long, currentSlot: Long, config: ChainConfig): Boolean =
    currentSlot >= timeout + config.assuranceTimeoutPeriod

  /**
   * Handle timeouts by clearing stale assignments.
   */
  private def handleTimeouts(state: AssuranceState, currentSlot: Long, config: ChainConfig): AssuranceState =
    val newAssignments = state.availAssignments.map {
      case Some(assignment) if isReportTimedOut(assignment.timeout, currentSlot, config) => None
      case other => other
    }
    state.copy(availAssignments = newAssignments)

  /**
   * Validate that assurance bitfields only reference engaged cores.
   */
  private def validateCoreEngagement(assurances: List[AssuranceExtrinsic], state: AssuranceState): Boolean =
    val coresCount = state.availAssignments.size
    assurances.forall { assurance =>
      val bitfieldBytes = assurance.bitfield.toArray
      val noPaddingBitsSet =
        (coresCount until (bitfieldBytes.length * 8)).forall(pos => !isBitSet(bitfieldBytes, pos))
      noPaddingBitsSet && state.availAssignments.zipWithIndex.forall {
        case (assignment, coreIndex) =>
          val isSet = isBitSet(bitfieldBytes, coreIndex)
          val isEngaged = assignment.isDefined
          // If bit is set, core must be engaged
          !isSet || isEngaged
      }
    }

  private def validateNonSig(
    assurance: AssuranceExtrinsic,
    input: AssuranceInput,
    state: AssuranceState
  ): Option[AssuranceErrorCode] =
    if assurance.validatorIndex.toInt >= state.currValidators.size then
      Some(AssuranceErrorCode.BadValidatorIndex)
    else if assurance.anchor != input.parent then
      Some(AssuranceErrorCode.BadAttestationParent)
    else
      None

  private def validateAssurances(
    input: AssuranceInput,
    state: AssuranceState
  ): Option[AssuranceErrorCode] =
    if input.assurances.isEmpty then
      None
    else
      val nonSigError = input.assurances.iterator
        .map(a => validateNonSig(a, input, state))
        .collectFirst { case Some(err) => err }

      nonSigError.orElse {
        if !ValidationHelpers.isSortedUniqueByInt(input.assurances)(_.validatorIndex.toInt) then
          Some(AssuranceErrorCode.NotSortedOrUniqueAssurers)
        else
          val assurances = input.assurances.toArray
          val n = assurances.length
          val validators = state.currValidators.toIndexedSeq
          val allValid = java.util.stream.IntStream
            .range(0, n)
            .parallel()
            .allMatch { i =>
              val a = assurances(i)
              verifyAssuranceSignature(a, validators(a.validatorIndex.toInt))
            }
          if !allValid then Some(AssuranceErrorCode.BadSignature) else None
      }

  /**
   * Find cores that have achieved supermajority.
   */
  private def findAvailableCores(
    input: AssuranceInput,
    state: AssuranceState,
    config: ChainConfig
  ): Set[Int] =
    // For each core, count how many validators have assured it
    val coresCount = state.availAssignments.size
    val counts = new Array[Int](coresCount)
    for assurance <- input.assurances do
      val bitfieldBytes = assurance.bitfield.toArray
      var coreIndex = 0
      while coreIndex < coresCount do
        if isBitSet(bitfieldBytes, coreIndex) then
          counts(coreIndex) = counts(coreIndex) + 1
        coreIndex += 1

    val result = scala.collection.mutable.Set[Int]()
    var coreIndex = 0
    while coreIndex < coresCount do
      if 3 * counts(coreIndex) > 2 * config.validatorCount then result += coreIndex
      coreIndex += 1
    result.toSet

  /**
   * Get work reports from available cores in sorted order.
   */
  private def processAvailableReports(availableCores: Set[Int], state: AssuranceState): List[WorkReport] =
    availableCores.toList.sorted.flatMap(coreIndex => state.availAssignments.lift(coreIndex).flatten.map(_.report))

  /**
   * Update state by removing available reports.
   */
  private def updateStateWithAvailableReports(
    state: AssuranceState,
    availableReports: List[WorkReport]
  ): AssuranceState =
    val availableReportSet = availableReports.toSet
    val newAssignments = state.availAssignments.map {
      case Some(assignment) if availableReportSet.contains(assignment.report) => None
      case other => other
    }
    state.copy(availAssignments = newAssignments)

  /**
   * Execute the Assurances STF using unified JamState.
   *
   * Reads: cores.reports (availAssignments), validators.current (currValidators)
   * Writes: cores.reports (availAssignments)
   *
   * @param input The assurance input containing assurances, slot, and parent hash.
   * @param state The unified JamState.
   * @param config The chain configuration.
   * @return Tuple of (updated JamState, output).
   */
  def stfViewWithValidators(
    input: AssuranceInput,
    view: TrieBackedJamState,
    validators: List[ValidatorKey]
  ): AssuranceOutput =
    val preStateBase = AssuranceBridge.extract(view)
    val preState = preStateBase.copy(currValidators = validators)
    val (postState, output) = stfInternal(input, preState, view.config)
    AssuranceBridge.apply(view, postState)
    output

  /**
   * Internal Assurances STF implementation using AssuranceState.
   *
   * @param input The assurance input containing assurances, slot, and parent hash.
   * @param preState The pre-transition state.
   * @param config The chain configuration.
   * @return Tuple of (post-transition state, output).
   */
  def stfInternal(
    input: AssuranceInput,
    preState: AssuranceState,
    config: ChainConfig
  ): (AssuranceState, AssuranceOutput) =
    // Handle timeouts first
    val postTimeoutState = handleTimeouts(preState, input.slot, config)

    // Validate core engagement
    if !validateCoreEngagement(input.assurances, preState) then
      (preState, StfResult.error(AssuranceErrorCode.CoreNotEngaged))
    else
      // Validate assurances
      validateAssurances(input, preState) match
        case Some(error) =>
          (preState, StfResult.error(error))
        case None =>
          // Find available cores and reports
          val availableCores = findAvailableCores(input, preState, config)
          val availableReports = processAvailableReports(availableCores, preState)

          if availableReports.isEmpty then
            (postTimeoutState, StfResult.success(AssuranceOutputMarks(List.empty)))
          else
            val finalState = updateStateWithAvailableReports(postTimeoutState, availableReports)
            (finalState, StfResult.success(AssuranceOutputMarks(availableReports)))
