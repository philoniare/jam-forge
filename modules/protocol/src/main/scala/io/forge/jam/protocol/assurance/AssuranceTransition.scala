package io.forge.jam.protocol.assurance

import io.forge.jam.core.{ChainConfig, Hashing, constants, StfResult, ValidationHelpers}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.protocol.assurance.AssuranceTypes.*
import io.forge.jam.crypto.Ed25519
import io.forge.jam.protocol.state.JamState

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
    assurances.forall { assurance =>
      val bitfieldBytes = assurance.bitfield.toArray
      state.availAssignments.zipWithIndex.forall {
        case (assignment, coreIndex) =>
          val isSet = isBitSet(bitfieldBytes, coreIndex)
          val isEngaged = assignment.isDefined
          // If bit is set, core must be engaged
          !isSet || isEngaged
      }
    }

  /**
   * Validate a single assurance and return error if invalid.
   */
  private def validateSingleAssurance(
    assurance: AssuranceExtrinsic,
    input: AssuranceInput,
    state: AssuranceState
  ): Option[AssuranceErrorCode] =
    // Check validator index bounds
    if assurance.validatorIndex.toInt >= state.currValidators.size then
      Some(AssuranceErrorCode.BadValidatorIndex)
    // Check parent block hash matches
    else if assurance.anchor != input.parent then
      Some(AssuranceErrorCode.BadAttestationParent)
    // Verify signature
    else if !verifyAssuranceSignature(assurance, state.currValidators(assurance.validatorIndex.toInt)) then
      Some(AssuranceErrorCode.BadSignature)
    else
      None

  /**
   * Validate all assurances.
   */
  private def validateAssurances(
    input: AssuranceInput,
    state: AssuranceState
  ): Option[AssuranceErrorCode] =
    // Early return if no assurances
    if input.assurances.isEmpty then
      None
    else
      // Find first error in assurances
      val firstError = input.assurances.iterator
        .map(a => validateSingleAssurance(a, input, state))
        .collectFirst { case Some(err) => err }

      firstError.orElse {
        // Check for sorted and unique validator indices
        if !ValidationHelpers.isSortedUniqueByInt(input.assurances)(_.validatorIndex.toInt) then
          Some(AssuranceErrorCode.NotSortedOrUniqueAssurers)
        else
          None
      }

  /**
   * Find cores that have achieved supermajority.
   */
  private def findAvailableCores(
    input: AssuranceInput,
    state: AssuranceState,
    config: ChainConfig
  ): Set[Int] =
    val requiredAssurances = config.superMajority

    // For each core, count how many validators have assured it
    val counts = scala.collection.mutable.Map[Int, Int]()
    for assurance <- input.assurances do
      val bitfieldBytes = assurance.bitfield.toArray
      for coreIndex <- state.availAssignments.indices do
        if isBitSet(bitfieldBytes, coreIndex) then
          counts(coreIndex) = counts.getOrElse(coreIndex, 0) + 1

    counts.filter { case (_, count) => count > requiredAssurances }.keySet.toSet

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
  def stf(
    input: AssuranceInput,
    state: JamState,
    config: ChainConfig
  ): (JamState, AssuranceOutput) =
    // Extract AssuranceState using lens bundle
    val preState = JamState.AssuranceLenses.extract(state)

    // Execute the internal STF logic
    val (postState, output) = stfInternal(input, preState, config)

    // Apply results back using lens bundle
    val updatedState = JamState.AssuranceLenses.apply(state, postState)

    (updatedState, output)

  /**
   * Execute the Assurances STF with explicit validators for signature verification.
   *
   * @param input The assurance input containing assurances, slot, and parent hash.
   * @param state The unified JamState.
   * @param config The chain configuration.
   * @param validators The validators to use for signature verification.
   * @return Tuple of (updated JamState, output).
   */
  def stfWithValidators(
    input: AssuranceInput,
    state: JamState,
    config: ChainConfig,
    validators: List[ValidatorKey]
  ): (JamState, AssuranceOutput) =
    // Extract AssuranceState using lens bundle, but override currValidators
    val preStateBase = JamState.AssuranceLenses.extract(state)
    val preState = preStateBase.copy(currValidators = validators)

    // Execute the internal STF logic
    val (postState, output) = stfInternal(input, preState, config)

    // Apply results back using lens bundle
    val updatedState = JamState.AssuranceLenses.apply(state, postState)

    (updatedState, output)

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
      (postTimeoutState, StfResult.error(AssuranceErrorCode.CoreNotEngaged))
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
