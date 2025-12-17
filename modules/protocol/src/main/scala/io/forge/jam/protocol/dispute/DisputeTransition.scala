package io.forge.jam.protocol.dispute

import cats.syntax.all.*
import io.forge.jam.core.{ChainConfig, Hashing, constants, StfResult, ValidationHelpers}
import io.forge.jam.core.JamBytes.compareUnsigned
import io.forge.jam.core.primitives.{Hash, Ed25519PublicKey, Ed25519Signature, Timeslot}
import io.forge.jam.core.types.extrinsic.{Dispute, Verdict}
import io.forge.jam.core.types.dispute.{Culprit, Fault}
import io.forge.jam.core.types.work.Vote
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.protocol.dispute.DisputeTypes.*
import io.forge.jam.core.types.workpackage.{WorkReport, AvailabilityAssignment}
import io.forge.jam.crypto.Ed25519
import io.forge.jam.protocol.state.JamState
import monocle.syntax.all.*
import _root_.scodec.Codec

/**
 * Disputes State Transition Function.
 *
 * Processes dispute verdicts, culprits, and faults:
 * - Verdicts: Validators vote on work report validity (good/bad/wonky)
 * - Culprits: False guarantors who guaranteed bad reports
 * - Faults: Validators who voted incorrectly
 *
 * Key operations:
 * - Verify Ed25519 signatures with appropriate prefixes
 * - Track judgment results in psi (good/bad/wonky/offenders)
 * - Clear invalid reports from rho availability assignments
 * - Maintain sorted/unique ordering requirements
 */
object DisputeTransition:
  /**
   * Validate judgment age is either current epoch or previous epoch.
   */
  private def validateJudgementAge(
    verdict: Verdict,
    currentEpoch: Long,
    epochLength: Int
  ): Boolean =
    val age = verdict.age.toInt.toLong
    val validAges = Set(currentEpoch, currentEpoch - 1)
    validAges.contains(age)

  /**
   * Validate vote distribution matches allowed thresholds.
   * Only allow: 0 (all negative), 1/3 (uncertain), or 2/3+1 (supermajority)
   */
  private def validateVoteDistribution(positiveVotes: Int, config: ChainConfig): Boolean =
    val validThresholds = Set(0, config.oneThird, config.votesPerVerdict)
    validThresholds.contains(positiveVotes)

  /**
   * Get the appropriate validator set based on verdict age.
   */
  private def getValidatorSet(verdict: Verdict, state: DisputeState, epochLength: Int): List[ValidatorKey] =
    val currentEpoch = state.tau / epochLength
    val age = verdict.age.toInt.toLong
    if age == currentEpoch then state.kappa else state.lambda

  /**
   * Compute which targets will be judged bad from the verdicts.
   */
  private def computeBadTargets(verdicts: List[Verdict]): Set[Hash] =
    verdicts.flatMap { verdict =>
      val positiveVotes = verdict.votes.count(_.vote)
      if positiveVotes == 0 then Some(verdict.target) else None
    }.toSet

  /**
   * Compute which targets will be judged good from the verdicts.
   */
  private def computeGoodTargets(verdicts: List[Verdict], config: ChainConfig): Set[Hash] =
    verdicts.flatMap { verdict =>
      val positiveVotes = verdict.votes.count(_.vote)
      if positiveVotes >= config.votesPerVerdict then Some(verdict.target) else None
    }.toSet

  /**
   * Validate all disputes.
   * Validation order is critical:
   * 1. Validate verdicts first (ordering and content)
   * 2. Then validate culprits (can now check targets against validated verdicts)
   * 3. Finally validate faults
   */
  private def validateDisputes(
    disputes: Dispute,
    state: DisputeState,
    config: ChainConfig
  ): Option[DisputeErrorCode] =
    val currentEpoch = state.tau / config.epochLength

    // Collect all ed25519 keys from kappa and lambda validator sets
    val validGuarantorKeys = (state.kappa ++ state.lambda).map(_.ed25519).toSet

    // 1. Validate verdicts ordering first
    if !ValidationHelpers.isSortedUniqueByBytes(disputes.verdicts)(_.target.bytes) then
      return Some(DisputeErrorCode.VerdictsNotSortedUnique)

    // 2. Validate each verdict (must happen before culprit target validation)
    for (verdict, verdictIdx) <- disputes.verdicts.zipWithIndex do
      // Validate judgment age
      if !validateJudgementAge(verdict, currentEpoch, config.epochLength) then
        return Some(DisputeErrorCode.BadJudgementAge)

      val validatorSet = getValidatorSet(verdict, state, config.epochLength)
      val positiveVotes = verdict.votes.count(_.vote)
      val negativeVotes = verdict.votes.count(!_.vote)

      // Validate vote distribution
      if !validateVoteDistribution(positiveVotes, config) then
        return Some(DisputeErrorCode.BadVoteSplit)

      // Validate votes are sorted and unique by index
      if !ValidationHelpers.isSortedUniqueByInt(verdict.votes)(_.validatorIndex.toInt) then
        return Some(DisputeErrorCode.JudgementsNotSortedUnique)

      // Verify all vote signatures
      for vote <- verdict.votes do
        if vote.validatorIndex.toInt >= validatorSet.size then
          return Some(DisputeErrorCode.BadSignature)

        val validator = validatorSet(vote.validatorIndex.toInt)
        val prefixBytes = if vote.vote then constants.JAM_VALID_BYTES else constants.JAM_INVALID_BYTES
        val message = prefixBytes ++ verdict.target.bytes

        if !Ed25519.verify(validator.ed25519, message, vote.signature) then
          return Some(DisputeErrorCode.BadSignature)

      // Validate target has not already been judged
      if state.psi.good.exists(h => java.util.Arrays.equals(h.bytes, verdict.target.bytes)) ||
        state.psi.bad.exists(h => java.util.Arrays.equals(h.bytes, verdict.target.bytes)) ||
        state.psi.wonky.exists(h => java.util.Arrays.equals(h.bytes, verdict.target.bytes))
      then
        return Some(DisputeErrorCode.AlreadyJudged)

      // For all-negative verdicts, require at least 2 culprits
      if negativeVotes == verdict.votes.size && positiveVotes == 0 then
        val culpritsForTarget = disputes.culprits.count(c =>
          java.util.Arrays.equals(c.target.bytes, verdict.target.bytes)
        )
        if culpritsForTarget < 2 then
          return Some(DisputeErrorCode.NotEnoughCulprits)

      // For supermajority positive verdicts, require at least one fault
      if positiveVotes >= config.votesPerVerdict then
        val hasFault = disputes.faults.exists(f =>
          java.util.Arrays.equals(f.target.bytes, verdict.target.bytes)
        )
        if !hasFault then
          return Some(DisputeErrorCode.NotEnoughFaults)

        // Validate fault votes are opposite of verdict outcome
        val matchingFaults = disputes.faults.filter(f =>
          java.util.Arrays.equals(f.target.bytes, verdict.target.bytes)
        )
        for fault <- matchingFaults do
          // For a good verdict (supermajority positive), faults must have voted false
          if fault.vote then
            return Some(DisputeErrorCode.FaultVerdictWrong)

    // Now compute bad targets from validated verdicts
    val newBadTargets = computeBadTargets(disputes.verdicts)

    // 3. Validate culprits ordering
    if !ValidationHelpers.isSortedUniqueByBytes(disputes.culprits)(_.key.bytes) then
      return Some(DisputeErrorCode.CulpritsNotSortedUnique)

    // 4. Validate each culprit
    for culprit <- disputes.culprits do
      // Validate culprit key is from a known guarantor (validator)
      if !validGuarantorKeys.exists(k => java.util.Arrays.equals(k.bytes, culprit.key.bytes)) then
        return Some(DisputeErrorCode.BadGuarantorKey)

      val message = constants.JAM_GUARANTEE_BYTES ++ culprit.target.bytes

      // Verify culprit signature
      if !Ed25519.verify(culprit.key, message, culprit.signature) then
        return Some(DisputeErrorCode.BadSignature)

      // Check if already an offender
      if state.psi.offenders.exists(k => java.util.Arrays.equals(k.bytes, culprit.key.bytes)) then
        return Some(DisputeErrorCode.OffenderAlreadyReported)

      // Validate that culprit target will be judged bad (or was already bad in state)
      val targetIsBad = newBadTargets.exists(h => java.util.Arrays.equals(h.bytes, culprit.target.bytes)) ||
        state.psi.bad.exists(h => java.util.Arrays.equals(h.bytes, culprit.target.bytes))
      if !targetIsBad then
        return Some(DisputeErrorCode.CulpritsVerdictNotBad)

    // 5. Validate faults ordering
    if !ValidationHelpers.isSortedUniqueByBytes(disputes.faults)(_.key.bytes) then
      return Some(DisputeErrorCode.FaultsNotSortedUnique)

    // 6. Validate each fault
    for fault <- disputes.faults do
      // Validate fault key is from a known validator
      if !validGuarantorKeys.exists(k => java.util.Arrays.equals(k.bytes, fault.key.bytes)) then
        return Some(DisputeErrorCode.BadAuditorKey)

      // Check if already an offender
      if state.psi.offenders.exists(k => java.util.Arrays.equals(k.bytes, fault.key.bytes)) then
        return Some(DisputeErrorCode.OffenderAlreadyReported)

      val prefixBytes = if fault.vote then constants.JAM_VALID_BYTES else constants.JAM_INVALID_BYTES
      val message = prefixBytes ++ fault.target.bytes

      if !Ed25519.verify(fault.key, message, fault.signature) then
        return Some(DisputeErrorCode.BadSignature)

    None

  /**
   * Process disputes and update state.
   * Returns tuple of (new state, offenders mark for output)
   * Note: The offenders mark in the output retains processing order (culprits then faults)
   *       while the state.psi.offenders list gets sorted offenders added to it.
   */
  private def processDisputes(
    disputes: Dispute,
    preState: DisputeState,
    config: ChainConfig
  ): (DisputeState, List[Ed25519PublicKey]) =
    // Intermediate state for tracking processed results
    case class ProcessState(
      good: List[Hash],
      bad: List[Hash],
      wonky: List[Hash],
      offendersMark: List[Ed25519PublicKey],
      newOffendersSet: Set[Ed25519PublicKey]
    )

    // Process verdicts using foldLeft
    val afterVerdicts = disputes.verdicts.foldLeft(
      ProcessState(preState.psi.good, preState.psi.bad, preState.psi.wonky, List.empty, Set.empty)
    ) { (state, verdict) =>
      val positiveVotes = verdict.votes.count(_.vote)
      if positiveVotes >= config.votesPerVerdict then
        state.copy(good = state.good :+ verdict.target)
      else if positiveVotes == 0 then
        state.copy(bad = state.bad :+ verdict.target)
      else if positiveVotes == config.oneThird then
        state.copy(wonky = state.wonky :+ verdict.target)
      else
        state
    }

    // Process culprits using foldLeft
    val afterCulprits = disputes.culprits.foldLeft(afterVerdicts) { (state, culprit) =>
      val keyInOffenders = preState.psi.offenders.exists(k => java.util.Arrays.equals(k.bytes, culprit.key.bytes))
      val keyAlreadyAdded = state.newOffendersSet.exists(k => java.util.Arrays.equals(k.bytes, culprit.key.bytes))

      if !keyInOffenders && !keyAlreadyAdded then
        state.copy(
          offendersMark = state.offendersMark :+ culprit.key,
          newOffendersSet = state.newOffendersSet + culprit.key
        )
      else
        state
    }

    // Process faults using foldLeft
    val afterFaults = disputes.faults.foldLeft(afterCulprits) { (state, fault) =>
      val isBad = state.bad.exists(h => java.util.Arrays.equals(h.bytes, fault.target.bytes))
      val isGood = state.good.exists(h => java.util.Arrays.equals(h.bytes, fault.target.bytes))

      // A fault exists if:
      // - Report is in good but validator voted false (they were wrong)
      // - Report is in bad but validator voted true (they were wrong)
      val voteConflicts = (isGood && !fault.vote) || (isBad && fault.vote)
      val keyInOffenders = preState.psi.offenders.exists(k => java.util.Arrays.equals(k.bytes, fault.key.bytes))
      val keyAlreadyAdded = state.newOffendersSet.exists(k => java.util.Arrays.equals(k.bytes, fault.key.bytes))

      if voteConflicts && !keyInOffenders && !keyAlreadyAdded then
        state.copy(
          offendersMark = state.offendersMark :+ fault.key,
          newOffendersSet = state.newOffendersSet + fault.key
        )
      else
        state
    }

    // Sort new offenders by key for adding to state
    val sortedNewOffenders = afterFaults.newOffendersSet.toList.sortWith((a, b) => compareUnsigned(a.bytes, b.bytes) < 0)
    val finalOffenders = preState.psi.offenders ++ sortedNewOffenders

    // Clear invalid reports from rho
    val newRho = preState.rho.map { assignmentOpt =>
      assignmentOpt.flatMap { assignment =>
        val reportHash = Hashing.blake2b256(
          summon[Codec[WorkReport]].encode(assignment.report).require.toByteArray
        )

        // Clear if the report is in bad or wonky
        val isInvalid = afterFaults.bad.exists(h => java.util.Arrays.equals(h.bytes, reportHash.bytes)) ||
          afterFaults.wonky.exists(h => java.util.Arrays.equals(h.bytes, reportHash.bytes))

        if isInvalid then None else Some(assignment)
      }
    }

    val newPsi = Psi(afterFaults.good, afterFaults.bad, afterFaults.wonky, finalOffenders)
    val newState = preState.copy(psi = newPsi, rho = newRho)

    // Return the unsorted offendersMark for the output
    (newState, afterFaults.offendersMark)

  /**
   * Execute the Disputes STF using unified JamState.
   *
   * Reads: judgements (psi), cores.reports (rho), tau, validators.current (kappa), validators.previous (lambda)
   * Writes: judgements (psi), cores.reports (rho)
   *
   * @param input The dispute input containing verdicts, culprits, and faults.
   * @param state The unified JamState.
   * @param config The chain configuration.
   * @return Tuple of (updated JamState, output).
   */
  def stf(
    input: DisputeInput,
    state: JamState,
    config: ChainConfig
  ): (JamState, DisputeOutput) =
    // Extract DisputeState using lens bundle
    val preState = JamState.DisputeLenses.extract(state)

    // Execute the internal STF logic
    val (postState, output) = stfInternal(input, preState, config)

    // Apply results back using lens bundle
    val updatedState = JamState.DisputeLenses.apply(state, postState)

    (updatedState, output)

  /**
   * Internal Disputes STF implementation using DisputeState.
   * Exposed for unit testing with module-specific state types.
   *
   * @param input The dispute input containing verdicts, culprits, and faults.
   * @param preState The pre-transition state.
   * @param config The chain configuration.
   * @return Tuple of (post-transition state, output).
   */
  def stfInternal(
    input: DisputeInput,
    preState: DisputeState,
    config: ChainConfig
  ): (DisputeState, DisputeOutput) =
    // Validate disputes
    validateDisputes(input.disputes, preState, config) match
      case Some(error) =>
        (preState, StfResult.error(error))
      case None =>
        // Process disputes and get new state + offenders mark
        val (postState, offendersMark) = processDisputes(input.disputes, preState, config)
        (postState, StfResult.success(DisputeOutputMarks(offendersMark)))
