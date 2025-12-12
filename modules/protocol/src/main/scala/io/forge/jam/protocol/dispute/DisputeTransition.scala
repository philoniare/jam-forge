package io.forge.jam.protocol.dispute

import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.JamBytes.compareUnsigned
import io.forge.jam.core.primitives.{Hash, Ed25519PublicKey, Ed25519Signature, Timeslot}
import io.forge.jam.core.types.extrinsic.{Dispute, Verdict}
import io.forge.jam.core.types.dispute.{Culprit, Fault}
import io.forge.jam.core.types.work.Vote
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.protocol.assurance.AssuranceTypes.AvailabilityAssignment
import io.forge.jam.protocol.dispute.DisputeTypes.*
import io.forge.jam.core.types.workpackage.WorkReport
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer

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

  private val JAM_VALID = "jam_valid"
  private val JAM_INVALID = "jam_invalid"
  private val JAM_GUARANTEE = "jam_guarantee"

  /**
   * Verify Ed25519 signature on a message.
   */
  private def verifyEd25519Signature(
    publicKey: Ed25519PublicKey,
    message: Array[Byte],
    signature: Ed25519Signature
  ): Boolean =
    try
      val pubKeyParams = new Ed25519PublicKeyParameters(publicKey.bytes, 0)
      val signer = new Ed25519Signer()
      signer.init(false, pubKeyParams)
      signer.update(message, 0, message.length)
      signer.verifySignature(signature.bytes)
    catch
      case _: Exception => false

  /**
   * Validate that culprits are sorted and unique by key.
   */
  private def validateCulpritsSortedUnique(culprits: List[Culprit]): Boolean =
    culprits.sliding(2).forall {
      case List(curr, next) => compareUnsigned(curr.key.bytes, next.key.bytes) < 0
      case _ => true
    }

  /**
   * Validate that verdicts are sorted and unique by target hash.
   */
  private def validateVerdictsSortedUnique(verdicts: List[Verdict]): Boolean =
    verdicts.sliding(2).forall {
      case List(curr, next) => compareUnsigned(curr.target.bytes, next.target.bytes) < 0
      case _ => true
    }

  /**
   * Validate that faults are sorted and unique by key.
   */
  private def validateFaultsSortedUnique(faults: List[Fault]): Boolean =
    faults.sliding(2).forall {
      case List(curr, next) => compareUnsigned(curr.key.bytes, next.key.bytes) < 0
      case _ => true
    }

  /**
   * Validate that votes within a verdict are sorted and unique by index.
   */
  private def validateVotesSortedUnique(votes: List[Vote]): Boolean =
    votes.sliding(2).forall {
      case List(curr, next) => curr.validatorIndex.toInt < next.validatorIndex.toInt
      case _ => true
    }

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
    if !validateVerdictsSortedUnique(disputes.verdicts) then
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
      if !validateVotesSortedUnique(verdict.votes) then
        return Some(DisputeErrorCode.JudgementsNotSortedUnique)

      // Verify all vote signatures
      for vote <- verdict.votes do
        if vote.validatorIndex.toInt >= validatorSet.size then
          return Some(DisputeErrorCode.BadSignature)

        val validator = validatorSet(vote.validatorIndex.toInt)
        val prefix = if vote.vote then JAM_VALID else JAM_INVALID
        val message = prefix.getBytes("UTF-8") ++ verdict.target.bytes

        if !verifyEd25519Signature(validator.ed25519, message, vote.signature) then
          return Some(DisputeErrorCode.BadSignature)

      // Validate target has not already been judged
      if state.psi.good.exists(h => java.util.Arrays.equals(h.bytes, verdict.target.bytes)) ||
         state.psi.bad.exists(h => java.util.Arrays.equals(h.bytes, verdict.target.bytes)) ||
         state.psi.wonky.exists(h => java.util.Arrays.equals(h.bytes, verdict.target.bytes)) then
        return Some(DisputeErrorCode.AlreadyJudged)

      // For all-negative verdicts, require at least 2 culprits
      if negativeVotes == verdict.votes.size && positiveVotes == 0 then
        val culpritsForTarget = disputes.culprits.count(c =>
          java.util.Arrays.equals(c.target.bytes, verdict.target.bytes))
        if culpritsForTarget < 2 then
          return Some(DisputeErrorCode.NotEnoughCulprits)

      // For supermajority positive verdicts, require at least one fault
      if positiveVotes >= config.votesPerVerdict then
        val hasFault = disputes.faults.exists(f =>
          java.util.Arrays.equals(f.target.bytes, verdict.target.bytes))
        if !hasFault then
          return Some(DisputeErrorCode.NotEnoughFaults)

        // Validate fault votes are opposite of verdict outcome
        val matchingFaults = disputes.faults.filter(f =>
          java.util.Arrays.equals(f.target.bytes, verdict.target.bytes))
        for fault <- matchingFaults do
          // For a good verdict (supermajority positive), faults must have voted false
          if fault.vote then
            return Some(DisputeErrorCode.FaultVerdictWrong)

    // Now compute bad targets from validated verdicts
    val newBadTargets = computeBadTargets(disputes.verdicts)

    // 3. Validate culprits ordering
    if !validateCulpritsSortedUnique(disputes.culprits) then
      return Some(DisputeErrorCode.CulpritsNotSortedUnique)

    // 4. Validate each culprit
    for culprit <- disputes.culprits do
      // Validate culprit key is from a known guarantor (validator)
      if !validGuarantorKeys.exists(k => java.util.Arrays.equals(k.bytes, culprit.key.bytes)) then
        return Some(DisputeErrorCode.BadGuarantorKey)

      val message = JAM_GUARANTEE.getBytes("UTF-8") ++ culprit.target.bytes

      // Verify culprit signature
      if !verifyEd25519Signature(culprit.key, message, culprit.signature) then
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
    if !validateFaultsSortedUnique(disputes.faults) then
      return Some(DisputeErrorCode.FaultsNotSortedUnique)

    // 6. Validate each fault
    for fault <- disputes.faults do
      // Validate fault key is from a known validator
      if !validGuarantorKeys.exists(k => java.util.Arrays.equals(k.bytes, fault.key.bytes)) then
        return Some(DisputeErrorCode.BadAuditorKey)

      // Check if already an offender
      if state.psi.offenders.exists(k => java.util.Arrays.equals(k.bytes, fault.key.bytes)) then
        return Some(DisputeErrorCode.OffenderAlreadyReported)

      val prefix = if fault.vote then JAM_VALID else JAM_INVALID
      val message = prefix.getBytes("UTF-8") ++ fault.target.bytes

      if !verifyEd25519Signature(fault.key, message, fault.signature) then
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
    // offendersMark tracks the output in processing order
    val offendersMark = scala.collection.mutable.ListBuffer[Ed25519PublicKey]()
    // newOffendersSet tracks unique offenders for state update
    val newOffendersSet = scala.collection.mutable.Set[Ed25519PublicKey]()

    // Initialize psi with copies of existing lists
    var good = preState.psi.good
    var bad = preState.psi.bad
    var wonky = preState.psi.wonky
    var offenders = preState.psi.offenders

    // Process verdicts
    for verdict <- disputes.verdicts do
      val positiveVotes = verdict.votes.count(_.vote)

      // Update PSI sets based on vote count
      if positiveVotes >= config.votesPerVerdict then
        good = good :+ verdict.target
      else if positiveVotes == 0 then
        bad = bad :+ verdict.target
      else if positiveVotes == config.oneThird then
        wonky = wonky :+ verdict.target

    // Process culprits - add to both mark and state if not already an offender
    for culprit <- disputes.culprits do
      val keyInOffenders = offenders.exists(k => java.util.Arrays.equals(k.bytes, culprit.key.bytes))

      if !keyInOffenders then
        val keyAlreadyAdded = newOffendersSet.exists(k => java.util.Arrays.equals(k.bytes, culprit.key.bytes))
        if !keyAlreadyAdded then
          newOffendersSet += culprit.key
          offendersMark += culprit.key

    // Process faults
    for fault <- disputes.faults do
      val isBad = bad.exists(h => java.util.Arrays.equals(h.bytes, fault.target.bytes))
      val isGood = good.exists(h => java.util.Arrays.equals(h.bytes, fault.target.bytes))

      // A fault exists if:
      // - Report is in good but validator voted false (they were wrong)
      // - Report is in bad but validator voted true (they were wrong)
      val voteConflicts = (isGood && !fault.vote) || (isBad && fault.vote)
      val keyInOffenders = offenders.exists(k => java.util.Arrays.equals(k.bytes, fault.key.bytes))

      if voteConflicts && !keyInOffenders then
        val keyAlreadyAdded = newOffendersSet.exists(k => java.util.Arrays.equals(k.bytes, fault.key.bytes))
        if !keyAlreadyAdded then
          newOffendersSet += fault.key
          offendersMark += fault.key

    // Sort new offenders by key for adding to state
    val sortedNewOffenders = newOffendersSet.toList.sortWith { (a, b) =>
      compareUnsigned(a.bytes, b.bytes) < 0
    }

    // Add sorted offenders to state
    offenders = offenders ++ sortedNewOffenders

    // Clear invalid reports from rho
    val newRho = preState.rho.map { assignmentOpt =>
      assignmentOpt.flatMap { assignment =>
        val reportHash = Hashing.blake2b256(
          WorkReport.given_JamEncoder_WorkReport.encode(assignment.report).toArray
        )

        // Clear if the report is in bad or wonky
        val isInvalid = bad.exists(h => java.util.Arrays.equals(h.bytes, reportHash.bytes)) ||
                        wonky.exists(h => java.util.Arrays.equals(h.bytes, reportHash.bytes))

        if isInvalid then None else Some(assignment)
      }
    }

    val newPsi = Psi(good, bad, wonky, offenders)
    val newState = preState.copy(psi = newPsi, rho = newRho)

    // Return the unsorted offendersMark for the output
    (newState, offendersMark.toList)

  /**
   * Execute the Disputes STF.
   *
   * @param input The dispute input containing verdicts, culprits, and faults.
   * @param preState The pre-transition state.
   * @param config The chain configuration.
   * @return Tuple of (post-transition state, output).
   */
  def stf(
    input: DisputeInput,
    preState: DisputeState,
    config: ChainConfig
  ): (DisputeState, DisputeOutput) =
    // Validate disputes
    validateDisputes(input.disputes, preState, config) match
      case Some(error) =>
        (preState, DisputeOutput(err = Some(error)))
      case None =>
        // Process disputes and get new state + offenders mark
        val (postState, offendersMark) = processDisputes(input.disputes, preState, config)
        (postState, DisputeOutput(ok = Some(DisputeOutputMarks(offendersMark))))
