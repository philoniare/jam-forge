package io.forge.jam.protocol.dispute

import io.forge.jam.core.{
  ChainConfig,
  Hashing,
  constants,
  StfResult,
  ValidationHelpers
}
import io.forge.jam.core.JamBytes.compareUnsigned
import io.forge.jam.core.primitives.{Hash, Ed25519PublicKey}
import io.forge.jam.core.types.extrinsic.{Dispute, Verdict}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.protocol.dispute.DisputeTypes.*
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.DisputeBridge
import io.forge.jam.core.types.workpackage.{AvailabilityAssignment, WorkReport}
import io.forge.jam.crypto.Ed25519
import _root_.scodec.Codec
import scala.util.boundary, boundary.break

/** Disputes State Transition Function.
  *
  * Processes dispute verdicts, culprits, and faults:
  *   - Verdicts: Validators vote on work report validity (good/bad/wonky)
  *   - Culprits: False guarantors who guaranteed bad reports
  *   - Faults: Validators who voted incorrectly
  *
  * Key operations:
  *   - Verify Ed25519 signatures with appropriate prefixes
  *   - Track judgment results in psi (good/bad/wonky/offenders)
  *   - Clear invalid reports from rho availability assignments
  *   - Maintain sorted/unique ordering requirements
  */
object DisputeTransition:
  /** Validate judgment age is either current epoch or previous epoch.
    */
  private def validateJudgementAge(
      verdict: Verdict,
      currentEpoch: Long,
      epochLength: Int
  ): Boolean =
    val age = verdict.age.value.toLong
    val validAges = Set(currentEpoch, currentEpoch - 1)
    validAges.contains(age)

  /** `floor(2/3 * |k|) + 1` for the validator set `k` identified by the verdict.
    */
  private def superMajorityOf(validatorSetSize: Int): Int =
    (2 * validatorSetSize) / 3 + 1

  /** `floor(1/3 * |k|)` for the validator set `k` identified by the verdict. */
  private def oneThirdOf(validatorSetSize: Int): Int =
    validatorSetSize / 3

  /** Validate vote distribution matches allowed thresholds. Only allow: 0 (all
    * negative), 1/3 (uncertain), or 2/3+1 (supermajority)
    */
  private def validateVoteDistribution(
      positiveVotes: Int,
      validatorSetSize: Int
  ): Boolean =
    val validThresholds =
      Set(0, oneThirdOf(validatorSetSize), superMajorityOf(validatorSetSize))
    validThresholds.contains(positiveVotes)

  /** Get the appropriate validator set based on verdict age.
    */
  private def getValidatorSet(
      verdict: Verdict,
      state: DisputeState,
      epochLength: Int
  ): List[ValidatorKey] =
    val currentEpoch = state.tau / epochLength
    val age = verdict.age.value.toLong
    if age == currentEpoch then state.kappa else state.lambda

  /** Compute which targets will be judged bad from the verdicts.
    */
  private def computeBadTargets(verdicts: List[Verdict]): Set[Hash] =
    verdicts.flatMap { verdict =>
      val positiveVotes = verdict.votes.count(_.vote)
      if positiveVotes == 0 then Some(verdict.target) else None
    }.toSet

  /** Compute which targets will be judged good from the verdicts.
    */
  private def computeGoodTargets(
      verdicts: List[Verdict],
      state: DisputeState,
      config: ChainConfig
  ): Set[Hash] =
    verdicts.flatMap { verdict =>
      val validatorSet = getValidatorSet(verdict, state, config.epochLength)
      val positiveVotes = verdict.votes.count(_.vote)
      if positiveVotes >= superMajorityOf(validatorSet.size) then Some(verdict.target)
      else None
    }.toSet

  /** Posterior judgement sets derived from the validated verdicts. Produced by
    * [[validateVerdicts]] and consumed by the culprit and fault phases.
    */
  private final case class VerdictOutcome(
      newBadTargets: Set[Hash],
      badsetPrime: Set[Hash],
      goodsetPrime: Set[Hash]
  )

  /** Phases 1-2: verdict ordering, then each verdict's content. Returns the
    * posterior bad/good sets the later phases check against.
    */
  private def validateVerdicts(
      disputes: Dispute,
      state: DisputeState,
      config: ChainConfig,
      currentEpoch: Long,
      psiGoodSet: Set[Hash],
      psiBadSet: Set[Hash],
      psiWonkySet: Set[Hash]
  ): Either[DisputeErrorCode, VerdictOutcome] =
    boundary:
      // 1. Validate verdicts ordering first
      if !ValidationHelpers.isSortedUniqueByBytes(disputes.verdicts)(
          _.target.bytes
        )
      then break(Left(DisputeErrorCode.VerdictsNotSortedUnique))
      val faultsByTarget = disputes.faults.groupBy(_.target)

      // 2. Validate each verdict (must happen before culprit target validation)
      for verdict <- disputes.verdicts do
        // Validate judgment age
        if !validateJudgementAge(verdict, currentEpoch, config.epochLength) then
          break(Left(DisputeErrorCode.BadJudgementAge))

        val validatorSet = getValidatorSet(verdict, state, config.epochLength)
        if verdict.votes.size != superMajorityOf(validatorSet.size) then
          break(Left(DisputeErrorCode.BadVotesCount))

        val positiveVotes = verdict.votes.count(_.vote)

        // Validate vote distribution
        if !validateVoteDistribution(positiveVotes, validatorSet.size) then
          break(Left(DisputeErrorCode.BadVoteSplit))

        // Validate votes are sorted and unique by index
        if !ValidationHelpers.isSortedUniqueByInt(verdict.votes)(
            _.validatorIndex.toInt
          )
        then break(Left(DisputeErrorCode.JudgementsNotSortedUnique))

        // Verify all vote signatures in parallel
        val voteCount = verdict.votes.size
        val sigInputs = new Array[
          (
              Ed25519PublicKey,
              Array[Byte],
              io.forge.jam.core.primitives.Ed25519Signature
          )
        ](voteCount)
        var voteIdx = 0
        for vote <- verdict.votes do
          if vote.validatorIndex.toInt >= validatorSet.size then
            break(Left(DisputeErrorCode.BadSignature))
          val validator = validatorSet(vote.validatorIndex.toInt)
          val prefixBytes =
            if vote.vote then constants.JAM_VALID_BYTES
            else constants.JAM_INVALID_BYTES
          val message = prefixBytes ++ verdict.target.bytes
          sigInputs(voteIdx) = (validator.ed25519, message, vote.signature)
          voteIdx += 1

        val allValid = java.util.stream.IntStream
          .range(0, voteCount)
          .parallel()
          .allMatch { i =>
            val (pk, msg, sig) = sigInputs(i)
            Ed25519.verify(pk, msg, sig)
          }
        if !allValid then break(Left(DisputeErrorCode.BadSignature))

        // Validate target has not already been judged
        if psiGoodSet.contains(verdict.target) ||
          psiBadSet.contains(verdict.target) ||
          psiWonkySet.contains(verdict.target)
        then break(Left(DisputeErrorCode.AlreadyJudged))

        // For supermajority positive verdicts, require at least one fault
        if positiveVotes >= superMajorityOf(validatorSet.size) then
          val matchingFaults = faultsByTarget.getOrElse(verdict.target, Nil)
          if matchingFaults.isEmpty then break(Left(DisputeErrorCode.NotEnoughFaults))

          // Validate fault votes are opposite of verdict outcome
          for fault <- matchingFaults do
            // For a good verdict (supermajority positive), faults must have voted false
            if fault.vote then break(Left(DisputeErrorCode.FaultVerdictWrong))

      // Now compute bad/good targets from validated verdicts -> posterior sets badset'/goodset'
      val newBadTargets = computeBadTargets(disputes.verdicts)
      val newGoodTargets = computeGoodTargets(disputes.verdicts, state, config)
      val badsetPrime: Set[Hash] = state.psi.bad.toSet ++ newBadTargets
      val goodsetPrime: Set[Hash] = state.psi.good.toSet ++ newGoodTargets
      Right(VerdictOutcome(newBadTargets, badsetPrime, goodsetPrime))

  /** Phases 3-4: culprit ordering, then each culprit's content. */
  private def validateCulprits(
      disputes: Dispute,
      validGuarantorKeys: Set[Ed25519PublicKey],
      psiBadSet: Set[Hash],
      psiOffendersSet: Set[Ed25519PublicKey],
      newBadTargets: Set[Hash]
  ): Either[DisputeErrorCode, Unit] =
    boundary:
      // 3. Validate culprits ordering
      if !ValidationHelpers.isSortedUniqueByBytes(disputes.culprits)(_.key.bytes)
      then break(Left(DisputeErrorCode.CulpritsNotSortedUnique))

      // 4. Validate each culprit
      for culprit <- disputes.culprits do
        // Validate culprit key is from a known guarantor (validator)
        if !validGuarantorKeys.contains(culprit.key)
        then break(Left(DisputeErrorCode.BadGuarantorKey))

        val message = constants.JAM_GUARANTEE_BYTES ++ culprit.target.bytes

        // Verify culprit signature
        if !Ed25519.verify(culprit.key, message, culprit.signature) then
          break(Left(DisputeErrorCode.BadSignature))

        // Check if already an offender
        if psiOffendersSet.contains(culprit.key)
        then break(Left(DisputeErrorCode.OffenderAlreadyReported))

        // Validate that culprit target will be judged bad (or was already bad in state)
        val targetIsBad =
          newBadTargets.contains(culprit.target) || psiBadSet.contains(culprit.target)
        if !targetIsBad then break(Left(DisputeErrorCode.CulpritsVerdictNotBad))

      Right(())

  /** Phases 5-6: fault ordering, then each fault's content. */
  private def validateFaults(
      disputes: Dispute,
      validGuarantorKeys: Set[Ed25519PublicKey],
      psiOffendersSet: Set[Ed25519PublicKey],
      badsetPrime: Set[Hash],
      goodsetPrime: Set[Hash]
  ): Either[DisputeErrorCode, Unit] =
    boundary:
      // 5. Validate faults ordering
      if !ValidationHelpers.isSortedUniqueByBytes(disputes.faults)(_.key.bytes)
      then break(Left(DisputeErrorCode.FaultsNotSortedUnique))

      // 6. Validate each fault
      for fault <- disputes.faults do
        // Validate fault key is from a known validator
        if !validGuarantorKeys.contains(fault.key)
        then break(Left(DisputeErrorCode.BadAuditorKey))

        // Check if already an offender
        if psiOffendersSet.contains(fault.key)
        then break(Left(DisputeErrorCode.OffenderAlreadyReported))

        val prefixBytes =
          if fault.vote then constants.JAM_VALID_BYTES
          else constants.JAM_INVALID_BYTES
        val message = prefixBytes ++ fault.target.bytes

        if !Ed25519.verify(fault.key, message, fault.signature) then
          break(Left(DisputeErrorCode.BadSignature))

        val faultInBad = badsetPrime.contains(fault.target)
        val faultInGood = goodsetPrime.contains(fault.target)
        if !(faultInBad == !faultInGood && faultInBad == fault.vote) then
          break(Left(DisputeErrorCode.FaultVerdictWrong))

      Right(())

  /** Validate all disputes. Validation order is critical:
    *   1. Validate verdicts first (ordering and content)
    *   2. Then validate culprits (can now check targets against validated
    *      verdicts)
    *   3. Finally validate faults
    */
  private def validateDisputes(
      disputes: Dispute,
      state: DisputeState,
      config: ChainConfig
  ): Option[DisputeErrorCode] =
    val currentEpoch = state.tau / config.epochLength

    // Collect all ed25519 keys from kappa and lambda validator sets
    val validGuarantorKeys = (state.kappa ++ state.lambda).map(_.ed25519).toSet

    val psiGoodSet = state.psi.good.toSet
    val psiBadSet = state.psi.bad.toSet
    val psiWonkySet = state.psi.wonky.toSet
    val psiOffendersSet = state.psi.offenders.toSet

    val validated =
      for
        outcome <- validateVerdicts(
          disputes,
          state,
          config,
          currentEpoch,
          psiGoodSet,
          psiBadSet,
          psiWonkySet
        )
        _ <- validateCulprits(
          disputes,
          validGuarantorKeys,
          psiBadSet,
          psiOffendersSet,
          outcome.newBadTargets
        )
        _ <- validateFaults(
          disputes,
          validGuarantorKeys,
          psiOffendersSet,
          outcome.badsetPrime,
          outcome.goodsetPrime
        )
      yield ()

    validated.left.toOption

  /** Intermediate state for tracking processed results. */
  private final case class ProcessState(
      good: List[Hash],
      bad: List[Hash],
      wonky: List[Hash],
      offendersMarkRev: List[Ed25519PublicKey],
      newOffendersSet: Set[Ed25519PublicKey]
  )

  /** Process verdicts using foldLeft: route each target into good/bad/wonky. */
  private def processVerdicts(
      disputes: Dispute,
      preState: DisputeState,
      config: ChainConfig
  ): ProcessState =
    disputes.verdicts.foldLeft(
      ProcessState(
        preState.psi.good,
        preState.psi.bad,
        preState.psi.wonky,
        List.empty,
        Set.empty
      )
    ) { (state, verdict) =>
      val positiveVotes = verdict.votes.count(_.vote)
      if positiveVotes >= config.votesPerVerdict then
        state.copy(good = state.good :+ verdict.target)
      else if positiveVotes == 0 then
        state.copy(bad = state.bad :+ verdict.target)
      else if positiveVotes == config.oneThird then
        state.copy(wonky = state.wonky :+ verdict.target)
      else state
    }

  /** Record a new offender key, skipping keys already known or already added.
    */
  private def recordOffender(
      state: ProcessState,
      key: Ed25519PublicKey,
      preOffendersSet: Set[Ed25519PublicKey]
  ): ProcessState =
    val keyInOffenders = preOffendersSet.contains(key)
    val keyAlreadyAdded = state.newOffendersSet.contains(key)

    if !keyInOffenders && !keyAlreadyAdded then
      state.copy(
        offendersMarkRev = key :: state.offendersMarkRev,
        newOffendersSet = state.newOffendersSet + key
      )
    else state

  /** Process culprits using foldLeft. */
  private def processCulprits(
      disputes: Dispute,
      afterVerdicts: ProcessState,
      preOffendersSet: Set[Ed25519PublicKey]
  ): ProcessState =
    disputes.culprits.foldLeft(afterVerdicts) { (state, culprit) =>
      recordOffender(state, culprit.key, preOffendersSet)
    }

  /** Process faults using foldLeft. */
  private def processFaults(
      disputes: Dispute,
      afterCulprits: ProcessState,
      preOffendersSet: Set[Ed25519PublicKey]
  ): ProcessState =
    disputes.faults.foldLeft(afterCulprits) { (state, fault) =>
      recordOffender(state, fault.key, preOffendersSet)
    }

  /** Clear invalid reports from rho. */
  private def clearDisputedReports(
      disputes: Dispute,
      preState: DisputeState,
      config: ChainConfig
  ): List[Option[AvailabilityAssignment]] =
    val clearedHashes: Set[Hash] = disputes.verdicts.flatMap { verdict =>
      val positiveVotes = verdict.votes.count(_.vote)
      if positiveVotes == 0 || positiveVotes == config.oneThird then
        Some(verdict.target)
      else None
    }.toSet

    if clearedHashes.isEmpty then preState.rho
    else
      preState.rho.map { assignmentOpt =>
        assignmentOpt.flatMap { assignment =>
          val reportHash = Hashing.blake2b256(
            summon[Codec[WorkReport]]
              .encode(assignment.report)
              .require
              .toByteArray
          )

          if clearedHashes.contains(reportHash) then None else Some(assignment)
        }
      }

  /** Assemble the posterior psi: sorted judgement lists plus the pre-existing
    * offenders extended with the (sorted) new ones.
    */
  private def finalisePsi(
      afterFaults: ProcessState,
      preState: DisputeState
  ): Psi =
    // Sort new offenders by key for adding to state
    val sortedNewOffenders =
      afterFaults.newOffendersSet.toList.sortWith((a, b) =>
        compareUnsigned(a.bytes, b.bytes) < 0
      )
    val finalOffenders = preState.psi.offenders ++ sortedNewOffenders

    def sortedHashes(hs: List[Hash]): List[Hash] =
      hs.sortWith((a, b) => compareUnsigned(a.bytes, b.bytes) < 0)
    Psi(
      sortedHashes(afterFaults.good),
      sortedHashes(afterFaults.bad),
      sortedHashes(afterFaults.wonky),
      finalOffenders.sortWith((a, b) => compareUnsigned(a.bytes, b.bytes) < 0)
    )

  /** Process disputes and update state. Returns tuple of (new state, offenders
    * mark for output)
    */
  private def processDisputes(
      disputes: Dispute,
      preState: DisputeState,
      config: ChainConfig
  ): (DisputeState, List[Ed25519PublicKey]) =
    val preOffendersSet: Set[Ed25519PublicKey] = preState.psi.offenders.toSet

    val afterVerdicts = processVerdicts(disputes, preState, config)
    val afterCulprits = processCulprits(disputes, afterVerdicts, preOffendersSet)
    val afterFaults = processFaults(disputes, afterCulprits, preOffendersSet)

    val newRho = clearDisputedReports(disputes, preState, config)
    val newPsi = finalisePsi(afterFaults, preState)
    val newState = preState.copy(psi = newPsi, rho = newRho)

    // Return the unsorted offendersMark for the output
    (newState, afterFaults.offendersMarkRev.reverse)

  def stfView(
      input: DisputeInput,
      view: TrieBackedJamState
  ): DisputeOutput =
    val preState = DisputeBridge.extract(view)
    val (postState, output) = stfInternal(input, preState, view.config)
    DisputeBridge.apply(view, postState)
    output

  def stfViewWithPriorState(
      input: DisputeInput,
      view: TrieBackedJamState,
      priorTau: Long,
      priorKappa: List[ValidatorKey],
      priorLambda: List[ValidatorKey]
  ): DisputeOutput =
    val preStateBase = DisputeBridge.extract(view)
    val preState = preStateBase.copy(
      tau = priorTau,
      kappa = priorKappa,
      lambda = priorLambda
    )
    val (postState, output) = stfInternal(input, preState, view.config)
    DisputeBridge.apply(view, postState)
    output

  /** Internal Disputes STF implementation using DisputeState. Exposed for unit
    * testing with module-specific state types.
    *
    * @param input
    *   The dispute input containing verdicts, culprits, and faults.
    * @param preState
    *   The pre-transition state.
    * @param config
    *   The chain configuration.
    * @return
    *   Tuple of (post-transition state, output).
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
        val (postState, offendersMark) =
          processDisputes(input.disputes, preState, config)
        (postState, StfResult.success(DisputeOutputMarks(offendersMark)))
