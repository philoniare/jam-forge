package io.forge.jam.protocol.pipeline

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash, ServiceId}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.protocol.SignatureBatch
import io.forge.jam.protocol.accumulation.{
  AccountChanges,
  AccumulationExecutor,
  AccumulationOutputData,
  AccumulationTransition,
  ServiceAccumulation
}
import io.forge.jam.protocol.assurance.AssuranceTransition
import io.forge.jam.protocol.authorization.AuthorizationTransition
import io.forge.jam.protocol.dispute.DisputeTransition
import io.forge.jam.protocol.history.HistoryTransition
import io.forge.jam.protocol.preimage.PreimageTransition
import io.forge.jam.protocol.report.ReportTransition
import io.forge.jam.protocol.report.ReportTypes.{Ancestry, ReportErrorCode, ReportInput}
import io.forge.jam.protocol.safrole.SafroleTransition
import io.forge.jam.protocol.safrole.SafroleTypes.{SafroleErrorCode, SafroleInput, SafroleOutputData}
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.statistics.StatisticsTransition

import scala.util.boundary, boundary.break
import scala.util.control.NonFatal

final case class BlockPipelineResult(
    safroleOutput: Option[SafroleOutputData],
    availableReports: List[WorkReport],
    accumulateRoot: Option[Hash],
    accumulationStats: Map[ServiceId, ServiceAccumulation],
    accumulationTransferCounts: Map[ServiceId, Int],
    headerHash: Hash,
    accumulateCosts: Map[Long, io.forge.jam.protocol.pvmhost.AccumulateCost] = Map.empty
)

object BlockPipeline:

  def posteriorOffenders(
      prior: List[Ed25519PublicKey],
      disputes: io.forge.jam.core.types.extrinsic.Dispute
  ): List[Ed25519PublicKey] =
    (prior ++ disputes.culprits.map(_.key) ++ disputes.faults.map(_.key)).distinct

  private final class Run(val block: Block, val view: TrieBackedJamState):
    val config: ChainConfig = view.config
    val header = new EncodedHeader(block.header, config)

    val priorSlot: Long = view.timeslot
    val priorCurrentValidators: IndexedSeq[ValidatorKey] = view.validators.current
    val priorPreviousValidators: IndexedSeq[ValidatorKey] = view.validators.previous

    val posteriorOffenders: List[Ed25519PublicKey] =
      BlockPipeline.posteriorOffenders(view.psi.offenders, block.extrinsic.disputes)

    val checks = new PendingChecks
    val signatures = new SignatureBatch

    def safroleInput: SafroleInput = InputExtractor.extractSafroleInput(block, header.entropyOutput)

  private type Stop = boundary.Label[Left[PipelineError, Nothing]]

  extension [A](step: Either[PipelineError, A])
    private inline def orStop(using Stop): A = step match
      case Right(value) => value
      case Left(error)  => break(Left(error))

  private inline def valueOf[E, A](step: Either[E, A])(inline wrap: E => PipelineError)(using Stop): A = step match
    case Right(value) => value
    case Left(error)  => break(Left(wrap(error)))

  private final class EarlyHeaderChecks(
      val seal: Either[PipelineError, Unit],
      val entropy: Either[PipelineError, Unit],
      val checks: PendingChecks
  )

  private def earlyHeaderChecks(run: Run): Option[EarlyHeaderChecks] =
    val slot = run.block.header.slot.toLong
    val epochLength = run.config.epochLength.toLong
    if slot <= run.priorSlot || slot / epochLength != run.priorSlot / epochLength then None
    else
      try
        val checks = new PendingChecks
        val seal = HeaderChecks.blockSeal(run.block, run.header, run.view, checks)
        val entropy = if seal.isRight then HeaderChecks.entropyVrf(run.block, run.view, checks) else seal
        Some(new EarlyHeaderChecks(seal, entropy, checks))
      catch case NonFatal(_) => None

  private def safroleEpoch(run: Run)(using Stop): SafroleOutputData =
    valueOf(SafroleTransition.epochStfView(run.safroleInput, run.view))(PipelineError.SafroleErr(_))

  /** The header checks of [[execute]] that run before any extrinsic is processed, on their own: the extrinsic hash,
    * Safrole's epoch step (slot after the state's, then the validator keys, entropy and sealing sequence for the
    * block's slot), then the seal and the entropy VRF signature under the author key that state implies. These are the
    * same steps `execute` runs, so a block refused here is refused by `execute` on the same state. `view` is advanced
    * by the epoch step and must be a throwaway.
    *
    * With `parentState` the view is the block's parent's posterior state and the epoch and winning-tickets markers
    * are checked too. Without it the view is an earlier ancestor's state that fixes the same author key, seal entropy
    * and sealing sequence for the block's slot (the caller's claim); the markers depend on the parent itself and are
    * left out.
    */
  def verifyHeader(block: Block, view: TrieBackedJamState, parentState: Boolean): Either[PipelineError, Unit] =
    val run = new Run(block, view)
    val result: Either[PipelineError, Unit] = boundary:
      HeaderChecks.extrinsicHash(block, run.config).orStop
      view.postOffenders = run.posteriorOffenders
      val safrole = safroleEpoch(run)
      if parentState then
        HeaderChecks.epochMark(block, safrole).orStop
        HeaderChecks.ticketsMark(block, safrole).orStop
      HeaderChecks.blockSeal(block, run.header, view, run.checks).orStop
      HeaderChecks.entropyVrf(block, view, run.checks).orStop
      Right(())
    result match
      case Left(err) => Left(run.checks.firstFailure().getOrElse(err))
      case Right(_)  => run.checks.firstFailure().toLeft(())

  /** `whileChecksRun` is the caller's work on a block whose steps all passed, done here so that it overlaps the
    * signature checks still running on other threads. The caller must not let it take effect until this returns
    * Right: a check may yet fail. `extrinsicHashChecked` is the caller's word that the block's body already matched
    * its header's extrinsic hash (HeaderChecks.extrinsicHash), which is then not computed again.
    */
  def execute(
      block: Block,
      view: TrieBackedJamState,
      skipAncestryValidation: Boolean = false,
      sharedExecutor: Option[AccumulationExecutor] = None,
      ancestry: Ancestry = Ancestry.none,
      whileChecksRun: BlockPipelineResult => Unit = _ => (),
      extrinsicHashChecked: Boolean = false
  ): Either[PipelineError, BlockPipelineResult] =
    val run = StageTimers.time("x/context")(new Run(block, view))
    val pending = run.checks
    val result =
      try
        val ran = steps(run, skipAncestryValidation, sharedExecutor, ancestry, extrinsicHashChecked)
        ran.foreach(whileChecksRun)
        ran
      catch
        case NonFatal(e) =>
          pending.firstFailure() match
            case Some(err) => Left(err)
            case None      => throw e
    val failed = StageTimers.time("await-checks")(pending.firstFailure())
    result match
      case Left(err)     => Left(failed.getOrElse(err))
      case Right(output) => failed.toLeft(output)

  private def steps(
      run: Run,
      skipAncestryValidation: Boolean,
      sharedExecutor: Option[AccumulationExecutor],
      ancestry: Ancestry,
      extrinsicHashChecked: Boolean
  ): Either[PipelineError, BlockPipelineResult] = boundary:
    import StageTimers.time
    val block = run.block
    val view = run.view
    val slot = block.header.slot.toLong

    val early = time("x/earlyHeaderChecks")(earlyHeaderChecks(run))

    // Step 0: Validate extrinsic hash
    if !extrinsicHashChecked then time("extrinsic-hash")(HeaderChecks.extrinsicHash(block, run.config)).orStop
    view.postOffenders = run.posteriorOffenders

    // Step 1: Safrole
    val safrole = time("safrole")(safroleEpoch(run))

    // Step 2: Block seal validation (uses post-Safrole state)
    time("x/validateEpochMark")(HeaderChecks.epochMark(block, safrole)).orStop
    time("x/validateTicketsMark")(HeaderChecks.ticketsMark(block, safrole)).orStop
    early match
      case Some(started) =>
        started.seal.orStop
        started.entropy.orStop
        run.checks.adopt(started.checks)
      case None =>
        time("seal")(HeaderChecks.blockSeal(block, run.header, view, run.checks)).orStop
        time("entropy-vrf")(HeaderChecks.entropyVrf(block, view, run.checks)).orStop

    time("tickets") {
      valueOf(
        SafroleTransition.ticketsStfView(
          run.safroleInput,
          view,
          Some(_.foreach(check =>
            run.checks.add(check.thenApply(ok =>
              if ok then None else Some(PipelineError.SafroleErr(SafroleErrorCode.BadTicketProof))
            ))
          ))
        )
      )(PipelineError.SafroleErr(_))
    }

    // Step 3: Disputes
    val disputeMarks = time("disputes") {
      valueOf(
        DisputeTransition.stfViewWithPriorState(
          InputExtractor.extractDisputeInput(block),
          view,
          run.priorSlot,
          run.priorCurrentValidators,
          run.priorPreviousValidators
        )
      )(PipelineError.DisputeErr(_))
    }
    time("x/validateOffendersMark")(HeaderChecks.offendersMark(block, disputeMarks.offenders)).orStop

    // Step 4: Assurances
    val availableReports = time("assurances") {
      valueOf(
        AssuranceTransition.stfViewWithValidators(
          InputExtractor.extractAssuranceInput(block),
          view,
          run.priorCurrentValidators,
          Some(run.signatures)
        )
      )(PipelineError.AssuranceErr(_))
    }.reported

    // Step 5: Update beta before Reports
    time("x/updateRecentHistoryPartial")(recordParentStateRoot(view, block.header.parentStateRoot))

    // Step 6: Reports
    time("reports") {
      valueOf(
        ReportTransition.stfView(
          ReportInput(guarantees = block.extrinsic.guarantees, slot = slot),
          view,
          skipAncestryValidation,
          ancestry,
          Some(run.signatures),
          // Registered where a sequential import verifies the batch, so it loses to every error raised before this
          // point and to the checks started before it, and wins over every error raised after it.
          Some(check =>
            run.checks.add(check.thenApply(ok => if ok then None else Some(PipelineError.BadSignatureBatch)))
          )
        )
      ) {
        case ReportErrorCode.BadSignature => PipelineError.BadSignatureBatch
        case e                            => PipelineError.ReportErr(e)
      }
    }: Unit

    Right(afterReports(block, view, run.header, run.priorSlot, safrole, availableReports, sharedExecutor))

  private def afterReports(
      block: Block,
      view: TrieBackedJamState,
      header: EncodedHeader,
      priorSlot: Long,
      safrole: SafroleOutputData,
      availableReports: List[WorkReport],
      sharedExecutor: Option[AccumulationExecutor]
  )(using Stop): BlockPipelineResult =
    import StageTimers.time
    val slot = block.header.slot.toLong

    time("x/savepointStorageView")(view.storage.savepoint())

    // Step 7: Accumulation
    val accumulated: AccumulationOutputData = time("accumulation") {
      try
        AccumulationTransition.stfView(
          InputExtractor.extractAccumulationInput(availableReports, slot),
          view,
          priorSlot,
          sharedExecutor,
          Some(view.storage)
        ) match
          case Right(output) => output
          case Left(nothing) => nothing
      catch
        case e: AccountChanges.DuplicateNewServiceId => break(Left(PipelineError.NewServiceIdConflict(e.serviceId)))
    }
    val accumulateRoot = Hash(accumulated.ok.toArray)
    view.lastAccumulationOutputs = accumulated.commitments

    // Step 8: History (uses accumulateRoot)
    time("history")(
      HistoryTransition.stfView(InputExtractor.extractHistoryInput(block, accumulateRoot, header.hash), view)
    )

    // Step 9: Authorization
    time("authorization")(AuthorizationTransition.stfView(InputExtractor.extractAuthInput(block), view))

    // Step 10: Preimages
    time("preimages") {
      valueOf(PreimageTransition.stfView(InputExtractor.extractPreimageInput(block, slot), view))(
        PipelineError.PreimageErr(_)
      )
    }

    // Step 11: Statistics (needs pre-transition tau)
    time("statistics")(StatisticsTransition.stfView(InputExtractor.extractStatInput(block), view, priorSlot)): Unit
    time("final-stats") {
      StatisticsTransition.completeActivityStatistics(
        view,
        block,
        availableReports,
        accumulated.accumulationStats,
        accumulated.accumulationTransferCounts
      )
    }

    time("x/discardStorageViewCheckpoint")(view.storage.discardCheckpoint())

    BlockPipelineResult(
      safroleOutput = Some(safrole),
      availableReports = availableReports,
      accumulateRoot = Some(accumulateRoot),
      accumulationStats = accumulated.accumulationStats,
      accumulationTransferCounts = accumulated.accumulationTransferCounts,
      headerHash = header.hash,
      accumulateCosts = accumulated.accumulateCosts
    )

  def executeAfterReports(
      block: Block,
      view: TrieBackedJamState,
      priorSlot: Long,
      safrole: SafroleOutputData,
      availableReports: List[WorkReport],
      sharedExecutor: Option[AccumulationExecutor] = None,
      whileChecksRun: BlockPipelineResult => Unit = _ => ()
  ): Either[PipelineError, BlockPipelineResult] =
    val header = new EncodedHeader(block.header, view.config)
    val pending = new PendingChecks
    val result =
      try
        val ran = authoredSteps(block, view, header, pending, priorSlot, safrole, availableReports, sharedExecutor)
        ran.foreach(whileChecksRun)
        ran
      catch
        case NonFatal(e) =>
          pending.firstFailure() match
            case Some(err) => Left(err)
            case None      => throw e
    val failed = StageTimers.time("await-checks")(pending.firstFailure())
    result match
      case Left(err)     => Left(failed.getOrElse(err))
      case Right(output) => failed.toLeft(output)

  private def authoredSteps(
      block: Block,
      view: TrieBackedJamState,
      header: EncodedHeader,
      pending: PendingChecks,
      priorSlot: Long,
      safrole: SafroleOutputData,
      availableReports: List[WorkReport],
      sharedExecutor: Option[AccumulationExecutor]
  ): Either[PipelineError, BlockPipelineResult] = boundary:
    StageTimers.time("seal")(HeaderChecks.blockSeal(block, header, view, pending)).orStop
    StageTimers.time("entropy-vrf")(HeaderChecks.entropyVrf(block, view, pending)).orStop
    Right(afterReports(block, view, header, priorSlot, safrole, availableReports, sharedExecutor))

  def recordParentStateRoot(view: TrieBackedJamState, parentStateRoot: Hash): Unit =
    val recentHistory = view.beta
    val history = recentHistory.history
    if history.nonEmpty then
      val updatedHistory = history.updated(
        history.length - 1,
        history.last.copy(stateRoot = parentStateRoot)
      )
      view.beta = recentHistory.copy(history = updatedHistory)
