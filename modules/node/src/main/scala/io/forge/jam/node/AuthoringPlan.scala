package io.forge.jam.node

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, Dispute, GuaranteeExtrinsic}
import io.forge.jam.protocol.assurance.AssuranceTransition
import io.forge.jam.protocol.assurance.AssuranceTypes.AssuranceInput
import io.forge.jam.protocol.dispute.DisputeTransition
import io.forge.jam.protocol.dispute.DisputeTypes.DisputeInput
import io.forge.jam.protocol.report.ReportTransition
import io.forge.jam.protocol.report.ReportTypes.{AncestorHeader, ReportErrorCode, ReportInput}
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.{AssuranceBridge, ReportBridge}

/** Chooses a block's assurances and guarantees so the block imports: they run through the Disputes, Assurances and
  * Reports state transitions, in the block pipeline's order, on a throwaway view, and only those that pass go in.
  * A pooled item can be invalid by the time we author without anyone misbehaving (a peer's block took the core,
  * a report became available or timed out, the anchor left recent history), so one that fails is left out and
  * reported for removal from the pool, and everything else still goes in.
  *
  * Each item is checked on its own first (linear in the number of items), then the chosen set as a whole; only if
  * the whole fails (items that conflict with each other, such as a duplicate package on two cores) is the set built
  * up item by item.
  */
object AuthoringPlan:

  final case class Plan(
      assurances: List[AssuranceExtrinsic],
      guarantees: List[GuaranteeExtrinsic],
      droppedAssurances: List[AssuranceExtrinsic],
      droppedGuarantees: List[GuaranteeExtrinsic]
  )

  /** @param view the posterior-Safrole view for the block; consumed (left in an undefined state)
    * @param priorTau, priorKappa, priorLambda the parent state's slot and current and previous validator sets, as
    *   the pipeline passes them
    * @param guarantees every usable pooled guarantee; at most one per core goes in, the first that passes
    */
  def plan(
      view: TrieBackedJamState,
      priorTau: Long,
      priorKappa: IndexedSeq[ValidatorKey],
      priorLambda: IndexedSeq[ValidatorKey],
      disputes: Dispute,
      slot: Long,
      parent: Hash,
      assurances: List[AssuranceExtrinsic],
      guarantees: List[GuaranteeExtrinsic],
      ancestry: List[AncestorHeader],
      config: ChainConfig
  ): Plan =
    // 2. Disputes: already checked against the same prior state when built; they clear the cores of bad and wonky
    // reports.
    DisputeTransition.stfViewWithPriorState(DisputeInput(disputes), view, priorTau, priorKappa, priorLambda)

    // 3. Assurances, against the parent state's current validator set.
    val assurancePre = AssuranceBridge.extract(view).copy(currValidators = priorKappa)
    def assure(as: List[AssuranceExtrinsic]) =
      AssuranceTransition.stfInternal(AssuranceInput(as, slot, parent), assurancePre, config)
    val sortedAssurances = assurances.sortBy(_.validatorIndex.toInt).distinctBy(_.validatorIndex.toInt)
    val singlyValid = sortedAssurances.filter(a => assure(List(a))._2.isRight)
    val chosenAssurances =
      if assure(singlyValid)._2.isRight then singlyValid
      else singlyValid.foldLeft(List.empty[AssuranceExtrinsic])((acc, a) => if assure(acc :+ a)._2.isRight then acc :+ a else acc)
    AssuranceBridge.apply(view, assure(chosenAssurances)._1)

    // 4. Reports, on the cores as the assurances left them.
    val reportPre = ReportBridge.extract(view)
    def report(gs: List[GuaranteeExtrinsic]) =
      ReportTransition.stfInternal(ReportInput(gs, slot), reportPre, config, skipAncestryValidation = false, ancestry)._2
    val alone = guarantees.map(g => g -> report(List(g))).toMap
    val byCore = guarantees.groupBy(_.report.coreIndex.toInt).toList.sortBy(_._1)
    // Per core, the first candidate that passes alone, oldest first.
    val singly = byCore.flatMap((_, gs) => gs.find(g => alone(g).isRight))
    val chosenGuarantees =
      if singly.isEmpty || report(singly).isRight then singly
      else singly.foldLeft(List.empty[GuaranteeExtrinsic])((acc, g) => if report(acc :+ g).isRight then acc :+ g else acc)

    Plan(
      chosenAssurances,
      chosenGuarantees,
      // An assurance is anchored at this block's parent: one that fails now is of no use to a later block.
      sortedAssurances.filterNot(singlyValid.contains),
      // A guarantee whose core is still engaged may go in once the core frees; any other failure lasts.
      guarantees.filter(g => alone(g).left.exists(_ != ReportErrorCode.CoreEngaged))
    )
