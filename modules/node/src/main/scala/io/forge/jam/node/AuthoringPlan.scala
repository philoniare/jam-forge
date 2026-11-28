package io.forge.jam.node

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, Dispute, GuaranteeExtrinsic}
import io.forge.jam.protocol.assurance.AssuranceTransition
import io.forge.jam.protocol.assurance.AssuranceTypes.AssuranceInput
import io.forge.jam.protocol.dispute.DisputeTransition
import io.forge.jam.protocol.pipeline.StageTimers
import io.forge.jam.protocol.dispute.DisputeTypes.DisputeInput
import io.forge.jam.crypto.Ed25519
import io.forge.jam.protocol.SignatureBatch
import io.forge.jam.protocol.report.ReportTransition
import io.forge.jam.protocol.report.ReportTypes.{AncestorHeader, ReportErrorCode}
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
      parentStateRoot: Hash,
      assurances: List[AssuranceExtrinsic],
      guarantees: List[GuaranteeExtrinsic],
      ancestry: List[AncestorHeader],
      config: ChainConfig
  ): Plan =
    // 2. Disputes: already checked against the same prior state when built; they clear the cores of bad and wonky
    // reports.
    StageTimers.time("plan/disputes")(DisputeTransition.stfViewWithPriorState(DisputeInput(disputes), view, priorTau, priorKappa, priorLambda))

    // 3. Assurances, against the parent state's current validator set.
    val assurancePre = StageTimers.time("plan/assurances-extract")(AssuranceBridge.extract(view).copy(currValidators = priorKappa))
    def assure(as: List[AssuranceExtrinsic], batch: SignatureBatch = new SignatureBatch) =
      AssuranceTransition.stfInternal(AssuranceInput(as, slot, parent), assurancePre, config, Some(batch))
    val sortedAssurances = assurances.sortBy(_.validatorIndex.toInt).distinctBy(_.validatorIndex.toInt)
    val (chosenAssurances, assured, singlyValid) = StageTimers.time("plan/assurances-set") {
      val wholeBatch = new SignatureBatch
      val whole = assure(sortedAssurances, wholeBatch)
      if whole._2.isRight && Ed25519.verifyAll(wholeBatch.snapshot) then (sortedAssurances, whole._1, sortedAssurances)
      else
        val candidates = sortedAssurances.flatMap { a =>
          val batch = new SignatureBatch
          Option.when(assure(List(a), batch)._2.isRight && batch.size == 1)(a -> batch.snapshot.head)
        }
        val signed = Ed25519.validity(candidates.map(_._2).toIndexedSeq)
        val singlyValid = candidates.iterator.zipWithIndex.collect { case ((a, _), i) if signed(i) => a }.toList
        val chosen =
          if assure(singlyValid)._2.isRight then singlyValid
          else singlyValid.foldLeft(List.empty[AssuranceExtrinsic])((acc, a) => if assure(acc :+ a)._2.isRight then acc :+ a else acc)
        (chosen, assure(chosen)._1, singlyValid)
    }
    StageTimers.time("plan/assurances-apply")(AssuranceBridge.apply(view, assured))

    io.forge.jam.protocol.pipeline.BlockPipeline.recordParentStateRoot(view, parentStateRoot)
    val reportPre = StageTimers.time("plan/reports-extract")(ReportBridge.extract(view))
    val reports =
      new ReportTransition.Checker(reportPre.state, slot, config, ancestry, serviceInfo = Some(reportPre.serviceInfo))
    def report(gs: List[GuaranteeExtrinsic]) = reports.check(gs, new SignatureBatch)
    val alone = StageTimers.time("plan/reports-each") {
      val checked = guarantees.map { g =>
        val batch = new SignatureBatch
        (g, reports.check(List(g), batch), batch.snapshot)
      }
      val signatures = checked.iterator.filter(_._2.isRight).flatMap(_._3).toIndexedSeq
      val signed = Ed25519.validity(signatures)
      var next = 0
      checked.map { (g, result, items) =>
        g -> result.flatMap { _ =>
          val from = next
          next += items.size
          Either.cond((from until next).forall(signed(_)), (), ReportErrorCode.BadSignature)
        }
      }.toMap
    }
    val byCore = guarantees.groupBy(_.report.coreIndex.toInt).toList.sortBy(_._1)
    // Per core, the first candidate that passes alone, oldest first.
    val singly = byCore.flatMap((_, gs) => gs.find(g => alone(g).isRight))
    val chosenGuarantees = StageTimers.time("plan/reports-set") {
      if singly.isEmpty || report(singly).isRight then singly
      else singly.foldLeft(List.empty[GuaranteeExtrinsic])((acc, g) => if report(acc :+ g).isRight then acc :+ g else acc)
    }

    Plan(
      chosenAssurances,
      chosenGuarantees,
      // An assurance is anchored at this block's parent: one that fails now is of no use to a later block.
      if singlyValid eq sortedAssurances then Nil else sortedAssurances.filterNot(singlyValid.contains),
      // A guarantee whose core is still engaged may go in once the core frees; any other failure lasts.
      guarantees.filter(g => alone(g).left.exists(_ != ReportErrorCode.CoreEngaged))
    )
