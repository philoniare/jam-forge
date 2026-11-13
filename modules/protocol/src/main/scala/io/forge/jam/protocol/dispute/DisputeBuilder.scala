package io.forge.jam.protocol.dispute

import io.forge.jam.core.{ChainConfig, constants}
import io.forge.jam.core.JamBytes.compareUnsigned
import io.forge.jam.core.primitives.{Ed25519PublicKey, Ed25519Signature, Hash, ValidatorIndex}
import io.forge.jam.core.types.dispute.{Culprit, Fault}
import io.forge.jam.core.types.extrinsic.{Dispute, Verdict}
import io.forge.jam.core.types.work.Vote
import io.forge.jam.crypto.Ed25519
import io.forge.jam.protocol.dispute.DisputeTypes.*

/** Builds a block author's disputes extrinsic from the judgments and guarantor credentials a
  * node has seen. Pure: the caller supplies the evidence and the prior dispute state, and gets back an extrinsic the
  * Disputes STF accepts against that state, with the offenders mark it implies.
  *
  * For each report with judgments of the epoch before or of the current one, at most one verdict of exactly
  * ⌊2V/3⌋+1 votes from that epoch's set:
  *   - good, all positive, when there are enough positive judgments and someone judged it invalid: those negative
  *     judges are faults (the STF requires one);
  *   - bad, all negative, when there are enough negative judgments: the guarantors we know of are culprits, the
  *     positive judges faults. The test vectors accept a bad verdict without culprits
  *     (progress_with_bad_verdict_without_culprits), so a verdict does not wait for them;
  *   - wonky, ⌊V/3⌋ positive, once every validator has judged and neither side has a supermajority.
  *
  * Judgments whose signature does not verify are left out, as are keys already offenders and reports already judged.
  * Within the extrinsic limits, verdicts are taken in target order and each is kept only if the STF still accepts
  * the extrinsic with it.
  */
object DisputeBuilder:

  final case class Judgment(validatorIndex: Int, valid: Boolean, signature: Ed25519Signature)

  /** A guarantor's signature on the report: key and its signature of `$jam_guarantee ++ H(r)`. */
  final case class Credential(key: Ed25519PublicKey, signature: Ed25519Signature)

  /** What is known of one report: judgments by validator index into the set of `epoch`, and its guarantors. */
  final case class Evidence(target: Hash, epoch: Long, judgments: Seq[Judgment], guarantors: Seq[Credential])

  final case class Built(dispute: Dispute, offendersMark: List[Ed25519PublicKey]):
    def isEmpty: Boolean = dispute.verdicts.isEmpty

  private final case class Candidate(verdict: Verdict, culprits: List[Culprit], faults: List[Fault])

  val empty: Built = Built(Dispute(Nil, Nil, Nil), Nil)

  def build(evidence: Iterable[Evidence], state: DisputeState, config: ChainConfig): Built =
    val currentEpoch = state.tau / config.epochLength
    val judged = (state.psi.good ++ state.psi.bad ++ state.psi.wonky).toSet
    val offenders = state.psi.offenders.toSet
    val candidates = evidence.toList
      .filter(e => (e.epoch == currentEpoch || e.epoch == currentEpoch - 1) && !judged.contains(e.target))
      .groupBy(_.target)
      .values
      .flatMap(es => candidate(merge(es), if es.head.epoch == currentEpoch then state.kappa else state.lambda, offenders))
      .toList
      .sortWith((a, b) => compareUnsigned(a.verdict.target.bytes, b.verdict.target.bytes) < 0)

    candidates.foldLeft(empty) { (acc, c) =>
      val culprits = (acc.dispute.culprits ++ c.culprits).distinctBy(_.key)
      val faults = (acc.dispute.faults ++ c.faults).distinctBy(_.key)
      val within =
        acc.dispute.verdicts.size < constants.Cmaxextrinsicverdicts &&
          culprits.size <= constants.Cmaxextrinsicoffenses && faults.size <= constants.Cmaxextrinsicoffenses
      if !within then acc
      else
        val next = Dispute(acc.dispute.verdicts :+ c.verdict, sortByKey(culprits)(_.key), sortByKey(faults)(_.key))
        val (_, out) = DisputeTransition.stfInternal(DisputeInput(next), state, config)
        out match
          case Right(marks) => Built(next, marks.offenders)
          case Left(_)      => acc
    }

  private def merge(es: Iterable[Evidence]): Evidence =
    val e = es.head
    e.copy(
      judgments = es.flatMap(_.judgments).toSeq.distinctBy(j => (j.validatorIndex, j.valid)),
      guarantors = es.flatMap(_.guarantors).toSeq.distinctBy(_.key)
    )

  private def candidate(
      e: Evidence,
      set: IndexedSeq[io.forge.jam.core.types.epoch.ValidatorKey],
      offenders: Set[Ed25519PublicKey]
  ): Option[Candidate] =
    val v = set.size
    val need = 2 * v / 3 + 1
    val third = v / 3
    def genuine(j: Judgment): Boolean =
      j.validatorIndex >= 0 && j.validatorIndex < v && {
        val context = if j.valid then constants.JAM_VALID_BYTES else constants.JAM_INVALID_BYTES
        Ed25519.verify(set(j.validatorIndex).ed25519, context ++ e.target.bytes, j.signature)
      }
    // A validator that signed both ways is counted on neither side.
    val checked = e.judgments.filter(genuine).groupBy(_.validatorIndex).collect { case (_, Seq(j)) => j }.toList
    val positive = checked.filter(_.valid).sortBy(_.validatorIndex)
    val negative = checked.filterNot(_.valid).sortBy(_.validatorIndex)
    def votes(js: Seq[Judgment]) =
      js.sortBy(_.validatorIndex).map(j => Vote(j.valid, ValidatorIndex(j.validatorIndex), j.signature)).toList
    def faultsOf(js: Seq[Judgment]) =
      js.map(j => Fault(e.target, j.valid, set(j.validatorIndex).ed25519, j.signature))
        .filterNot(f => offenders.contains(f.key))
        .toList
    val culprits = e.guarantors
      .filterNot(c => offenders.contains(c.key))
      .filter(c => Ed25519.verify(c.key, constants.JAM_GUARANTEE_BYTES ++ e.target.bytes, c.signature))
      .map(c => Culprit(e.target, c.key, c.signature))
      .toList
    val age = io.forge.jam.core.primitives.Timeslot(e.epoch.toInt)
    if v == 0 then None
    else if positive.size >= need && faultsOf(negative).nonEmpty then
      Some(Candidate(Verdict(e.target, age, votes(positive.take(need))), Nil, faultsOf(negative)))
    else if negative.size >= need then
      Some(Candidate(Verdict(e.target, age, votes(negative.take(need))), culprits, faultsOf(positive)))
    else if third > 0 && positive.size + negative.size == v && positive.size >= third && negative.size >= need - third
    then Some(Candidate(Verdict(e.target, age, votes(positive.take(third) ++ negative.take(need - third))), Nil, Nil))
    else None

  private def sortByKey[A](xs: List[A])(key: A => Ed25519PublicKey): List[A] =
    xs.sortWith((a, b) => compareUnsigned(key(a).bytes, key(b).bytes) < 0)
