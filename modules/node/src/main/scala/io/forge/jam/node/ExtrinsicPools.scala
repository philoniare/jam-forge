package io.forge.jam.node

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, GuaranteeExtrinsic, Preimage}
import io.forge.jam.core.types.tickets.TicketEnvelope
import io.forge.jam.protocol.preimage.PreimageTransition

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Pending extrinsic content for block authoring, fed by the distribution
  * protocols (CE 135 guarantees, CE 141 assurances, CE 142/143 preimages).
  * Validation here is best-effort dedup/anchoring — the block import pipeline
  * remains the gate.
  */
final class ExtrinsicPools(
    maxGuaranteesPerCore: Int = ExtrinsicPools.MaxGuaranteesPerCore,
    maxGuarantees: Int = ExtrinsicPools.MaxGuarantees
):

  /** Guarantees keyed by work-report hash: at most `maxGuaranteesPerCore` per core and `maxGuarantees` in all,
    * the oldest making room for a new one.
    */
  private val guarantees =
    new java.util.concurrent.ConcurrentHashMap[Hash, GuaranteeExtrinsic]()

  /** Arrival order of the pooled guarantees, for eviction. Guarded by `guaranteeLock`, as is every insert. */
  private val guaranteeArrival = new java.util.HashMap[Hash, java.lang.Long]()
  private var guaranteeSeq = 0L
  private val guaranteeLock = new Object

  /** Assurances keyed by (validator index, anchor). */
  private val assurances =
    new java.util.concurrent.ConcurrentHashMap[(Int, Hash), AssuranceExtrinsic]()

  /** Preimages keyed by (service, blob hash). */
  private val preimages =
    new java.util.concurrent.ConcurrentHashMap[(Long, Hash), Preimage]()

  /** Ticket envelopes keyed by their (verified) ticket id, with the epoch
    * whose contest they entered and the η_2 their proof was verified
    * against: the proof is bound to that entropy,
    * so it is useless in a later epoch or on a fork whose epoch
    * entropy differs.
    */
  private val tickets =
    new java.util.concurrent.ConcurrentHashMap[Hash, (Long, Hash, TicketEnvelope)]()

  /** Genuine audit judgments (ours and CE 145) keyed by (report hash, epoch, validator index into that epoch's set):
    * validity claim and its Ed25519 signature. The evidence for a disputes verdict.
    */
  private val judgments =
    new java.util.concurrent.ConcurrentHashMap[(Hash, Long, Int), (Boolean, Array[Byte])]()

  /** The guarantee credentials of reports included on chain, by report hash: the including block's parent (whose
    * validator sets the indices refer to), its slot, and (validator index, signature) pairs. Culprits for a bad verdict.
    */
  private val credentials =
    new java.util.concurrent.ConcurrentHashMap[Hash, (Hash, Long, List[(Int, Array[Byte])])]()

  private def reportHashOf(g: GuaranteeExtrinsic): Hash = Hashing.blake2b256(g.report.encode.toArray)

  /** Whether a guarantee of the same work report is pooled. */
  def hasGuarantee(g: GuaranteeExtrinsic): Boolean = guarantees.containsKey(reportHashOf(g))

  /** Pool a guarantee unless its report is pooled already. When its core, or the pool, is full the oldest
    * guarantee (of that core, or of the pool) is evicted first.
    */
  def addGuarantee(g: GuaranteeExtrinsic): Unit =
    val reportHash = reportHashOf(g)
    guaranteeLock.synchronized {
      if !guarantees.containsKey(reportHash) then
        val core = g.report.coreIndex.toInt
        def oldest(of: GuaranteeExtrinsic => Boolean): Option[Hash] =
          guarantees.entrySet.asScala.iterator
            .filter(e => of(e.getValue))
            .map(_.getKey)
            .minByOption(h => Option(guaranteeArrival.get(h)).fold(Long.MinValue)(_.longValue))
        if guarantees.values.asScala.count(_.report.coreIndex.toInt == core) >= maxGuaranteesPerCore then
          oldest(_.report.coreIndex.toInt == core).foreach(guarantees.remove)
        if guarantees.size >= maxGuarantees then oldest(_ => true).foreach(guarantees.remove)
        guaranteeArrival.keySet.retainAll(guarantees.keySet)
        guaranteeSeq += 1
        guaranteeArrival.put(reportHash, guaranteeSeq)
        guarantees.put(reportHash, g)
    }

  def addAssurance(a: AssuranceExtrinsic): Unit =
    assurances.putIfAbsent((a.validatorIndex.toInt, a.anchor), a)

  def addPreimage(p: Preimage): Unit =
    preimages.putIfAbsent(
      (p.requester.toLong, Hashing.blake2b256(p.blob.toArray)),
      p
    )

  def addTicket(id: Hash, envelope: TicketEnvelope, epoch: Long, entropy: Hash): Unit =
    tickets.putIfAbsent(id, (epoch, entropy, envelope))

  def addJudgment(reportHash: Hash, epoch: Long, validatorIndex: Int, valid: Boolean, signature: Array[Byte]): Unit =
    judgments.putIfAbsent((reportHash, epoch, validatorIndex), (valid, signature))

  def judgmentsFor(reportHash: Hash): Map[Int, Boolean] =
    judgments.asScala.collect {
      case ((h, _, idx), (valid, _)) if h == reportHash => idx -> valid
    }.toMap

  /** Drop the judgments on reports whose audit is over, unless someone judged one invalid: those stay as dispute
    * evidence until their epoch is too old for a verdict ([[pruneDisputes]]). The auditor adds judgments only for
    * reports of blocks it is auditing and calls this when it stops, so the pool holds at most validators × reports
    * awaiting audit, plus the disputed ones.
    */
  def removeJudgments(reportHashes: Iterable[Hash]): Unit =
    val disputed = judgments.asScala.collect { case ((h, _, _), (false, _)) => h }.toSet
    val gone = reportHashes.toSet -- disputed
    if gone.nonEmpty then judgments.keySet.removeIf(k => gone.contains(k._1)): Unit

  def judgmentCount: Int = judgments.size

  /** Judgments that a report is invalid. */
  def negativeJudgmentCount: Int = judgments.values.asScala.count(!_._1)

  /** Record the credentials of the guarantees in a block on `parent` at `slot`. */
  def recordCredentials(parent: Hash, slot: Long, guarantees: Iterable[GuaranteeExtrinsic]): Unit =
    guarantees.foreach { g =>
      credentials.put(
        reportHashOf(g),
        (parent, slot, g.signatures.map(s => s.validatorIndex.toInt -> s.signature.bytes))
      )
    }

  /** Reports someone judged invalid, with every judgment on them by epoch, and their credentials if known. */
  def disputed: List[ExtrinsicPools.Disputed] =
    val all = judgments.asScala.toList
    val targets = all.collect { case ((h, _, _), (false, _)) => h }.toSet
    all
      .filter(e => targets.contains(e._1._1))
      .groupBy(e => (e._1._1, e._1._2))
      .toList
      .map { case ((h, epoch), js) =>
        ExtrinsicPools.Disputed(
          h,
          epoch,
          js.map { case ((_, _, v), (valid, sig)) => (v, valid, sig) },
          Option(credentials.get(h))
        )
      }

  /** Forget dispute evidence too old to use: judgments of epochs before `currentEpoch - 1` (a verdict's age must be
    * the current or the previous epoch), the judgments on reports `judged` already, and credentials of blocks two
    * epochs back.
    */
  def pruneDisputes(currentEpoch: Long, epochLength: Int, judged: Set[Hash]): Unit =
    judgments.keySet.removeIf(k => k._2 < currentEpoch - 1 || judged.contains(k._1))
    credentials.values.removeIf(c => c._2 / epochLength < currentEpoch - 1)
    ()

  /** Tickets for inclusion in a block of `epoch` whose posterior η_2 is
    * `entropy`: ascending ticket-id order,
    * excluding ids already in
    * the accumulator `accumulated`, at most `max`. Only tickets that will be
    * retained are offered: once the accumulator plus the chosen tickets
    * would exceed `epochLength` (E), a ticket must rank below the E-th
    * smallest id or the block is invalid.
    */
  def takeTickets(
      epoch: Long,
      entropy: Hash,
      accumulated: Set[JamBytes],
      max: Int,
      epochLength: Int
  ): List[(Hash, TicketEnvelope)] =
    val accumulatedHex = accumulated.toList.map(_.toHex.stripPrefix("0x")).sorted
    val candidates = tickets.entrySet.asScala.toList
      .filter(e => e.getValue._1 == epoch && e.getValue._2 == entropy)
      .filterNot(e => accumulated.contains(JamBytes(e.getKey.bytes.toArray)))
      .map(e => (e.getKey.toHex.stripPrefix("0x"), e.getKey, e.getValue._3))
      .sortBy(_._1)
    val chosen = mutable.ListBuffer.empty[(Hash, TicketEnvelope)]
    val it = candidates.iterator
    var full = false
    while chosen.size < max && !full && it.hasNext do
      val (idHex, id, env) = it.next()
      // Rank of this id among accumulator ∪ chosen (all chosen ids are smaller).
      val rank = accumulatedHex.count(_ < idHex) + chosen.size
      if rank < epochLength then chosen += ((id, env))
      else full = true
    chosen.toList

  def removeTickets(ids: Iterable[Hash]): Unit =
    ids.foreach(tickets.remove)

  /** Drop tickets entered for any epoch before `epoch`. */
  def pruneTicketsBefore(epoch: Long): Unit =
    tickets.entrySet.removeIf(_.getValue._1 < epoch)
    ()

  def ticketCount: Int = tickets.size

  /** Guarantees for inclusion: sorted by core index (extrinsic ordering
    * requirement), at most one per core.
    */
  def takeGuarantees(usable: GuaranteeExtrinsic => Boolean = _ => true): List[GuaranteeExtrinsic] =
    guarantees.values.asScala.toList
      .filter(usable)
      .groupBy(_.report.coreIndex.toInt)
      .values
      .map(_.head)
      .toList
      .sortBy(_.report.coreIndex.toInt)

  /** Assurances anchored at `parent`, sorted by validator index. */
  def takeAssurances(parent: Hash): List[AssuranceExtrinsic] =
    assurances.values.asScala.toList
      .filter(_.anchor == parent)
      .sortBy(_.validatorIndex.toInt)

  /** Preimages for inclusion, in the order block import requires:
    * by requester, then by blob bytes, not by hash.
    * Unique, since the pool is keyed by (requester, blob hash).
    */
  def takePreimages(usable: Preimage => Boolean = _ => true): List[Preimage] =
    preimages.values.asScala.toList
      .filter(usable)
      .sorted(using PreimageTransition.preimageOrdering)

  def findPreimage(hash: Hash): Option[Preimage] =
    preimages.values.asScala.find(p => Hashing.blake2b256(p.blob.toArray) == hash)

  /** Drop content consumed by (or stale after) an imported block. */
  def pruneAfterImport(
      includedGuarantees: List[GuaranteeExtrinsic],
      includedAssurances: List[AssuranceExtrinsic],
      includedPreimages: List[Preimage],
      newBestHash: Hash
  ): Unit =
    includedGuarantees.foreach { g =>
      guarantees.remove(Hashing.blake2b256(g.report.encode.toArray))
    }
    includedAssurances.foreach { a =>
      assurances.remove((a.validatorIndex.toInt, a.anchor))
    }
    includedPreimages.foreach { p =>
      preimages.remove((p.requester.toLong, Hashing.blake2b256(p.blob.toArray)))
    }
    // Assurances only ever anchor at the parent of the next block: drop stale.
    assurances.keySet.asScala.filterNot(_._2 == newBestHash).toList.foreach(assurances.remove)

  def clearGuarantees(): Unit = guarantees.clear()

  /** Drop guarantees made before `slot`: with `slot` the first slot of the rotation before the best block's, they
    * can go in no later block (their rotation is neither that block's nor the one before it).
    */
  def pruneGuaranteesBefore(slot: Long): Unit =
    guarantees.values.removeIf(_.slot.toLong < slot)
    ()

  def pendingGuarantees: List[GuaranteeExtrinsic] = guarantees.values.asScala.toList

  /** The pooled guarantees, oldest first. */
  def guaranteesByArrival: List[GuaranteeExtrinsic] =
    guaranteeLock.synchronized {
      guarantees.entrySet.asScala.toList
        .sortBy(e => Option(guaranteeArrival.get(e.getKey)).fold(Long.MaxValue)(_.longValue))
        .map(_.getValue)
    }

  def pendingPreimages: List[Preimage] = preimages.values.asScala.toList

  def removePreimages(ps: Iterable[Preimage]): Unit =
    ps.foreach(p => preimages.remove((p.requester.toLong, Hashing.blake2b256(p.blob.toArray))))

  def removeAssurances(as: Iterable[AssuranceExtrinsic]): Unit =
    as.foreach(a => assurances.remove((a.validatorIndex.toInt, a.anchor), a))

  def removeGuarantees(gs: Iterable[GuaranteeExtrinsic]): Unit =
    gs.foreach(g => guarantees.remove(Hashing.blake2b256(g.report.encode.toArray)))

  def clear(): Unit =
    guarantees.clear()
    assurances.clear()
    preimages.clear()

  def guaranteeCount: Int = guarantees.size
  def assuranceCount: Int = assurances.size

object ExtrinsicPools:
  /** Evidence on one report judged invalid by someone: judgments of `epoch` as (validator index, valid, signature),
    * and the including block's parent, slot and credentials when we saw it.
    */
  final case class Disputed(
      reportHash: Hash,
      epoch: Long,
      judgments: List[(Int, Boolean, Array[Byte])],
      credentials: Option[(Hash, Long, List[(Int, Array[Byte])])]
  )

  /** A core takes one report at a time, so more than a few pending for one core are a guarantor signing several
    * reports for a slot.
    */
  val MaxGuaranteesPerCore: Int = 4

  /** Above `MaxGuaranteesPerCore` times the 341 cores of the full configuration. */
  val MaxGuarantees: Int = 2048
