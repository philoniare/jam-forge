package io.forge.jam.node

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, GuaranteeExtrinsic, Preimage}
import io.forge.jam.core.types.tickets.TicketEnvelope

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Pending extrinsic content for block authoring, fed by the distribution
  * protocols (CE 135 guarantees, CE 141 assurances, CE 142/143 preimages).
  * Validation here is best-effort dedup/anchoring — the block import pipeline
  * remains the gate.
  */
final class ExtrinsicPools:

  /** Guarantees keyed by work-report hash. */
  private val guarantees =
    new java.util.concurrent.ConcurrentHashMap[Hash, GuaranteeExtrinsic]()

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

  /** Audit judgments (CE 145) keyed by (report hash, validator index):
    * validity claim and its Ed25519 signature
    */
  private val judgments =
    new java.util.concurrent.ConcurrentHashMap[(Hash, Int), (Boolean, Array[Byte])]()

  def addGuarantee(g: GuaranteeExtrinsic): Unit =
    val reportHash = Hashing.blake2b256(g.report.encode.toArray)
    guarantees.putIfAbsent(reportHash, g)

  def addAssurance(a: AssuranceExtrinsic): Unit =
    assurances.putIfAbsent((a.validatorIndex.toInt, a.anchor), a)

  def addPreimage(p: Preimage): Unit =
    preimages.putIfAbsent(
      (p.requester.toLong, Hashing.blake2b256(p.blob.toArray)),
      p
    )

  def addTicket(id: Hash, envelope: TicketEnvelope, epoch: Long, entropy: Hash): Unit =
    tickets.putIfAbsent(id, (epoch, entropy, envelope))

  def addJudgment(reportHash: Hash, validatorIndex: Int, valid: Boolean, signature: Array[Byte]): Unit =
    judgments.putIfAbsent((reportHash, validatorIndex), (valid, signature))

  def judgmentsFor(reportHash: Hash): Map[Int, Boolean] =
    judgments.asScala.collect {
      case ((h, idx), (valid, _)) if h == reportHash => idx -> valid
    }.toMap

  def judgmentCount: Int = judgments.size

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

  def takePreimages(usable: Preimage => Boolean = _ => true): List[Preimage] =
    preimages.values.asScala.toList
      .filter(usable)
      .sortBy(p => (p.requester.toLong, Hashing.blake2b256(p.blob.toArray).toHex))

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

  def pendingGuarantees: List[GuaranteeExtrinsic] = guarantees.values.asScala.toList

  def pendingPreimages: List[Preimage] = preimages.values.asScala.toList

  def removePreimages(ps: Iterable[Preimage]): Unit =
    ps.foreach(p => preimages.remove((p.requester.toLong, Hashing.blake2b256(p.blob.toArray))))

  def removeGuarantees(gs: Iterable[GuaranteeExtrinsic]): Unit =
    gs.foreach(g => guarantees.remove(Hashing.blake2b256(g.report.encode.toArray)))

  def clear(): Unit =
    guarantees.clear()
    assurances.clear()
    preimages.clear()

  def guaranteeCount: Int = guarantees.size
  def assuranceCount: Int = assurances.size
