package io.forge.jam.node

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.scodec.JamCodecs.encode
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

  /** Ticket envelopes keyed by their (verified) ticket id. */
  private val tickets =
    new java.util.concurrent.ConcurrentHashMap[Hash, TicketEnvelope]()

  def addGuarantee(g: GuaranteeExtrinsic): Unit =
    val reportHash = Hashing.blake2b256(g.report.encode.toArray)
    guarantees.putIfAbsent(reportHash, g)

  def addAssurance(a: AssuranceExtrinsic): Unit =
    assurances.putIfAbsent((a.validatorIndex.value.toInt, a.anchor), a)

  def addPreimage(p: Preimage): Unit =
    preimages.putIfAbsent(
      (p.requester.value.toLong, Hashing.blake2b256(p.blob.toArray)),
      p
    )

  def addTicket(id: Hash, envelope: TicketEnvelope): Unit =
    tickets.putIfAbsent(id, envelope)

  /** Tickets for inclusion: ascending ticket-id order (extrinsic
    * requirement), excluding ids already accumulated, at most `max`.
    */
  def takeTickets(exclude: Set[JamBytes], max: Int): List[(Hash, TicketEnvelope)] =
    tickets.entrySet.asScala.toList
      .filterNot(e => exclude.contains(JamBytes(e.getKey.bytes.toArray)))
      .sortBy(e => e.getKey.toHex)
      .take(max)
      .map(e => (e.getKey, e.getValue))

  def removeTickets(ids: Iterable[Hash]): Unit =
    ids.foreach(tickets.remove)

  def ticketCount: Int = tickets.size

  /** Guarantees for inclusion: sorted by core index (extrinsic ordering
    * requirement), at most one per core.
    */
  def takeGuarantees(): List[GuaranteeExtrinsic] =
    guarantees.values.asScala.toList
      .groupBy(_.report.coreIndex.toInt)
      .values
      .map(_.head)
      .toList
      .sortBy(_.report.coreIndex.toInt)

  /** Assurances anchored at `parent`, sorted by validator index. */
  def takeAssurances(parent: Hash): List[AssuranceExtrinsic] =
    assurances.values.asScala.toList
      .filter(_.anchor == parent)
      .sortBy(_.validatorIndex.value.toInt)

  def takePreimages(): List[Preimage] =
    preimages.values.asScala.toList
      .sortBy(p => (p.requester.value.toLong, Hashing.blake2b256(p.blob.toArray).toHex))

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
      assurances.remove((a.validatorIndex.value.toInt, a.anchor))
    }
    includedPreimages.foreach { p =>
      preimages.remove((p.requester.value.toLong, Hashing.blake2b256(p.blob.toArray)))
    }
    // Assurances only ever anchor at the parent of the next block: drop stale.
    assurances.keySet.asScala.filterNot(_._2 == newBestHash).toList.foreach(assurances.remove)

  def clear(): Unit =
    guarantees.clear()
    assurances.clear()
    preimages.clear()

  def guaranteeCount: Int = guarantees.size
  def assuranceCount: Int = assurances.size
