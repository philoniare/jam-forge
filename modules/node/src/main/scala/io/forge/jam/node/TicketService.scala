package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.tickets.TicketEnvelope
import io.forge.jam.crypto.BandersnatchVrf
import spire.math.UByte

import java.util.concurrent.ConcurrentHashMap

/** Safrole ticket generation: once per epoch, for every held validator key present in the
  * pending set γ_k, ring-VRF proofs over `jam_ticket_seal ++ η'_2 ++ attempt`
  * are generated for each permitted entry index. Tickets go to the local
  * extrinsic pool for inclusion while the submission window is open, and the
  * (id → key, attempt) mapping is retained for ticketed sealing in the next
  * epoch.
  *
  * CE 131/132 proxy distribution is a follow-up; on a devnet where authors
  * generate their own tickets, local pooling suffices.
  */
final class TicketService(
    chain: ChainManager,
    pools: ExtrinsicPools,
    validatorKeys: Seq[ValidatorKeySet]
) extends LazyLogging:

  /** Our tickets: id bytes → (keys, attempt), consulted when sealing a
    * ticketed slot.
    */
  val ownTickets = new ConcurrentHashMap[Seq[Byte], (ValidatorKeySet, UByte)]()

  /** Epoch each ticket id was generated in, so stale entries can be pruned
    * once their ticketed-sealing window has passed.
    */
  private val ticketEpoch = new ConcurrentHashMap[Seq[Byte], Long]()

  private val bsByPublic: Map[Seq[Byte], ValidatorKeySet] =
    validatorKeys.map(k => k.bandersnatchPublic.toSeq -> k).toMap

  @volatile private var generatedForEpoch: Long = -1L

  /** Generate this epoch's tickets if not already done. */
  def maybeGenerate(): Unit =
    val view = chain.stateView()
    val epoch = view.timeslot / chain.config.epochLength
    if epoch == generatedForEpoch then return
    synchronized {
      if epoch == generatedForEpoch then return
      generatedForEpoch = epoch
      pruneStale(epoch)
      generate(view)
    }

  /** Drop tickets generated more than one epoch ago: a ticket from epoch E is
    * consulted for ticketed sealing in epoch E+1, so it is safe to forget by
    * the time epoch E+2 starts
    */
  private def pruneStale(currentEpoch: Long): Unit =
    val it = ticketEpoch.entrySet().iterator()
    while it.hasNext do
      val entry = it.next()
      if entry.getValue < currentEpoch - 1 then
        ownTickets.remove(entry.getKey)
        it.remove()

  private def generate(view: io.forge.jam.protocol.state.TrieBackedJamState): Unit =
    val pending = view.validators.nextEpoch // γ_k: the ring for next epoch's contest
    val commitment = view.gamma.z // γ_z: ring root the STF verifies against
    val entropy = // η_2: ticket-contest entropy
      if view.entropy.pool.length > 2 then view.entropy.pool(2)
      else Hash(new Array[Byte](32))
    val ringKeys = pending.map(_.bandersnatch)
    val ringSize = ringKeys.size

    val epoch = view.timeslot / chain.config.epochLength
    var produced = 0
    pending.zipWithIndex.foreach { case (vk, index) =>
      bsByPublic.get(vk.bandersnatch.bytes.toArray.toSeq).foreach { keys =>
        for attemptInt <- 0 until chain.config.ticketsPerValidator do
          val attempt = UByte(attemptInt)
          BandersnatchVrf.createRingProof(
            keys.bandersnatchSecret,
            ringKeys,
            index,
            entropy,
            attempt
          ) match
            case None =>
              logger.warn(s"ring proof failed for validator index $index attempt $attemptInt")
            case Some(proof) =>
              // Verify our own proof to learn the ticket id (the VRF output).
              BandersnatchVrf.verifyRingProof(
                proof,
                commitment,
                entropy,
                attempt,
                ringSize
              ) match
                case None =>
                  logger.warn(s"self-verification failed for index $index attempt $attemptInt")
                case Some(result) =>
                  val id = Hash(result.ticketId.toArray)
                  pools.addTicket(id, TicketEnvelope(attempt, proof))
                  val idSeq = result.ticketId.toArray.toSeq
                  ownTickets.put(idSeq, (keys, attempt))
                  ticketEpoch.put(idSeq, epoch)
                  produced += 1
      }
    }
    if produced > 0 then
      logger.info(s"generated $produced Safrole ticket(s) for epoch $epoch")
