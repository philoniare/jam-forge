package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block

private[node] final class ForkChoice(config: ChainConfig, meta: ChainMeta, headers: StoredHeaders)
    extends LazyLogging:

  // Fork-choice metadata, derived from headers alone so unvalidated side
  // branches can be weighed:
  //   ticketed:<h>  block h's epoch is sealed by tickets (γ'_s ∈ tickets)
  //   marked:<h>    h's chain carries a winning-tickets marker in h's epoch
  //   weight:<h>    number of ticket-sealed blocks among h and its ancestors
  // γ'_s takes the ticket sequence exactly when e' = e + 1 and the previous
  // epoch closed with a saturated accumulator, which is when that epoch's
  // chain carried H_w (§safrole eqs. slotkeysequence, winningticketsmarker).

  def weightOf(h: Hash): Long = meta.weight(h)

  def preferred(weight: Long, slot: Long, bestWeight: Long, bestSlot: Long): Boolean =
    weight > bestWeight || (weight == bestWeight && slot > bestSlot)

  /** A block whose seal was not checked may claim anything in its header, so it is credited with nothing of its
    * own: no ticket-sealed weight, no winning-tickets marker of its own and no slot author. It still passes on what
    * its parent established, and is weighed again when a replay validates it.
    */
  def record(hash: Hash, block: Block, sealChecked: Boolean): Option[Long] =
    val parent = block.header.parent
    val slot = block.header.slot.toLong
    val epoch = slot / config.epochLength
    val parentEpoch = headers.slotOrZero(parent) / config.epochLength
    val ticketed =
      if epoch == parentEpoch then meta.ticketed(parent)
      else epoch == parentEpoch + 1 && meta.marked(parent)
    val marked =
      (sealChecked && block.header.ticketsMark.isDefined) || (epoch == parentEpoch && meta.marked(parent))
    val credit = if ticketed && sealChecked then 1L else 0L
    meta.putForkChoice(hash, ticketed, marked, meta.weight(parent) + credit)
    if sealChecked then recordSlotAuthor(hash, slot, block.header.authorIndex.toInt) else None

  def recordGenesis(genesisHash: Hash, ticketed: Boolean): Unit =
    meta.putForkChoice(genesisHash, ticketed, marked = false, weight = 0L)

  /** Equivocation tracking: two distinct blocks at one timeslot by one author. A branch holding an unfinalized
    * equivocated slot is never adopted, and the best chain leaves one as soon as it is seen ([[importBlock]]).
    */
  private def recordSlotAuthor(hash: Hash, slot: Long, author: Int): Option[Long] =
    meta.slotAuthor(slot, author) match
      case None =>
        meta.putSlotAuthor(slot, author, hash)
        None
      case Some(seen) if seen != hash =>
        val first = !meta.equivocated(slot)
        if first then
          logger.warn(
            s"equivocation at slot $slot by validator $author: ${seen.toHex.take(18)} vs ${hash.toHex.take(18)}"
          )
        meta.markEquivocated(slot)
        Option.when(first)(slot)
      case _ => None

  def isEquivocated(h: Hash): Boolean = meta.equivocated(headers.slotOrZero(h))

  // A block that failed validation, and so every block built on it, is never adopted.
  def branchInvalid(tip: Hash): Boolean =
    var cursor = tip
    while meta.blockRoot(cursor).isEmpty do
      if meta.invalid(cursor) then return true
      headers.parentOf(cursor) match
        case Some(p) => cursor = p
        case None    => return false
    false

  /** Whether the unvalidated branch ending at `tip` (back to its first validated ancestor) holds a block at an
    * equivocated slot.
    */
  def branchEquivocates(tip: Hash): Boolean =
    var cursor = tip
    while meta.blockRoot(cursor).isEmpty do
      if isEquivocated(cursor) then return true
      headers.parentOf(cursor) match
        case Some(p) => cursor = p
        case None    => return false
    false
