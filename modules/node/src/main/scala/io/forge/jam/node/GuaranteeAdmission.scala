package io.forge.jam.node

import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.protocol.report.ReportTransition
import io.forge.jam.protocol.safrole.SafroleTransition
import io.forge.jam.protocol.safrole.SafroleTypes.SafroleInput

import scala.util.control.NonFatal

/** The receipt-time check of a guarantee distributed over CE 135, before it may enter the pool.
  *
  * A guarantee is checked as the Reports STF would check it in the earliest block that can still carry it: a block on
  * the best head at slot `s = max(best slot + 1, guarantee slot)`. The validator sets, entropy and offenders are those
  * of the best state, taken through Safrole's epoch step when `s` opens the next epoch, and the keys and core
  * assignments are resolved by [[ReportTransition.verifyGuaranteeCredentials]], the STF's own rule. So what is
  * verified here is: the slot window (not ahead of `now + SlotTolerance`, rotation not older than the one before
  * `s`'s, lookup anchor within the maximum lookup-anchor age), the core index, two or three ordered guarantors that
  * are assigned to the core and are not offenders, and their signatures.
  *
  * Not verified at receipt, because the block that will carry the guarantee is not known yet: that the anchor and
  * lookup anchor are on that block's chain, the authorizer is in the core's pool, the core is free, the dependencies
  * are met and the work results match the services. Those stay with block authoring (which drops guarantees that
  * aged out) and block import; the pool's caps bound what a real guarantor can add with reports that fail them.
  *
  * A guarantee whose slot is in an epoch after the next one of the best state is refused: the keys of that epoch are
  * not determined yet. That only happens to a node more than an epoch behind, which does not author.
  */
final class GuaranteeAdmission(chain: ChainManager, now: () => Long):
  import GuaranteeAdmission.*

  def check(g: GuaranteeExtrinsic): Either[String, Unit] =
    val config = chain.config
    val best = chain.best
    val slot = g.slot.toLong
    val inclusionSlot = math.max(best.slot + 1, slot)
    val epochsAhead = inclusionSlot / config.epochLength - best.slot / config.epochLength
    if slot > math.max(now(), best.slot) + SlotTolerance then Left(s"slot $slot is ahead of the current slot")
    else if inclusionSlot - g.report.context.lookupAnchorSlot.toLong > config.maxLookupAnchorAge then
      Left("lookup anchor too old")
    else if epochsAhead > 1 then Left(s"slot $slot is more than an epoch ahead of the best block")
    else
      try
        val view = chain.stateView()
        val rotated =
          if epochsAhead == 0 then Right(())
          else
            SafroleTransition
              .epochStfView(SafroleInput(inclusionSlot, Hash(new Array[Byte](32)), Nil), view)
              .left
              .map(e => s"safrole: $e")
        rotated.flatMap { _ =>
          ReportTransition
            .verifyGuaranteeCredentials(
              g,
              inclusionSlot,
              view.validators.current,
              view.validators.previous,
              view.entropy.pool,
              view.psi.offenders.map(k => Hash(k.bytes)).toSet,
              config
            )
            .left
            .map(_.toString)
        }
      catch case NonFatal(e) => Left(s"cannot check: ${e.getMessage}")

object GuaranteeAdmission:
  /** Slots a guarantee may be ahead of this node's clock: one, for clock skew between guarantor and receiver. */
  val SlotTolerance: Long = 1L
