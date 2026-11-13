package io.forge.jam.node

import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.traces.ImportError

/** Why [[ChainManager]] refused an import, finalization, reorg or disregard. */
enum ChainError:
  case Undecodable(detail: String)
  case AlreadyImported
  case PreviouslyInvalid
  case UnknownParent(parent: Hash)
  case StoredOnSideBranch(weight: Long, height: Long, bestWeight: Long, bestHeight: Long)
  case ImportFailed(error: ImportError, detail: String)
  case UnknownBlock
  case NotOnMainChain
  case BelowFinalized
  case NotValidated
  case BranchBlockMissing
  case RevertsFinalized
  case ReplayFailed(cause: ChainError)
  case NotOnBestChain
  case NoParent
  case AlreadyFinalized(parent: Hash)
  /** A side-branch block failed a check that needs no execution (slot order, clock, seal); it was not stored. */
  case SideBranchRefused(detail: String)
  /** A side-branch block whose seal cannot be checked yet, refused because the `scope` (node or peer) cap is full. */
  case UnverifiedLimit(scope: String)
  /** A block for a slot that has not started by our clock (such a block is not yet valid). Not a verdict on the
    * block: it is not stored or marked, and imports once its slot comes (a peer announces it again, or a child of
    * it brings it back as a missing ancestor).
    */
  case FutureSlot(slot: Long, now: Long)

  def message: String = this match
    case SideBranchRefused(detail) => s"side-branch block refused: $detail"
    case UnverifiedLimit(scope)    => s"too many unverified side-branch blocks ($scope limit)"
    case FutureSlot(slot, now)     => s"block of slot $slot is ahead of the current slot $now"
    case Undecodable(detail)  => detail
    case AlreadyImported      => "block already imported"
    case PreviouslyInvalid    => "block failed validation before"
    case UnknownParent(p)     => s"unknown parent ${p.toHex.take(18)}"
    case StoredOnSideBranch(w, h, bw, bh) =>
      s"stored on side branch (weight $w height $h vs best $bw/$bh)"
    case ImportFailed(error, detail) => s"$error: $detail"
    case UnknownBlock         => "unknown block"
    case NotOnMainChain       => "cannot finalize a non-main-chain block"
    case BelowFinalized       => "cannot finalize below the finalized head"
    case NotValidated         => "block not on a validated chain"
    case BranchBlockMissing   => "reorg: branch block missing"
    case RevertsFinalized     => "reorg would revert finalized blocks"
    case ReplayFailed(cause)  => s"reorg replay failed: ${cause.message}"
    case NotOnBestChain       => "not on the best chain"
    case NoParent             => "no parent"
    case AlreadyFinalized(p)  => s"already finalized; parent ${p.toHex.take(18)}"

  override def toString: String = message
