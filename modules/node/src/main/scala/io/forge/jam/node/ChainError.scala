package io.forge.jam.node

import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.importing.ImportError

/** Why [[ChainManager]] refused an import, finalization, reorg or disregard. */
enum ChainError:
  case Undecodable(detail: String)
  case AlreadyImported
  case PreviouslyInvalid
  case Disregarded
  case BodyMismatch
  case UnknownParent(parent: Hash)
  case StoredOnSideBranch(weight: Long, slot: Long, bestWeight: Long, bestSlot: Long)
  /** The block was checked and is invalid. */
  case InvalidBlock(error: ImportError, detail: String)
  /** The import failed on our side: not a verdict on the block, which may import on a later try. */
  case ImportFailed(detail: String)
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
  case CannotCheck(detail: String)
  case BelowPruneFloor(forkHeight: Long, floorHeight: Long)

  def message: String = this match
    case BelowPruneFloor(fork, floor) =>
      s"the branch forks at height $fork, below the oldest state kept (height $floor)"
    case CannotCheck(detail)       => s"cannot check the block: $detail"
    case SideBranchRefused(detail) => s"side-branch block refused: $detail"
    case UnverifiedLimit(scope)    => s"too many unverified side-branch blocks ($scope limit)"
    case FutureSlot(slot, now)     => s"block of slot $slot is ahead of the current slot $now"
    case Undecodable(detail)  => detail
    case AlreadyImported      => "block already imported"
    case PreviouslyInvalid    => "block failed validation before"
    case Disregarded          => "block was disregarded"
    case BodyMismatch         => "block body does not match its header"
    case UnknownParent(p)     => s"unknown parent ${p.toHex.take(18)}"
    case StoredOnSideBranch(w, s, bw, bs) =>
      s"stored on side branch (weight $w slot $s vs best $bw/$bs)"
    case InvalidBlock(error, detail) => s"$error: $detail"
    case ImportFailed(detail)        => s"import failed: $detail"
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
