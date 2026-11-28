package io.forge.jam.node

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.StateTrie
import io.forge.jam.core.types.block.Block
import io.forge.jam.protocol.report.ReportTypes.AncestorHeader
import io.forge.jam.protocol.state.TrieBackedJamState

final case class ChainHead(hash: Hash, slot: Long, stateRoot: Hash)

trait ChainReader:
  def config: ChainConfig

  def best: ChainHead
  def finalized: ChainHead

  def nextBlockSlot: Long

  def hasBlock(hash: Hash): Boolean

  /** Decoded stored block. */
  def block(h: Hash): Option[Block]

  /** Slot of a stored block, from its header. */
  def slotOf(h: Hash): Option[Long]

  def stateView(): TrieBackedJamState

  def stateViewOf(head: ChainHead): TrieBackedJamState

  private[node] def executionStateOf(head: ChainHead): Option[io.forge.jam.protocol.importing.BlockImporter.OpenState] = None

  /** As [[stateViewOf]], by the head's state root. */
  def stateViewAtRoot(root: Hash): TrieBackedJamState

  /** A read view over the posterior state of stored block `headerHash`. */
  def stateViewAt(headerHash: Hash): Option[TrieBackedJamState]

  def stateAtRoot(root: Hash): Option[StateTrie]

  /** Raw read of a 31-byte state key from the current best state. */
  def readRawState(stateKey: JamBytes): Option[JamBytes]

  private[node] def ancestorsOf(parent: Hash, parentPostRoot: Hash, slot: Long): List[AncestorHeader]
