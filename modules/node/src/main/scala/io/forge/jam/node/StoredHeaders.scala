package io.forge.jam.node

import io.forge.jam.core.LittleEndian
import io.forge.jam.core.primitives.Hash
import io.forge.jam.db.BlockStore

private[node] final class StoredHeaders(store: BlockStore):

  private val parentReads = new java.util.concurrent.atomic.LongAdder

  def parentLookups: Long = parentReads.sum()

  /** Parent of a stored block. */
  def parentOf(h: Hash): Option[Hash] =
    parentReads.increment()
    store.getHeader(h).filter(_.length >= 32).map(b => Hash(b.take(32)))

  def slotOf(h: Hash): Option[Long] =
    store.getHeader(h).filter(_.length >= 100).map(b => LittleEndian.get(b, 96, 4))

  def slotOrZero(h: Hash): Long = slotOf(h).getOrElse(0L)
