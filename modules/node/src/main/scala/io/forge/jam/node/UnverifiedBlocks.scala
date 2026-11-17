package io.forge.jam.node

import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash}

import scala.collection.mutable

private[node] final class UnverifiedBlocks(meta: ChainMeta, maxBlocks: Int, maxPerPeer: Int, maxBytes: Long):
  import UnverifiedBlocks.Entry

  private val blocks = mutable.LinkedHashMap.empty[Hash, Entry]
  private val perPeer = mutable.HashMap.empty[Ed25519PublicKey, Int]
  private var bytes = 0L

  def size: Int = blocks.size
  def nonEmpty: Boolean = blocks.nonEmpty
  def contains(hash: Hash): Boolean = blocks.contains(hash)

  def admits(size: Int, source: Option[Ed25519PublicKey]): Either[ChainError, Unit] =
    if blocks.size >= maxBlocks || bytes + size > maxBytes then Left(ChainError.UnverifiedLimit("node"))
    else if source.exists(p => perPeer.getOrElse(p, 0) >= maxPerPeer) then Left(ChainError.UnverifiedLimit("peer"))
    else Right(())

  def track(hash: Hash, slot: Long, parent: Hash, size: Long, peer: Option[Ed25519PublicKey]): Unit =
    if !blocks.contains(hash) then
      blocks(hash) = Entry(slot, parent, size, peer)
      bytes += size
      peer.foreach(p => perPeer(p) = perPeer.getOrElse(p, 0) + 1)
      // Persisted (with the size) so a restart can count and prune it again.
      meta.putUnverified(hash, size)

  def clear(): Unit =
    blocks.clear()
    perPeer.clear()
    bytes = 0L

  def untrack(hash: Hash): Unit =
    blocks.remove(hash).foreach { u =>
      bytes -= u.size
      u.peer.foreach { p =>
        val left = perPeer.getOrElse(p, 1) - 1
        if left <= 0 then perPeer.remove(p) else perPeer(p) = left
      }
      meta.dropUnverified(hash)
    }

  def doomedAt(finalizedSlot: Long): mutable.LinkedHashMap[Hash, Entry] =
    val doomed = mutable.LinkedHashMap.empty[Hash, Entry]
    // Insertion order has parents before children, so one pass closes over descendants.
    blocks.foreach { (h, u) =>
      if u.slot <= finalizedSlot || doomed.contains(u.parent) then doomed(h) = u
    }
    doomed

private[node] object UnverifiedBlocks:
  final case class Entry(slot: Long, parent: Hash, size: Long, peer: Option[Ed25519PublicKey])
