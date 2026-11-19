package io.forge.jam.node

import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.report.ReportTypes.Ancestry

private[node] final class AncestorWindow(maxAge: Long, link: Hash => Option[HeaderLink]):
  import AncestorWindow.Entry

  private val entries = new java.util.HashMap[Hash, Entry]()
  private val order = new java.util.ArrayDeque[Hash]()
  private var tip: Hash | Null = null
  private var walks = 0L

  def rebuilds: Long = walks

  def size: Int = entries.size

  def clear(): Unit =
    entries.clear()
    order.clear()
    tip = null

  def at(parent: Hash, parentPostRoot: Hash, slot: Long): Ancestry =
    if tip != parent then rebuild(parent)
    val window = entries
    new Ancestry:
      private def inAge(e: Entry): Boolean = slot - e.slot <= maxAge
      def nonEmpty: Boolean =
        val e = window.get(parent)
        e != null && inAge(e)
      def containsLookupAnchor(hash: Hash, anchorSlot: Long, postStateRoot: Hash): Boolean =
        val e = window.get(hash)
        e != null && e.slot == anchorSlot && inAge(e) && {
          if hash == parent then postStateRoot == parentPostRoot else postStateRoot == e.postRoot
        }

  def extend(hash: Hash, slot: Long, parent: Hash, priorStateRoot: Hash): Unit =
    if tip != parent then clear()
    else
      entries.get(parent).postRoot = priorStateRoot
      entries.put(hash, new Entry(slot, null))
      order.addLast(hash)
      tip = hash
      while !order.isEmpty && slot - entries.get(order.peekFirst()).slot > maxAge do entries.remove(order.pollFirst())

  private def rebuild(parent: Hash): Unit =
    clear()
    walks += 1
    var cursor = parent
    var childPriorRoot: Hash | Null = null
    var tipSlot = Long.MinValue
    var more = true
    while more do
      link(cursor) match
        case Some(l) =>
          if tipSlot == Long.MinValue then tipSlot = l.slot
          if tipSlot - l.slot > maxAge then more = false
          else
            entries.put(cursor, new Entry(l.slot, childPriorRoot))
            order.addFirst(cursor)
            childPriorRoot = l.priorStateRoot
            if l.parent == Hash.zero then more = false else cursor = l.parent
        case None => more = false
    tip = if entries.containsKey(parent) then parent else null

private[node] object AncestorWindow:
  private final class Entry(val slot: Long, var postRoot: Hash | Null)
