package io.forge.jam.node

import io.forge.jam.core.primitives.Hash

import scala.jdk.CollectionConverters.*

private[node] final class FetchQueue[P <: AnyRef](
    maxInFlight: Int,
    maxPerPeer: Int,
    maxPending: Int,
    maxAlternates: Int,
    isOpen: P => Boolean,
    samePeer: (P, P) => Boolean = (a: P, b: P) => a eq b,
    order: List[FetchQueue.Request[P]] => List[FetchQueue.Request[P]] = (rs: List[FetchQueue.Request[P]]) => rs,
    preferred: P => Boolean = (_: P) => false,
    keptForPreferred: Int = 0
):
  import FetchQueue.Request

  private val pending = new java.util.ArrayDeque[Request[P]]()
  private val inFlight = new java.util.ArrayList[Request[P]]()
  /** Other peers that announced a block already waiting or being fetched, tried in turn if that fetch does not
    * bring the block. Entries exist only while a fetch for the block waits or runs.
    */
  private val alternates = new java.util.HashMap[Hash, java.util.ArrayDeque[Request[P]]]()

  def size: Int = synchronized(pending.size)

  private def waitingFor(from: Hash): Boolean = pending.stream().anyMatch(_.from == from)
  private def flyingFor(from: Hash): Boolean = inFlight.stream().anyMatch(_.from == from)

  // A peer can announce faster than we fetch (each fetch waits on it for up to 30 s), so the waiting fetches
  // are bounded: one per (peer, block), a few per peer, and when over a limit the oldest gives way. A fetch
  // walks back from its block, so the newest one covers what an older one for the same chain would have brought.
  def offer(request: Request[P]): List[Request[P]] = synchronized {
    val from = request.from
    val peer = request.peer
    val it = pending.iterator()
    var samePeer = 0
    var duplicate = false
    // The same block wanted from another peer: one fetch brings it, so this peer is only kept as a fallback.
    var elsewhere = flyingFor(from)
    while it.hasNext do
      val p = it.next()
      if p.from == from then
        if p.peer eq peer then duplicate = true else elsewhere = true
      if p.peer eq peer then samePeer += 1
    if duplicate then List(request)
    else if elsewhere then
      val others = alternates.computeIfAbsent(from, _ => new java.util.ArrayDeque[Request[P]]())
      if others.size < maxAlternates && !others.stream().anyMatch(_.peer eq peer) then
        others.addLast(request)
        Nil
      else List(request)
    else
      pending.addLast(request)
      val out = List.newBuilder[Request[P]]
      if samePeer + 1 > maxPerPeer then
        val oldest = pending.iterator()
        var removed = false
        while !removed && oldest.hasNext do
          val p = oldest.next()
          if p.peer eq peer then
            oldest.remove()
            out += p
            removed = true
      if pending.size > maxPending then out += pending.pollFirst()
      // A fetch that gives way takes its fallbacks with it, unless another fetch for the block still waits.
      out.result().flatMap { p =>
        val stillWanted = waitingFor(p.from) || flyingFor(p.from)
        val others = if stillWanted then null else alternates.remove(p.from)
        if others == null then List(p) else p :: others.asScala.toList
      }
  }

  def next(): Option[Request[P]] = synchronized {
    if inFlight.size >= maxInFlight then None
    else
      val free = pending.asScala.iterator.filter(p => !inFlight.stream().anyMatch(f => samePeer(f.peer, p.peer))).toList
      val ordered = order(free)
      val othersOut = inFlight.asScala.count(f => !preferred(f.peer))
      val next =
        ordered.find(r => preferred(r.peer))
          .orElse(if othersOut < maxInFlight - keptForPreferred then ordered.headOption else None)
      next.map { found =>
        found.dequeuedAt = System.nanoTime()
        pending.removeFirstOccurrence(found)
        inFlight.add(found)
        found
      }
  }

  def finished(request: Request[P], have: Boolean, answered: Boolean = true): List[Request[P]] = synchronized {
    inFlight.remove(request)
    val released = List.newBuilder[Request[P]]
    val others = alternates.get(request.from)
    if others != null then
      if have then
        alternates.remove(request.from)
        released ++= others.asScala
      else if !waitingFor(request.from) then released ++= askNextPeer(request.from, first = true)
    if !answered then
      val it = pending.iterator()
      val dropped = List.newBuilder[Request[P]]
      while it.hasNext do
        val p = it.next()
        if p.peer eq request.peer then
          it.remove()
          dropped += p
      dropped.result().foreach { p =>
        released += p
        if !waitingFor(p.from) && !flyingFor(p.from) then released ++= askNextPeer(p.from, first = false)
      }
    released.result()
  }

  private def askNextPeer(from: Hash, first: Boolean): List[Request[P]] =
    val others = alternates.get(from)
    if others == null then Nil
    else
      // The fetch did not bring the block: ask the next peer that announced it, ahead of other fetches.
      var fallback = others.pollFirst()
      val skipped = List.newBuilder[Request[P]]
      while fallback != null && !isOpen(fallback.peer) do
        skipped += fallback
        fallback = others.pollFirst()
      if fallback == null || others.isEmpty then alternates.remove(from)
      if fallback != null then
        fallback.queuedAt = System.nanoTime()
        if first then pending.addFirst(fallback) else pending.addLast(fallback)
      skipped.result()

private[node] object FetchQueue:
  final class Request[P](
      val peer: P,
      val from: Hash,
      val maxBlocks: Int,
      val onDone: () => Unit,
      val ascending: Boolean = false,
      val announced: Boolean = false
  ):
    @volatile var announcedAt: Long = 0L
    @volatile var queuedAt: Long = 0L
    @volatile var dequeuedAt: Long = 0L
