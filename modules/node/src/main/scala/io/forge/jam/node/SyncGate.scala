package io.forge.jam.node

final class SyncGate(lagSlots: Long, patienceMillis: Long):

  private var holdingSince = -1L
  private var lastBest = -1L
  private var progressAt = 0L

  def holding(ourBest: Long, peerBests: Iterable[Long], nowMillis: Long): Boolean = synchronized {
    if !SyncGate.behind(ourBest, peerBests, lagSlots) then
      holdingSince = -1L
      false
    else
      if holdingSince < 0 then
        holdingSince = nowMillis
        lastBest = ourBest
        progressAt = nowMillis
      else if ourBest > lastBest then
        lastBest = ourBest
        progressAt = nowMillis
      nowMillis - progressAt < patienceMillis
  }

object SyncGate:

  def behind(ourBest: Long, peerBests: Iterable[Long], lagSlots: Long): Boolean =
    val sorted = peerBests.toArray.sorted
    sorted.nonEmpty && sorted((sorted.length - 1) / 2) > ourBest + lagSlots
