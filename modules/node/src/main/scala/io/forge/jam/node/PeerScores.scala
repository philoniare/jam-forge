package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes

enum Offence(val points: Int):
  case InvalidBlock extends Offence(60)
  case BadProof extends Offence(60)
  case BadSignature extends Offence(60)
  case Malformed extends Offence(20)
  case Unanswered extends Offence(5)

final class PeerScores(
    isValidator: Array[Byte] => Boolean = _ => false,
    onBan: Array[Byte] => Unit = _ => (),
    clock: () => Long = () => System.currentTimeMillis(),
    maxPeers: Int = PeerScores.MaxPeers
) extends LazyLogging:
  import PeerScores.*

  private final class Entry(var points: Double, var at: Long, var bannedUntil: Long)
  private val entries = new java.util.LinkedHashMap[JamBytes, Entry](64, 0.75f, true)

  private def decayed(e: Entry, now: Long): Double =
    e.points * math.pow(0.5, math.max(0L, now - e.at).toDouble / HalfLifeMillis)

  def report(key: Array[Byte], offence: Offence): Unit =
    val banned = synchronized {
      val now = clock()
      val k = JamBytes(key)
      val e = entries.computeIfAbsent(k, _ => new Entry(0, now, 0L))
      e.points = decayed(e, now) + offence.points
      e.at = now
      if entries.size > maxPeers then
        val it = entries.entrySet.iterator
        var dropped = false
        while !dropped && it.hasNext do
          val old = it.next()
          if old.getValue.bannedUntil <= now && old.getKey != k then
            it.remove()
            dropped = true
      if e.points >= BanAt && e.bannedUntil <= now && !isValidator(key) then
        e.bannedUntil = now + BanMillis
        e.points = 0 // the ban is the answer to these; it starts clean afterwards
        true
      else false
    }
    logger.debug(s"peer ${JamNode.hex(key).take(16)}: $offence")
    if banned then
      logger.warn(s"peer ${JamNode.hex(key).take(16)} banned for ${BanMillis / 60000} min after: $offence")
      onBan(key)

  def banned(key: Array[Byte]): Boolean = synchronized {
    Option(entries.get(JamBytes(key))).exists(_.bannedUntil > clock())
  }

  def points(key: Array[Byte]): Double = synchronized {
    Option(entries.get(JamBytes(key))).fold(0.0)(decayed(_, clock()))
  }

  def leastFaultedFirst[P](peers: List[P])(key: P => Array[Byte]): List[P] =
    if synchronized(entries.isEmpty) then peers // the usual case: nobody has been charged
    else peers.sortBy(p => math.round(points(key(p)) / Offence.Unanswered.points))

object PeerScores:
  val BanAt: Int = 100
  val BanMillis: Long = 10 * 60 * 1000L
  val HalfLifeMillis: Long = 10 * 60 * 1000L
  val MaxPeers: Int = 4096
