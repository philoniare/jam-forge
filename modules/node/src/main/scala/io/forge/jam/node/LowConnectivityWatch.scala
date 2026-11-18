package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging

final case class ValidatorConnectivity(connected: Int, others: Int, low: Boolean)

final class LowConnectivityWatch(graceMillis: Long) extends LazyLogging:
  import LowConnectivityWatch.*

  private var belowSince: Long = -1L
  private var warned = false
  @volatile private var last: Option[ValidatorConnectivity] = None

  def current: Option[ValidatorConnectivity] = last

  def observe(connected: Int, others: Int, now: Long): Option[Change] = synchronized {
    val isBelow = below(connected, others)
    val change =
      if !isBelow then
        val restored = Option.when(warned)(Change.Restored(connected, others))
        belowSince = -1L
        warned = false
        restored
      else
        if belowSince < 0 then belowSince = now
        Option.when(!warned && now - belowSince >= graceMillis) {
          warned = true
          Change.Low(connected, others, now - belowSince)
        }
    last = Some(ValidatorConnectivity(connected, others, low = warned))
    change.foreach {
      case Change.Low(c, o, forMillis) =>
        logger.warn(
          s"low validator connectivity: connected to $c of the $o other current validators for ${forMillis / 1000} s " +
            s"(at least ${needed(o)} are needed, 2/3): guarantees, assurances, audits and finality votes may not " +
            "reach enough of them. Check that this node's UDP port is reachable at the endpoint in its validator " +
            "metadata (--external-ip, firewall, NAT) and that it can reach the others' endpoints"
        )
      case Change.Restored(c, o) =>
        logger.info(s"validator connectivity restored: connected to $c of the $o other current validators")
    }
    change
  }

  def clear(): Unit = synchronized {
    belowSince = -1L
    warned = false
    last = None
  }

object LowConnectivityWatch:

  enum Change:
    case Low(connected: Int, others: Int, forMillis: Long)
    case Restored(connected: Int, others: Int)

  def below(connected: Int, others: Int): Boolean = 3L * connected < 2L * others

  def needed(others: Int): Int = ((2L * others + 2) / 3).toInt

  val DefaultGraceMillis: Long = 60_000L
