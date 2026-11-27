package io.forge.jam.network

import java.util.concurrent.ConcurrentHashMap

/** Keeps the connections this node dialed from idling out. A peer closes a connection that has been silent for its
  * idle timeout (15 s for polkajam), and a validator must stay connected to validators it may exchange nothing
  * with for long: those that are not its grid neighbours, while no work is reported. QUIC's own answer is a PING
  * frame, which the transport gives no way to send, so a silent connection is given traffic by `ping`: a request
  * any peer answers.
  *
  * Only dialed connections are kept alive, by the usual convention: the peer does the same for those it dialed,
  * and its traffic keeps them from being silent here.
  *
  * @param ping gives the connection traffic; called from [[tick]]'s thread, so it must not wait for the answer
  */
final class KeepAlive(ping: PeerConnection => Unit, now: () => Long = () => System.currentTimeMillis()):

  /** A connection's traffic count when it last moved (or when traffic was last asked for), and when that was. */
  private final case class Seen(count: Long, since: Long)

  private val seen = new ConcurrentHashMap[PeerConnection, Seen]()

  /** Connections whose traffic is being followed. */
  def watched: Int = seen.size

  /** Look at each connection's traffic, and give those silent for [[KeepAlive.SilentShare]] of their idle timeout
    * some. To be called a few times per second with the connections that are open.
    */
  def tick(connections: Iterable[PeerConnection]): Unit =
    val at = now()
    val kept = connections.filter(c => c.initiator && c.isOpen && c.idleTimeoutMillis > 0).toSet
    seen.keySet.removeIf(c => !kept.contains(c))
    kept.foreach { c =>
      val count = c.trafficCount
      val last = seen.get(c)
      if last == null || last.count != count then seen.put(c, Seen(count, at))
      else if at - last.since >= (c.idleTimeoutMillis * KeepAlive.SilentShare).toLong then
        seen.put(c, Seen(count, at))
        try ping(c)
        catch case _: Exception => () // the connection is closing: nothing to keep
    }

object KeepAlive:

  /** The share of its idle timeout a connection may be silent before it is given traffic: early enough that a
    * second try still comes before the timeout when the first is lost.
    */
  val SilentShare: Double = 0.4
