package io.forge.jam.network

import java.util.concurrent.{CompletableFuture, ConcurrentLinkedQueue, Executor, Executors}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import io.forge.jam.core.primitives.Ed25519PublicKey

/** Two [[PeerConnection]]s joined in memory, for exercising protocol services without QUIC. What one side sends the
  * other receives, in order, on the pair's delivery thread (as a network event loop would deliver it): messages,
  * then the FIN or reset. Frame limits, flow control and idle timeouts are not modelled.
  */
object Loopback:

  /** A connected pair: `a` dialed `b`. Streams one side opens are handed to the other side's handler for the kind;
    * a kind with no handler is reset, as an unknown kind is on the wire.
    *
    * @param deliver
    *   runs every delivery, in order; by default one daemon thread for the pair
    */
  def pair(
      aKey: Array[Byte],
      bKey: Array[Byte],
      aHandlers: Byte => Option[PeerStreamHandler],
      bHandlers: Byte => Option[PeerStreamHandler],
      deliver: Executor = newDeliveryThread()
  ): (Connection, Connection) =
    val a = new Connection(peerKey = bKey, initiator = true, deliver)
    val b = new Connection(peerKey = aKey, initiator = false, deliver)
    a.link(b, bHandlers)
    b.link(a, aHandlers)
    (a, b)

  private def newDeliveryThread(): Executor =
    Executors.newSingleThreadExecutor { r =>
      val t = new Thread(r, "loopback")
      t.setDaemon(true)
      t
    }

  final class Connection private[Loopback] (val peerKey: Array[Byte], val initiator: Boolean, deliver: Executor)
      extends PeerConnection:
    private var remote: Connection = null
    private var remoteHandlers: Byte => Option[PeerStreamHandler] = _ => None
    @volatile private var open = true
    private val streams = new AtomicInteger()
    private val ids = new AtomicLong()
    private val held = new AtomicLong()

    private[Loopback] def link(other: Connection, handlers: Byte => Option[PeerStreamHandler]): Unit =
      remote = other
      remoteHandlers = handlers

    val openedAtMillis: Long = System.currentTimeMillis()
    lazy val peerPublicKey: Ed25519PublicKey = Ed25519PublicKey(peerKey)
    def negotiatedAlpn: String = "loopback"
    def holdInbound(bytes: Long): Boolean = { held.addAndGet(bytes); true }
    def releaseInbound(bytes: Long): Unit = held.addAndGet(-bytes): Unit
    def heldInboundBytes: Long = held.get()
    def pendingOutboundBytes: Long = 0L
    def droppedOutboundMessages: Long = 0L
    def isOpen: Boolean = open
    def openStreamCount: Int = streams.get()
    def remoteAddress: Option[java.net.SocketAddress] = None

    /** Close both ends: every later stream fails to open. Streams already open are left as they are. */
    def close(): Unit =
      open = false
      if remote != null then remote.open = false

    def openStream(kind: Byte): CompletableFuture[Stream] =
      if !open then CompletableFuture.failedFuture(new java.io.IOException("loopback connection closed"))
      else
        val id = ids.getAndIncrement()
        val ours = new Stream(kind, id, deliver, () => streams.decrementAndGet(): Unit)
        val theirs = new Stream(kind, id, deliver, () => remote.streams.decrementAndGet(): Unit)
        ours.peer = theirs
        theirs.peer = ours
        streams.incrementAndGet()
        remote.streams.incrementAndGet()
        deliver.execute { () =>
          remoteHandlers(kind) match
            case Some(handler) =>
              try handler.onStream(remote, theirs)
              catch case _: Exception => theirs.reset()
            case None => theirs.reset()
        }
        CompletableFuture.completedFuture(ours)

  /** One end of a stream. */
  final class Stream private[Loopback] (val kind: Byte, val id: Long, deliver: Executor, onEnd: () => Unit)
      extends MessageStream:
    private[Loopback] var peer: Stream = null
    private val inbox = new ConcurrentLinkedQueue[Array[Byte]]()
    private var receiver: Array[Byte] => Unit = null
    private var closeListener: () => Unit = null

    /** The peer finished or either side reset: no more messages arrive. */
    private var ended = false
    private var endDelivered = false
    @volatile private var dead = false // reset or closed: nothing more is sent either way
    @volatile private var finished = false
    private var counted = true

    def pendingOutboundBytes: Long = 0L
    def isOpen: Boolean = !dead

    def onMessage(f: Array[Byte] => Unit): Stream =
      synchronized { receiver = f }
      drain()
      this

    def onClosed(f: () => Unit): Stream =
      synchronized { closeListener = f }
      drain()
      this

    def send(message: Array[Byte]): Unit =
      if !dead && !finished then
        val target = peer
        deliver.execute { () =>
          if !target.dead then
            target.inbox.add(message)
            target.drain()
        }

    def finish(): Unit =
      if !dead && !finished then
        finished = true
        val target = peer
        deliver.execute(() => target.end())

    def reset(): Unit =
      if !dead then
        dead = true
        inbox.clear()
        uncount()
        val target = peer
        deliver.execute { () =>
          target.dead = true
          target.inbox.clear()
          target.uncount()
          target.end()
        }
        end()

    def close(): Unit = reset()

    private def uncount(): Unit =
      val first = synchronized { val c = counted; counted = false; c }
      if first then onEnd()

    private def end(): Unit =
      synchronized { ended = true }
      // Finished in both directions: the stream no longer counts against the connection.
      if finished || dead then uncount()
      drain()

    /** Hand over what has arrived to whatever is registered: messages first, then the end, each once. Listeners run
      * outside the lock, one drain at a time.
      */
    private def drain(): Unit =
      var again = true
      while again do
        val (f, m) = synchronized {
          if receiver != null && !inbox.isEmpty then (receiver, inbox.poll()) else (null, null)
        }
        if m != null then f(m)
        else
          again = false
          val fire = synchronized {
            if ended && !endDelivered && closeListener != null && (receiver == null || inbox.isEmpty) then
              endDelivered = true
              closeListener
            else null
          }
          if fire != null then fire()
