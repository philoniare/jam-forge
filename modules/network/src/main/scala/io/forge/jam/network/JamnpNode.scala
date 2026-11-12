package io.forge.jam.network

import java.net.InetSocketAddress
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, TimeUnit}

import io.forge.jam.core.primitives.Ed25519PublicKey
import io.netty.bootstrap.Bootstrap
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter, ChannelInitializer}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioDatagramChannel
import io.netty.handler.codec.ByteToMessageDecoder
import io.netty.handler.ssl.ClientAuth
import io.netty.handler.ssl.util.{SimpleKeyManagerFactory, SimpleTrustManagerFactory}
import io.netty.incubator.codec.quic.{
  BoringSSLContextOption,
  InsecureQuicTokenHandler,
  QuicChannel,
  QuicClientCodecBuilder,
  QuicServerCodecBuilder,
  QuicSslContext,
  QuicSslContextBuilder,
  QuicStreamChannel,
  QuicStreamType
}

import java.security.{KeyStore, Principal, PrivateKey}
import java.security.cert.X509Certificate
import javax.net.ssl.{
  KeyManager,
  ManagerFactoryParameters,
  SSLEngine,
  TrustManager,
  X509ExtendedKeyManager,
  X509TrustManager
}
import scala.jdk.CollectionConverters.*

/** Configuration for a JAMNP-S endpoint.
  *
  * @param genesisHashPrefix first 8 hex nibbles (lower-case) of the chain's
  *   genesis header hash, used in the ALPN identifier
  * @param builder true for work-package builder endpoints (ALPN /builder
  *   suffix)
  */
final case class JamnpConfig(
    genesisHashPrefix: String,
    builder: Boolean = false,
    maxIdleTimeoutMillis: Long = 30000,
    /** Set SO_REUSEPORT on the listening socket, so a local relay (e.g. a
      * network-fault injector) can bind the same port on a more specific
      * address. Off by default: any process of the same user could then
      * receive this node's traffic.
      */
    reusePort: Boolean = false,
    /** Frame size cap; must exceed the largest protocol message (bundles can
      * approach Cmaxbundlesize ≈ 13.8 MB).
      */
    maxFrameBytes: Int = 32 * 1024 * 1024,
    /** Cap on accepted (inbound) connections. */
    maxInboundConnections: Int = 1200,
    /** Inbound slots only work-package builders (ALPN `/builder`) may use
      * (JAMNP-S "Required connectivity": "e.g. 20").
      */
    builderSlots: Int = 20,
    /** A CE stream with no bytes read or written for this long is reset, so a
      * silent peer cannot hold stream credit forever. UP streams are exempt
      * (they stay open and may be quiet). 0 disables the timeout.
      */
    streamIdleTimeoutMillis: Long = JamnpConfig.DefaultStreamIdleTimeoutMillis,
    /** Limits on bytes queued for a peer that is not taking them. */
    outbound: OutboundLimits = OutboundLimits()
):
  private def baseAlpn: String = s"jamnp-s/${JamnpConfig.ProtocolVersion}/$genesisHashPrefix"

  /** The ALPN identifier this endpoint dials with: `jamnp-s/V/H`, plus
    * `/builder` when connecting as a work-package builder (JAMNP-S §ALPN).
    */
  def alpn: String =
    if builder then s"$baseAlpn/builder" else baseAlpn

  /** ALPN identifiers accepted on inbound connections: the acceptor always
    * permits the `/builder` suffix (JAMNP-S §ALPN).
    */
  def acceptedAlpns: Seq[String] = Seq(baseAlpn, s"$baseAlpn/builder")

object JamnpConfig:
  /** JAMNP-S protocol version `V` in the ALPN identifier (jam-np 56ce86c). */
  val ProtocolVersion: Int = 1

  /** Default idle limit for a CE stream. Well above the longest wait a valid
    * exchange has with nothing on the wire (30 s for a CE 134 co-signature).
    */
  val DefaultStreamIdleTimeoutMillis: Long = 120000

  def isBuilderAlpn(alpn: String): Boolean = alpn != null && alpn.endsWith("/builder")

/** Limits on outbound bytes waiting in memory for a peer that is slow to take them, or has stopped.
  *
  * A write to a QUIC stream is queued on the heap until QUIC flow control and the congestion window let it out, and
  * that queue has no bound of its own (Netty's write-buffer water marks do not apply to QUIC stream channels: their
  * writability only says whether the stream has send credit). So every send is counted until its write completes,
  * per stream and per connection, and [[JamnpStream.send]] keeps both counts under these limits. A send to a peer
  * that keeps up never reaches them, and its bytes on the wire are the same.
  *
  * @param maxStreamPendingBytes most bytes waiting on one CE stream. A message is always accepted on a stream with
  *   nothing waiting, so one message of any legal size can be sent.
  * @param maxConnectionPendingBytes most bytes waiting on all streams of one connection (the default equals the
  *   connection flow-control window this node grants its peers)
  * @param sendWaitMillis how long a send from an ordinary thread waits for room before its stream is reset. A send
  *   from a network event loop cannot wait (the loop is what drains the queue): its stream is reset at once, which
  *   JAMNP-S reads as the request having failed.
  * @param upHighWaterBytes most bytes waiting on a UP stream; further messages are dropped until it drains (a later
  *   block announcement supersedes the ones dropped)
  * @param stallMillis a connection with bytes waiting of which the peer has taken none for this long is closed
  */
final case class OutboundLimits(
    maxStreamPendingBytes: Long = 40L * 1024 * 1024,
    maxConnectionPendingBytes: Long = 64L * 1024 * 1024,
    sendWaitMillis: Long = 5000,
    upHighWaterBytes: Long = 1024 * 1024,
    stallMillis: Long = 30000
)

/** Outbound bytes waiting on one connection. */
private[network] final class OutboundFlow(val limits: OutboundLimits):
  val pending = new java.util.concurrent.atomic.AtomicLong()
  val waiters = new java.util.concurrent.atomic.AtomicInteger()
  val dropped = new java.util.concurrent.atomic.AtomicLong()
  @volatile var lastProgressNanos: Long = System.nanoTime()

  /** A write of `n` bytes finished (or failed with its stream): the peer took them, or they are gone. */
  def completed(n: Long): Unit =
    pending.addAndGet(-n)
    lastProgressNanos = System.nanoTime()
    if waiters.get() > 0 then synchronized(notifyAll())

  /** Bytes are waiting and none has left for `stallMillis`. */
  def stalled: Boolean =
    pending.get() > 0 && System.nanoTime() - lastProgressNanos > limits.stallMillis * 1000000L

/** A single message-framed JAMNP stream. Messages are framed as a 32-bit
  * little-endian content length followed by the content.
  */
final class JamnpStream private[network] (
    private[network] val channel: QuicStreamChannel,
    val kind: Byte,
    flow: OutboundFlow = new OutboundFlow(OutboundLimits())
):
  private val pendingOut = new java.util.concurrent.atomic.AtomicLong()

  /** Bytes sent on this stream whose write has not completed: what a slow peer keeps in this node's memory. */
  def pendingOutboundBytes: Long = pendingOut.get()

  private def fits(size: Long): Boolean =
    val limits = flow.limits
    val onStream = pendingOut.get()
    val onConnection = flow.pending.get()
    val streamLimit = if StreamKind.isUp(kind) then limits.upHighWaterBytes else limits.maxStreamPendingBytes
    (onStream == 0 || onStream + size <= streamLimit) &&
    (onConnection == 0 || onConnection + size <= limits.maxConnectionPendingBytes)

  /** Count `size` bytes as waiting if they fit. Caller holds `flow`'s monitor. */
  private def take(size: Long): Boolean =
    if !fits(size) then false
    else
      pendingOut.addAndGet(size)
      if flow.pending.getAndAdd(size) == 0 then flow.lastProgressNanos = System.nanoTime()
      true

  /** Whether `size` more bytes may be queued on this stream now; see [[OutboundLimits]] for what happens when not. */
  private def admit(size: Long): Boolean =
    if flow.stalled then
      // The peer has taken nothing for the stall limit: stop queueing for it altogether.
      channel.parent().close()
      return false
    if flow.synchronized(take(size)) then return true
    if StreamKind.isUp(kind) then
      flow.dropped.incrementAndGet()
      false
    else if JamnpNode.onNetworkThread(channel) then
      reset()
      false
    else
      val deadline = System.nanoTime() + flow.limits.sendWaitMillis * 1000000L
      flow.waiters.incrementAndGet()
      val admitted =
        try
          flow.synchronized {
            var ok = take(size)
            var left = deadline - System.nanoTime()
            while !ok && left > 0 && channel.isActive && !flow.stalled do
              flow.wait(math.max(1L, math.min(left / 1000000L, 100L)))
              ok = take(size)
              left = deadline - System.nanoTime()
            ok
          }
        finally flow.waiters.decrementAndGet()
      if !admitted then reset()
      admitted

  private def sent(bytes: Long): io.netty.channel.ChannelFutureListener =
    (_: io.netty.channel.ChannelFuture) =>
      pendingOut.addAndGet(-bytes)
      flow.completed(bytes)

  private var receiver: Array[Byte] => Unit = null
  // Messages that arrive before a receiver is registered (the peer can reply
  // as soon as `openStream` returns), or while `onMessage` replays them.
  // Capped, since send-only streams never register a receiver.
  private val pending = new java.util.ArrayDeque[Array[Byte]]()
  private var pendingBytes = 0L
  private var draining = false
  private var closed = false
  private var closeDelivered = false
  private var closeListener: () => Unit = null
  @volatile private var finWritten = false

  /** Register the message receiver. Messages received before this call are
    * delivered first, in order, on the caller's thread.
    */
  def onMessage(f: Array[Byte] => Unit): JamnpStream =
    synchronized {
      receiver = f
      draining = true
    }
    var batch = nextPending()
    while batch != null do
      batch.foreach(f)
      batch = nextPending()
    this

  private def nextPending(): List[Array[Byte]] = synchronized {
    if pending.isEmpty then
      draining = false
      null
    else
      val out = List.newBuilder[Array[Byte]]
      while !pending.isEmpty do out += pending.poll()
      pendingBytes = 0
      out.result()
  }

  /** Register a close/FIN listener. `openStream` returns after the kind byte
    * is sent, so the peer can finish or reject the stream before this runs; a
    * close that already happened fires the listener here, on the caller's
    * thread.
    */
  def onClosed(f: () => Unit): JamnpStream =
    val fireNow = synchronized {
      closeListener = f
      if closed && !closeDelivered then
        closeDelivered = true
        true
      else false
    }
    if fireNow then f()
    this

  private[network] def dispatch(message: Array[Byte]): Unit =
    val f = synchronized {
      if receiver == null || draining then
        if draining || pendingBytes + message.length <= JamnpStream.MaxPendingBytes then
          pending.add(message)
          pendingBytes += message.length
        null
      else receiver
    }
    if f != null then f(message)

  /** Fires the close listener exactly once (remote FIN and channel-inactive
    * both signal closure; handlers must not run twice).
    */
  private[network] def dispatchClosed(): Unit =
    val listener = synchronized {
      if closed then null
      else
        closed = true
        if closeListener == null then null
        else
          closeDelivered = true
          closeListener
    }
    if listener != null then listener()

  /** Send one framed message. A large frame is handed to the channel in
    * slices (the bytes on the wire are the same), so each slice that gets
    * through counts as progress for the stream idle timeout.
    *
    * The message is not sent when the peer is not taking what was sent
    * before and the outbound limits are reached ([[OutboundLimits]]): on a
    * CE stream the send first waits for room when the calling thread may
    * wait, and the stream is reset if there is still none; on a UP stream the
    * message is dropped. Either way the caller sees a stream that closed or
    * a peer that did not answer, as with any other failed exchange.
    */
  def send(message: Array[Byte]): Unit =
    val size = 4L + message.length
    if !admit(size) then return
    val buf = channel.alloc().buffer(4 + message.length)
    buf.writeIntLE(message.length)
    buf.writeBytes(message)
    if buf.readableBytes() <= JamnpStream.SendSliceBytes then channel.writeAndFlush(buf).addListener(sent(size))
    else
      // On the event loop in one go, so slices of concurrent sends cannot
      // interleave; writes queued after this one stay behind it.
      val writeSlices: Runnable = () =>
        while buf.isReadable do
          val n = math.min(JamnpStream.SendSliceBytes, buf.readableBytes())
          channel.write(buf.readRetainedSlice(n)).addListener(sent(n.toLong))
        buf.release()
        channel.flush()
        ()
      if channel.eventLoop().inEventLoop() then writeSlices.run() else channel.eventLoop().execute(writeSlices)
    ()

  /** Half-close the write side (FIN), signalling end of response/request.
    * Ordered after any queued writes via an empty flush so a large in-flight
    * message is not truncated by the FIN.
    */
  def finish(): Unit =
    channel
      .writeAndFlush(Unpooled.EMPTY_BUFFER)
      .addListener(new io.netty.channel.ChannelFutureListener {
        override def operationComplete(f: io.netty.channel.ChannelFuture): Unit =
          channel
            .shutdownOutput()
            .addListener(new io.netty.channel.ChannelFutureListener {
              override def operationComplete(g: io.netty.channel.ChannelFuture): Unit =
                if g.isSuccess then
                  finWritten = true
                  closeIfDone()
            })
          ()
      })
    ()

  /** Close the channel once our FIN is written and the peer's has been read.
    * Netty usually does this itself, but not in every ordering of the two,
    * and a channel left behind is held until the idle timeout. Runs on the
    * event loop.
    */
  private[network] def closeIfDone(): Unit =
    if finWritten && channel.isInputShutdown && channel.isOpen then channel.close()
    ()

  /** Close the channel, sending FIN if the write side is still open. This
    * does not tell the peer to stop sending: to give up on a stream the peer
    * may still be writing to (a timeout, a failed request), use [[reset]], or
    * QUIC keeps the stream, and its share of the stream limit, alive.
    */
  def close(): Unit =
    channel.close()

  /** Reset both halves (QUIC RESET_STREAM and STOP_SENDING). JAMNP-S reads an
    * acceptor's reset as rejection or failure of the request and an
    * initiator's as cancellation (JAMNP-S §Protocols and streams).
    */
  def reset(): Unit = JamnpNode.resetStream(channel)

  def isOpen: Boolean = channel.isActive

  /** The QUIC stream ID. */
  def id: Long = channel.streamId()

object JamnpStream:
  /** Cap on bytes buffered before a receiver is registered; later frames are dropped. */
  val MaxPendingBytes: Long = 4L * 1024 * 1024

  /** Largest piece of a frame written to the channel in one go. */
  val SendSliceBytes: Int = 256 * 1024

  /** Application error code sent with a stream reset. */
  val ResetErrorCode: Int = 0

/** A JAMNP connection to a single peer, wrapping a QUIC channel. */
final class JamnpConnection private[network] (
    private[network] val quic: QuicChannel,
    val initiator: Boolean,
    streamIdleTimeoutMillis: Long = JamnpConfig.DefaultStreamIdleTimeoutMillis,
    outbound: OutboundLimits = OutboundLimits()
):
  private val flow = JamnpNode.outboundFlow(quic, outbound)

  /** Bytes sent on this connection's streams whose write has not completed. */
  def pendingOutboundBytes: Long = flow.pending.get()

  /** UP messages dropped because the peer was not taking earlier ones. */
  def droppedOutboundMessages: Long = flow.dropped.get()

  /** The peer's raw Ed25519 public key, from its TLS certificate. */
  lazy val peerKey: Array[Byte] =
    val certs = quic.sslEngine().getSession.getPeerCertificates
    NodeIdentity.peerPublicKey(certs(0).asInstanceOf[X509Certificate])

  /** [[peerKey]] as a content-compared value, for map keys and set membership. */
  lazy val peerPublicKey: Ed25519PublicKey = Ed25519PublicKey(peerKey)

  def negotiatedAlpn: String = quic.sslEngine().getApplicationProtocol

  /** Open a stream of the given kind: sends the kind byte, then messages are
    * framed. UP streams should only be opened by the connection initiator.
    */
  def openStream(kind: Byte): CompletableFuture[JamnpStream] =
    val result = new CompletableFuture[JamnpStream]()
    quic
      .createStream(QuicStreamType.BIDIRECTIONAL, null)
      .addListener(
        new io.netty.util.concurrent.GenericFutureListener[
          io.netty.util.concurrent.Future[QuicStreamChannel]
        ] {
          override def operationComplete(
              f: io.netty.util.concurrent.Future[QuicStreamChannel]
          ): Unit =
            if !f.isSuccess then result.completeExceptionally(f.cause())
            else
              val ch = f.getNow
              JamnpNode.trackStream(ch)
              val stream = new JamnpStream(ch, kind, flow)
              if StreamKind.isCe(kind) then JamnpNode.addIdleTimeout(ch, streamIdleTimeoutMillis)
              ch.pipeline()
                .addLast(new JamnpNode.FrameDecoder(JamnpNode.DefaultMaxFrame))
                .addLast(new JamnpNode.StreamDispatchHandler(stream))
              // Kind byte precedes all messages.
              val kindBuf = ch.alloc().buffer(1)
              kindBuf.writeByte(kind.toInt)
              ch.writeAndFlush(kindBuf)
              result.complete(stream)
        }
      )
    result

  def close(): Unit = quic.close()

  def isOpen: Boolean = quic.isActive

  /** Stream channels currently open on this connection, whoever opened them.
    * A stream leaves the count when its channel closes. That alone does not
    * return the peer's QUIC stream credit: the stream must also have been
    * finished or reset in both directions.
    */
  def openStreamCount: Int = JamnpNode.streamCounter(quic).get()

  /** The peer's UDP address, when the QUIC path is known. */
  def remoteAddress: Option[java.net.SocketAddress] =
    try Option(quic.remoteSocketAddress())
    catch case _: Exception => None

/** Handles streams opened by remote peers, keyed by stream kind. */
trait StreamHandler:
  /** Called when a peer opens a stream of a kind this handler is registered
    * for, after the kind byte has been consumed. Register receivers on the
    * stream to consume messages.
    */
  def onStream(connection: JamnpConnection, stream: JamnpStream): Unit

/** A JAMNP-S endpoint: listens for peer connections and initiates its own,
  * dispatching incoming streams to registered [[StreamHandler]]s by kind.
  */
final class JamnpNode(
    val identity: NodeIdentity,
    val config: JamnpConfig
):
  import JamnpNode.*

  private val group = new NioEventLoopGroup()
  private val handlers = new ConcurrentHashMap[Byte, StreamHandler]()
  private val connections = ConcurrentHashMap.newKeySet[JamnpConnection]()
  private val connectionListeners =
    new java.util.concurrent.CopyOnWriteArrayList[(JamnpConnection, Boolean) => Unit]()

  /** Register a listener called with `true` when a connection (accepted or
    * dialed) comes up and `false` when it closes, e.g. on the QUIC idle
    * timeout after a link goes quiet.
    */
  def onConnectionChange(f: (JamnpConnection, Boolean) => Unit): JamnpNode =
    connectionListeners.add(f)
    this

  private def fireConnectionChange(conn: JamnpConnection, up: Boolean): Unit =
    connectionListeners.forEach { l =>
      try l(conn, up)
      catch case _: Exception => ()
    }
  @volatile private var serverChannel: Channel = null

  def registerHandler(kind: Byte, handler: StreamHandler): JamnpNode =
    handlers.put(kind, handler)
    this

  /** Tracks accepted connections */
  @io.netty.channel.ChannelHandler.Sharable
  private final class ConnectionTracker extends ChannelInboundHandlerAdapter:
    override def channelActive(ctx: ChannelHandlerContext): Unit =
      val conn =
        new JamnpConnection(
          ctx.channel().asInstanceOf[QuicChannel],
          initiator = false,
          config.streamIdleTimeoutMillis,
          config.outbound
        )
      if !admitInbound(conn) then
        ctx.close()
        return
      ctx.channel().attr(ConnectionKey).set(conn)
      connections.add(conn)
      fireConnectionChange(conn, true)
      ctx.fireChannelActive()
    override def channelInactive(ctx: ChannelHandlerContext): Unit =
      val conn = ctx.channel().attr(ConnectionKey).get()
      if conn != null then
        connections.remove(conn)
        fireConnectionChange(conn, false)
      ctx.fireChannelInactive()

  /** Inbound admission: builders may use any free slot, other nodes all but
    * the `builderSlots` reserved for builders.
    */
  private def admitInbound(conn: JamnpConnection): Boolean =
    val inbound = connections.asScala.filter(c => !c.initiator && c.isOpen)
    if JamnpConfig.isBuilderAlpn(conn.negotiatedAlpn) then inbound.size < config.maxInboundConnections
    else
      val others = inbound.count(c => !JamnpConfig.isBuilderAlpn(c.negotiatedAlpn))
      others < config.maxInboundConnections - config.builderSlots

  def boundPort: Int =
    serverChannel.localAddress().asInstanceOf[InetSocketAddress].getPort

  private def sslServer(): QuicSslContext =
    QuicSslContextBuilder
      .forServer(new IdentityKeyManagerFactory(identity), null)
      .trustManager(PeerCertificateTrustManagerFactory)
      .clientAuth(ClientAuth.REQUIRE)
      .applicationProtocols(config.acceptedAlpns*)
      .option(
        BoringSSLContextOption.SERVER_KEY_TYPES,
        Map("ECDHE_ECDSA" -> Ed25519KeyType).asJava
      )
      .option(BoringSSLContextOption.SIGNATURE_ALGORITHMS, Array("ed25519"))
      .build()

  private lazy val sslClient: QuicSslContext =
    QuicSslContextBuilder
      .forClient()
      .keyManager(new IdentityKeyManagerFactory(identity), null)
      .trustManager(PeerCertificateTrustManagerFactory)
      .applicationProtocols(config.alpn)
      .option(BoringSSLContextOption.CLIENT_KEY_TYPES, Set(Ed25519KeyType).asJava)
      .option(BoringSSLContextOption.SIGNATURE_ALGORITHMS, Array("ed25519"))
      .build()

  /** Start listening on the given address. */
  def start(bindAddress: InetSocketAddress): JamnpNode =
    val codec = new QuicServerCodecBuilder()
      .sslContext(sslServer())
      .maxIdleTimeout(config.maxIdleTimeoutMillis, TimeUnit.MILLISECONDS)
      .initialMaxData(InitialMaxData)
      .initialMaxStreamDataBidirectionalLocal(InitialMaxStreamData)
      .initialMaxStreamDataBidirectionalRemote(InitialMaxStreamData)
      .initialMaxStreamsBidirectional(MaxConcurrentStreams)
      .tokenHandler(InsecureQuicTokenHandler.INSTANCE)
      .handler(new ConnectionTracker())
      .streamHandler(new ChannelInitializer[QuicStreamChannel]() {
        override def initChannel(ch: QuicStreamChannel): Unit =
          trackStream(ch)
          // From the start: a peer that opens a stream and never sends its
          // kind byte must not hold it either.
          addIdleTimeout(ch, config.streamIdleTimeoutMillis)
          ch.pipeline().addLast(
            new IncomingStreamHandler(handlers, config.maxFrameBytes, config.streamIdleTimeoutMillis, config.outbound)
          )
      })
      .build()

    val bootstrap = new Bootstrap()
      .group(group)
      .channel(classOf[NioDatagramChannel])
      .handler(codec)
    if config.reusePort then
      bootstrap.option(
        io.netty.channel.socket.nio.NioChannelOption.of(java.net.StandardSocketOptions.SO_REUSEPORT),
        java.lang.Boolean.TRUE
      )
    serverChannel = bootstrap
      .bind(bindAddress)
      .sync()
      .channel()
    this

  /** Connect to a peer. The returned connection has completed the TLS
    * handshake (peer key available).
    */
  def connect(remote: InetSocketAddress): CompletableFuture[JamnpConnection] =
    val codec = new QuicClientCodecBuilder()
      .sslContext(sslClient)
      .maxIdleTimeout(config.maxIdleTimeoutMillis, TimeUnit.MILLISECONDS)
      .initialMaxData(InitialMaxData)
      .initialMaxStreamDataBidirectionalLocal(InitialMaxStreamData)
      .initialMaxStreamDataBidirectionalRemote(InitialMaxStreamData)
      .initialMaxStreamsBidirectional(MaxConcurrentStreams)
      .build()

    val result = new CompletableFuture[JamnpConnection]()
    // The socket is of the peer's address family. Left to the default it is a dual-stack IPv6 socket, and the
    // port the system picks for one ignores the ports IPv4 sockets hold (on macOS it lands on a held port as
    // often as chance allows). Replies from an IPv4 peer then go to the IPv4 socket, ours never sees
    // them, and the handshake waits out its timeout. An IPv4 socket is never given a port another one holds.
    val family = remote.getAddress match
      case _: java.net.Inet4Address => io.netty.channel.socket.InternetProtocolFamily.IPv4
      case _: java.net.Inet6Address => io.netty.channel.socket.InternetProtocolFamily.IPv6
      case _                        => null
    val channel =
      try
        new Bootstrap()
          .group(group)
          .channelFactory(() => if family == null then new NioDatagramChannel() else new NioDatagramChannel(family))
          .handler(codec)
          .bind(0)
          .sync()
          .channel()
      catch
        case e: Exception =>
          result.completeExceptionally(e)
          return result

    QuicChannel
      .newBootstrap(channel)
      .handler(new ChannelInboundHandlerAdapter())
      .streamHandler(new ChannelInitializer[QuicStreamChannel]() {
        override def initChannel(ch: QuicStreamChannel): Unit =
          trackStream(ch)
          // From the start: a peer that opens a stream and never sends its
          // kind byte must not hold it either.
          addIdleTimeout(ch, config.streamIdleTimeoutMillis)
          ch.pipeline().addLast(
            new IncomingStreamHandler(handlers, config.maxFrameBytes, config.streamIdleTimeoutMillis, config.outbound)
          )
      })
      .remoteAddress(remote)
      .connect()
      .addListener(
        new io.netty.util.concurrent.GenericFutureListener[
          io.netty.util.concurrent.Future[QuicChannel]
        ] {
          override def operationComplete(
              f: io.netty.util.concurrent.Future[QuicChannel]
          ): Unit =
            if !f.isSuccess then
              result.completeExceptionally(f.cause())
              channel.close()
            else
              val quic = f.getNow
              val conn = new JamnpConnection(quic, initiator = true, config.streamIdleTimeoutMillis, config.outbound)
              quic.attr(ConnectionKey).set(conn)
              connections.add(conn)
              fireConnectionChange(conn, true)
              quic
                .closeFuture()
                .addListener(
                  new io.netty.channel.ChannelFutureListener {
                    override def operationComplete(cf: io.netty.channel.ChannelFuture): Unit =
                      connections.remove(conn)
                      fireConnectionChange(conn, false)
                      channel.close()
                  }
                )
              result.complete(conn)
        }
      )
    result

  def shutdown(): Unit =
    connections.forEach(_.close())
    if serverChannel != null then serverChannel.close().sync()
    group.shutdownGracefully(0, 1, TimeUnit.SECONDS).sync()

object JamnpNode:
  private[network] val Ed25519KeyType = "Ed25519"
  private[network] val DefaultMaxFrame = 32 * 1024 * 1024
  private val InitialMaxData = 64L * 1024 * 1024
  private val InitialMaxStreamData = 32L * 1024 * 1024
  private val MaxConcurrentStreams = 256L

  private[network] val ConnectionKey =
    io.netty.util.AttributeKey.valueOf[JamnpConnection]("jamnp-connection")

  private val StreamCountKey =
    io.netty.util.AttributeKey.valueOf[java.util.concurrent.atomic.AtomicInteger]("jamnp-open-streams")

  private[network] def streamCounter(quic: QuicChannel): java.util.concurrent.atomic.AtomicInteger =
    val attr = quic.attr(StreamCountKey)
    val existing = attr.get()
    if existing != null then existing
    else
      attr.setIfAbsent(new java.util.concurrent.atomic.AtomicInteger())
      attr.get()

  private val OutboundFlowKey = io.netty.util.AttributeKey.valueOf[OutboundFlow]("jamnp-outbound-flow")

  /** The outbound byte count of a connection, shared by all its streams. */
  private[network] def outboundFlow(quic: QuicChannel, limits: OutboundLimits): OutboundFlow =
    val attr = quic.attr(OutboundFlowKey)
    val existing = attr.get()
    if existing != null then existing
    else
      attr.setIfAbsent(new OutboundFlow(limits))
      attr.get()

  /** True on a thread that must not wait for a write to drain: the stream's own event loop, which does the
    * draining, or any other Netty event loop, which other connections depend on.
    */
  private[network] def onNetworkThread(ch: QuicStreamChannel): Boolean =
    ch.eventLoop().inEventLoop() || Thread.currentThread().isInstanceOf[io.netty.util.concurrent.FastThreadLocalThread]

  /** Count a stream channel against its connection until it closes. */
  private[network] def trackStream(ch: QuicStreamChannel): Unit =
    val counter = streamCounter(ch.parent())
    counter.incrementAndGet()
    ch.closeFuture()
      .addListener(new io.netty.channel.ChannelFutureListener {
        override def operationComplete(f: io.netty.channel.ChannelFuture): Unit =
          counter.decrementAndGet()
          ()
      })
    ()

  /** Reset both halves of a stream (RESET_STREAM and STOP_SENDING), then
    * close its channel. Closing alone only sends FIN: QUIC then keeps the
    * stream until the peer's remaining data has been read, which a closed
    * channel never does, and the peer never gets the stream credit back.
    */
  private[network] def resetStream(ch: QuicStreamChannel): Unit =
    ch.shutdown(JamnpStream.ResetErrorCode).addListener(io.netty.channel.ChannelFutureListener.CLOSE)
    ()

  private val IdleHandlerName = "jamnp-idle"

  /** Raise an idle event on `ch` after `millis` with nothing read or written. */
  private[network] def addIdleTimeout(ch: QuicStreamChannel, millis: Long): Unit =
    if millis > 0 then
      ch.pipeline()
        .addFirst(
          IdleHandlerName,
          new io.netty.handler.timeout.IdleStateHandler(0L, 0L, millis, TimeUnit.MILLISECONDS)
        )
    ()

  private def removeIdleTimeout(ch: QuicStreamChannel): Unit =
    if ch.pipeline().get(IdleHandlerName) != null then ch.pipeline().remove(IdleHandlerName)
    ()

  private def isIdle(evt: Object): Boolean = evt.isInstanceOf[io.netty.handler.timeout.IdleStateEvent]

  /** Key manager that always serves the node's Ed25519 identity. */
  private final class IdentityKeyManager(id: NodeIdentity) extends X509ExtendedKeyManager:
    private val alias = "jamnp"
    override def chooseEngineServerAlias(kt: String, is: Array[Principal], e: SSLEngine): String = alias
    override def chooseEngineClientAlias(kt: Array[String], is: Array[Principal], e: SSLEngine): String = alias
    override def chooseServerAlias(kt: String, is: Array[Principal], s: java.net.Socket): String = alias
    override def chooseClientAlias(kt: Array[String], is: Array[Principal], s: java.net.Socket): String = alias
    override def getServerAliases(kt: String, is: Array[Principal]): Array[String] = Array(alias)
    override def getClientAliases(kt: String, is: Array[Principal]): Array[String] = Array(alias)
    override def getCertificateChain(a: String): Array[X509Certificate] = Array(id.certificate)
    override def getPrivateKey(a: String): PrivateKey = id.privateKey

  private[network] final class IdentityKeyManagerFactory(id: NodeIdentity)
      extends SimpleKeyManagerFactory:
    override def engineInit(keyStore: KeyStore, password: Array[Char]): Unit = ()
    override def engineInit(spec: ManagerFactoryParameters): Unit = ()
    override def engineGetKeyManagers(): Array[KeyManager] =
      Array(new IdentityKeyManager(id))

  /** Accepts a peer only when its certificate meets JAMNP-S
    * (`NodeIdentity.validateCertificate`); the handshake fails otherwise, so
    * the connection is never established. No chain of trust is checked.
    */
  private object PeerCertificateTrust extends X509TrustManager:
    private def check(chain: Array[X509Certificate]): Unit =
      if chain == null || chain.isEmpty then throw new java.security.cert.CertificateException("no peer certificate")
      NodeIdentity.validateCertificate(chain(0)) match
        case Left(why) => throw new java.security.cert.CertificateException(why)
        case Right(_)  => ()
    override def checkClientTrusted(chain: Array[X509Certificate], authType: String): Unit = check(chain)
    override def checkServerTrusted(chain: Array[X509Certificate], authType: String): Unit = check(chain)
    override def getAcceptedIssuers: Array[X509Certificate] = Array.empty

  private[network] object PeerCertificateTrustManagerFactory extends SimpleTrustManagerFactory:
    override def engineInit(keyStore: KeyStore): Unit = ()
    override def engineInit(spec: ManagerFactoryParameters): Unit = ()
    override def engineGetTrustManagers(): Array[TrustManager] = Array(PeerCertificateTrust)

  /** Splits the byte stream into JAMNP frames (u32-LE length + content) and
    * emits the content as byte arrays.
    */
  private[network] final class FrameDecoder(maxFrame: Int) extends ByteToMessageDecoder:
    override def decode(
        ctx: ChannelHandlerContext,
        in: ByteBuf,
        out: java.util.List[Object]
    ): Unit =
      while in.readableBytes() >= 4 do
        in.markReaderIndex()
        val len = in.readIntLE()
        if len < 0 || len > maxFrame then
          // Reset, not close: the rest of the oversized frame is never read.
          resetStream(ctx.channel().asInstanceOf[QuicStreamChannel])
          return
        if in.readableBytes() < len then
          in.resetReaderIndex()
          return
        val content = new Array[Byte](len)
        in.readBytes(content)
        out.add(content)

  /** Delivers decoded frames and close events to a [[JamnpStream]]. */
  private[network] final class StreamDispatchHandler(stream: JamnpStream)
      extends ChannelInboundHandlerAdapter:
    override def channelRead(ctx: ChannelHandlerContext, msg: Object): Unit =
      stream.dispatch(msg.asInstanceOf[Array[Byte]])
    override def channelInactive(ctx: ChannelHandlerContext): Unit =
      stream.dispatchClosed()
      ctx.fireChannelInactive()
    override def userEventTriggered(ctx: ChannelHandlerContext, evt: Object): Unit =
      // Remote FIN arrives as a shutdown-input event on QUIC streams.
      if evt.isInstanceOf[io.netty.channel.socket.ChannelInputShutdownEvent] ||
        evt.isInstanceOf[io.netty.channel.socket.ChannelInputShutdownReadComplete]
      then
        stream.dispatchClosed()
        stream.closeIfDone()
      else if isIdle(evt) then stream.reset()
      ctx.fireUserEventTriggered(evt)
    // A peer's RESET_STREAM surfaces as a read error: the stream is over.
    // Reset our halves too, so nothing the peer still sends is left unread.
    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit =
      stream.dispatchClosed()
      stream.reset()

  /** First reads the stream-kind byte of a peer-opened stream, then installs
    * framing and hands the stream to the registered handler. Streams of
    * unregistered kinds are reset.
    */
  private[network] final class IncomingStreamHandler(
      handlers: ConcurrentHashMap[Byte, StreamHandler],
      maxFrame: Int,
      streamIdleTimeoutMillis: Long = JamnpConfig.DefaultStreamIdleTimeoutMillis,
      outbound: OutboundLimits = OutboundLimits()
  ) extends ByteToMessageDecoder:
    // Set once the kind is refused: bytes still in flight are not a new kind byte.
    private var rejected = false

    override def userEventTriggered(ctx: ChannelHandlerContext, evt: Object): Unit =
      // No kind byte within the idle limit.
      if isIdle(evt) then resetStream(ctx.channel().asInstanceOf[QuicStreamChannel])
      ctx.fireUserEventTriggered(evt)

    override def decode(
        ctx: ChannelHandlerContext,
        in: ByteBuf,
        out: java.util.List[Object]
    ): Unit =
      if rejected then
        in.skipBytes(in.readableBytes())
        return
      if in.readableBytes() < 1 then return
      val kind = in.readByte()
      val handler = handlers.get(kind)
      val ch = ctx.channel().asInstanceOf[QuicStreamChannel]
      if handler == null then
        // Reset, not close: closing sends FIN only and leaves whatever the
        // peer goes on to send unread, so QUIC never retires the stream and
        // the peer never gets its stream credit back.
        rejected = true
        in.skipBytes(in.readableBytes())
        resetStream(ch)
        return
      // UP streams stay open and may be quiet.
      if StreamKind.isUp(kind) then removeIdleTimeout(ch)

      val quicParent = ch.parent()
      var conn = quicParent.attr(ConnectionKey).get()
      if conn == null then
        conn = new JamnpConnection(quicParent, initiator = false, streamIdleTimeoutMillis, outbound)
        quicParent.attr(ConnectionKey).set(conn)

      val stream = new JamnpStream(ch, kind, outboundFlow(quicParent, outbound))
      val pipeline = ctx.pipeline()
      pipeline.addAfter(
        ctx.name(),
        "jamnp-dispatch",
        new StreamDispatchHandler(stream)
      )
      pipeline.addAfter(ctx.name(), "jamnp-frames", new FrameDecoder(maxFrame))
      // Give the handler a chance to attach receivers before any frames flow.
      handler.onStream(conn, stream)
      // Remove ourselves; buffered bytes are replayed into the frame decoder.
      pipeline.remove(this)
