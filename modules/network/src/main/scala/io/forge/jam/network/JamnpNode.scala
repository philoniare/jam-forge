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
    builderSlots: Int = 20
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

  def isBuilderAlpn(alpn: String): Boolean = alpn != null && alpn.endsWith("/builder")

/** A single message-framed JAMNP stream. Messages are framed as a 32-bit
  * little-endian content length followed by the content.
  */
final class JamnpStream private[network] (
    private[network] val channel: QuicStreamChannel,
    val kind: Byte
):
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

  /** Send one framed message. */
  def send(message: Array[Byte]): Unit =
    val buf = channel.alloc().buffer(4 + message.length)
    buf.writeIntLE(message.length)
    buf.writeBytes(message)
    channel.writeAndFlush(buf)

  /** Half-close the write side (FIN), signalling end of response/request.
    * Ordered after any queued writes via an empty flush so a large in-flight
    * message is not truncated by the FIN.
    */
  def finish(): Unit =
    channel
      .writeAndFlush(Unpooled.EMPTY_BUFFER)
      .addListener(QuicStreamChannel.SHUTDOWN_OUTPUT)

  /** Fully close (reset if not yet finished). */
  def close(): Unit =
    channel.close()

  /** Reset both halves (QUIC RESET_STREAM and STOP_SENDING). JAMNP-S reads an
    * acceptor's reset as rejection or failure of the request and an
    * initiator's as cancellation (JAMNP-S §Protocols and streams).
    */
  def reset(): Unit =
    channel.shutdown(JamnpStream.ResetErrorCode).addListener(io.netty.channel.ChannelFutureListener.CLOSE)

  def isOpen: Boolean = channel.isActive

  /** The QUIC stream ID. */
  def id: Long = channel.streamId()

object JamnpStream:
  /** Cap on bytes buffered before a receiver is registered; later frames are dropped. */
  val MaxPendingBytes: Long = 4L * 1024 * 1024

  /** Application error code sent with a stream reset. */
  val ResetErrorCode: Int = 0

/** A JAMNP connection to a single peer, wrapping a QUIC channel. */
final class JamnpConnection private[network] (
    private[network] val quic: QuicChannel,
    val initiator: Boolean
):
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
              val stream = new JamnpStream(ch, kind)
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
      val conn = new JamnpConnection(ctx.channel().asInstanceOf[QuicChannel], initiator = false)
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
          ch.pipeline().addLast(new IncomingStreamHandler(handlers, config.maxFrameBytes))
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
    val channel = new Bootstrap()
      .group(group)
      .channel(classOf[NioDatagramChannel])
      .handler(codec)
      .bind(0)
      .sync()
      .channel()

    QuicChannel
      .newBootstrap(channel)
      .handler(new ChannelInboundHandlerAdapter())
      .streamHandler(new ChannelInitializer[QuicStreamChannel]() {
        override def initChannel(ch: QuicStreamChannel): Unit =
          ch.pipeline().addLast(new IncomingStreamHandler(handlers, config.maxFrameBytes))
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
              val conn = new JamnpConnection(quic, initiator = true)
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
          ctx.close()
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
      then stream.dispatchClosed()
      ctx.fireUserEventTriggered(evt)
    // A peer's RESET_STREAM surfaces as a read error: the stream is over.
    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit =
      stream.dispatchClosed()
      ctx.close()
      ()

  /** First reads the stream-kind byte of a peer-opened stream, then installs
    * framing and hands the stream to the registered handler. Streams of
    * unregistered kinds are reset.
    */
  private[network] final class IncomingStreamHandler(
      handlers: ConcurrentHashMap[Byte, StreamHandler],
      maxFrame: Int
  ) extends ByteToMessageDecoder:
    override def decode(
        ctx: ChannelHandlerContext,
        in: ByteBuf,
        out: java.util.List[Object]
    ): Unit =
      if in.readableBytes() < 1 then return
      val kind = in.readByte()
      val handler = handlers.get(kind)
      if handler == null then
        ctx.close()
        return

      val ch = ctx.channel().asInstanceOf[QuicStreamChannel]
      val quicParent = ch.parent()
      var conn = quicParent.attr(ConnectionKey).get()
      if conn == null then
        conn = new JamnpConnection(quicParent, initiator = false)
        quicParent.attr(ConnectionKey).set(conn)

      val stream = new JamnpStream(ch, kind)
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
