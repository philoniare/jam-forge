package io.forge.jam.network

import java.net.InetSocketAddress
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, TimeUnit}

import io.netty.bootstrap.Bootstrap
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter, ChannelInitializer}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioDatagramChannel
import io.netty.handler.codec.ByteToMessageDecoder
import io.netty.handler.ssl.ClientAuth
import io.netty.handler.ssl.util.{InsecureTrustManagerFactory, SimpleKeyManagerFactory}
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
import javax.net.ssl.{KeyManager, ManagerFactoryParameters, SSLEngine, X509ExtendedKeyManager}
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
    /** Frame size cap; must exceed the largest protocol message (bundles can
      * approach Cmaxbundlesize ≈ 13.8 MB).
      */
    maxFrameBytes: Int = 32 * 1024 * 1024
):
  def alpn: String =
    val base = s"jamnp-s/0/$genesisHashPrefix"
    if builder then s"$base/builder" else base

/** A single message-framed JAMNP stream. Messages are framed as a 32-bit
  * little-endian content length followed by the content.
  */
final class JamnpStream private[network] (
    private[network] val channel: QuicStreamChannel,
    val kind: Byte
):
  @volatile private var receiver: Array[Byte] => Unit = _ => ()
  @volatile private var closeListener: () => Unit = () => ()
  private val closedFired = new java.util.concurrent.atomic.AtomicBoolean(false)

  /** Register the message receiver (called on the event loop). */
  def onMessage(f: Array[Byte] => Unit): JamnpStream =
    receiver = f
    this

  /** Register a close/FIN listener. */
  def onClosed(f: () => Unit): JamnpStream =
    closeListener = f
    this

  private[network] def dispatch(message: Array[Byte]): Unit = receiver(message)

  /** Fires the close listener exactly once (remote FIN and channel-inactive
    * both signal closure; handlers must not run twice).
    */
  private[network] def dispatchClosed(): Unit =
    if closedFired.compareAndSet(false, true) then closeListener()

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

  def isOpen: Boolean = channel.isActive

/** A JAMNP connection to a single peer, wrapping a QUIC channel. */
final class JamnpConnection private[network] (
    private[network] val quic: QuicChannel,
    val initiator: Boolean
):
  /** The peer's raw Ed25519 public key, from its TLS certificate. */
  lazy val peerKey: Array[Byte] =
    val certs = quic.sslEngine().getSession.getPeerCertificates
    NodeIdentity.peerPublicKey(certs(0).asInstanceOf[X509Certificate])

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
  @volatile private var serverChannel: Channel = null

  def registerHandler(kind: Byte, handler: StreamHandler): JamnpNode =
    handlers.put(kind, handler)
    this

  /** Tracks accepted connections */
  @io.netty.channel.ChannelHandler.Sharable
  private final class ConnectionTracker extends ChannelInboundHandlerAdapter:
    override def channelActive(ctx: ChannelHandlerContext): Unit =
      val conn = new JamnpConnection(ctx.channel().asInstanceOf[QuicChannel], initiator = false)
      ctx.channel().attr(ConnectionKey).set(conn)
      connections.add(conn)
      ctx.fireChannelActive()
    override def channelInactive(ctx: ChannelHandlerContext): Unit =
      val conn = ctx.channel().attr(ConnectionKey).get()
      if conn != null then connections.remove(conn)
      ctx.fireChannelInactive()

  def boundPort: Int =
    serverChannel.localAddress().asInstanceOf[InetSocketAddress].getPort

  private def sslServer(): QuicSslContext =
    QuicSslContextBuilder
      .forServer(new IdentityKeyManagerFactory(identity), null)
      .trustManager(InsecureTrustManagerFactory.INSTANCE)
      .clientAuth(ClientAuth.REQUIRE)
      .applicationProtocols(config.alpn)
      .option(
        BoringSSLContextOption.SERVER_KEY_TYPES,
        Map("ECDHE_ECDSA" -> Ed25519KeyType).asJava
      )
      .option(BoringSSLContextOption.SIGNATURE_ALGORITHMS, Array("ed25519"))
      .build()

  private def sslClient(): QuicSslContext =
    QuicSslContextBuilder
      .forClient()
      .keyManager(new IdentityKeyManagerFactory(identity), null)
      .trustManager(InsecureTrustManagerFactory.INSTANCE)
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

    serverChannel = new Bootstrap()
      .group(group)
      .channel(classOf[NioDatagramChannel])
      .handler(codec)
      .bind(bindAddress)
      .sync()
      .channel()
    this

  /** Connect to a peer. The returned connection has completed the TLS
    * handshake (peer key available).
    */
  def connect(remote: InetSocketAddress): CompletableFuture[JamnpConnection] =
    val codec = new QuicClientCodecBuilder()
      .sslContext(sslClient())
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
              quic
                .closeFuture()
                .addListener(
                  new io.netty.channel.ChannelFutureListener {
                    override def operationComplete(cf: io.netty.channel.ChannelFuture): Unit =
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
