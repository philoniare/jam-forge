package io.forge.jam.network

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import io.netty.bootstrap.Bootstrap
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.{ChannelHandler, ChannelHandlerContext, ChannelInboundHandlerAdapter, ChannelInitializer}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioDatagramChannel
import io.netty.handler.ssl.ClientAuth
import io.netty.handler.ssl.util.{InsecureTrustManagerFactory, SimpleKeyManagerFactory}
import io.netty.incubator.codec.quic.{
  BoringSSLContextOption,
  InsecureQuicTokenHandler,
  QuicChannel,
  QuicClientCodecBuilder,
  QuicServerCodecBuilder,
  QuicSslContextBuilder,
  QuicStreamChannel,
  QuicStreamType
}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.net.Socket
import java.security.{KeyStore, Principal, PrivateKey}
import java.security.cert.X509Certificate
import javax.net.ssl.{KeyManager, SSLEngine, X509ExtendedKeyManager}
import scala.jdk.CollectionConverters.*

class QuicTransportSpikeSpec extends AnyFunSuite with Matchers:

  private val Alpn = "jamnp-s/0/deadbeef"
  private val Ed25519KeyType = "Ed25519"

  /** Key manager that always serves the node's Ed25519 identity. */
  private final class IdentityKeyManager(id: NodeIdentity)
      extends X509ExtendedKeyManager:
    private val alias = "jamnp"
    override def chooseEngineServerAlias(
        keyType: String,
        issuers: Array[Principal],
        engine: SSLEngine
    ): String = alias
    override def chooseEngineClientAlias(
        keyTypes: Array[String],
        issuers: Array[Principal],
        engine: SSLEngine
    ): String = alias
    override def chooseServerAlias(
        keyType: String,
        issuers: Array[Principal],
        socket: Socket
    ): String = alias
    override def chooseClientAlias(
        keyTypes: Array[String],
        issuers: Array[Principal],
        socket: Socket
    ): String = alias
    override def getServerAliases(keyType: String, issuers: Array[Principal]): Array[String] =
      Array(alias)
    override def getClientAliases(keyType: String, issuers: Array[Principal]): Array[String] =
      Array(alias)
    override def getCertificateChain(a: String): Array[X509Certificate] =
      Array(id.certificate)
    override def getPrivateKey(a: String): PrivateKey = id.privateKey

  private final class IdentityKeyManagerFactory(id: NodeIdentity)
      extends SimpleKeyManagerFactory:
    override def engineInit(keyStore: KeyStore, password: Array[Char]): Unit = ()
    override def engineInit(
        spec: javax.net.ssl.ManagerFactoryParameters
    ): Unit = ()
    override def engineGetKeyManagers(): Array[KeyManager] =
      Array(new IdentityKeyManager(id))

  test("Ed25519 identity produces a valid self-signed cert with the JAMNP alt-name") {
    val id = NodeIdentity.generate()
    id.publicKeyBytes.length shouldBe 32
    id.altName.length shouldBe 53
    id.altName.head shouldBe 'e'
    id.altName.tail.forall(c => "abcdefghijklmnopqrstuvwxyz234567".contains(c)) shouldBe true
    // Self-signature verifies with its own key.
    id.certificate.verify(id.keyPair.getPublic)
    // SAN carries the alt-name.
    val sans = id.certificate.getSubjectAlternativeNames
    sans should not be null
    sans.iterator().next().get(1) shouldBe id.altName
    // Raw key extraction round-trips through the certificate.
    NodeIdentity.peerPublicKey(id.certificate) shouldBe id.publicKeyBytes
  }

  test("QUIC mTLS handshake with Ed25519 certs, ALPN negotiation, and stream echo") {
    val serverId = NodeIdentity.generate()
    val clientId = NodeIdentity.generate()

    val group = new NioEventLoopGroup(2)
    try
      // ---- Server ----
      val serverSsl = QuicSslContextBuilder
        .forServer(new IdentityKeyManagerFactory(serverId), null)
        .trustManager(InsecureTrustManagerFactory.INSTANCE)
        .clientAuth(ClientAuth.REQUIRE)
        .applicationProtocols(Alpn)
        .option(
          BoringSSLContextOption.SERVER_KEY_TYPES,
          Map("ECDHE_ECDSA" -> Ed25519KeyType).asJava
        )
        // Advertise/accept the ed25519 signature scheme (not in BoringSSL's
        // defaults).
        .option(BoringSSLContextOption.SIGNATURE_ALGORITHMS, Array("ed25519"))
        .build()

      val serverCodec = new QuicServerCodecBuilder()
        .sslContext(serverSsl)
        .maxIdleTimeout(5000, TimeUnit.MILLISECONDS)
        .initialMaxData(10000000)
        .initialMaxStreamDataBidirectionalLocal(1000000)
        .initialMaxStreamDataBidirectionalRemote(1000000)
        .initialMaxStreamsBidirectional(100)
        .tokenHandler(InsecureQuicTokenHandler.INSTANCE)
        .handler(new ChannelInboundHandlerAdapter()) // per-connection
        .streamHandler(new ChannelInitializer[QuicStreamChannel]() {
          override def initChannel(ch: QuicStreamChannel): Unit =
            ch.pipeline().addLast(new ChannelInboundHandlerAdapter() {
              override def channelRead(ctx: ChannelHandlerContext, msg: Object): Unit =
                // Echo whatever arrives back on the same stream.
                ctx.writeAndFlush(msg)
            })
        })
        .build()

      val serverChannel = new Bootstrap()
        .group(group)
        .channel(classOf[NioDatagramChannel])
        .handler(serverCodec)
        .bind(new InetSocketAddress("127.0.0.1", 0))
        .sync()
        .channel()
      val serverPort =
        serverChannel.localAddress().asInstanceOf[InetSocketAddress].getPort

      // ---- Client ----
      val clientSsl = QuicSslContextBuilder
        .forClient()
        .keyManager(new IdentityKeyManagerFactory(clientId), null)
        .trustManager(InsecureTrustManagerFactory.INSTANCE)
        .applicationProtocols(Alpn)
        .option(
          BoringSSLContextOption.CLIENT_KEY_TYPES,
          Set(Ed25519KeyType).asJava
        )
        .option(BoringSSLContextOption.SIGNATURE_ALGORITHMS, Array("ed25519"))
        .build()

      val clientCodec = new QuicClientCodecBuilder()
        .sslContext(clientSsl)
        .maxIdleTimeout(5000, TimeUnit.MILLISECONDS)
        .initialMaxData(10000000)
        .initialMaxStreamDataBidirectionalLocal(1000000)
        .build()

      val clientChannel = new Bootstrap()
        .group(group)
        .channel(classOf[NioDatagramChannel])
        .handler(clientCodec)
        .bind(0)
        .sync()
        .channel()

      val quicChannel = QuicChannel
        .newBootstrap(clientChannel)
        .handler(new ChannelInboundHandlerAdapter())
        .streamHandler(new ChannelInboundHandlerAdapter())
        .remoteAddress(new InetSocketAddress("127.0.0.1", serverPort))
        .connect()
        .get(10, TimeUnit.SECONDS)

      // ALPN negotiated the jamnp-s protocol.
      val engine = quicChannel.sslEngine()
      engine should not be null
      engine.getApplicationProtocol shouldBe Alpn

      // The server presented its Ed25519 certificate; extract the raw key.
      val peerCerts = engine.getSession.getPeerCertificates
      peerCerts should not be empty
      val serverKey = NodeIdentity.peerPublicKey(
        peerCerts(0).asInstanceOf[java.security.cert.X509Certificate]
      )
      serverKey shouldBe serverId.publicKeyBytes

      // ---- Stream: JAMNP framing (kind byte + 32-bit LE length + content) ----
      val received = new java.util.concurrent.LinkedBlockingQueue[Array[Byte]]()
      val stream = quicChannel
        .createStream(
          QuicStreamType.BIDIRECTIONAL,
          new ChannelInboundHandlerAdapter() {
            override def channelRead(ctx: ChannelHandlerContext, msg: Object): Unit =
              val buf = msg.asInstanceOf[ByteBuf]
              val out = new Array[Byte](buf.readableBytes())
              buf.readBytes(out)
              buf.release()
              received.offer(out)
          }
        )
        .sync()
        .getNow

      val payload = "hello jamnp".getBytes("UTF-8")
      val frame = Unpooled.buffer()
      frame.writeByte(0) // UP stream kind 0
      frame.writeIntLE(payload.length)
      frame.writeBytes(payload)
      stream.writeAndFlush(frame).sync()

      // Collect the echoed bytes (possibly split across reads).
      val expectedLen = 1 + 4 + payload.length
      val echoed = new java.io.ByteArrayOutputStream()
      val deadline = System.currentTimeMillis() + 10000
      while echoed.size < expectedLen && System.currentTimeMillis() < deadline do
        val chunk = received.poll(1000, TimeUnit.MILLISECONDS)
        if chunk != null then echoed.write(chunk)

      val bytes = echoed.toByteArray
      bytes.length shouldBe expectedLen
      bytes(0) shouldBe 0
      java.nio.ByteBuffer
        .wrap(bytes, 1, 4)
        .order(java.nio.ByteOrder.LITTLE_ENDIAN)
        .getInt shouldBe payload.length
      bytes.drop(5) shouldBe payload

      quicChannel.close().sync()
      clientChannel.close().sync()
      serverChannel.close().sync()
    finally group.shutdownGracefully(0, 1, TimeUnit.SECONDS).sync()
  }
