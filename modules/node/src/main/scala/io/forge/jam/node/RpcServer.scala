package io.forge.jam.node

import java.net.InetSocketAddress
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.circe.Json
import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInitializer, ChannelOption, SimpleChannelInboundHandler, WriteBufferWaterMark}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http.{HttpObjectAggregator, HttpServerCodec}
import io.netty.handler.codec.http.websocketx.{TextWebSocketFrame, WebSocketServerProtocolHandler}

final class RpcServer(bind: InetSocketAddress, api: RpcApi, threads: NodeThreads = new NodeThreads) extends LazyLogging:
  import RpcServer.*

  private val io = new NioEventLoopGroup(
    1,
    (r: Runnable) => { val t = new Thread(r, "jam-rpc-io"); t.setDaemon(true); t }
  )
  private val worker = threads.scheduled("jam-rpc")
  private val sessions = new ConcurrentHashMap[Channel, RpcSession]()
  @volatile private var channel: Channel = null

  def start(): RpcServer =
    val bootstrap = new ServerBootstrap()
      .group(io)
      .channel(classOf[NioServerSocketChannel])
      .childOption(ChannelOption.WRITE_BUFFER_WATER_MARK, new WriteBufferWaterMark(MaxPendingBytes / 2, MaxPendingBytes))
      .childHandler(new ChannelInitializer[SocketChannel]:
        override def initChannel(ch: SocketChannel): Unit =
          if sessions.size >= MaxConnections then ch.close(): Unit
          else
            ch.pipeline()
              .addLast(new HttpServerCodec())
              .addLast(new HttpObjectAggregator(64 * 1024))
              .addLast(new WebSocketServerProtocolHandler("/", null, true, MaxMessageBytes, false, true))
              .addLast(new Connection(ch))
            ()
      )
    channel = bootstrap.bind(bind).sync().channel()
    worker.scheduleWithFixedDelay(() => refreshAll(), RefreshMillis, RefreshMillis, TimeUnit.MILLISECONDS)
    logger.info(s"RPC (JIP-2, WebSocket) on ${bind.getHostString}:$boundPort")
    this

  def boundPort: Int = channel.localAddress().asInstanceOf[InetSocketAddress].getPort

  private[node] def connectionCount: Int = sessions.size

  def stop(): Unit =
    if channel != null then channel.close().awaitUninterruptibly(2, TimeUnit.SECONDS)
    sessions.keySet.forEach(c => c.close(): Unit)
    io.shutdownGracefully(0, 2, TimeUnit.SECONDS).awaitUninterruptibly(3, TimeUnit.SECONDS)
    worker.shutdownNow()
    ()

  private def refreshAll(): Unit =
    try if !sessions.isEmpty then api.refresh(sessions.values.toArray(Array.empty[RpcSession]).toSeq)
    catch case e: Exception => logger.warn(s"RPC subscriptions: ${e.getMessage}")

  private def refreshOne(session: RpcSession): Unit =
    try worker.execute(() => api.refresh(List(session)))
    catch case _: java.util.concurrent.RejectedExecutionException => ()

  private final class Connection(ch: Channel) extends SimpleChannelInboundHandler[TextWebSocketFrame]:
    private val session = new RpcSession(send)

    private def send(message: Json): Unit =
      if !ch.isWritable then ch.close(): Unit
      else ch.writeAndFlush(new TextWebSocketFrame(message.noSpaces)): Unit

    override def handlerAdded(ctx: ChannelHandlerContext): Unit = sessions.put(ch, session): Unit
    override def channelInactive(ctx: ChannelHandlerContext): Unit =
      sessions.remove(ch)
      ctx.fireChannelInactive(): Unit

    override def channelRead0(ctx: ChannelHandlerContext, frame: TextWebSocketFrame): Unit =
      val text = frame.text()
      try
        worker.execute { () =>
          try
            api.handle(
              text,
              session,
              send,
              () => if !session.subscriptions.isEmpty then refreshOne(session)
            )
          catch case e: Exception => logger.warn(s"RPC request failed: ${e.getMessage}")
        }
      catch case _: java.util.concurrent.RejectedExecutionException => ch.close(): Unit // shutting down

    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit =
      logger.debug(s"RPC connection closed: ${cause.getMessage}")
      ctx.close(): Unit

object RpcServer:
  val RefreshMillis: Long = 200L

  val MaxMessageBytes: Int = 24 * 1024 * 1024

  val MaxPendingBytes: Int = 8 * 1024 * 1024

  val MaxConnections: Int = 64
