package io.forge.jam.node

import java.net.InetSocketAddress
import java.util.concurrent.{ConcurrentHashMap, RejectedExecutionException, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

import com.typesafe.scalalogging.LazyLogging
import io.circe.Json
import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.{Channel, ChannelFuture, ChannelFutureListener, ChannelHandlerContext, ChannelInboundHandlerAdapter, ChannelInitializer, ChannelOption, SimpleChannelInboundHandler, WriteBufferWaterMark}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http.{DefaultFullHttpResponse, HttpHeaderNames, HttpObjectAggregator, HttpRequest, HttpResponseStatus, HttpServerCodec, HttpUtil, HttpVersion}
import io.netty.handler.codec.http.websocketx.{TextWebSocketFrame, WebSocketFrameAggregator, WebSocketServerProtocolHandler}
import io.netty.util.ReferenceCountUtil


final class RpcServer(
    bind: InetSocketAddress,
    api: RpcApi,
    threads: NodeThreads = new NodeThreads,
    allowedOrigins: Set[String] = Set.empty,
    methods: RpcMethods = RpcMethods.Auto,
    limits: RpcServer.Limits = RpcServer.Limits()
) extends LazyLogging:
  import RpcServer.*

  private val io = new NioEventLoopGroup(
    1,
    (r: Runnable) => { val t = new Thread(r, "jam-rpc-io"); t.setDaemon(true); t }
  )
  private val worker = threads.scheduled("jam-rpc")
  private val sessions = new ConcurrentHashMap[Channel, RpcSession]()
  private val origins = allowedOrigins.map(_.toLowerCase)
  private val unsafeAllowed = Option(bind.getAddress).exists(RpcMethods.allowsUnsafe(methods, _))
  private val clients = new RpcServer.Clients(limits)

  private val refusing = new java.util.concurrent.atomic.AtomicInteger()

  private val queuedTotal = new AtomicLong()
  @volatile private var channel: Channel = null

  def start(): RpcServer =
    val bootstrap = new ServerBootstrap()
      .group(io)
      .channel(classOf[NioServerSocketChannel])
      .childOption(ChannelOption.WRITE_BUFFER_WATER_MARK, new WriteBufferWaterMark(MaxPendingBytes / 2, MaxPendingBytes))
      .childHandler(new ChannelInitializer[SocketChannel]:
        override def initChannel(ch: SocketChannel): Unit =
          val address = Option(ch.remoteAddress()).map(_.getAddress).getOrElse(java.net.InetAddress.getLoopbackAddress)
          val refusal =
            if sessions.size >= MaxConnections then Some(HttpResponseStatus.SERVICE_UNAVAILABLE)
            else if !clients.connect(address) then Some(HttpResponseStatus.TOO_MANY_REQUESTS)
            else None
          refusal match
            case Some(_) if refusing.get >= MaxConnections =>
              ch.close(): Unit
            case Some(status) =>
              logger.debug(s"RPC connection from ${address.getHostAddress} refused: $status")
              refusing.incrementAndGet()
              ch.closeFuture().addListener(new ChannelFutureListener:
                def operationComplete(f: ChannelFuture): Unit = refusing.decrementAndGet(): Unit
              )
              ch.pipeline().addLast(new HttpServerCodec()).addLast(new Refuse(status))
              ch.eventLoop().schedule((() => ch.close(): Unit): Runnable, RefusalLingerSeconds, TimeUnit.SECONDS): Unit
            case None =>
              ch.closeFuture().addListener(new ChannelFutureListener:
                def operationComplete(f: ChannelFuture): Unit = clients.disconnect(address)
              )
              ch.pipeline()
                .addLast(new HttpServerCodec())
                .addLast(new HttpObjectAggregator(64 * 1024))
                .addLast(new OriginCheck)
                .addLast(new WebSocketServerProtocolHandler("/", null, true, MaxMessageBytes, false, true))
                .addLast(new WebSocketFrameAggregator(MaxMessageBytes))
                .addLast(new Connection(ch, address))
              ()
      )
    channel = bootstrap.bind(bind).sync().channel()
    worker.scheduleWithFixedDelay(refreshTask(refreshAll()), RefreshMillis, RefreshMillis, TimeUnit.MILLISECONDS)
    logger.info(
      s"RPC (JIP-2, WebSocket) on ${bind.getHostString}:$boundPort, " +
        s"${if unsafeAllowed then "all methods" else "safe methods only"} (--rpc-methods ${methods.toString.toLowerCase})"
    )
    if unsafeAllowed && !Option(bind.getAddress).exists(_.isLoopbackAddress) then
      logger.warn(
        s"the RPC on ${bind.getHostString} serves the unsafe methods (work-package submission, segment recovery) " +
          "to any client that can reach it"
      )
    this

  def boundPort: Int = channel.localAddress().asInstanceOf[InetSocketAddress].getPort

  private[node] def connectionCount: Int = sessions.size

  private[node] def onWorker(task: Runnable): Unit = worker.execute(task)

  private[node] def queuedBytesTotal: Long = queuedTotal.get

  @volatile private[node] var peakQueuedRequests: Int = 0
  @volatile private[node] var peakQueuedBytes: Long = 0L

  def stop(): Unit =
    if channel != null then channel.close().awaitUninterruptibly(2, TimeUnit.SECONDS)
    sessions.keySet.forEach(c => c.close(): Unit)
    io.shutdownGracefully(0, 2, TimeUnit.SECONDS).awaitUninterruptibly(3, TimeUnit.SECONDS)
    worker.shutdownNow()
    ()

  private val refreshTask = new Periodic("RPC subscriptions refresh")

  private def refreshAll(): Unit =
    if !sessions.isEmpty then api.refresh(sessions.values.toArray(Array.empty[RpcSession]).toSeq)

  private def refreshOne(session: RpcSession): Unit =
    try worker.execute(() => api.refresh(List(session)))
    catch case _: RejectedExecutionException => ()

  private final class Refuse(status: HttpResponseStatus) extends ChannelInboundHandlerAdapter:
    override def channelRead(ctx: ChannelHandlerContext, msg: Any): Unit =
      ReferenceCountUtil.release(msg)
      msg match
        case _: HttpRequest =>
          val refused = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, status)
          refused.headers().set(HttpHeaderNames.RETRY_AFTER, RetryAfterSeconds.toString)
          HttpUtil.setContentLength(refused, 0)
          ctx.writeAndFlush(refused).addListener(ChannelFutureListener.CLOSE): Unit
        case _ => ()

  private final class OriginCheck extends ChannelInboundHandlerAdapter:
    override def channelRead(ctx: ChannelHandlerContext, msg: Any): Unit = msg match
      case request: HttpRequest =>
        val origin = request.headers().get(HttpHeaderNames.ORIGIN)
        if origin != null && !origins.contains(origin.toLowerCase) then
          ReferenceCountUtil.release(msg)
          val refused = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.FORBIDDEN)
          HttpUtil.setContentLength(refused, 0)
          ctx.writeAndFlush(refused).addListener(ChannelFutureListener.CLOSE): Unit
        else
          ctx.pipeline().remove(this)
          ctx.fireChannelRead(msg): Unit
      case _ => ctx.fireChannelRead(msg): Unit

  private final class Connection(ch: Channel, address: java.net.InetAddress)
      extends SimpleChannelInboundHandler[TextWebSocketFrame]:
    private val session = new RpcSession(send, unsafeAllowed)

    private var queued = 0
    private var queuedBytes = 0L
    private val held = new java.util.ArrayDeque[(String, Long, Boolean)]()
    private var closed = false

    private def send(message: Json): Unit =
      if !ch.isWritable then ch.close(): Unit
      else ch.writeAndFlush(new TextWebSocketFrame(message.noSpaces)): Unit

    override def handlerAdded(ctx: ChannelHandlerContext): Unit = sessions.put(ch, session): Unit
    override def channelInactive(ctx: ChannelHandlerContext): Unit =
      sessions.remove(ch)
      synchronized { closed = true; held.clear() }
      ctx.fireChannelInactive(): Unit

    override def channelRead0(ctx: ChannelHandlerContext, frame: TextWebSocketFrame): Unit =
      val request = (frame.text(), frame.content().readableBytes().toLong, !clients.request(address))
      synchronized { if !closed then held.add(request): Unit; admit() }

    private def admit(): Unit = synchronized {
      while !held.isEmpty && !closed && (queued == 0 ||
          (queued < MaxQueuedRequests && queuedBytes + held.peek._2 <= MaxQueuedRequestBytes)) do
        val (text, size, overRate) = held.poll()
        queued += 1
        queuedBytes += size
        queuedTotal.addAndGet(size)
        if queued > peakQueuedRequests then peakQueuedRequests = queued
        if queuedBytes > peakQueuedBytes then peakQueuedBytes = queuedBytes
        submit(text, size, overRate)
      val full = !held.isEmpty || queued >= MaxQueuedRequests || queuedBytes >= MaxQueuedRequestBytes
      val overTotal = queued > 0 && queuedTotal.get > MaxQueuedBytesTotal
      ch.config().setAutoRead(!closed && !full && !overTotal): Unit
    }

    private def release(size: Long): Unit = synchronized {
      queued -= 1
      queuedBytes -= size
      queuedTotal.addAndGet(-size)
      admit()
    }

    private def submit(text: String, size: Long, overRate: Boolean): Unit =
      val released = new AtomicBoolean(false)
      def done(): Unit = if released.compareAndSet(false, true) then release(size)
      try
        worker.execute { () =>
          try
            if !ch.isActive then done()
            else if overRate then
              RpcApi.refusal(text, RpcError.limitExceeded).foreach(send)
              done()
            else
              api.handle(
                text,
                session,
                send,
                () =>
                  done()
                  if !session.subscriptions.isEmpty then refreshOne(session)
              )
          catch
            case e: Exception =>
              done()
              logger.warn(s"RPC request failed: ${e.getMessage}")
        }
      catch
        case _: RejectedExecutionException => // shutting down
          closed = true
          done()
          ch.close(): Unit

    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit =
      logger.debug(s"RPC connection closed: ${cause.getMessage}")
      ctx.close(): Unit

enum RpcMethods:
  case Safe, Unsafe, Auto

object RpcMethods:
  def parse(value: String): Option[RpcMethods] = values.find(_.toString.equalsIgnoreCase(value))

  def allowsUnsafe(methods: RpcMethods, address: java.net.InetAddress): Boolean = methods match
    case Safe   => false
    case Unsafe => true
    case Auto   => address.isLoopbackAddress

object RpcServer:
  final case class Limits(connectionsPerIp: Int = 16, requestsPerSecond: Double = 100.0, burst: Int = 200)

  private[node] final class Clients(limits: Limits, nanos: () => Long = () => System.nanoTime()):
    private final class Client(var connections: Int, var tokens: Double, var at: Long)
    private val byAddress = scala.collection.mutable.HashMap.empty[java.net.InetAddress, Client]

    private def refill(c: Client, now: Long): Unit =
      c.tokens = math.min(limits.burst.toDouble, c.tokens + (now - c.at) / 1e9 * limits.requestsPerSecond)
      c.at = now

    def connect(address: java.net.InetAddress): Boolean = synchronized {
      val now = nanos()
      byAddress.filterInPlace { (_, c) =>
        c.connections > 0 || { refill(c, now); c.tokens < limits.burst }
      }
      val c = byAddress.getOrElse(address, new Client(0, limits.burst.toDouble, now))
      if c.connections >= limits.connectionsPerIp then false
      else
        c.connections += 1
        byAddress.update(address, c)
        true
    }

    def disconnect(address: java.net.InetAddress): Unit = synchronized {
      byAddress.get(address).foreach(c => c.connections = math.max(0, c.connections - 1))
    }

    def request(address: java.net.InetAddress): Boolean = synchronized {
      byAddress.get(address) match
        case None => true
        case Some(c) =>
          refill(c, nanos())
          if c.tokens >= 1.0 then
            c.tokens -= 1.0
            true
          else false
    }

  private val RefusalLingerSeconds: Long = 5L
  private val RetryAfterSeconds: Int = 1

  val RefreshMillis: Long = 200L

  val MaxMessageBytes: Int = 24 * 1024 * 1024

  val MaxPendingBytes: Int = 8 * 1024 * 1024

  val MaxConnections: Int = 64

  val MaxQueuedRequests: Int = 16

  val MaxQueuedRequestBytes: Int = 32 * 1024 * 1024

  val MaxQueuedBytesTotal: Long = 128L * 1024 * 1024
