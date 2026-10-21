package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{ClosedChannelException, ServerSocketChannel, SocketChannel}
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

/** Fuzz-protocol server on a Unix domain socket, served from one dedicated platform thread with blocking NIO.
  *
  * Connections are served one after another, each as a fresh session (new [[StateStore]] and [[ProtocolHandler]]), so
  * nothing from an earlier fuzzer session, or from the warmup, is visible to the next one. [[close]] is safe to call
  * from a shutdown hook: it closes the listening and the current channel, which unblocks the server thread, and waits
  * for it to finish.
  */
final class FuzzServer private (
    serverChannel: ServerSocketChannel,
    val socketPath: Path,
    config: ChainConfig,
    logger: FileLogger
):
  @volatile private var closing = false
  @volatile private var current: SocketChannel = null
  @volatile private var failure: Option[Throwable] = None

  private val thread = new Thread(() => acceptLoop(), "jam-fuzz-server")

  private def acceptLoop(): Unit =
    try
      while !closing do
        val channel =
          try serverChannel.accept()
          catch case _: ClosedChannelException if closing => null
        if channel != null then
          current = channel
          try serve(channel)
          finally
            current = null
            closeQuietly(channel)
    catch
      case NonFatal(_) if closing => ()
      case e: Throwable =>
        failure = Some(e)
        throw e

  private def serve(channel: SocketChannel): Unit =
    logger.logSessionStart()
    try new ProtocolHandler(new StateStore(config), logger, config).handleConnection(channel)
    finally logger.logSessionEnd()

  /** Block until the server thread ends: after [[close]], or when it died from an error (see [[failed]]). */
  def awaitTermination(): Unit = thread.join()

  def isRunning: Boolean = thread.isAlive

  /** The error that stopped the server thread, if it stopped without [[close]]. */
  def failed: Option[Throwable] = failure

  /** Stop accepting, end the current session and wait up to `timeoutMs` for the server thread. Idempotent. */
  def close(timeoutMs: Long = 5000): Unit =
    closing = true
    closeQuietly(serverChannel)
    Option(current).foreach(closeQuietly)
    if Thread.currentThread() ne thread then thread.join(timeoutMs)
    try Files.deleteIfExists(socketPath)
    catch case NonFatal(_) => ()

  private def closeQuietly(c: java.io.Closeable): Unit =
    try c.close()
    catch case NonFatal(_) => ()

object FuzzServer:
  /** Bind `socketPath` (replacing a stale socket file) and start serving on a new thread. */
  def start(socketPath: Path, config: ChainConfig, logger: FileLogger): FuzzServer =
    Files.deleteIfExists(socketPath)
    val channel = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    try channel.bind(UnixDomainSocketAddress.of(socketPath))
    catch
      case e: Exception =>
        channel.close()
        throw e
    val server = new FuzzServer(channel, socketPath, config, logger)
    server.thread.start()
    server
