package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamKind}

import scala.collection.mutable

/** A JAMNP-S request/response on one CE stream: open, send one message, FIN, then every message the peer sends until
  * it closes. Blocks the calling thread up to `timeoutMs` for the open and again for the response, so it runs on I/O
  * threads, never on a refine or event-loop thread.
  */
object StreamRequest extends LazyLogging:

  /** The response messages; None when the peer reset, sent nothing, or did not answer in time. */
  def collect(conn: JamnpConnection, kind: Byte, payload: Array[Byte], timeoutMs: Long): Option[List[Array[Byte]]] =
    var stream: JamnpStream = null
    try
      stream = conn.openStream(kind).get(timeoutMs, TimeUnit.MILLISECONDS)
      val messages = mutable.ListBuffer.empty[Array[Byte]]
      val done = new CompletableFuture[List[Array[Byte]]]()
      stream.onMessage(m => messages.synchronized(messages += m))
      stream.onClosed(() => done.complete(messages.synchronized(messages.toList)))
      stream.send(payload)
      stream.finish()
      Some(done.get(timeoutMs, TimeUnit.MILLISECONDS)).filter(_.nonEmpty)
    catch
      case e: Exception =>
        logger.debug(s"request on ${StreamKind.name(kind)} failed: ${e.getMessage}")
        // The peer has not closed the stream (timeout, error): give it up on both sides.
        if stream != null then stream.reset()
        None
