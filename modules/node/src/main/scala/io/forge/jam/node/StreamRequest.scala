package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, TimeUnit, TimeoutException}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.network.{PeerConnection, MessageStream, StreamKind}

import scala.collection.mutable

/** A JAMNP-S request/response on one CE stream: open, send the request messages, FIN, then the peer's answer. Blocks
  * the calling thread for the open and again for the answer, so it runs on I/O threads, never on a refine or
  * event-loop thread.
  */
object StreamRequest extends LazyLogging:

  /** Why an exchange brought no answer. The stream, when it was opened, has been reset. */
  enum Failure:
    /** The stream did not open in time, or opening it failed. */
    case NotOpened(detail: String)
    /** Sending the request failed. */
    case NotSent(detail: String)
    /** The peer neither answered nor closed in time. */
    case TimedOut
    case Refused
    case Excess(detail: String)

    def message: String = this match
      case NotOpened(detail) => s"stream not opened: $detail"
      case NotSent(detail)   => s"request not sent: $detail"
      case TimedOut          => "no answer in time"
      case Refused           => "answer refused: over our inbound buffer limit"
      case Excess(detail)    => s"answer refused: $detail"

  /** When the answer is complete. */
  enum Until:
    /** The peer closed the stream: every message it sent, up to `maxMessages`. */
    case Closed
    /** The first message, or the peer closing without one. The peer's FIN may still be outstanding. */
    case FirstMessage

  /** Send `request` and return the peer's answer, which is empty when it closed the stream without one. Every
    * outcome but a complete answer resets the stream, so neither side keeps it: a failure, and an empty answer.
    *
    * @param responseTimeoutMs
    *   read once the request is sent, so a caller with one deadline for both waits can pass what is left of it
    * @param maxMessages
    *   answer messages kept; a peer that sends more is not buffered without limit
    */
  def exchange(
      conn: PeerConnection,
      kind: Byte,
      request: Seq[Array[Byte]],
      openTimeoutMs: Long,
      responseTimeoutMs: () => Long,
      until: Until = Until.Closed,
      maxMessages: Int = DefaultMaxMessages,
      excessFails: Boolean = false,
      maxBytes: Long = Long.MaxValue,
      onSent: () => Unit = () => ()
  ): Either[Failure, List[Array[Byte]]] =
    val opened =
      try
        if openTimeoutMs <= 0 then Left(Failure.NotOpened("no time left"))
        else Right(conn.openStream(kind).get(openTimeoutMs, TimeUnit.MILLISECONDS))
      catch case e: Exception => Left(Failure.NotOpened(describe(e)))
    opened.flatMap { stream =>
      val result = answer(stream, request, responseTimeoutMs, until, maxMessages, excessFails, maxBytes, onSent)
      if !result.exists(_.nonEmpty) then
        try stream.reset()
        catch case e: Exception => logger.debug(s"reset of ${StreamKind.name(kind)} failed: ${e.getMessage}")
      result
    } match
      case l @ Left(failure) =>
        logger.debug(s"request on ${StreamKind.name(kind)} failed: ${failure.message}")
        l
      case r => r

  private def answer(
      stream: MessageStream,
      request: Seq[Array[Byte]],
      responseTimeoutMs: () => Long,
      until: Until,
      maxMessages: Int,
      excessFails: Boolean,
      maxBytes: Long,
      onSent: () => Unit
  ): Either[Failure, List[Array[Byte]]] =
    val messages = mutable.ListBuffer.empty[Array[Byte]]
    val done = new CompletableFuture[List[Array[Byte]]]()
    var received = 0L // under `messages`
    val excess = new java.util.concurrent.atomic.AtomicReference[Option[String]](None)
    def snapshot(): List[Array[Byte]] = messages.synchronized(messages.toList)
    stream.onMessage { m =>
      val over = messages.synchronized {
        if excess.get.isDefined then None
        else
          received += m.length
          val tooMany = excessFails && messages.size >= maxMessages
          if tooMany then Some(s"more than $maxMessages message(s)")
          else if received > maxBytes then Some(s"more than $maxBytes byte(s)")
          else
            if messages.size < maxMessages then messages += m
            None
      }
      over match
        case Some(why) =>
          excess.set(Some(why))
          done.complete(Nil): Unit
        case None =>
          if until == Until.FirstMessage then done.complete(snapshot()): Unit
    }
    stream.onClosed(() => done.complete(snapshot()): Unit)
    val sent =
      try
        request.foreach(stream.send)
        stream.finish()
        Right(())
      catch case e: Exception => Left(Failure.NotSent(describe(e)))
    sent.foreach(_ => onSent())
    sent.flatMap { _ =>
      try
        val wait = responseTimeoutMs()
        if wait <= 0 then Left(Failure.TimedOut)
        else
          val answer = done.get(wait, TimeUnit.MILLISECONDS)
          excess.get match
            case Some(why)                => Left(Failure.Excess(why))
            case None if stream.refusedHere => Left(Failure.Refused)
            case None                     => Right(answer)
      catch
        case _: TimeoutException => Left(Failure.TimedOut)
        case e: Exception        => Left(Failure.NotSent(describe(e)))
    }

  private def describe(e: Exception): String = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)

  val DefaultMaxMessages: Int = 4096

  /** The response messages; None when the peer reset, sent nothing, or did not answer in time. */
  def collect(conn: PeerConnection, kind: Byte, payload: Array[Byte], timeoutMs: Long): Option[List[Array[Byte]]] =
    exchange(conn, kind, Seq(payload), timeoutMs, () => timeoutMs).toOption.filter(_.nonEmpty)

  def collectOrWhy(
      conn: PeerConnection,
      kind: Byte,
      payload: Array[Byte],
      timeoutMs: Long,
      onSent: () => Unit
  ): Either[String, List[Array[Byte]]] =
    exchange(conn, kind, Seq(payload), timeoutMs, () => timeoutMs, onSent = onSent) match
      case Left(failure)                 => Left(failure.message)
      case Right(answer) if answer.isEmpty => Left("refused: no answer")
      case Right(answer)                 => Right(answer)
