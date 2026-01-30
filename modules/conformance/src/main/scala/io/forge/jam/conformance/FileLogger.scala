package io.forge.jam.conformance

import cats.effect.{IO, Resource}
import java.io.{BufferedWriter, FileWriter, PrintWriter, StringWriter}
import java.nio.file.Path
import java.time.{Instant, Duration}
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

/**
 * Simple append-only file logger for conformance testing sessions.
 *
 * @param echoToStderr If true, also print log messages to stderr for Docker visibility
 */
class FileLogger(
  private[conformance] val writer: BufferedWriter,
  sessionStart: AtomicReference[Instant],
  errorCount: AtomicLong,
  echoToStderr: Boolean = false
):
  private val timestampFormat = DateTimeFormatter.ISO_INSTANT

  private def writeAndEcho(msg: String): Unit =
    writer.write(msg)
    writer.flush()
    if echoToStderr then System.err.println(msg.stripSuffix("\n"))

  /**
   * Log a message with timestamp, direction, type, size, and key fields.
   */
  def log(direction: String, msgType: String, size: Int, keyFields: String = ""): IO[Unit] =
    IO.blocking {
      val timestamp = timestampFormat.format(Instant.now())
      val fields = if keyFields.nonEmpty then s" [$keyFields]" else ""
      writeAndEcho(s"[$timestamp] [$direction] $msgType (${size}b)$fields\n")
    }

  /**
   * Log a received message.
   */
  def logReceived(msgType: String, size: Int, keyFields: String = ""): IO[Unit] =
    log("RX", msgType, size, keyFields)

  /**
   * Log a sent message.
   */
  def logSent(msgType: String, size: Int, keyFields: String = ""): IO[Unit] =
    log("TX", msgType, size, keyFields)

  /**
   * Log session start.
   */
  def logSessionStart(): IO[Unit] =
    IO.blocking {
      val now = Instant.now()
      sessionStart.set(now)
      errorCount.set(0)
      val timestamp = timestampFormat.format(now)
      writeAndEcho(s"[$timestamp] [SESSION] Connected\n")
    }

  /**
   * Log session end with duration and error counts.
   */
  def logSessionEnd(): IO[Unit] =
    IO.blocking {
      val now = Instant.now()
      val start = sessionStart.get()
      val duration = if start != null then Duration.between(start, now) else Duration.ZERO
      val errors = errorCount.get()
      val timestamp = timestampFormat.format(now)
      writeAndEcho(s"[$timestamp] [SESSION] Disconnected (duration=${duration.toMillis}ms, errors=$errors)\n")
    }

  /**
   * Log an error with full stack trace.
   */
  def logError(message: String, error: Throwable): IO[Unit] =
    IO.blocking {
      errorCount.incrementAndGet()
      val timestamp = timestampFormat.format(Instant.now())
      val sw = new StringWriter()
      error.printStackTrace(new PrintWriter(sw))
      writeAndEcho(s"[$timestamp] [ERROR] $message\n$sw\n")
    }

  /**
   * Log a warning message.
   */
  def logWarning(message: String): IO[Unit] =
    IO.blocking {
      val timestamp = timestampFormat.format(Instant.now())
      writeAndEcho(s"[$timestamp] [WARN] $message\n")
    }

  /**
   * Log an info message.
   */
  def logInfo(message: String): IO[Unit] =
    IO.blocking {
      val timestamp = timestampFormat.format(Instant.now())
      writeAndEcho(s"[$timestamp] [INFO] $message\n")
    }

  /**
   * Close the writer.
   */
  def close(): Unit = writer.close()

object FileLogger:
  /**
   * Check if verbose logging is enabled via environment variable.
   */
  private def isVerbose: Boolean =
    sys.env.get("LOG_LEVEL").exists(level =>
      level.equalsIgnoreCase("DEBUG") || level.equalsIgnoreCase("INFO") || level.equalsIgnoreCase("TRACE")
    )

  /**
   * Create a FileLogger resource that manages the file lifecycle.
   * If LOG_LEVEL is DEBUG/INFO/TRACE, also echoes to stderr.
   */
  def resource(logPath: Path): Resource[IO, FileLogger] =
    Resource.make(
      IO.blocking {
        val writer = new BufferedWriter(new FileWriter(logPath.toFile, true))
        new FileLogger(
          writer,
          new AtomicReference[Instant](),
          new AtomicLong(0),
          echoToStderr = isVerbose
        )
      }
    ) { logger =>
      IO.blocking {
        logger.close()
      }
    }

  /**
   * Create a no-op logger for testing.
   */
  def noop: FileLogger =
    new FileLogger(
      new BufferedWriter(new StringWriter()),
      new AtomicReference[Instant](),
      new AtomicLong(0),
      echoToStderr = false
    )
