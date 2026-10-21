package io.forge.jam.conformance

import java.io.{BufferedWriter, FileWriter, PrintWriter, StringWriter}
import java.nio.file.{Files, Path}
import java.time.{Instant, Duration}
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.util.control.NonFatal

/**
 * Simple append-only file logger for conformance testing sessions.
 *
 * @param echoToStderr If true, also print log messages to stderr for Docker visibility
 * @param verbose      If true, per-message INFO/RX/TX lines are written to the file
 * @param logPath      Backing file path; if None, rotation is disabled (e.g. the no-op logger)
 */
class FileLogger(
  initialWriter: BufferedWriter,
  sessionStart: AtomicReference[Instant],
  errorCount: AtomicLong,
  echoToStderr: Boolean = false,
  verbose: Boolean = false,
  logPath: Option[Path] = None
):
  private val timestampFormat = DateTimeFormatter.ISO_INSTANT

  // Reassigned on rotation (reopening the file requires a new writer / file descriptor).
  private[conformance] var writer: BufferedWriter = initialWriter

  // Track bytes written so we can rotate before the file grows unbounded.
  private val bytesWritten = new AtomicLong(0)

  /**
   * Write a log line.
   *
   * @param msg   The line to write (newline-terminated).
   * @param gated If true, the file write is skipped unless verbose logging is enabled
   *              (errors/warnings/session markers pass false so they are always kept).
   * @param flush If true, flush immediately (used for errors); otherwise rely on
   *              buffered flushing / close to persist.
   */
  private def writeAndEcho(msg: String, gated: Boolean = true, flush: Boolean = false): Unit =
    if !gated || verbose then
      synchronized {
        writer.write(msg)
        if flush then writer.flush()
        if bytesWritten.addAndGet(msg.length.toLong) >= FileLogger.maxLogBytes then rotate()
      }
    if echoToStderr then System.err.println(msg.stripSuffix("\n"))

  private def rotate(): Unit =
    logPath match
      case Some(path) =>
        try
          writer.flush()
          writer.close()
          val backup = path.resolveSibling(path.getFileName.toString + ".1")
          Files.deleteIfExists(backup)
          if Files.exists(path) then Files.move(path, backup)
          writer = new BufferedWriter(new FileWriter(path.toFile, false))
          bytesWritten.set(0)
        catch
          case NonFatal(_) =>
            // Recovery: ensure we still have a working writer rather than a closed one.
            try writer = new BufferedWriter(new FileWriter(path.toFile, true))
            catch case NonFatal(_) => ()
      case None => ()

  /** True when gated (per-message) lines go anywhere; when false, logging them returns before building the line. */
  private def gatedEnabled: Boolean = verbose || echoToStderr

  /**
   * Log a message with timestamp, direction, type, size, and key fields.
   */
  def log(direction: String, msgType: String, size: Int, keyFields: => String = ""): Unit =
    if gatedEnabled then
      val timestamp = timestampFormat.format(Instant.now())
      val fields = keyFields
      val suffix = if fields.nonEmpty then s" [$fields]" else ""
      writeAndEcho(s"[$timestamp] [$direction] $msgType (${size}b)$suffix\n")

  /**
   * Log a received message.
   */
  def logReceived(msgType: String, size: Int, keyFields: => String = ""): Unit =
    log("RX", msgType, size, keyFields)

  /**
   * Log a sent message.
   */
  def logSent(msgType: String, size: Int, keyFields: => String = ""): Unit =
    log("TX", msgType, size, keyFields)

  /**
   * Log session start.
   */
  def logSessionStart(): Unit =
    val now = Instant.now()
    sessionStart.set(now)
    errorCount.set(0)
    val timestamp = timestampFormat.format(now)
    writeAndEcho(s"[$timestamp] [SESSION] Connected\n", gated = false)

  /**
   * Log session end with duration and error counts.
   */
  def logSessionEnd(): Unit =
    val now = Instant.now()
    val start = sessionStart.get()
    val duration = if start != null then Duration.between(start, now) else Duration.ZERO
    val errors = errorCount.get()
    val timestamp = timestampFormat.format(now)
    writeAndEcho(
      s"[$timestamp] [SESSION] Disconnected (duration=${duration.toMillis}ms, errors=$errors)\n",
      gated = false,
      flush = true
    )

  /**
   * Log an error with full stack trace.
   */
  def logError(message: String, error: Throwable): Unit =
    errorCount.incrementAndGet()
    val timestamp = timestampFormat.format(Instant.now())
    val sw = new StringWriter()
    error.printStackTrace(new PrintWriter(sw))
    writeAndEcho(s"[$timestamp] [ERROR] $message\n$sw\n", gated = false, flush = true)

  /**
   * Log a warning message.
   */
  def logWarning(message: String): Unit =
    val timestamp = timestampFormat.format(Instant.now())
    writeAndEcho(s"[$timestamp] [WARN] $message\n", gated = false)

  /**
   * Log an info message.
   */
  def logInfo(message: => String): Unit =
    if gatedEnabled then
      val timestamp = timestampFormat.format(Instant.now())
      writeAndEcho(s"[$timestamp] [INFO] $message\n")

  /**
   * Close the writer.
   */
  def close(): Unit = synchronized { writer.flush(); writer.close() }

object FileLogger:
  /** Rotate the log once it grows past this many bytes (default 16 MiB). */
  val maxLogBytes: Long = 16L * 1024 * 1024

  /**
   * Check if verbose logging is enabled
   */
  private[conformance] def isVerbose: Boolean =
    sys.props.get("LOG_LEVEL").orElse(sys.env.get("LOG_LEVEL")).exists(level =>
      level.equalsIgnoreCase("DEBUG") || level.equalsIgnoreCase("INFO") || level.equalsIgnoreCase("TRACE")
    )

  /**
   * Open a FileLogger appending to `logPath`; the caller closes it. If LOG_LEVEL is DEBUG/INFO/TRACE, also echoes
   * to stderr.
   */
  def open(logPath: Path): FileLogger =
    val verbose = isVerbose
    val writer = new BufferedWriter(new FileWriter(logPath.toFile, true))
    new FileLogger(
      writer,
      new AtomicReference[Instant](),
      new AtomicLong(0),
      echoToStderr = verbose,
      verbose = verbose,
      logPath = Some(logPath)
    )

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
