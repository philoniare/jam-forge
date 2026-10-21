package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig
import java.nio.file.{Path, Paths}
import scala.util.control.NonFatal

/** Configuration for the conformance testing server.
  *
  * @param socketPath
  *   Path to the Unix domain socket
  * @param logPath
  *   Path to the log file
  * @param dataPath
  *   Optional persistence directory
  * @param chainConfig
  *   Chain spec (tiny/full)
  */
final case class ServerConfig(
    socketPath: Path = Paths.get("/tmp/jam_target.sock"),
    logPath: Path = Paths.get("/tmp/jam_conformance.log"),
    dataPath: Option[Path] = None,
    chainConfig: ChainConfig
)

object ServerConfig:
  /** Parse configuration from command line arguments.
    */
  def fromArgs(args: List[String]): Either[String, ServerConfig] =
    var config = ServerConfig(chainConfig = ChainConfig.TINY)
    var remaining = args

    while remaining.nonEmpty do
      remaining match
        case "--socket-path" :: path :: tail =>
          config = config.copy(socketPath = Paths.get(path))
          remaining = tail
        case "--log-path" :: path :: tail =>
          config = config.copy(logPath = Paths.get(path))
          remaining = tail
        case "--help" :: _ =>
          return Left(
            """JAM Forge Conformance Testing Server
              |
              |Usage:
              |  ConformanceServerApp [options]
              |  ConformanceServerApp fuzz <socket-path> [--spec tiny|full|fuzz]
              |  ConformanceServerApp warmup-and-serve --socket-path <path> [--spec tiny|full|fuzz]
              |
              |Options:
              |  --socket-path <path>  Path to Unix domain socket (default: /tmp/jam_target.sock)
              |  --log-path <path>     Path to log file (default: /tmp/jam_conformance.log)
              |  --spec tiny|full|fuzz Chain spec for fuzz/warmup-and-serve (default: tiny)
              |  --help                Show this help message
              |""".stripMargin
          )
        case unknown :: _ =>
          return Left(s"Unknown argument: $unknown")
        case Nil =>
          remaining = Nil

    Right(config)

/** Main entry point for the JAM Forge Conformance Testing Server.
  *
  * Commands: warmup - Run JVM warmup and trigger CRaC checkpoint; fuzz <socket-path> [--spec
  * tiny|full|fuzz] - Start conformance server on Unix socket (default: tiny); (default) - Start server with
  * --socket-path/--log-path options. With `JAM_FUZZ` set, the fuzz packaging env decides and the arguments are ignored.
  *
  * A plain `main`: the server is one blocking thread ([[FuzzServer]]), and SIGTERM stops it from a shutdown hook.
  */
object ConformanceServerApp:

  def main(args: Array[String]): Unit =
    // Install JVM-level uncaught exception handler so crashes are always visible
    Thread.setDefaultUncaughtExceptionHandler { (thread, throwable) =>
      throwable match
        case _: OutOfMemoryError =>
          System.err.println(
            s"[JAM-FORGE FATAL OOM] thread=${thread.getName} class=${throwable.getClass.getSimpleName} msg=${throwable.getMessage}. Heap dump at /tmp/jam_oom.hprof if enabled."
          )
        case _ =>
          System.err.println(
            s"[JAM-FORGE FATAL] Uncaught exception in thread ${thread.getName}: ${throwable.getClass.getSimpleName} - ${throwable.getMessage}"
          )
          throwable.printStackTrace(System.err)
    }

    val exitCode = FuzzEnv.fromSystemEnv() match
      case Left(err) =>
        println(s"[JAM-FORGE FATAL] $err")
        1
      case Right(Some(cfg)) =>
        cfg.dataPath.foreach(FuzzEnv.ensureDataPath)
        WarmupRunner.warmupIfEnabled()
        runServer(cfg)
      case Right(None) =>
        runFromArgs(args.toList)
    // Exiting with 0 is left to the JVM: when the server stopped from the shutdown hook, System.exit would block.
    if exitCode != 0 then sys.exit(exitCode)

  private def runFromArgs(args: List[String]): Int =
    args match
      // CRaC warmup command (warm + checkpoint + exit)
      case "warmup" :: _ =>
        WarmupRunner.run()
        0

      // CRaC warm-and-serve: warm on all traces, checkpoint, post-restore
      // start fuzz server. Args: --socket-path <p> [--spec tiny|full|fuzz]
      case "warmup-and-serve" :: rest =>
        val socketPath = rest
          .sliding(2)
          .collectFirst { case List("--socket-path", p) => Paths.get(p) }
          .getOrElse(Paths.get("/tmp/jam_target.sock"))
        parseSpec(rest) match
          case Left(msg) =>
            println(msg)
            1
          case Right(chainConfig) =>
            WarmupRunner.warmupAndCheckpoint(socketPath)
            runServer(ServerConfig(socketPath = socketPath, chainConfig = chainConfig))

      // Fuzz command (used by conformance harness). Args: <socket-path> [--spec tiny|full|fuzz]
      case "fuzz" :: socketPath :: rest =>
        parseSpec(rest) match
          case Left(msg) =>
            println(msg)
            1
          case Right(chainConfig) =>
            runServer(ServerConfig(socketPath = Paths.get(socketPath), chainConfig = chainConfig))

      // Legacy/default: parse --socket-path, --log-path options
      case _ =>
        ServerConfig.fromArgs(args) match
          case Left(msg) =>
            println(msg)
            if msg.contains("Usage:") then 0 else 1
          case Right(config) =>
            runServer(config)

  /** Parse an optional `--spec tiny|full|fuzz` flag, defaulting to TINY. */
  private def parseSpec(args: List[String]): Either[String, ChainConfig] =
    args.sliding(2).collectFirst { case List("--spec", v) => v } match
      case None    => Right(ChainConfig.TINY)
      case Some(v) => ChainConfig.fromName(v).left.map(_.capitalize)

  /** Protocol version string derived from the wire-level Version.JAM_VERSION. */
  private val jamVersionString: String =
    val v = Version.JAM_VERSION
    s"${v.major}.${v.minor}.${v.patch}"

  /** Serve until SIGTERM (exit code 0) or until the server thread dies from an error (1). */
  private def runServer(config: ServerConfig): Int =
    val specName = ChainConfig.nameOf(config.chainConfig)
    println(s"JAM Forge Conformance Testing Server v$jamVersionString")
    println(s"Socket path: ${config.socketPath}")
    println(s"Log path: ${config.logPath}")
    println(s"Chain spec: $specName")
    config.dataPath.foreach(p => println(s"Data path: $p"))
    println("Starting server...")

    val logger = FileLogger.open(config.logPath)
    val server =
      try FuzzServer.start(config.socketPath, config.chainConfig, logger)
      catch
        case NonFatal(error) =>
          println(
            s"[JAM-FORGE ERROR] Server error: ${error.getClass.getSimpleName} - ${Option(error.getMessage).getOrElse("unknown")}"
          )
          error.printStackTrace(System.err)
          logger.close()
          return 1
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      System.err.println("[JAM-FORGE] Shutting down (if this is unexpected, check for OOM / cgroup kill).")
      server.close()
      logger.close()
      System.err.flush()
    }, "jam-shutdown"))
    println("Server started, waiting for connections...")

    server.awaitTermination()
    server.failed match
      case Some(error) =>
        println(s"[JAM-FORGE ERROR] Server stopped: ${error.getClass.getSimpleName} - ${error.getMessage}")
        1
      case None => 0
