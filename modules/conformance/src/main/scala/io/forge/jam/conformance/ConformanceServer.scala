package io.forge.jam.conformance

import cats.effect.{IO, IOApp, ExitCode, Resource}
import cats.syntax.all.*
import fs2.io.net.unixsocket.{UnixSocketAddress, UnixSockets}
import java.nio.file.{Files, Path, Paths}

/**
 * Configuration for the conformance testing server.
 *
 * @param socketPath Path to the Unix domain socket
 * @param logPath Path to the log file
 */
final case class ServerConfig(
  socketPath: Path = Paths.get("/tmp/jam_target.sock"),
  logPath: Path = Paths.get("/tmp/jam_conformance.log")
)

object ServerConfig:
  /**
   * Parse configuration from command line arguments.
   */
  def fromArgs(args: List[String]): Either[String, ServerConfig] =
    var config = ServerConfig()
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
              |Usage: ConformanceServerApp [options]
              |
              |Options:
              |  --socket-path <path>  Path to Unix domain socket (default: /tmp/jam_target.sock)
              |  --log-path <path>     Path to log file (default: /tmp/jam_conformance.log)
              |  --help                Show this help message
              |""".stripMargin
          )
        case unknown :: _ =>
          return Left(s"Unknown argument: $unknown")
        case Nil =>
          remaining = Nil

    Right(config)

/**
 * Main entry point for the JAM Forge Conformance Testing Server.
 */
object ConformanceServerApp extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    ServerConfig.fromArgs(args.toList) match
      case Left(msg) =>
        IO.println(msg).as(
          if msg.contains("Usage:") then ExitCode.Success else ExitCode.Error
        )
      case Right(config) =>
        runServer(config)

  private def runServer(config: ServerConfig): IO[ExitCode] =
    for
      _ <- IO.println(s"JAM Forge Conformance Testing Server v0.7.1")
      _ <- IO.println(s"Socket path: ${config.socketPath}")
      _ <- IO.println(s"Log path: ${config.logPath}")
      _ <- IO.println("Starting server...")

      // Clean up existing socket file if present
      _ <- IO.blocking {
        Files.deleteIfExists(config.socketPath)
      }

      // Run the server
      exitCode <- SocketServer.run(config)
    yield exitCode

/**
 * Unix domain socket server for handling conformance testing protocol.
 */
object SocketServer:

  /**
   * Run the conformance testing server.
   */
  def run(config: ServerConfig): IO[ExitCode] =
    serverResource(config).use { _ =>
      IO.println("Server started, waiting for connections...") *>
        IO.never[ExitCode]
    }.handleErrorWith { error =>
      IO.println(s"Server error: ${error.getMessage}") *>
        IO.pure(ExitCode.Error)
    }

  /**
   * Create a resource that manages the server lifecycle.
   */
  def serverResource(config: ServerConfig): Resource[IO, Unit] =
    for
      logger <- FileLogger.resource(config.logPath)
      stateStore = new StateStore()
      handler = new ProtocolHandler(stateStore, logger)
      _ <- acceptConnections(config.socketPath, handler, logger)
    yield ()

  private def acceptConnections(
    socketPath: Path,
    handler: ProtocolHandler,
    logger: FileLogger
  ): Resource[IO, Unit] =
    val unixSockets = UnixSockets.forIO
    unixSockets
      .server(UnixSocketAddress(socketPath.toString))
      .evalMap { socket =>
        logger.logSessionStart() *>
          handler.handleConnection(socket)
            .guarantee(logger.logSessionEnd())
            .handleErrorWith(error => logger.logError("Connection error", error))
      }
      .compile
      .drain
      .background
      .void
