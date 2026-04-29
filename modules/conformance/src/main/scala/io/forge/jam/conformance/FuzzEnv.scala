package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig

import java.nio.file.{Files, Path, Paths}

/**
 * when `JAM_FUZZ` is defined, `JAM_FUZZ_SPEC`, `JAM_FUZZ_DATA_PATH`,
 * and `JAM_FUZZ_SOCK_PATH` MUST all be provided. When `JAM_FUZZ` is not defined the target operates
 * in normal mode and these variables are ignored.
 */
object FuzzEnv:

  def fromSystemEnv(): Either[String, Option[ServerConfig]] =
    fromEnv(name => Option(System.getenv(name)))

  def fromEnv(env: String => Option[String]): Either[String, Option[ServerConfig]] =
    env("JAM_FUZZ").filter(_.nonEmpty) match
      case None => Right(None)
      case Some(_) =>
        for
          spec <- required(env, "JAM_FUZZ_SPEC")
          dataPath <- required(env, "JAM_FUZZ_DATA_PATH")
          sockPath <- required(env, "JAM_FUZZ_SOCK_PATH")
          chain <- parseSpec(spec)
        yield
          env("JAM_FUZZ_LOG_LEVEL").filter(_.nonEmpty).foreach { lvl =>
            System.setProperty("LOG_LEVEL", lvl.toUpperCase)
          }
          Some(ServerConfig(
            socketPath = Paths.get(sockPath),
            logPath = ServerConfig().logPath,
            dataPath = Some(Paths.get(dataPath)),
            chainConfig = chain
          ))

  private def required(env: String => Option[String], name: String): Either[String, String] =
    env(name).filter(_.nonEmpty).toRight(
      s"JAM_FUZZ is set but required environment variable $name is missing or empty. " +
        "When JAM_FUZZ is defined, JAM_FUZZ_SPEC, JAM_FUZZ_DATA_PATH and JAM_FUZZ_SOCK_PATH must all be provided."
    )

  private def parseSpec(value: String): Either[String, ChainConfig] =
    value.trim.toLowerCase match
      case "tiny" => Right(ChainConfig.TINY)
      case "full" => Right(ChainConfig.FULL)
      case other => Left(s"JAM_FUZZ_SPEC must be 'tiny' or 'full', got: '$other'")

  def ensureDataPath(path: Path): Unit =
    if !Files.exists(path) then Files.createDirectories(path)
