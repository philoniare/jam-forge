package io.forge.jam.node

import java.nio.file.{FileAlreadyExistsException, Files, Path, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import java.security.SecureRandom
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import io.forge.jam.network.NodeIdentity

object Keystore:

  val SeedBytes = 32

  final case class Seed(path: Path, secret: Array[Byte]):
    lazy val keys: ValidatorKeySet = ValidatorKeySet.fromSeed(secret)
    def peerId: String = NodeIdentity.altName(keys.ed25519Public)

  def dir(configDir: Path): Path = configDir.resolve("keys")

  def read(path: Path): Either[String, Seed] =
    if !Files.isRegularFile(path) then Left(s"$path: No such file or directory")
    else
      val bytes = Files.readAllBytes(path)
      if bytes.length != SeedBytes then
        Left(s"$path: Seed file is the wrong size: expected $SeedBytes bytes, read ${bytes.length} bytes")
      else Right(Seed(path, bytes))

  def load(keys: Path, required: Boolean): (List[Seed], List[String]) =
    if !Files.isDirectory(keys) then (Nil, if required then List(s"$keys: No such file or directory") else Nil)
    else
      val stream = Files.list(keys)
      val files =
        try stream.toArray.toList.collect { case p: Path if p.getFileName.toString.endsWith(".seed") => p }
        finally stream.close()
      val read = files.sortBy(_.getFileName.toString).map(this.read)
      (read.collect { case Right(s) => s }, read.collect { case Left(e) => e })

  def fileFor(keys: Path, name: Option[String], now: LocalDateTime): Path =
    val base = name.getOrElse(now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd-HH-mm-ss")))
    keys.resolve(if base.endsWith(".seed") then base else s"$base.seed")

  def create(path: Path): Either[String, Seed] =
    val seed = new Array[Byte](SeedBytes)
    new SecureRandom().nextBytes(seed)
    try
      Option(path.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
      val ownerOnly = PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
      try Files.createFile(path, ownerOnly)
      catch case _: UnsupportedOperationException => Files.createFile(path)
      Files.write(path, seed, StandardOpenOption.TRUNCATE_EXISTING)
      Right(Seed(path, seed))
    catch
      case _: FileAlreadyExistsException => Left(s"$path exists: not replacing a secret key (choose another name)")
      case e: java.io.IOException          => Left(s"$path: $e")

  def describe(seed: Seed): List[String] =
    List(s"Peer ID: ${seed.peerId}", s"Bandersnatch key: ${io.forge.jam.core.JamBytes(seed.keys.bandersnatchPublic).toHex.stripPrefix("0x")}")
