package io.forge.jam.crypto

import io.circe.{Decoder, parser}

import java.io.File
import java.nio.file.Files

/** Test vectors of the jam-crypto submodule (github.com/jam-conformance/jam-crypto). */
object JamCryptoVectors:

  lazy val root: File =
    val cwd = new File(System.getProperty("user.dir"))
    Iterator.iterate(cwd)(_.getParentFile).takeWhile(_ != null).map(new File(_, "jam-crypto"))
      .find(_.isDirectory).getOrElse(new File(cwd, "jam-crypto"))

  def file(path: String): File =
    val f = new File(root, path)
    require(f.isFile, s"Missing jam-crypto vector ${f.getPath}: run `git submodule update --init jam-crypto`")
    f

  def load[A: Decoder](path: String): A =
    parser.decode[A](new String(Files.readAllBytes(file(path).toPath), "UTF-8")).fold(e => throw e, identity)

  /** The `.json` files of a folder, sorted by name. */
  def jsonFiles(dir: String): List[String] =
    val d = new File(root, dir)
    require(d.isDirectory, s"Missing jam-crypto folder ${d.getPath}: run `git submodule update --init jam-crypto`")
    d.list().filter(_.endsWith(".json")).sorted.map(n => s"$dir/$n").toList

  def hex(s: String): Array[Byte] =
    s.stripPrefix("0x").grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
