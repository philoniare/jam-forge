package io.forge.jam.protocol

import io.circe.{Decoder, parser}
import java.io.File
import java.nio.file.{Files, Paths}
import scala.util.Try

/**
 * Utility for loading test vectors from the jamtestvectors submodule.
 */
object TestFileLoader:

  /**
   * Lazily resolved path to the jamtestvectors directory.
   * Handles running from both project root and module subdirectories.
   */
  lazy val jamTestVectorsPath: File =
    val cwd = new File(System.getProperty("user.dir"))
    val testVectorsInCwd = new File(cwd, "jamtestvectors")
    if testVectorsInCwd.exists() then
      testVectorsInCwd
    else
      // Try parent directory (when running from a module subdirectory)
      new File(cwd.getParentFile, "jamtestvectors")

  /**
   * Checks if the jamtestvectors directory can be located.
   */
  def canLocateTestVectors: Boolean =
    jamTestVectorsPath.exists() && jamTestVectorsPath.isDirectory

  /**
   * Loads JSON data from the jamtestvectors submodule.
   *
   * @param subPath The path within jamtestvectors (e.g., "stf/history/tiny")
   * @param filename The name of the JSON file (without extension) to load.
   * @return Either an error message or the parsed value of type A.
   */
  def loadJsonFromTestVectors[A: Decoder](subPath: String, filename: String): Either[String, A] =
    val file = new File(jamTestVectorsPath, s"$subPath/$filename.json")
    if !file.exists() then
      Left(s"File not found: ${file.getAbsolutePath}")
    else
      Try {
        val content = new String(Files.readAllBytes(file.toPath), "UTF-8")
        parser.decode[A](content)
      }.toEither.left.map(_.getMessage).flatMap(_.left.map(_.getMessage))

  /**
   * Loads binary data from the jamtestvectors submodule.
   *
   * @param subPath The path within jamtestvectors (e.g., "stf/history/tiny")
   * @param filename The name of the binary file (without extension) to load.
   * @param fileExtension The file extension (default ".bin").
   * @return Either an error message or the binary data as Array[Byte].
   */
  def loadBinaryFromTestVectors(subPath: String, filename: String, fileExtension: String = ".bin"): Either[String, Array[Byte]] =
    val file = new File(jamTestVectorsPath, s"$subPath/$filename$fileExtension")
    if !file.exists() then
      Left(s"File not found: ${file.getAbsolutePath}")
    else
      Try(Files.readAllBytes(file.toPath)).toEither.left.map(_.getMessage)

  /**
   * Loads both JSON and binary data from the jamtestvectors submodule.
   *
   * @param subPath The path within jamtestvectors (e.g., "stf/history/tiny")
   * @param filename The name of the file (without extension) to load.
   * @param fileExtension The binary file extension (default ".bin").
   * @return Either an error message or a tuple containing (parsed JSON data, binary data).
   */
  def loadTestDataFromTestVectors[A: Decoder](
    subPath: String,
    filename: String,
    fileExtension: String = ".bin"
  ): Either[String, (A, Array[Byte])] =
    for
      json <- loadJsonFromTestVectors[A](subPath, filename)
      binary <- loadBinaryFromTestVectors(subPath, filename, fileExtension)
    yield (json, binary)

  /**
   * Gets all test filenames (without extension) from a directory in jamtestvectors.
   * Only includes files that have a .json extension.
   *
   * @param subPath The path within jamtestvectors (e.g., "stf/history/tiny")
   * @return Either an error message or list of filenames without extensions.
   */
  def getTestFilenamesFromTestVectors(subPath: String): Either[String, List[String]] =
    val dir = new File(jamTestVectorsPath, subPath)
    if !dir.exists() || !dir.isDirectory then
      Left(s"Directory not found: ${dir.getAbsolutePath}")
    else
      val files = Option(dir.listFiles())
        .getOrElse(Array.empty[File])
        .filter(f => f.isFile && f.getName.endsWith(".json"))
        .map(_.getName.stripSuffix(".json"))
        .toList
        .sorted
      Right(files)

  /**
   * Gets all trace step filenames (numbered files like 00000001) from a traces subfolder.
   * Excludes genesis.json.
   *
   * @param traceName The name of the trace folder (e.g., "fallback", "safrole")
   * @return Either an error message or list of filenames sorted numerically.
   */
  def getTraceStepFilenames(traceName: String): Either[String, List[String]] =
    val dir = new File(jamTestVectorsPath, s"traces/$traceName")
    if !dir.exists() || !dir.isDirectory then
      Left(s"Trace directory not found: ${dir.getAbsolutePath}")
    else
      val files = Option(dir.listFiles())
        .getOrElse(Array.empty[File])
        .filter(f => f.isFile && f.getName.endsWith(".json") && f.getName != "genesis.json")
        .map(_.getName.stripSuffix(".json"))
        .toList
        .sortBy(name => scala.util.Try(name.toInt).getOrElse(Int.MaxValue))
      Right(files)
