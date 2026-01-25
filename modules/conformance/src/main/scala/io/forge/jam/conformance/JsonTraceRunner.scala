package io.forge.jam.conformance

import io.circe.{Decoder, parser}
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.types.block.Block
import io.forge.jam.protocol.traces.{BlockImporter, ImportResult, KeyValue, StateMerklization, TraceStep}

import java.io.File
import java.nio.file.{Files, Path}
import scala.util.Try

// Import Block JSON decoder (provides all needed decoders transitively)
import io.forge.jam.core.types.block.Block.given

/**
 * Result of processing a single JSON trace test case.
 */
sealed trait JsonTraceResult
object JsonTraceResult:
  case class Success(
    traceId: String,
    fileName: String,
    slot: Long
  ) extends JsonTraceResult

  case class Failure(
    traceId: String,
    fileName: String,
    slot: Long,
    expectedRoot: String,
    actualRoot: String,
    keyvalDiffs: Option[String] = None
  ) extends JsonTraceResult

  case class Error(
    traceId: String,
    fileName: String,
    errorMessage: String
  ) extends JsonTraceResult

/**
 * JSON-based trace runner for conformance tests.
 *
 * Processes JSON trace files that contain:
 * - pre_state: Initial state (state_root + keyvals)
 * - block: Block to import (header + extrinsic)
 * - post_state: Expected state after import (state_root + keyvals)
 *
 * @param config Chain configuration (default TINY)
 * @param verbose Enable verbose logging
 * @param compareKeyvals When true, also compare individual keyvals on failure
 */
class JsonTraceRunner(
  config: ChainConfig = ChainConfig.TINY,
  verbose: Boolean = false,
  compareKeyvals: Boolean = false
):
  // Create decoder for TraceStep using imported givens
  given Decoder[TraceStep] = TraceStep.decoder

  /**
   * Run all traces in a directory containing trace subdirectories.
   *
   * @param tracesBaseDir Base directory (e.g., fuzz-reports/0.7.1/traces/)
   * @param maxTraces Maximum number of trace directories to process (0 = all)
   * @param maxFilesPerTrace Maximum files per trace directory (0 = all)
   * @return List of test results
   */
  def runAllTraces(
    tracesBaseDir: Path,
    maxTraces: Int = 0,
    maxFilesPerTrace: Int = 0
  ): List[JsonTraceResult] =
    val traceSubdirs = Option(tracesBaseDir.toFile.listFiles())
      .getOrElse(Array.empty[File])
      .filter(_.isDirectory)
      .sortBy(_.getName)
      .toList

    val dirsToProcess = if maxTraces > 0 then traceSubdirs.take(maxTraces) else traceSubdirs

    dirsToProcess.flatMap(subdir => runTraceDirectory(subdir.toPath, maxFilesPerTrace))

  /**
   * Run all trace files in a single trace directory.
   *
   * @param traceDir Directory containing JSON trace files
   * @param maxFiles Maximum number of files to process (0 = all)
   * @return List of test results
   */
  def runTraceDirectory(traceDir: Path, maxFiles: Int = 0): List[JsonTraceResult] =
    val traceId = traceDir.getFileName.toString
    val jsonFiles = Option(traceDir.toFile.listFiles())
      .getOrElse(Array.empty[File])
      .filter(f => f.isFile && f.getName.endsWith(".json") && f.getName != "genesis.json")
      .sortBy(_.getName)
      .toList

    val filesToProcess = if maxFiles > 0 then jsonFiles.take(maxFiles) else jsonFiles

    filesToProcess.map(jsonFile => runSingleTrace(jsonFile.toPath, traceId))

  /**
   * Run a single trace file.
   *
   * @param jsonPath Path to the JSON trace file
   * @param traceId Optional trace ID (defaults to parent directory name)
   * @return Test result
   */
  def runSingleTrace(jsonPath: Path, traceId: String = ""): JsonTraceResult =
    val fileName = jsonPath.getFileName.toString.stripSuffix(".json")
    val effectiveTraceId = if traceId.nonEmpty then traceId else jsonPath.getParent.getFileName.toString

    try
      // Step 1: Load and parse JSON
      val content = new String(Files.readAllBytes(jsonPath), "UTF-8")
      val traceStep = parser.decode[TraceStep](content) match
        case Left(error) =>
          return JsonTraceResult.Error(effectiveTraceId, fileName, s"JSON parse error: ${error.getMessage}")
        case Right(step) => step

      val slot = traceStep.block.header.slot.value.toLong

      // Step 2: Verify pre-state root (sanity check)
      val computedPreRoot = StateMerklization.stateMerklize(traceStep.preState.keyvals)
      if computedPreRoot != traceStep.preState.stateRoot then
        return JsonTraceResult.Error(
          effectiveTraceId,
          fileName,
          s"Pre-state root mismatch: computed=${computedPreRoot.toHex.take(16)}..., expected=${traceStep.preState.stateRoot.toHex.take(16)}..."
        )

      // Step 3: Import block using BlockImporter
      val importer = new BlockImporter(config, skipAncestryValidation = true)
      val result = importer.importBlock(traceStep.block, traceStep.preState)

      // Step 4: Compare post-state
      result match
        case ImportResult.Success(actualPostState, _, _) =>
          if actualPostState.stateRoot == traceStep.postState.stateRoot then
            JsonTraceResult.Success(effectiveTraceId, fileName, slot)
          else
            val keyvalDiffs = if compareKeyvals then
              Some(computeKeyvalDiffs(traceStep.postState.keyvals, actualPostState.keyvals))
            else
              None

            if verbose then
              println(s"  [$fileName] FAIL - state root mismatch")
              println(s"    Expected: ${traceStep.postState.stateRoot.toHex}")
              println(s"    Actual:   ${actualPostState.stateRoot.toHex}")
              keyvalDiffs.foreach(d => println(s"    Diffs: $d"))

            JsonTraceResult.Failure(
              effectiveTraceId,
              fileName,
              slot,
              traceStep.postState.stateRoot.toHex,
              actualPostState.stateRoot.toHex,
              keyvalDiffs
            )

        case ImportResult.Failure(error, message) =>
          // If post_state == pre_state, the test expects block rejection (no state change)
          // In this case, a "failure" with unchanged state is actually correct
          if traceStep.postState.stateRoot == traceStep.preState.stateRoot then
            JsonTraceResult.Success(effectiveTraceId, fileName, slot)
          else
            if verbose then
              println(s"  [$fileName] ERROR - import failed: $error - $message")
            JsonTraceResult.Error(effectiveTraceId, fileName, s"Import failed: $error - $message")

    catch
      case e: Exception =>
        JsonTraceResult.Error(effectiveTraceId, fileName, s"Exception: ${e.getMessage}")

  /**
   * Compute differences between expected and actual keyvals.
   */
  private def computeKeyvalDiffs(expected: List[KeyValue], actual: List[KeyValue]): String =
    val expectedMap = expected.map(kv => kv.key.toHex -> kv.value.toHex).toMap
    val actualMap = actual.map(kv => kv.key.toHex -> kv.value.toHex).toMap

    val missingKeys = expectedMap.keySet -- actualMap.keySet
    val extraKeys = actualMap.keySet -- expectedMap.keySet
    val changedKeys = expectedMap.keySet.intersect(actualMap.keySet).filter(k => expectedMap(k) != actualMap(k))

    val sb = new StringBuilder()

    if missingKeys.nonEmpty then
      sb.append(s"missing=${missingKeys.size}")
      missingKeys.toList.sorted.take(5).foreach(k => sb.append(s"\n    MISSING: ${k.take(32)}..."))

    if extraKeys.nonEmpty then
      if sb.nonEmpty then sb.append(", ")
      sb.append(s"extra=${extraKeys.size}")
      extraKeys.toList.sorted.take(5).foreach(k => sb.append(s"\n    EXTRA: ${k.take(32)}..."))

    if changedKeys.nonEmpty then
      if sb.nonEmpty then sb.append(", ")
      sb.append(s"changed=${changedKeys.size}")
      changedKeys.toList.sorted.foreach { k =>
        val prefix = k.take(2)
        val expLen = expectedMap(k).length / 2
        val actLen = actualMap(k).length / 2
        sb.append(s"\n    CHANGED: 0x$prefix ${k.take(32)}... expLen=$expLen actLen=$actLen")
        // Show first bytes that differ
        val expBytes = expectedMap(k)
        val actBytes = actualMap(k)
        val firstDiffPos = expBytes.zip(actBytes).indexWhere { case (e, a) => e != a }
        if firstDiffPos >= 0 then
          val bytePos = firstDiffPos / 2
          sb.append(s" firstDiff@byte$bytePos")
          // Show first 64 bytes of expected and actual
          sb.append(s"\n      expected[0..64]: ${expBytes.take(128)}")
          sb.append(s"\n      actual[0..64]:   ${actBytes.take(128)}")
          // Also show bytes around the diff location
          val contextStart = Math.max(0, firstDiffPos - 32)
          val contextEnd = Math.min(expBytes.length, firstDiffPos + 32)
          sb.append(
            s"\n      expected[${contextStart / 2}..${contextEnd / 2}]: ${expBytes.slice(contextStart, contextEnd)}"
          )
          sb.append(
            s"\n      actual[${contextStart / 2}..${contextEnd / 2}]:   ${actBytes.slice(contextStart, contextEnd)}"
          )
      }

    if sb.isEmpty then "no differences detected (root mismatch may be encoding issue)"
    else sb.toString

/**
 * Companion object with utility methods.
 */
object JsonTraceRunner:

  /**
   * Lazily resolved path to the fuzz-reports traces directory.
   * Handles running from both project root and module subdirectories.
   */
  def findTracesDir(version: String = "0.7.1"): Option[Path] =
    val cwd = new File(System.getProperty("user.dir"))

    // Try various locations
    val candidates = List(
      new File(cwd, s"jam-conformance/fuzz-reports/$version/traces"),
      new File(cwd, s"../jam-conformance/fuzz-reports/$version/traces"),
      new File(cwd.getParentFile, s"jam-conformance/fuzz-reports/$version/traces")
    )

    candidates.find(_.exists()).map(_.toPath)

  /**
   * Get all trace directory IDs from the traces base directory.
   */
  def getTraceIds(tracesDir: Path): List[String] =
    Option(tracesDir.toFile.listFiles())
      .getOrElse(Array.empty[File])
      .filter(_.isDirectory)
      .map(_.getName)
      .sorted
      .toList

  /**
   * Load a single TraceStep from a JSON file.
   */
  def loadTraceStep(jsonPath: Path): Either[String, TraceStep] =
    // Import decoders
    given Decoder[TraceStep] = TraceStep.decoder

    Try {
      val content = new String(Files.readAllBytes(jsonPath), "UTF-8")
      parser.decode[TraceStep](content)
    }.toEither.left.map(_.getMessage).flatMap(_.left.map(_.getMessage))
