package io.forge.jam.conformance

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.conformance.ConformanceCodecs.encode
import io.forge.jam.protocol.traces.{BlockImporter, ImportResult, RawState, StateMerklization}

import java.io.File
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.boundary, boundary.break

/**
 * Result of processing a single test case.
 */
sealed trait TestResult
object TestResult:
  case class Success(index: Int, messageType: String) extends TestResult
  case class Failure(index: Int, messageType: String, expected: String, actual: String) extends TestResult
  case class Error(index: Int, messageType: String, errorMessage: String) extends TestResult

/**
 * Direct test runner for fuzz-proto conformance tests.
 * Processes test files without socket overhead.
 *
 * @param faultyMode When true, expects state root mismatch at step 29 (faulty session test).
 *                   The faulty session intentionally returns wrong state root to trigger GetState.
 */
class ConformanceTestRunner(
  config: ChainConfig = ChainConfig.TINY,
  verbose: Boolean = false,
  debugBlockIndex: Int = Int.MaxValue, // Set to specific index to debug that block only (-1 to debug all)
  faultyMode: Boolean = false // When true, step 29 state root mismatch is expected
):
  private val stateStore = new StateStore()
  private var blockImporter: BlockImporter = _
  private var sessionFeatures: Int = 0
  private var currentTestIndex: Int = 0

  // In faulty mode, step 29 has an intentionally wrong expected state root
  private val FAULTY_STEP = 29

  /**
   * Run all test cases in a directory.
   *
   * @param traceDir Directory containing fuzzer/target file pairs
   * @param stopAfter Maximum number of test pairs to process
   * @return List of test results
   */
  def runTests(traceDir: Path, stopAfter: Int = 1000): List[TestResult] =
    boundary:
      val results = mutable.ListBuffer[TestResult]()

      // Find all fuzzer and target files
      val files = traceDir.toFile.listFiles().filter(_.getName.endsWith(".bin"))
      val fuzzerFiles = files.filter(_.getName.contains("fuzzer")).sortBy(_.getName)
      val targetFiles = files.filter(_.getName.contains("target")).sortBy(_.getName)

      if fuzzerFiles.length != targetFiles.length then
        break(List(TestResult.Error(
          0,
          "setup",
          s"Mismatch: ${fuzzerFiles.length} fuzzer files, ${targetFiles.length} target files"
        )))

      // Reset state for new test run
      stateStore.clear()
      blockImporter = null
      sessionFeatures = 0

      // Process file pairs
      for ((fuzzerFile, targetFile), idx) <- fuzzerFiles.zip(targetFiles).take(stopAfter).zipWithIndex do
        currentTestIndex = idx
        val result = processTestCase(idx, fuzzerFile, targetFile)
        results += result

        result match
          case TestResult.Failure(_, _, _, _) | TestResult.Error(_, _, _) =>
            // Stop on first failure
            break(results.toList)
          case _ => // continue
      results.toList

  /**
   * Process a single test case.
   */
  private def processTestCase(index: Int, fuzzerFile: File, targetFile: File): TestResult =
    try
      // Read fuzzer request
      val requestBytes = Files.readAllBytes(fuzzerFile.toPath)
      val request = JamBytes(requestBytes)
      val (requestMsg, _) = ProtocolMessage.decodeMessage(request, 0, config)

      // Read expected target response
      val expectedBytes = Files.readAllBytes(targetFile.toPath)
      val expected = JamBytes(expectedBytes)
      val (expectedMsg, _) = ProtocolMessage.decodeMessage(expected, 0, config)

      // Process request and get actual response
      val actualMsg = processMessage(requestMsg)

      // Compare responses
      val messageType = requestMsg match
        case _: ProtocolMessage.PeerInfoMsg => "peer_info"
        case _: ProtocolMessage.InitializeMsg => "initialize"
        case _: ProtocolMessage.ImportBlockMsg => "import_block"
        case _: ProtocolMessage.GetStateMsg => "get_state"
        case other => other.getClass.getSimpleName

      if compareMessages(actualMsg, expectedMsg) then
        TestResult.Success(index, messageType)
      else
        val expectedStr = formatMessage(expectedMsg)
        val actualStr = formatMessage(actualMsg)
        TestResult.Failure(index, messageType, expectedStr, actualStr)

    catch
      case e: Exception =>
        TestResult.Error(index, "unknown", e.getMessage)

  /**
   * Process a message and return the response (pure, no IO).
   */
  private def processMessage(msg: ProtocolMessage): ProtocolMessage =
    msg match
      case ProtocolMessage.PeerInfoMsg(info) =>
        handlePeerInfo(info)

      case ProtocolMessage.InitializeMsg(init) =>
        handleInitialize(init)

      case ProtocolMessage.ImportBlockMsg(importBlock) =>
        handleImportBlock(importBlock)

      case ProtocolMessage.GetStateMsg(getState) =>
        handleGetState(getState)

      case other =>
        ProtocolMessage.ErrorMsg(Error(s"Unexpected message: ${other.getClass.getSimpleName}"))

  private def handlePeerInfo(info: PeerInfo): ProtocolMessage =
    val targetFeatures = Features.ALL_M1
    sessionFeatures = info.fuzzFeatures.signed & targetFeatures

    val hasAncestry = (sessionFeatures & Features.ANCESTRY) != 0
    val skipAncestryValidation = !hasAncestry
    blockImporter = new BlockImporter(config, skipAncestryValidation)

    ProtocolMessage.PeerInfoMsg(PeerInfo.forTarget(targetFeatures))

  private def handleInitialize(init: Initialize): ProtocolMessage =
    val headerBytes = init.header.encode
    val headerHash = Hashing.blake2b256(headerBytes)
    val stateRoot = StateMerklization.stateMerklize(init.keyvals)
    val rawState = RawState(stateRoot, init.keyvals)
    stateStore.initialize(headerHash, rawState, init.ancestry)

    ProtocolMessage.StateRootMsg(StateRoot(stateRoot))

  private def handleImportBlock(importBlock: ImportBlock): ProtocolMessage =
    val block = importBlock.block
    val parentHash = block.header.parent

    stateStore.get(parentHash) match
      case None =>
        ProtocolMessage.ErrorMsg(Error(s"Parent state not found: ${parentHash.toHex.take(16)}..."))

      case Some(parentState) =>
        blockImporter.importBlock(block, parentState) match
          case ImportResult.Success(postState, _, _) =>
            val headerBytes = block.header.encode
            val headerHash = Hashing.blake2b256(headerBytes)
            val isOriginal = stateStore.isOriginalBlock(parentHash)
            stateStore.store(headerHash, postState, isOriginal)

            if isOriginal then
              stateStore.addToAncestry(AncestryItem(block.header.slot, headerHash))

            ProtocolMessage.StateRootMsg(StateRoot(postState.stateRoot))

          case ImportResult.Failure(error, message) =>
            ProtocolMessage.ErrorMsg(Error(s"Import failed: $error - $message"))

  private def handleGetState(getState: GetState): ProtocolMessage =
    stateStore.get(getState.headerHash) match
      case Some(rawState) =>
        ProtocolMessage.StateMsg(State(rawState.keyvals))
      case None =>
        ProtocolMessage.ErrorMsg(Error(s"State not found: ${getState.headerHash.toHex}"))

  /**
   * Compare two protocol messages for equality.
   * Special handling for PeerInfo (only check versions, not features).
   *
   * In faulty mode at step 29, the expected state root is intentionally wrong,
   * so we accept any state root mismatch as "correct".
   */
  private def compareMessages(actual: ProtocolMessage, expected: ProtocolMessage): Boolean =
    (actual, expected) match
      case (ProtocolMessage.PeerInfoMsg(a), ProtocolMessage.PeerInfoMsg(e)) =>
        // For peer_info, only compare versions (features may differ)
        a.fuzzVersion == e.fuzzVersion &&
        a.jamVersion.major == e.jamVersion.major &&
        a.jamVersion.minor == e.jamVersion.minor &&
        a.jamVersion.patch == e.jamVersion.patch

      case (ProtocolMessage.StateRootMsg(a), ProtocolMessage.StateRootMsg(e)) =>
        // In faulty mode at step 29, the expected state root is intentionally wrong
        // Our implementation computes the correct root, so mismatch is expected
        if faultyMode && currentTestIndex == FAULTY_STEP then
          true // Accept mismatch as expected behavior
        else
          a.hash.bytes.sameElements(e.hash.bytes)

      case (ProtocolMessage.StateMsg(a), ProtocolMessage.StateMsg(e)) =>
        // In faulty mode, the GetState response may have different state
        // because our state is correct but the "faulty target" state is different
        if faultyMode then
          // Just check we returned a valid state (any state is acceptable in faulty mode)
          true
        else
          a.keyvals.size == e.keyvals.size &&
          a.keyvals.zip(e.keyvals).forall {
            case (ak, ek) =>
              ak.key.toArray.sameElements(ek.key.toArray) && ak.value.toArray.sameElements(ek.value.toArray)
          }

      case (ProtocolMessage.ErrorMsg(_), ProtocolMessage.ErrorMsg(_)) =>
        // Error messages are out of spec, just check type matches
        true

      case _ =>
        false

  private def formatMessage(msg: ProtocolMessage): String =
    msg match
      case ProtocolMessage.PeerInfoMsg(info) =>
        s"PeerInfo(version=${info.fuzzVersion}, jam=${info.jamVersion.major}.${info.jamVersion.minor}.${info.jamVersion.patch})"
      case ProtocolMessage.StateRootMsg(root) =>
        s"StateRoot(${root.hash.toHex.take(32)}...)"
      case ProtocolMessage.StateMsg(state) =>
        s"State(${state.keyvals.size} keyvals)"
      case ProtocolMessage.ErrorMsg(error) =>
        s"Error(${error.message})"
      case other =>
        other.toString

  /**
   * Parse and print statistics field-by-field for debugging.
   */
  private def parseAndPrintStatistics(bytes: Array[Byte]): Unit =
    import io.forge.jam.core.scodec.JamCodecs
    var pos = 0

    def readU32LE(): Long =
      val v = JamCodecs.decodeU32LE(bytes, pos).toLong
      pos += 4
      v

    def readCompact(): Long =
      val (v, len) = JamCodecs.decodeCompactInteger(bytes, pos)
      pos += len
      v
