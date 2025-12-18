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
  debugBlockIndex: Int = -1, // Set to specific index to debug that block only
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
        if verbose then
          println(s"[$index] $messageType: PASS")
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
    val isDebugBlock = debugBlockIndex < 0 || currentTestIndex == debugBlockIndex

    if isDebugBlock then
      println(s"\n=== SCALA BLOCK $currentTestIndex (timeslot ${block.header.slot}) ===")
      println(s"  guarantees: ${block.extrinsic.guarantees.size}")
      println(s"  assurances: ${block.extrinsic.assurances.size}")
      println(s"  tickets: ${block.extrinsic.tickets.size}")
      println(s"  preimages: ${block.extrinsic.preimages.size}")
      for (g, i) <- block.extrinsic.guarantees.zipWithIndex do
        println(s"  guarantee[$i] core=${g.report.coreIndex} packageLength=${g.report.packageSpec.length}")
        for (r, j) <- g.report.results.zipWithIndex do
          println(
            s"    result[$j] gasUsed=${r.refineLoad.gasUsed} imports=${r.refineLoad.imports} exports=${r.refineLoad.exports}"
          )
      for (a, i) <- block.extrinsic.assurances.zipWithIndex do
        println(s"  assurance[$i] validator=${a.validatorIndex} bitfield=${a.bitfield.toHex}")

    stateStore.get(parentHash) match
      case None =>
        ProtocolMessage.ErrorMsg(Error(s"Parent state not found: ${parentHash.toHex.take(16)}..."))

      case Some(parentState) =>
        if verbose then
          println(s"  Parent state: ${parentState.keyvals.size} keyvals")
          val preByPrefix = parentState.keyvals.groupBy(kv => kv.key.toArray(0).toInt & 0xff)
          println(s"  Pre-state prefixes: ${preByPrefix.keys.toList.sorted.map(p => f"0x$p%02x").mkString(", ")}")
          // Show sizes for all keyvals
          println(s"  Pre-state sizes by prefix:")
          for prefix <- preByPrefix.keys.toList.sorted do
            val kvs = preByPrefix(prefix)
            println(f"    0x$prefix%02x: ${kvs.map(_.value.length).mkString(", ")} bytes")

        blockImporter.importBlock(block, parentState) match
          case ImportResult.Success(postState, _, _) =>
            // Always print statistics for debug block
            if isDebugBlock then
              val statsKv = postState.keyvals.find(kv => (kv.key.toArray(0).toInt & 0xff) == 0x0d)
              statsKv.foreach { kv =>
                println(s"  STATISTICS len=${kv.value.length} bytes")
                println(s"  STATISTICS FULL: ${kv.value.toHex}")
                // Parse and print field-by-field
                parseAndPrintStatistics(kv.value.toArray)
              }

            if verbose then
              println(s"  Post state: ${postState.keyvals.size} keyvals")
              val postByPrefix = postState.keyvals.groupBy(kv => kv.key.toArray(0).toInt & 0xff)
              println(s"  Post-state prefixes: ${postByPrefix.keys.toList.sorted.map(p => f"0x$p%02x").mkString(", ")}")
              // Show sizes for all keyvals
              println(s"  Post-state sizes by prefix:")
              for prefix <- postByPrefix.keys.toList.sorted do
                val kvs = postByPrefix(prefix)
                println(f"    0x$prefix%02x: ${kvs.map(_.value.length).mkString(", ")} bytes")

              // Dump all keyval hashes for comparison
              println(s"  === Full keyval dump ===")
              for kv <- postState.keyvals.sortBy(_.key.toHex) do
                val keyHash = Hashing.blake2b256(kv.key.toArray).toHex.take(16)
                val valueHash = Hashing.blake2b256(kv.value.toArray).toHex.take(16)
                println(
                  f"    key=${kv.key.toHex.take(16)}... len=${kv.value.length}%5d keyHash=$keyHash valueHash=$valueHash"
                )
                // For small values, show the full hex
                if kv.value.length <= 32 then
                  println(f"      value=${kv.value.toHex}")
                // For key 0x03 (history), dump full hex
                if (kv.key.toArray(0).toInt & 0xff) == 0x03 then
                  println(f"      HISTORY FULL: ${kv.value.toHex}")
                // For key 0x0d (statistics), dump full hex
                if (kv.key.toArray(0).toInt & 0xff) == 0x0d then
                  println(f"      STATISTICS FULL: ${kv.value.toHex}")

              // Compare keyvals
              val preMap = parentState.keyvals.map(kv => kv.key.toHex -> kv.value.toHex).toMap
              val postMap = postState.keyvals.map(kv => kv.key.toHex -> kv.value.toHex).toMap

              // Keys only in pre
              val removedKeys = preMap.keySet -- postMap.keySet
              if removedKeys.nonEmpty then
                println(s"  Removed keys: ${removedKeys.size}")
                removedKeys.take(3).foreach(k => println(s"    ${k.take(20)}..."))

              // Keys only in post
              val addedKeys = postMap.keySet -- preMap.keySet
              if addedKeys.nonEmpty then
                println(s"  Added keys: ${addedKeys.size}")
                addedKeys.take(3).foreach(k => println(s"    ${k.take(20)}..."))

              // Changed values
              val changedKeys = preMap.keySet.intersect(postMap.keySet).filter(k => preMap(k) != postMap(k))
              if changedKeys.nonEmpty then
                println(s"  Changed keys: ${changedKeys.size}")
                changedKeys.take(5).foreach { k =>
                  println(s"    ${k.take(20)}...")
                  println(s"      pre len:  ${preMap(k).length / 2}")
                  println(s"      post len: ${postMap(k).length / 2}")
                  // Show full hex for small values, truncated for large
                  val preHex = preMap(k)
                  val postHex = postMap(k)
                  println(s"      pre bytes:  ${preHex.take(400)}${if preHex.length > 400 then "..." else ""}")
                  println(s"      post bytes: ${postHex.take(600)}${if postHex.length > 600 then "..." else ""}")
                }

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
          println(s"[FAULTY MODE] Step 29: expected wrong root ${e.hash.toHex.take(32)}...")
          println(s"[FAULTY MODE] Step 29: actual correct root ${a.hash.toHex.take(32)}...")
          true // Accept mismatch as expected behavior
        else
          a.hash.bytes.sameElements(e.hash.bytes)

      case (ProtocolMessage.StateMsg(a), ProtocolMessage.StateMsg(e)) =>
        // In faulty mode, the GetState response may have different state
        // because our state is correct but the "faulty target" state is different
        if faultyMode then
          // Just check we returned a valid state (any state is acceptable in faulty mode)
          println(s"[FAULTY MODE] GetState: returned ${a.keyvals.size} keyvals")
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

    println("  --- Validator Accumulator (6 validators) ---")
    for i <- 0 until 6 do
      val blocks = readU32LE()
      val tickets = readU32LE()
      val preimages = readU32LE()
      val preimagesBytes = readU32LE()
      val guarantees = readU32LE()
      val assurances = readU32LE()
      if blocks > 0 || tickets > 0 || guarantees > 0 || assurances > 0 then
        println(
          s"    validator[$i]: blocks=$blocks tickets=$tickets preimages=$preimages preimagesBytes=$preimagesBytes guarantees=$guarantees assurances=$assurances"
        )

    println("  --- Validator Previous (6 validators) ---")
    for i <- 0 until 6 do
      val blocks = readU32LE()
      val tickets = readU32LE()
      val preimages = readU32LE()
      val preimagesBytes = readU32LE()
      val guarantees = readU32LE()
      val assurances = readU32LE()
      if blocks > 0 || tickets > 0 || guarantees > 0 || assurances > 0 then
        println(
          s"    validator[$i]: blocks=$blocks tickets=$tickets preimages=$preimages preimagesBytes=$preimagesBytes guarantees=$guarantees assurances=$assurances"
        )

    println(s"  --- Core Stats (2 cores, starting at byte $pos) ---")
    for i <- 0 until 2 do
      val startPos = pos
      val dataSize = readCompact()
      val assuranceCount = readCompact()
      val imports = readCompact()
      val extrinsicCount = readCompact()
      val extrinsicSize = readCompact()
      val exports = readCompact()
      val packageSize = readCompact()
      val gasUsed = readCompact()
      println(
        s"    core[$i] (bytes $startPos-$pos): dataSize=$dataSize assuranceCount=$assuranceCount imports=$imports extrinsicCount=$extrinsicCount extrinsicSize=$extrinsicSize exports=$exports packageSize=$packageSize gasUsed=$gasUsed"
      )

    println(s"  --- Service Stats (starting at byte $pos) ---")
    val serviceCount = readCompact()
    println(s"    service count: $serviceCount")
    for _ <- 0 until serviceCount.toInt do
      val serviceId = readU32LE()
      val preimagesCount = readCompact()
      val preimagesSize = readCompact()
      val refinesCount = readCompact()
      val refinesGas = readCompact()
      val importsCount = readCompact()
      val extrinsicCount = readCompact()
      val extrinsicSize = readCompact()
      val exportsCount = readCompact()
      val accumulatesCount = readCompact()
      val accumulatesGas = readCompact()
      val transfersCount = readCompact()
      val transfersGas = readCompact()
      println(
        s"    service[$serviceId]: preimages=($preimagesCount,$preimagesSize) refines=($refinesCount,$refinesGas) imports=$importsCount extrinsicCount=$extrinsicCount extrinsicSize=$extrinsicSize exports=$exportsCount accumulates=($accumulatesCount,$accumulatesGas) transfers=($transfersCount,$transfersGas)"
      )

    println(s"  --- End of statistics (consumed $pos of ${bytes.length} bytes) ---")
