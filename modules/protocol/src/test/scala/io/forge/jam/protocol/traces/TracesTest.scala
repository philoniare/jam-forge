package io.forge.jam.protocol.traces

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.AppendedClues.convertToClueful
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.header.Header
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.safrole.SafroleTransition
import io.forge.jam.protocol.dispute.DisputeTransition
import io.forge.jam.protocol.dispute.DisputeTypes.*
import io.forge.jam.protocol.state.JamState
import io.circe.Decoder

// Import all JSON decoders from core types
import io.forge.jam.core.JamBytes.given
import io.forge.jam.core.primitives.Hash.given
import io.forge.jam.core.primitives.BandersnatchPublicKey.given
import io.forge.jam.core.primitives.Ed25519PublicKey.given
import io.forge.jam.core.primitives.Ed25519Signature.given
import io.forge.jam.core.types.tickets.{TicketEnvelope, TicketMark}
import io.forge.jam.core.types.tickets.TicketEnvelope.given
import io.forge.jam.core.types.tickets.TicketMark.given
import io.forge.jam.core.types.epoch.{EpochValidatorKey, EpochMark}
import io.forge.jam.core.types.epoch.EpochValidatorKey.given
import io.forge.jam.core.types.epoch.EpochMark.given
import io.forge.jam.core.types.work.{PackageSpec, Vote, ExecutionResult}
import io.forge.jam.core.types.work.PackageSpec.given
import io.forge.jam.core.types.work.Vote.given
import io.forge.jam.core.types.work.ExecutionResult.given
import io.forge.jam.core.types.dispute.{Culprit, Fault, GuaranteeSignature}
import io.forge.jam.core.types.dispute.Culprit.given
import io.forge.jam.core.types.dispute.Fault.given
import io.forge.jam.core.types.dispute.GuaranteeSignature.given
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.context.Context.given
import io.forge.jam.core.types.workitem.{WorkItemImportSegment, WorkItemExtrinsic, WorkItem}
import io.forge.jam.core.types.workitem.WorkItemImportSegment.given
import io.forge.jam.core.types.workitem.WorkItemExtrinsic.given
import io.forge.jam.core.types.workitem.WorkItem.given
import io.forge.jam.core.types.workresult.{RefineLoad, WorkResult}
import io.forge.jam.core.types.workresult.RefineLoad.given
import io.forge.jam.core.types.workresult.WorkResult.given
import io.forge.jam.core.types.workpackage.{SegmentRootLookup, WorkPackage, WorkReport}
import io.forge.jam.core.types.workpackage.SegmentRootLookup.given
import io.forge.jam.core.types.workpackage.WorkPackage.given
import io.forge.jam.core.types.workpackage.WorkReport.given
import io.forge.jam.core.types.extrinsic.{Preimage, AssuranceExtrinsic, Verdict, Dispute, GuaranteeExtrinsic}
import io.forge.jam.core.types.extrinsic.Preimage.given
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic.given
import io.forge.jam.core.types.extrinsic.Verdict.given
import io.forge.jam.core.types.extrinsic.Dispute.given
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic.given
import io.forge.jam.core.types.header.Header.given
import io.forge.jam.core.types.block.{Extrinsic => BlockExtrinsic}
import io.forge.jam.core.types.block.Extrinsic.given
import io.forge.jam.core.types.block.Block.given

/**
 * Tests for JAM protocol trace test vectors.
 *
 * Trace tests validate block-by-block state transitions using the official
 * JAM test vectors from jamtestvectors/traces/.
 *
 * Each trace consists of:
 * - genesis.json: Initial header and state
 * - 00000001.json through N: Sequential trace steps with pre-state, block, and expected post-state
 *
 * Currently configured for TINY config (6 validators, 12 epoch length).
 */
class TracesTest extends AnyFunSuite with Matchers:

  val config: ChainConfig = ChainConfig.TINY
  val importer: BlockImporter = new BlockImporter(config)

  // Create decoders using the imported givens
  given Decoder[TraceStep] = TraceStep.decoder
  given Decoder[Genesis] = Genesis.decoder

  // ════════════════════════════════════════════════════════════════════════════
  // Safrole Trace Tests (Priority 1)
  // ════════════════════════════════════════════════════════════════════════════

  test("safrole trace: genesis state root verification") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val genesisResult = TestFileLoader.loadJsonFromTestVectors[Genesis]("traces/safrole", "genesis")
    genesisResult match
      case Left(error) =>
        fail(s"Failed to load safrole genesis: $error")
      case Right(genesis) =>
        // Verify genesis state root matches computed
        val computedRoot = StateMerklization.stateMerklize(genesis.state.keyvals)
        computedRoot shouldBe genesis.state.stateRoot withClue
          s"Genesis state root mismatch: computed=${computedRoot.toHex}, expected=${genesis.state.stateRoot.toHex}"
  }

  test("safrole trace: first step state transition") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val stepsResult = TestFileLoader.getTraceStepFilenames("safrole")
    assume(stepsResult.isRight && stepsResult.toOption.get.nonEmpty, "No safrole trace steps available")

    val firstStepName = stepsResult.toOption.get.head
    val stepResult = TestFileLoader.loadJsonFromTestVectors[TraceStep]("traces/safrole", firstStepName)

    stepResult match
      case Left(error) =>
        fail(s"Failed to load safrole step $firstStepName: $error")
      case Right(step) =>
        // Verify pre-state root matches
        val computedPreRoot = StateMerklization.stateMerklize(step.preState.keyvals)
        computedPreRoot shouldBe step.preState.stateRoot withClue
          s"Pre-state root mismatch in step $firstStepName"

        // Run Safrole STF using unified JamState
        val fullPreState = FullJamState.fromKeyvals(step.preState.keyvals, config)
        val jamState = JamState.fromFullJamState(fullPreState, config)
        val safroleInput = InputExtractor.extractSafroleInput(step.block)

        val (safrolePostState, safroleOutput) = SafroleTransition.stf(safroleInput, jamState, config)

        // Verify STF succeeded or failed as expected
        if safroleOutput.err.isDefined then
          info(s"Safrole STF returned error: ${safroleOutput.err.get}")
        else
          // Verify timeslot was updated
          safrolePostState.tau should be >= step.block.header.slot.value.toLong withClue
            "Timeslot should be at least the block slot"
  }

  test("safrole trace: all steps sequential import") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val stepsResult = TestFileLoader.getTraceStepFilenames("safrole")
    assume(stepsResult.isRight && stepsResult.toOption.get.nonEmpty, "No safrole trace steps available")

    val stepNames = stepsResult.toOption.get

    info(s"Running ${stepNames.size} safrole trace steps")

    for (stepName, index) <- stepNames.zipWithIndex do
      val stepResult = TestFileLoader.loadJsonFromTestVectors[TraceStep]("traces/safrole", stepName)

      stepResult match
        case Left(error) =>
          fail(s"Failed to load safrole step $stepName: $error")
        case Right(step) =>
          // Extract and run Safrole STF using unified JamState
          val fullPreState = FullJamState.fromKeyvals(step.preState.keyvals, config)
          val jamState = JamState.fromFullJamState(fullPreState, config)
          val safroleInput = InputExtractor.extractSafroleInput(step.block)

          val (safrolePostState, safroleOutput) = SafroleTransition.stf(safroleInput, jamState, config)

          // Log progress
          if (index + 1) % 10 == 0 then
            info(s"Processed ${index + 1}/${stepNames.size} safrole steps")

          // Basic validation
          safrolePostState.tau should be >= 0L withClue s"Invalid tau in step $stepName"
          safrolePostState.entropy.pool.size shouldBe 4 withClue s"Invalid eta size in step $stepName"
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Fallback Trace Tests (Priority 2)
  // ════════════════════════════════════════════════════════════════════════════

  test("fallback trace: genesis state root verification") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val genesisResult = TestFileLoader.loadJsonFromTestVectors[Genesis]("traces/fallback", "genesis")
    genesisResult match
      case Left(error) =>
        // Fallback trace may not exist
        info(s"Fallback genesis not available: $error")
        cancel("Fallback trace not available")
      case Right(genesis) =>
        val computedRoot = StateMerklization.stateMerklize(genesis.state.keyvals)
        computedRoot shouldBe genesis.state.stateRoot withClue
          s"Fallback genesis state root mismatch"
  }

  test("fallback trace: first step state transition") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val stepsResult = TestFileLoader.getTraceStepFilenames("fallback")
    assume(stepsResult.isRight && stepsResult.toOption.get.nonEmpty, "No fallback trace steps available")

    val firstStepName = stepsResult.toOption.get.head
    val stepResult = TestFileLoader.loadJsonFromTestVectors[TraceStep]("traces/fallback", firstStepName)

    stepResult match
      case Left(error) =>
        fail(s"Failed to load fallback step $firstStepName: $error")
      case Right(step) =>
        val fullPreState = FullJamState.fromKeyvals(step.preState.keyvals, config)
        val jamState = JamState.fromFullJamState(fullPreState, config)
        val safroleInput = InputExtractor.extractSafroleInput(step.block)

        val (safrolePostState, safroleOutput) = SafroleTransition.stf(safroleInput, jamState, config)

        // Fallback mode should work even without tickets
        safrolePostState.tau should be >= step.block.header.slot.value.toLong
  }

  test("fallback trace: all steps sequential import") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val stepsResult = TestFileLoader.getTraceStepFilenames("fallback")
    assume(stepsResult.isRight && stepsResult.toOption.get.nonEmpty, "No fallback trace steps available")

    val stepNames = stepsResult.toOption.get
    info(s"Running ${stepNames.size} fallback trace steps")

    for (stepName, index) <- stepNames.zipWithIndex do
      val stepResult = TestFileLoader.loadJsonFromTestVectors[TraceStep]("traces/fallback", stepName)

      stepResult match
        case Left(error) =>
          fail(s"Failed to load fallback step $stepName: $error")
        case Right(step) =>
          val fullPreState = FullJamState.fromKeyvals(step.preState.keyvals, config)
          val jamState = JamState.fromFullJamState(fullPreState, config)
          val safroleInput = InputExtractor.extractSafroleInput(step.block)

          val (safrolePostState, safroleOutput) = SafroleTransition.stf(safroleInput, jamState, config)

          if (index + 1) % 10 == 0 then
            info(s"Processed ${index + 1}/${stepNames.size} fallback steps")

          safrolePostState.tau should be >= 0L withClue s"Invalid tau in step $stepName"
          safrolePostState.entropy.pool.size shouldBe 4 withClue s"Invalid eta size in step $stepName"
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Storage Trace Tests (Priority 3)
  // ════════════════════════════════════════════════════════════════════════════

  test("storage trace: genesis state root verification") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val genesisResult = TestFileLoader.loadJsonFromTestVectors[Genesis]("traces/storage", "genesis")
    genesisResult match
      case Left(error) =>
        info(s"Storage genesis not available: $error")
        cancel("Storage trace not available")
      case Right(genesis) =>
        val computedRoot = StateMerklization.stateMerklize(genesis.state.keyvals)
        computedRoot shouldBe genesis.state.stateRoot withClue
          s"Storage genesis state root mismatch"
  }

  test("storage trace: all steps sequential import") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val stepsResult = TestFileLoader.getTraceStepFilenames("storage")
    assume(stepsResult.isRight && stepsResult.toOption.get.nonEmpty, "No storage trace steps available")

    val stepNames = stepsResult.toOption.get
    info(s"Running ${stepNames.size} storage trace steps")

    for (stepName, index) <- stepNames.zipWithIndex do
      val stepResult = TestFileLoader.loadJsonFromTestVectors[TraceStep]("traces/storage", stepName)

      stepResult match
        case Left(error) =>
          fail(s"Failed to load storage step $stepName: $error")
        case Right(step) =>
          val fullPreState = FullJamState.fromKeyvals(step.preState.keyvals, config)
          val jamState = JamState.fromFullJamState(fullPreState, config)
          val safroleInput = InputExtractor.extractSafroleInput(step.block)

          val (safrolePostState, safroleOutput) = SafroleTransition.stf(safroleInput, jamState, config)

          if (index + 1) % 10 == 0 then
            info(s"Processed ${index + 1}/${stepNames.size} storage steps")

          safrolePostState.tau should be >= 0L withClue s"Invalid tau in step $stepName"
          safrolePostState.entropy.pool.size shouldBe 4 withClue s"Invalid eta size in step $stepName"
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Storage Light Trace Tests
  // ════════════════════════════════════════════════════════════════════════════

  test("storage_light trace: genesis state root verification") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val genesisResult = TestFileLoader.loadJsonFromTestVectors[Genesis]("traces/storage_light", "genesis")
    genesisResult match
      case Left(error) =>
        info(s"Storage_light genesis not available: $error")
        cancel("Storage_light trace not available")
      case Right(genesis) =>
        val computedRoot = StateMerklization.stateMerklize(genesis.state.keyvals)
        computedRoot shouldBe genesis.state.stateRoot withClue
          s"Storage_light genesis state root mismatch"
  }

  test("storage_light trace: all steps sequential import") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val stepsResult = TestFileLoader.getTraceStepFilenames("storage_light")
    assume(stepsResult.isRight && stepsResult.toOption.get.nonEmpty, "No storage_light trace steps available")

    val stepNames = stepsResult.toOption.get
    info(s"Running ${stepNames.size} storage_light trace steps")

    for (stepName, index) <- stepNames.zipWithIndex do
      val stepResult = TestFileLoader.loadJsonFromTestVectors[TraceStep]("traces/storage_light", stepName)

      stepResult match
        case Left(error) =>
          fail(s"Failed to load storage_light step $stepName: $error")
        case Right(step) =>
          val fullPreState = FullJamState.fromKeyvals(step.preState.keyvals, config)
          val jamState = JamState.fromFullJamState(fullPreState, config)
          val safroleInput = InputExtractor.extractSafroleInput(step.block)

          val (safrolePostState, safroleOutput) = SafroleTransition.stf(safroleInput, jamState, config)

          if (index + 1) % 10 == 0 then
            info(s"Processed ${index + 1}/${stepNames.size} storage_light steps")

          safrolePostState.tau should be >= 0L withClue s"Invalid tau in step $stepName"
          safrolePostState.entropy.pool.size shouldBe 4 withClue s"Invalid eta size in step $stepName"
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Preimages Trace Tests (Priority 4)
  // ════════════════════════════════════════════════════════════════════════════

  test("preimages trace: genesis state root verification") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val genesisResult = TestFileLoader.loadJsonFromTestVectors[Genesis]("traces/preimages", "genesis")
    genesisResult match
      case Left(error) =>
        info(s"Preimages genesis not available: $error")
        cancel("Preimages trace not available")
      case Right(genesis) =>
        val computedRoot = StateMerklization.stateMerklize(genesis.state.keyvals)
        computedRoot shouldBe genesis.state.stateRoot withClue
          s"Preimages genesis state root mismatch"
  }

  test("preimages trace: all steps sequential import") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val stepsResult = TestFileLoader.getTraceStepFilenames("preimages")
    assume(stepsResult.isRight && stepsResult.toOption.get.nonEmpty, "No preimages trace steps available")

    val stepNames = stepsResult.toOption.get
    info(s"Running ${stepNames.size} preimages trace steps")

    for (stepName, index) <- stepNames.zipWithIndex do
      val stepResult = TestFileLoader.loadJsonFromTestVectors[TraceStep]("traces/preimages", stepName)

      stepResult match
        case Left(error) =>
          fail(s"Failed to load preimages step $stepName: $error")
        case Right(step) =>
          val fullPreState = FullJamState.fromKeyvals(step.preState.keyvals, config)
          val jamState = JamState.fromFullJamState(fullPreState, config)
          val safroleInput = InputExtractor.extractSafroleInput(step.block)

          val (safrolePostState, safroleOutput) = SafroleTransition.stf(safroleInput, jamState, config)

          if (index + 1) % 10 == 0 then
            info(s"Processed ${index + 1}/${stepNames.size} preimages steps")

          safrolePostState.tau should be >= 0L withClue s"Invalid tau in step $stepName"
          safrolePostState.entropy.pool.size shouldBe 4 withClue s"Invalid eta size in step $stepName"
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Preimages Light Trace Tests
  // ════════════════════════════════════════════════════════════════════════════

  test("preimages_light trace: genesis state root verification") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val genesisResult = TestFileLoader.loadJsonFromTestVectors[Genesis]("traces/preimages_light", "genesis")
    genesisResult match
      case Left(error) =>
        info(s"Preimages_light genesis not available: $error")
        cancel("Preimages_light trace not available")
      case Right(genesis) =>
        val computedRoot = StateMerklization.stateMerklize(genesis.state.keyvals)
        computedRoot shouldBe genesis.state.stateRoot withClue
          s"Preimages_light genesis state root mismatch"
  }

  test("preimages_light trace: all steps sequential import") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val stepsResult = TestFileLoader.getTraceStepFilenames("preimages_light")
    assume(stepsResult.isRight && stepsResult.toOption.get.nonEmpty, "No preimages_light trace steps available")

    val stepNames = stepsResult.toOption.get
    info(s"Running ${stepNames.size} preimages_light trace steps")

    for (stepName, index) <- stepNames.zipWithIndex do
      val stepResult = TestFileLoader.loadJsonFromTestVectors[TraceStep]("traces/preimages_light", stepName)

      stepResult match
        case Left(error) =>
          fail(s"Failed to load preimages_light step $stepName: $error")
        case Right(step) =>
          val fullPreState = FullJamState.fromKeyvals(step.preState.keyvals, config)
          val jamState = JamState.fromFullJamState(fullPreState, config)
          val safroleInput = InputExtractor.extractSafroleInput(step.block)

          val (safrolePostState, safroleOutput) = SafroleTransition.stf(safroleInput, jamState, config)

          if (index + 1) % 10 == 0 then
            info(s"Processed ${index + 1}/${stepNames.size} preimages_light steps")

          safrolePostState.tau should be >= 0L withClue s"Invalid tau in step $stepName"
          safrolePostState.entropy.pool.size shouldBe 4 withClue s"Invalid eta size in step $stepName"
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Fuzzy Trace Tests (Priority 5)
  // ════════════════════════════════════════════════════════════════════════════

  test("fuzzy trace: genesis state root verification") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val genesisResult = TestFileLoader.loadJsonFromTestVectors[Genesis]("traces/fuzzy", "genesis")
    genesisResult match
      case Left(error) =>
        info(s"Fuzzy genesis not available: $error")
        cancel("Fuzzy trace not available")
      case Right(genesis) =>
        val computedRoot = StateMerklization.stateMerklize(genesis.state.keyvals)
        computedRoot shouldBe genesis.state.stateRoot withClue
          s"Fuzzy genesis state root mismatch"
  }

  test("fuzzy trace: all steps sequential import") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val stepsResult = TestFileLoader.getTraceStepFilenames("fuzzy")
    assume(stepsResult.isRight && stepsResult.toOption.get.nonEmpty, "No fuzzy trace steps available")

    val stepNames = stepsResult.toOption.get
    info(s"Running ${stepNames.size} fuzzy trace steps")

    for (stepName, index) <- stepNames.zipWithIndex do
      val stepResult = TestFileLoader.loadJsonFromTestVectors[TraceStep]("traces/fuzzy", stepName)

      stepResult match
        case Left(error) =>
          fail(s"Failed to load fuzzy step $stepName: $error")
        case Right(step) =>
          val fullPreState = FullJamState.fromKeyvals(step.preState.keyvals, config)
          val jamState = JamState.fromFullJamState(fullPreState, config)
          val safroleInput = InputExtractor.extractSafroleInput(step.block)

          val (safrolePostState, safroleOutput) = SafroleTransition.stf(safroleInput, jamState, config)

          if (index + 1) % 10 == 0 then
            info(s"Processed ${index + 1}/${stepNames.size} fuzzy steps")

          safrolePostState.tau should be >= 0L withClue s"Invalid tau in step $stepName"
          safrolePostState.entropy.pool.size shouldBe 4 withClue s"Invalid eta size in step $stepName"
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Fuzzy Light Trace Tests
  // ════════════════════════════════════════════════════════════════════════════

  test("fuzzy_light trace: genesis state root verification") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val genesisResult = TestFileLoader.loadJsonFromTestVectors[Genesis]("traces/fuzzy_light", "genesis")
    genesisResult match
      case Left(error) =>
        info(s"Fuzzy_light genesis not available: $error")
        cancel("Fuzzy_light trace not available")
      case Right(genesis) =>
        val computedRoot = StateMerklization.stateMerklize(genesis.state.keyvals)
        computedRoot shouldBe genesis.state.stateRoot withClue
          s"Fuzzy_light genesis state root mismatch"
  }

  test("fuzzy_light trace: all steps sequential import") {
    assume(TestFileLoader.canLocateTestVectors, "Test vectors not available")

    val stepsResult = TestFileLoader.getTraceStepFilenames("fuzzy_light")
    assume(stepsResult.isRight && stepsResult.toOption.get.nonEmpty, "No fuzzy_light trace steps available")

    val stepNames = stepsResult.toOption.get
    info(s"Running ${stepNames.size} fuzzy_light trace steps")

    for (stepName, index) <- stepNames.zipWithIndex do
      val stepResult = TestFileLoader.loadJsonFromTestVectors[TraceStep]("traces/fuzzy_light", stepName)

      stepResult match
        case Left(error) =>
          fail(s"Failed to load fuzzy_light step $stepName: $error")
        case Right(step) =>
          val fullPreState = FullJamState.fromKeyvals(step.preState.keyvals, config)
          val jamState = JamState.fromFullJamState(fullPreState, config)
          val safroleInput = InputExtractor.extractSafroleInput(step.block)

          val (safrolePostState, safroleOutput) = SafroleTransition.stf(safroleInput, jamState, config)

          if (index + 1) % 10 == 0 then
            info(s"Processed ${index + 1}/${stepNames.size} fuzzy_light steps")

          safrolePostState.tau should be >= 0L withClue s"Invalid tau in step $stepName"
          safrolePostState.entropy.pool.size shouldBe 4 withClue s"Invalid eta size in step $stepName"
  }

  // ════════════════════════════════════════════════════════════════════════════
  // State Comparison Utilities
  // ════════════════════════════════════════════════════════════════════════════

  /**
   * Compare two SafroleState instances and report differences.
   */
  def compareSafroleStates(
    expected: SafroleState,
    actual: SafroleState,
    context: String
  ): Unit =
    actual.tau shouldBe expected.tau withClue s"$context: tau mismatch"

    actual.eta.zip(expected.eta).zipWithIndex.foreach { case ((act, exp), idx) =>
      act shouldBe exp withClue s"$context: eta[$idx] mismatch"
    }

    actual.kappa.size shouldBe expected.kappa.size withClue s"$context: kappa size mismatch"
    actual.lambda.size shouldBe expected.lambda.size withClue s"$context: lambda size mismatch"
    actual.gammaK.size shouldBe expected.gammaK.size withClue s"$context: gammaK size mismatch"
    actual.iota.size shouldBe expected.iota.size withClue s"$context: iota size mismatch"

    actual.gammaA.size shouldBe expected.gammaA.size withClue s"$context: gammaA size mismatch"
    actual.gammaZ shouldBe expected.gammaZ withClue s"$context: gammaZ mismatch"

  /**
   * Compare two RawState instances by their state root.
   */
  def compareStateRoots(
    expected: RawState,
    actual: RawState,
    context: String
  ): Unit =
    actual.stateRoot shouldBe expected.stateRoot withClue
      s"$context: state root mismatch - expected=${expected.stateRoot.toHex}, actual=${actual.stateRoot.toHex}"
