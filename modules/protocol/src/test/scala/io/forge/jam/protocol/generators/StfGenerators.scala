package io.forge.jam.protocol.generators

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{
  Hash,
  BandersnatchPublicKey,
  Ed25519PublicKey,
  BlsPublicKey,
  Ed25519Signature,
  CoreIndex,
  Gas,
  ServiceId,
  Timeslot,
  ValidatorIndex
}
import io.forge.jam.core.types.epoch.{ValidatorKey, EpochMark, EpochValidatorKey}
import io.forge.jam.core.types.tickets.{TicketMark, TicketEnvelope}
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.core.types.workpackage.{WorkReport, AvailabilityAssignment, SegmentRootLookup}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.work.{PackageSpec, ExecutionResult}
import io.forge.jam.core.types.workresult.{WorkResult, RefineLoad}
import io.forge.jam.protocol.safrole.SafroleTypes
import io.forge.jam.protocol.safrole.SafroleTypes.{SafroleState, SafroleInput, TicketsOrKeys}
import io.forge.jam.protocol.accumulation.{
  AccumulationContext,
  DeferredTransfer,
  OperandTuple,
  PartialState,
  ServiceAccount,
  PreimageKey,
  PreimageRequest
}
import io.forge.jam.protocol.dispute.DisputeTypes.{DisputeState, DisputeInput, Psi}
import io.forge.jam.protocol.assurance.AssuranceTypes.{AssuranceState, AssuranceInput}
import io.forge.jam.protocol.history.HistoryTypes.{HistoricalState, HistoricalInput, ReportedWorkPackage}
import io.forge.jam.protocol.preimage.PreimageTypes.{
  PreimageState,
  PreimageInput,
  PreimageAccount,
  AccountInfo,
  PreimageHistory,
  PreimageHistoryKey
}
import io.forge.jam.protocol.statistics.StatisticsTypes.{StatState, StatInput, StatCount, StatExtrinsic}
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, Dispute, Preimage, Verdict}
import io.forge.jam.core.types.history.{HistoricalBetaContainer, HistoricalBeta, HistoricalMmr}
import io.forge.jam.core.types.preimage.PreimageHash
import spire.math.{UByte, UShort, UInt}

import scala.collection.mutable

/**
 * ScalaCheck generators for State Transition Function (STF) testing.
 *
 * This module provides generators for all major protocol types used in STF property tests.
 */
object StfGenerators:

  // ==========================================================================
  // Primitive Generators
  // ==========================================================================

  /**
   * Generator for random byte arrays of specified length.
   *
   * @param size Number of bytes to generate
   * @return Generator producing byte arrays of exactly `size` bytes
   */
  def genBytes(size: Int): Gen[Array[Byte]] =
    Gen.listOfN(size, Gen.choose(0, 255).map(_.toByte)).map(_.toArray)

  /**
   * Generator for JamBytes of specified length.
   *
   * @param size Number of bytes to generate
   * @return Generator producing JamBytes of exactly `size` bytes
   */
  def genJamBytes(size: Int): Gen[JamBytes] =
    genBytes(size).map(JamBytes.apply)

  /**
   * Generator for variable-length JamBytes within bounds.
   *
   * @param minSize Minimum number of bytes (inclusive)
   * @param maxSize Maximum number of bytes (inclusive)
   * @return Generator producing JamBytes between minSize and maxSize bytes
   */
  def genJamBytesRange(minSize: Int, maxSize: Int): Gen[JamBytes] =
    Gen.choose(minSize, maxSize).flatMap(genJamBytes)

  /**
   * Generator for 32-byte Hash values.
   * Uses the Hash.apply factory method which validates size.
   */
  val genHash: Gen[Hash] =
    genBytes(Hash.Size).map(Hash.apply)

  /**
   * Generator for Hash that produces zero hash with some probability.
   * Useful for edge case testing.
   */
  val genHashWithZero: Gen[Hash] =
    Gen.frequency(
      (9, genHash),
      (1, Gen.const(Hash.zero))
    )

  /**
   * Generator for 32-byte Bandersnatch public keys.
   */
  val genBandersnatchPublicKey: Gen[BandersnatchPublicKey] =
    genBytes(BandersnatchPublicKey.Size).map(BandersnatchPublicKey.apply)

  /**
   * Generator for 32-byte Ed25519 public keys.
   */
  val genEd25519PublicKey: Gen[Ed25519PublicKey] =
    genBytes(Ed25519PublicKey.Size).map(Ed25519PublicKey.apply)

  /**
   * Generator for 64-byte Ed25519 signatures.
   */
  val genEd25519Signature: Gen[Ed25519Signature] =
    genBytes(Ed25519Signature.Size).map(Ed25519Signature.apply)

  /**
   * Generator for 144-byte BLS public keys.
   */
  val genBlsPublicKey: Gen[BlsPublicKey] =
    genBytes(BlsPublicKey.Size).map(BlsPublicKey.apply)

  // ==========================================================================
  // Validator and Epoch Generators
  // ==========================================================================

  /**
   * Generator for full validator keys (336 bytes total).
   * Contains Bandersnatch (32) + Ed25519 (32) + BLS (144) + metadata (128).
   */
  val genValidatorKey: Gen[ValidatorKey] =
    for
      bandersnatch <- genBandersnatchPublicKey
      ed25519 <- genEd25519PublicKey
      bls <- genBlsPublicKey
      metadata <- genJamBytes(ValidatorKey.MetadataSize)
    yield ValidatorKey(bandersnatch, ed25519, bls, metadata)

  /**
   * Generator for a list of validator keys with specified count.
   *
   * @param count Number of validators to generate
   * @return Generator producing exactly `count` validator keys
   */
  def genValidatorKeys(count: Int): Gen[List[ValidatorKey]] =
    Gen.listOfN(count, genValidatorKey)

  /**
   * Generator for short-form epoch validator keys (64 bytes).
   * Contains only Bandersnatch + Ed25519 keys.
   */
  val genEpochValidatorKey: Gen[EpochValidatorKey] =
    for
      bandersnatch <- genBandersnatchPublicKey
      ed25519 <- genEd25519PublicKey
    yield EpochValidatorKey(bandersnatch, ed25519)

  /**
   * Generator for epoch marks.
   *
   * @param validatorCount Number of validators in the epoch mark
   * @return Generator producing valid EpochMark instances
   */
  def genEpochMark(validatorCount: Int): Gen[EpochMark] =
    for
      entropy <- genHash
      ticketsEntropy <- genHash
      validators <- Gen.listOfN(validatorCount, genEpochValidatorKey)
    yield EpochMark(entropy, ticketsEntropy, validators)

  // ==========================================================================
  // Ticket Generators
  // ==========================================================================

  /**
   * Generator for ticket marks (33 bytes: 32-byte ID + 1-byte attempt).
   * ID is a random 32-byte value.
   * Attempt is constrained to valid range [0, ticketsPerValidator).
   *
   * @param maxAttempt Maximum attempt number (exclusive)
   */
  def genTicketMark(maxAttempt: Int = 3): Gen[TicketMark] =
    for
      id <- genJamBytes(Hash.Size)
      attempt <- Gen.choose(0, maxAttempt - 1).map(a => UByte(a))
    yield TicketMark(id, attempt)

  /**
   * Generator for sorted list of ticket marks.
   * The list is sorted by ticket ID to satisfy gammaA invariant.
   *
   * @param count Number of tickets to generate
   * @param maxAttempt Maximum attempt number
   */
  def genSortedTicketMarks(count: Int, maxAttempt: Int = 3): Gen[List[TicketMark]] =
    Gen.listOfN(count, genTicketMark(maxAttempt)).map { tickets =>
      tickets.distinctBy(t => t.id.toHex).sortBy(_.id.toHex)
    }

  /**
   * Generator for ticket envelopes with ring VRF signatures (785 bytes).
   */
  def genTicketEnvelope(maxAttempt: Int = 3): Gen[TicketEnvelope] =
    for
      attempt <- Gen.choose(0, maxAttempt - 1).map(a => UByte(a))
      signature <- genJamBytes(784)
    yield TicketEnvelope(attempt, signature)

  /**
   * Generator for TicketsOrKeys sealed trait.
   * Either contains a list of ticket marks or fallback Bandersnatch keys.
   *
   * @param epochLength Number of elements in the sequence
   */
  def genTicketsOrKeys(epochLength: Int): Gen[TicketsOrKeys] =
    Gen.oneOf(
      Gen.listOfN(epochLength, genTicketMark()).map(TicketsOrKeys.Tickets.apply),
      Gen.listOfN(epochLength, genBandersnatchPublicKey).map(TicketsOrKeys.Keys.apply)
    )

  // ==========================================================================
  // Safrole State Generators
  // ==========================================================================

  /**
   * Generator for complete Safrole state.
   * Respects config constraints for validator counts and epoch length.
   *
   * @param config Chain configuration (TINY or FULL)
   * @return Generator producing valid SafroleState instances
   */
  def genSafroleState(config: ChainConfig): Gen[SafroleState] =
    for
      tau <- Gen.choose(0L, config.epochLength.toLong - 1)
      eta <- Gen.listOfN(4, genHash)
      lambda <- genValidatorKeys(config.validatorCount)
      kappa <- genValidatorKeys(config.validatorCount)
      gammaK <- genValidatorKeys(config.validatorCount)
      iota <- genValidatorKeys(config.validatorCount)
      ticketCount <- Gen.choose(0, config.epochLength)
      gammaA <- genSortedTicketMarks(ticketCount, config.ticketsPerValidator)
      gammaS <- genTicketsOrKeys(config.epochLength)
      gammaZ <- genJamBytes(SafroleTypes.BandersnatchRingCommitmentSize)
      offendersCount <- Gen.choose(0, 3)
      postOffenders <- Gen.listOfN(offendersCount, genEd25519PublicKey)
    yield SafroleState(
      tau = tau,
      eta = eta,
      lambda = lambda,
      kappa = kappa,
      gammaK = gammaK,
      iota = iota,
      gammaA = gammaA,
      gammaS = gammaS,
      gammaZ = gammaZ,
      postOffenders = postOffenders
    )

  /**
   * Generator for Safrole state at epoch boundary (tau = 0).
   */
  def genSafroleStateAtEpochStart(config: ChainConfig): Gen[SafroleState] =
    genSafroleState(config).map(_.copy(tau = 0))

  /**
   * Generator for Safrole state at epoch end (tau = epochLength - 1).
   */
  def genSafroleStateAtEpochEnd(config: ChainConfig): Gen[SafroleState] =
    genSafroleState(config).map(_.copy(tau = config.epochLength - 1))

  /**
   * Generator for Safrole input.
   *
   * @param config Chain configuration
   * @param currentTau Current tau value (to generate valid slot > tau)
   */
  def genSafroleInput(config: ChainConfig, currentTau: Long = 0): Gen[SafroleInput] =
    for
      slot <- Gen.choose(currentTau + 1, currentTau + 100)
      entropy <- genHash
      ticketCount <- Gen.choose(0, config.maxTicketsPerExtrinsic)
      extrinsic <- Gen.listOfN(ticketCount, genTicketEnvelope(config.ticketsPerValidator))
    yield SafroleInput(slot, entropy, extrinsic)

  /**
   * Generator for Safrole input with empty extrinsic.
   */
  def genSafroleInputEmpty(currentTau: Long = 0): Gen[SafroleInput] =
    for
      slot <- Gen.choose(currentTau + 1, currentTau + 100)
      entropy <- genHash
    yield SafroleInput(slot, entropy, List.empty)

  // ==========================================================================
  // Service and Accumulation Generators
  // ==========================================================================

  /**
   * Generator for service info.
   */
  val genServiceInfo: Gen[ServiceInfo] =
    for
      version <- Gen.choose(0, 255)
      codeHash <- genHash
      balance <- Gen.choose(100L, 1000000L)
      minItemGas <- Gen.choose(10L, 1000L)
      minMemoGas <- Gen.choose(10L, 1000L)
      bytesUsed <- Gen.choose(0L, 10000L)
      depositOffset <- Gen.choose(0L, 1000L)
      items <- Gen.choose(0, 100)
      creationSlot <- Gen.choose(0L, 1000L)
      lastAccumulationSlot <- Gen.choose(0L, 1000L)
      parentService <- Gen.choose(0L, 100L)
    yield ServiceInfo(
      version = version,
      codeHash = codeHash,
      balance = balance,
      minItemGas = minItemGas,
      minMemoGas = minMemoGas,
      bytesUsed = bytesUsed,
      depositOffset = depositOffset,
      items = items,
      creationSlot = creationSlot,
      lastAccumulationSlot = lastAccumulationSlot,
      parentService = parentService
    )

  /**
   * Generator for service accounts (accumulation).
   */
  val genAccumulationServiceAccount: Gen[ServiceAccount] =
    for
      info <- genServiceInfo
      storageSize <- Gen.choose(0, 5)
      storage <- Gen.mapOfN(
        storageSize,
        for
          key <- genJamBytes(32)
          value <- genJamBytesRange(1, 100)
        yield (key, value)
      )
      preimageCount <- Gen.choose(0, 3)
      preimages <- Gen.mapOfN(
        preimageCount,
        for
          hash <- genHash
          data <- genJamBytesRange(1, 100)
        yield (hash, data)
      )
      requestCount <- Gen.choose(0, 2)
      preimageRequests <- Gen.mapOfN(
        requestCount,
        for
          hash <- genHash
          length <- Gen.choose(1, 100)
          timestampCount <- Gen.choose(0, 3)
          timestamps <- Gen.listOfN(timestampCount, Gen.choose(0L, 1000L))
        yield (PreimageKey(hash, length), PreimageRequest(timestamps))
      )
      lastAccumulated <- Gen.choose(0L, 1000L)
    yield ServiceAccount(
      info = info,
      storage = mutable.Map.from(storage),
      preimages = mutable.Map.from(preimages),
      preimageRequests = mutable.Map.from(preimageRequests),
      lastAccumulated = lastAccumulated
    )

  /**
   * Generator for deferred transfers.
   */
  val genDeferredTransfer: Gen[DeferredTransfer] =
    for
      source <- Gen.choose(0L, 100L)
      destination <- Gen.choose(0L, 100L)
      amount <- Gen.choose(1L, 10000L)
      memo <- genJamBytes(DeferredTransfer.MEMO_SIZE)
      gasLimit <- Gen.choose(100L, 10000L)
    yield DeferredTransfer(source, destination, amount, memo, gasLimit)

  /**
   * Generator for partial state used in accumulation.
   *
   * @param config Chain configuration
   */
  def genPartialState(config: ChainConfig): Gen[PartialState] =
    for
      accountCount <- Gen.choose(1, 5)
      accountList <- Gen.listOfN(
        accountCount,
        for
          id <- Gen.choose(config.minPublicServiceIndex, config.minPublicServiceIndex + 100)
          account <- genAccumulationServiceAccount
        yield (id, account)
      )
      stagingSetSize <- Gen.choose(0, config.validatorCount)
      stagingSet <- Gen.listOfN(stagingSetSize, genJamBytes(ValidatorKey.Size))
      authQueueSize <- Gen.choose(0, config.coresCount)
      authQueue <- Gen.listOfN(authQueueSize, Gen.listOfN(Gen.choose(0, 3).sample.get, genJamBytes(32)))
      manager <- Gen.choose(0L, 10L)
      assignerCount <- Gen.choose(0, config.coresCount)
      assigners <- Gen.listOfN(assignerCount, Gen.choose(0L, 100L))
      delegator <- Gen.choose(0L, 10L)
      registrar <- Gen.choose(0L, 10L)
      alwaysAccerCount <- Gen.choose(0, 3)
      alwaysAccers <- Gen.mapOfN(
        alwaysAccerCount,
        for
          serviceId <- Gen.choose(0L, 100L)
          gas <- Gen.choose(1000L, 100000L)
        yield (serviceId, gas)
      )
    yield PartialState(
      accounts = mutable.Map.from(accountList),
      stagingSet = mutable.ListBuffer.from(stagingSet),
      authQueue = mutable.ListBuffer.from(authQueue.map(q => mutable.ListBuffer.from(q))),
      manager = manager,
      assigners = mutable.ListBuffer.from(assigners),
      delegator = delegator,
      registrar = registrar,
      alwaysAccers = mutable.Map.from(alwaysAccers)
    )

  /**
   * Generator for accumulation context.
   *
   * @param config Chain configuration
   */
  def genAccumulationContext(config: ChainConfig): Gen[AccumulationContext] =
    for
      initialState <- genPartialState(config)
      serviceIndex <- Gen.choose(config.minPublicServiceIndex, config.minPublicServiceIndex + 100)
      timeslot <- Gen.choose(1L, 1000L)
      entropy <- genJamBytes(32)
    yield AccumulationContext(
      initialState,
      serviceIndex,
      timeslot,
      entropy,
      nextAccountIndex = config.minPublicServiceIndex + 200,
      minPublicServiceIndex = config.minPublicServiceIndex
    )

  /**
   * Generator for execution results.
   */
  val genExecutionResult: Gen[ExecutionResult] =
    Gen.frequency(
      (7, genJamBytesRange(0, 100).map(ExecutionResult.Ok.apply)),
      (1, Gen.const(ExecutionResult.OOG)),
      (1, Gen.const(ExecutionResult.Panic)),
      (1, Gen.const(ExecutionResult.BadExports))
    )

  /**
   * Generator for operand tuples used in accumulation.
   */
  val genOperandTuple: Gen[OperandTuple] =
    for
      packageHash <- genJamBytes(32)
      segmentRoot <- genJamBytes(32)
      authorizerHash <- genJamBytes(32)
      payloadHash <- genJamBytes(32)
      gasLimit <- Gen.choose(1000L, 1000000L)
      authTrace <- genJamBytesRange(0, 50)
      result <- genExecutionResult
    yield OperandTuple(
      packageHash,
      segmentRoot,
      authorizerHash,
      payloadHash,
      gasLimit,
      authTrace,
      result
    )

  // ==========================================================================
  // Dispute State Generators
  // ==========================================================================

  /**
   * Generator for Psi (judgment state).
   */
  val genPsi: Gen[Psi] =
    for
      goodCount <- Gen.choose(0, 3)
      good <- Gen.listOfN(goodCount, genHash)
      badCount <- Gen.choose(0, 3)
      bad <- Gen.listOfN(badCount, genHash)
      wonkyCount <- Gen.choose(0, 2)
      wonky <- Gen.listOfN(wonkyCount, genHash)
      offendersCount <- Gen.choose(0, 3)
      offenders <- Gen.listOfN(offendersCount, genEd25519PublicKey)
    yield Psi(good, bad, wonky, offenders)

  /**
   * Generator for empty Dispute (no verdicts, culprits, or faults).
   */
  val genEmptyDispute: Gen[Dispute] =
    Gen.const(Dispute(List.empty, List.empty, List.empty))

  /**
   * Generator for dispute state.
   *
   * @param config Chain configuration
   */
  def genDisputeState(config: ChainConfig): Gen[DisputeState] =
    for
      psi <- genPsi
      rhoSize <- Gen.const(config.coresCount)
      rho <- Gen.listOfN(rhoSize, Gen.option(genAvailabilityAssignment(config)))
      tau <- Gen.choose(0L, 1000L)
      kappa <- genValidatorKeys(config.validatorCount)
      lambda <- genValidatorKeys(config.validatorCount)
    yield DisputeState(psi, rho, tau, kappa, lambda)

  /**
   * Generator for dispute input (empty disputes for now).
   */
  def genDisputeInput(config: ChainConfig): Gen[DisputeInput] =
    genEmptyDispute.map(DisputeInput.apply)

  // ==========================================================================
  // Assurance State Generators
  // ==========================================================================

  /**
   * Generator for context (work package context).
   */
  val genContext: Gen[Context] =
    for
      anchor <- genHash
      stateRoot <- genHash
      beefyRoot <- genHash
      lookupAnchor <- genHash
      lookupAnchorSlot <- Gen.choose(0, 1000).map(Timeslot.apply)
      prereqCount <- Gen.choose(0, 3)
      prereqs <- Gen.listOfN(prereqCount, genHash)
    yield Context(anchor, stateRoot, beefyRoot, lookupAnchor, lookupAnchorSlot, prereqs)

  /**
   * Generator for package specification.
   */
  val genPackageSpec: Gen[PackageSpec] =
    for
      hash <- genHash
      length <- Gen.choose(100, 10000).map(UInt.apply)
      erasureRoot <- genHash
      exportsRoot <- genHash
      exportsCount <- Gen.choose(0, 10).map(UShort.apply)
    yield PackageSpec(hash, length, erasureRoot, exportsRoot, exportsCount)

  /**
   * Generator for refine load statistics.
   */
  val genRefineLoad: Gen[RefineLoad] =
    for
      gasUsed <- Gen.choose(1000L, 100000L).map(Gas.apply)
      imports <- Gen.choose(0, 10).map(UShort.apply)
      extrinsicCount <- Gen.choose(0, 5).map(UShort.apply)
      extrinsicSize <- Gen.choose(0, 1000).map(UInt.apply)
      exports <- Gen.choose(0, 10).map(UShort.apply)
    yield RefineLoad(gasUsed, imports, extrinsicCount, extrinsicSize, exports)

  /**
   * Generator for work results.
   */
  val genWorkResult: Gen[WorkResult] =
    for
      serviceId <- Gen.choose(0, 100).map(ServiceId.apply)
      codeHash <- genHash
      payloadHash <- genHash
      accumulateGas <- Gen.choose(1000L, 100000L).map(Gas.apply)
      result <- genExecutionResult
      refineLoad <- genRefineLoad
    yield WorkResult(serviceId, codeHash, payloadHash, accumulateGas, result, refineLoad)

  /**
   * Generator for segment root lookups.
   */
  val genSegmentRootLookup: Gen[SegmentRootLookup] =
    for
      workPackageHash <- genHash
      segmentTreeRoot <- genHash
    yield SegmentRootLookup(workPackageHash, segmentTreeRoot)

  /**
   * Generator for work reports.
   *
   * @param config Chain configuration
   */
  def genWorkReport(config: ChainConfig): Gen[WorkReport] =
    for
      packageSpec <- genPackageSpec
      context <- genContext
      coreIndex <- Gen.choose(0, config.coresCount - 1).map(CoreIndex.apply)
      authorizerHash <- genHash
      authGasUsed <- Gen.choose(1000L, 100000L).map(Gas.apply)
      authOutput <- genJamBytesRange(0, 50)
      lookupCount <- Gen.choose(0, 3)
      segmentRootLookup <- Gen.listOfN(lookupCount, genSegmentRootLookup)
      resultCount <- Gen.choose(1, 5)
      results <- Gen.listOfN(resultCount, genWorkResult)
    yield WorkReport(
      packageSpec,
      context,
      coreIndex,
      authorizerHash,
      authGasUsed,
      authOutput,
      segmentRootLookup,
      results
    )

  /**
   * Generator for availability assignments.
   *
   * @param config Chain configuration
   */
  def genAvailabilityAssignment(config: ChainConfig): Gen[AvailabilityAssignment] =
    for
      report <- genWorkReport(config)
      timeout <- Gen.choose(1L, 1000L)
    yield AvailabilityAssignment(report, timeout)

  /**
   * Generator for assurance state.
   *
   * @param config Chain configuration
   */
  def genAssuranceState(config: ChainConfig): Gen[AssuranceState] =
    for
      rhoSize <- Gen.const(config.coresCount)
      availAssignments <- Gen.listOfN(rhoSize, Gen.option(genAvailabilityAssignment(config)))
      currValidators <- genValidatorKeys(config.validatorCount)
    yield AssuranceState(availAssignments, currValidators)

  /** Helper to extract validator index as Int for sorting */
  private def assuranceValidatorIndexAsInt(ae: AssuranceExtrinsic): Int = ae.validatorIndex.toInt

  /**
   * Generator for assurance input.
   *
   * @param config Chain configuration
   */
  def genAssuranceInput(config: ChainConfig): Gen[AssuranceInput] =
    for
      assuranceCount <- Gen.choose(0, config.validatorCount)
      assurances <- Gen.listOfN(assuranceCount, genAssuranceExtrinsic(config))
      slot <- Gen.choose(1L, 1000L)
      parent <- genHash
    yield
      val unique = assurances.distinctBy(assuranceValidatorIndexAsInt)
      val sorted = unique.sortBy(assuranceValidatorIndexAsInt)
      AssuranceInput(sorted, slot, parent)

  /**
   * Generator for assurance extrinsics.
   */
  def genAssuranceExtrinsic(config: ChainConfig): Gen[AssuranceExtrinsic] =
    for
      anchor <- genHash
      bitfield <- genJamBytes((config.coresCount + 7) / 8)
      validatorIndex <- Gen.choose(0, config.validatorCount - 1).map(ValidatorIndex.apply)
      signature <- genEd25519Signature
    yield AssuranceExtrinsic(anchor, bitfield, validatorIndex, signature)

  // ==========================================================================
  // History State Generators
  // ==========================================================================

  /**
   * Generator for reported work packages.
   */
  val genReportedWorkPackage: Gen[ReportedWorkPackage] =
    for
      hash <- genHash
      exportsRoot <- genHash
    yield ReportedWorkPackage(hash, exportsRoot)

  /**
   * Generator for historical MMR (Merkle Mountain Range).
   * Peaks are wrapped in Option as required by the type.
   */
  val genHistoricalMmr: Gen[HistoricalMmr] =
    for
      peaksCount <- Gen.choose(1, 4)
      peaks <- Gen.listOfN(peaksCount, Gen.option(genHash))
    yield HistoricalMmr(peaks)

  /**
   * Generator for historical beta entries.
   * HistoricalBeta takes (headerHash, beefyRoot, stateRoot, reported).
   */
  val genHistoricalBeta: Gen[HistoricalBeta] =
    for
      headerHash <- genHash
      beefyRoot <- genHash
      stateRoot <- genHash
      workPackageCount <- Gen.choose(0, 3)
      workPackages <- Gen.listOfN(workPackageCount, genReportedWorkPackage)
    yield HistoricalBeta(headerHash, beefyRoot, stateRoot, workPackages)

  /**
   * Generator for historical beta container.
   */
  val genHistoricalBetaContainer: Gen[HistoricalBetaContainer] =
    for
      size <- Gen.choose(1, 8)
      entries <- Gen.listOfN(size, genHistoricalBeta)
      mmr <- genHistoricalMmr
    yield HistoricalBetaContainer(entries, mmr)

  /**
   * Generator for historical state.
   */
  val genHistoricalState: Gen[HistoricalState] =
    genHistoricalBetaContainer.map(HistoricalState.apply)

  /**
   * Generator for historical input.
   *
   * @param config Chain configuration
   */
  def genHistoricalInput(config: ChainConfig): Gen[HistoricalInput] =
    for
      headerHash <- genHash
      parentStateRoot <- genHash
      accumulateRoot <- genHash
      workPackageCount <- Gen.choose(0, config.coresCount.min(5))
      workPackages <- Gen.listOfN(workPackageCount, genReportedWorkPackage)
    yield HistoricalInput(headerHash, parentStateRoot, accumulateRoot, workPackages)

  // ==========================================================================
  // Preimage State Generators
  // ==========================================================================

  /**
   * Generator for preimage hash entries.
   */
  val genPreimageHash: Gen[PreimageHash] =
    for
      hash <- genHash
      data <- genJamBytesRange(1, 100)
    yield PreimageHash(hash, data)

  /**
   * Generator for preimage history keys.
   */
  val genPreimageHistoryKey: Gen[PreimageHistoryKey] =
    for
      hash <- genHash
      length <- Gen.choose(1L, 1000L)
    yield PreimageHistoryKey(hash, length)

  /**
   * Generator for preimage history entries.
   */
  val genPreimageHistory: Gen[PreimageHistory] =
    for
      key <- genPreimageHistoryKey
      timestampCount <- Gen.choose(0, 3)
      timestamps <- Gen.listOfN(timestampCount, Gen.choose(0L, 1000L))
    yield PreimageHistory(key, timestamps)

  /**
   * Generator for account info (preimage module).
   */
  val genAccountInfo: Gen[AccountInfo] =
    for
      preimageCount <- Gen.choose(0, 3)
      preimages <- Gen.listOfN(preimageCount, genPreimageHash)
      lookupMetaCount <- Gen.choose(0, 3)
      lookupMeta <- Gen.listOfN(lookupMetaCount, genPreimageHistory)
    yield AccountInfo(preimages, lookupMeta)

  /**
   * Generator for preimage accounts.
   */
  val genPreimageAccount: Gen[PreimageAccount] =
    for
      id <- Gen.choose(0L, 100L)
      data <- genAccountInfo
    yield PreimageAccount(id, data)

  /**
   * Generator for preimage state.
   */
  val genPreimageState: Gen[PreimageState] =
    for
      accountCount <- Gen.choose(0, 5)
      accounts <- Gen.listOfN(accountCount, genPreimageAccount)
    yield PreimageState(accounts, List.empty)

  /**
   * Generator for preimage extrinsics.
   * Preimage requires ServiceId type for requester.
   */
  val genPreimageExtrinsic: Gen[Preimage] =
    for
      requester <- Gen.choose(0, 100).map(ServiceId.apply)
      blob <- genJamBytesRange(1, 100)
    yield Preimage(requester, blob)

  /**
   * Generator for preimage input.
   */
  val genPreimageInput: Gen[PreimageInput] =
    for
      preimageCount <- Gen.choose(0, 3)
      preimages <- Gen.listOfN(preimageCount, genPreimageExtrinsic)
      slot <- Gen.choose(1L, 1000L)
    yield PreimageInput(preimages, slot)

  // ==========================================================================
  // Statistics State Generators
  // ==========================================================================

  /**
   * Generator for stat count entries.
   */
  val genStatCount: Gen[StatCount] =
    for
      blocks <- Gen.choose(0L, 100L)
      tickets <- Gen.choose(0L, 100L)
      preImages <- Gen.choose(0L, 100L)
      preImagesSize <- Gen.choose(0L, 10000L)
      guarantees <- Gen.choose(0L, 100L)
      assurances <- Gen.choose(0L, 100L)
    yield StatCount(blocks, tickets, preImages, preImagesSize, guarantees, assurances)

  /**
   * Generator for statistics state.
   *
   * @param config Chain configuration
   */
  def genStatState(config: ChainConfig): Gen[StatState] =
    for
      valsCurrStats <- Gen.listOfN(config.validatorCount, genStatCount)
      valsLastStats <- Gen.listOfN(config.validatorCount, genStatCount)
      slot <- Gen.choose(0L, 1000L)
      currValidators <- genValidatorKeys(config.validatorCount)
    yield StatState(valsCurrStats, valsLastStats, slot, currValidators)

  /**
   * Generator for stat extrinsic.
   */
  def genStatExtrinsic(config: ChainConfig): Gen[StatExtrinsic] =
    for
      ticketCount <- Gen.choose(0, config.maxTicketsPerExtrinsic)
      tickets <- Gen.listOfN(ticketCount, genTicketEnvelope(config.ticketsPerValidator))
      preimageCount <- Gen.choose(0, 3)
      preimages <- Gen.listOfN(preimageCount, genPreimageExtrinsic)
      emptyDispute <- genEmptyDispute
    yield StatExtrinsic(
      tickets = tickets,
      preimages = preimages,
      guarantees = List.empty,
      assurances = List.empty,
      disputes = emptyDispute
    )

  /**
   * Generator for statistics input.
   *
   * @param config Chain configuration
   */
  def genStatInput(config: ChainConfig): Gen[StatInput] =
    for
      slot <- Gen.choose(1L, 1000L)
      authorIndex <- Gen.choose(0L, config.validatorCount.toLong - 1)
      extrinsic <- genStatExtrinsic(config)
    yield StatInput(slot, authorIndex, extrinsic)

  // ==========================================================================
  // Arbitrary Instances
  // ==========================================================================

  /** Arbitrary instance for Hash */
  given Arbitrary[Hash] = Arbitrary(genHash)

  /** Arbitrary instance for BandersnatchPublicKey */
  given Arbitrary[BandersnatchPublicKey] = Arbitrary(genBandersnatchPublicKey)

  /** Arbitrary instance for Ed25519PublicKey */
  given Arbitrary[Ed25519PublicKey] = Arbitrary(genEd25519PublicKey)

  /** Arbitrary instance for BlsPublicKey */
  given Arbitrary[BlsPublicKey] = Arbitrary(genBlsPublicKey)

  /** Arbitrary instance for ValidatorKey */
  given Arbitrary[ValidatorKey] = Arbitrary(genValidatorKey)

  /** Arbitrary instance for TicketMark */
  given Arbitrary[TicketMark] = Arbitrary(genTicketMark())

  /** Arbitrary instance for ServiceInfo */
  given Arbitrary[ServiceInfo] = Arbitrary(genServiceInfo)

  /** Arbitrary instance for DeferredTransfer */
  given Arbitrary[DeferredTransfer] = Arbitrary(genDeferredTransfer)

  /** Arbitrary instance for Psi */
  given Arbitrary[Psi] = Arbitrary(genPsi)

  /** Arbitrary instance for StatCount */
  given Arbitrary[StatCount] = Arbitrary(genStatCount)
