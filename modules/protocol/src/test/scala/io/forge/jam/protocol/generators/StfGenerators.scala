package io.forge.jam.protocol.generators

import org.scalacheck.Gen
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{
  Hash,
  BandersnatchPublicKey,
  Ed25519PublicKey,
  BlsPublicKey,
  CoreIndex,
  Gas,
  ServiceId,
  Timeslot
}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.core.types.workpackage.{WorkReport, AvailabilityAssignment, SegmentRootLookup}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.work.{PackageSpec, ExecutionResult}
import io.forge.jam.core.types.workresult.{WorkResult, RefineLoad}
import io.forge.jam.protocol.accumulation.{
  OperandTuple,
  PartialState,
  ServiceAccount,
  PreimageKey,
  PreimageRequest
}
import io.forge.jam.protocol.dispute.DisputeTypes.{DisputeState, Psi}
import io.forge.jam.protocol.history.HistoryTypes.ReportedWorkPackage
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.history.{HistoricalBetaContainer, HistoricalBeta, HistoricalMmr}
import spire.math.{UShort, UInt}

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
   * Generator for 144-byte BLS public keys.
   */
  val genBlsPublicKey: Gen[BlsPublicKey] =
    genBytes(BlsPublicKey.Size).map(BlsPublicKey.apply)

  // ==========================================================================
  // Validator Generators
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
  def genValidatorKeys(count: Int): Gen[IndexedSeq[ValidatorKey]] =
    Gen.listOfN(count, genValidatorKey).map(_.toIndexedSeq)

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
      storage = storage.toMap,
      preimages = preimages.toMap,
      preimageRequests = preimageRequests.toMap,
      lastAccumulated = lastAccumulated
    )

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
      accounts = accountList.toMap,
      stagingSet = mutable.ListBuffer.from(stagingSet),
      authQueue = authQueue.map(_.map(jb => Hash(jb.toArray))).toVector,
      manager = manager,
      assigners = mutable.ListBuffer.from(assigners),
      delegator = delegator,
      registrar = registrar,
      alwaysAccers = mutable.Map.from(alwaysAccers)
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
    def sortedHashes(hs: List[Hash]): List[Hash] =
      hs.distinct.sortWith((a, b) => JamBytes.compareUnsigned(a.bytes, b.bytes) < 0)
    for
      goodCount <- Gen.choose(0, 3)
      good <- Gen.listOfN(goodCount, genHash)
      badCount <- Gen.choose(0, 3)
      bad <- Gen.listOfN(badCount, genHash)
      wonkyCount <- Gen.choose(0, 2)
      wonky <- Gen.listOfN(wonkyCount, genHash)
      offendersCount <- Gen.choose(0, 3)
      offenders <- Gen.listOfN(offendersCount, genEd25519PublicKey)
    yield Psi(
      sortedHashes(good),
      sortedHashes(bad),
      sortedHashes(wonky),
      offenders.distinct.sortWith((a, b) => JamBytes.compareUnsigned(a.bytes, b.bytes) < 0)
    )

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

  // ==========================================================================
  // Work Report and Availability Generators
  // ==========================================================================

  /**
   * Generator for context (work package context).
   */
  val genContext: Gen[Context] =
    for
      anchor <- genHash
      anchorSlot <- Gen.choose(0, 1000).map(Timeslot.apply)
      stateRoot <- genHash
      beefyRoot <- genHash
      lookupAnchor <- genHash
      lookupAnchorSlot <- Gen.choose(0, 1000).map(Timeslot.apply)
      lookupAnchorStateRoot <- genHash
      prereqCount <- Gen.choose(0, 3)
      prereqs <- Gen.listOfN(prereqCount, genHash)
    yield Context(anchor, anchorSlot, stateRoot, beefyRoot, lookupAnchor, lookupAnchorSlot, lookupAnchorStateRoot, prereqs)

  /**
   * Generator for package specification.
   */
  val genPackageSpec: Gen[PackageSpec] =
    for
      hash <- genHash
      length <- Gen.choose(100, 10000).map(UInt.apply)
      erasureRoot <- genHash
      erasureShards <- Gen.choose(0, 1023).map(UShort.apply)
      exportsRoot <- genHash
      exportsCount <- Gen.choose(0, 10).map(UShort.apply)
    yield PackageSpec(hash, length, erasureRoot, erasureShards, exportsRoot, exportsCount)

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
      registeredSlot <- Gen.choose(1L, 1000L)
    yield AvailabilityAssignment(
      GuaranteeExtrinsic(report, Timeslot(registeredSlot.toInt), List.empty),
      registeredSlot
    )

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
   * HistoricalBeta takes (headerHash, beefyRoot, stateRoot, slot, reported).
   */
  val genHistoricalBeta: Gen[HistoricalBeta] =
    for
      headerHash <- genHash
      beefyRoot <- genHash
      stateRoot <- genHash
      slot <- Gen.choose(0L, 1000L)
      workPackageCount <- Gen.choose(0, 3)
      workPackages <- Gen.listOfN(workPackageCount, genReportedWorkPackage)
    yield HistoricalBeta(headerHash, beefyRoot, stateRoot, slot, workPackages)

  /**
   * Generator for historical beta container.
   */
  val genHistoricalBetaContainer: Gen[HistoricalBetaContainer] =
    for
      size <- Gen.choose(1, 8)
      entries <- Gen.listOfN(size, genHistoricalBeta)
      mmr <- genHistoricalMmr
    yield HistoricalBetaContainer(entries, mmr)
