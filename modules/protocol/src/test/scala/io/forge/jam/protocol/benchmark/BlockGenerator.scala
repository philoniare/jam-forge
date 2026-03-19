package io.forge.jam.protocol.benchmark

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Hash, ValidatorIndex, Timeslot, ServiceId, Gas, Ed25519Signature, CoreIndex}
import io.forge.jam.core.types.block.{Block, Extrinsic}
import io.forge.jam.core.types.header.Header
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, Dispute, GuaranteeExtrinsic, Preimage}
import io.forge.jam.core.types.tickets.TicketEnvelope
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.work.{ExecutionResult, PackageSpec}
import io.forge.jam.core.types.workpackage.{WorkReport, SegmentRootLookup}
import io.forge.jam.core.types.workresult.{RefineLoad, WorkResult}
import io.forge.jam.core.types.dispute.GuaranteeSignature
import io.forge.jam.core.{constants, Shuffle}
import io.forge.jam.crypto.{Ed25519ZebraWrapper, SigningContext}
import io.forge.jam.protocol.safrole.SafroleTransition
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.state.JamState
import io.forge.jam.protocol.traces.{FullJamState, RawState}
import io.forge.jam.vrfs.BandersnatchWrapper
import spire.math.{UInt, UShort}
import _root_.scodec.Codec

import scala.util.Random

/**
 * Generates valid blocks with diverse extrinsics that chain indefinitely.
 * Exercises the full pipeline: Safrole, guarantees, assurances, PVM accumulation,
 * storage writes, transfers, preimages, and service lifecycle.
 */
class BlockGenerator(config: ChainConfig, validators: Array[DevKeyStore.ValidatorKeys], seed: Long = 42L):

  private var lastHeaderHash: Option[Hash] = None
  private var blockCounter = 0L
  private var packageNonce = 0
  private val rng = new Random(seed)

  // Track submitted package hashes for prerequisite references
  private val recentPackageHashes = scala.collection.mutable.Queue[Hash]()

  // Preimage data templates for the preimage lifecycle
  private val preimageBlobs = (0 until 5).map(i => s"stress-test-preimage-$i".getBytes).toArray

  // Result data templates extracted from real fuzzy traces:
  // 0x0b = minimal op, 0x01 = service-list grow, 0x02 = refill, 0x00 = gift
  private val resultTemplates: Array[Array[Byte]] = Array(
    // 0x0b: minimal (19 bytes) - basic checkpoint/yield
    Array[Byte](0x01, 0x0b) ++ new Array[Byte](17),
    // 0x01: service list grow (31 bytes)
    hexToBytes("010107000000007979ea7584c85676aec856761710adeb4110adebf0dfdd6b"),
    // 0x02: refill (150 bytes)
    hexToBytes("010200000000973300000000000023a2000000000000726566696c6c") ++ new Array[Byte](122),
    // 0x00: gift transfer (195 bytes)
    hexToBytes("0100bb8648e2eb5cab1ebb8129eca29c8af2e5155fe1ac9400ab01852a1e0cc82b1e3d390200000000001027000000000000102700000000000010a601000000006769667400000000000000000000000000000000") ++ new Array[Byte](27),
    // 0x04: auth operation (38 bytes)
    hexToBytes("010468b1c031bb8648e2eb5cab1ebb8129eca29c8af2e5155fe1ac9400ab01852a1e0cc82b1e"),
  )

  private val bsKeyToIndex: Map[String, Int] =
    validators.zipWithIndex.map { case (v, i) => v.bandersnatchPublic.map("%02x".format(_)).mkString -> i }.toMap

  def generateBlock(preState: RawState, slot: Long): Block =
    blockCounter += 1
    val fullState = FullJamState.fromKeyvals(preState.keyvals, config)
    val jamState = JamState.fromFullJamState(fullState, config)

    val dummyEntropy = Hash(new Array[Byte](32))
    val safroleInput = SafroleInput(slot = slot, entropy = dummyEntropy, extrinsic = List.empty)
    val (postSafroleJamState, safroleOutput) = SafroleTransition.stf(safroleInput, jamState, config)

    val outputData = safroleOutput match
      case Right(data) => data
      case Left(_) => throw new RuntimeException(s"Safrole failed for slot $slot")

    val slotIndex = (slot % config.epochLength).toInt
    val (authorIndex, authorBsSecret) = findSealingValidator(postSafroleJamState, slotIndex)

    val (guarantees, assurances) = buildExtrinsics(jamState, postSafroleJamState, slot)
    val preimages = buildPreimages(jamState, slot)

    val extrinsic = Extrinsic(
      tickets = List.empty,
      preimages = preimages,
      guarantees = guarantees,
      assurances = assurances,
      disputes = Dispute(List.empty, List.empty, List.empty)
    )
    val extrinsicHash = computeExtrinsicHash(extrinsic)

    val parentHash = lastHeaderHash.getOrElse {
      jamState.beta.history.lastOption.map(_.headerHash).getOrElse(Hash.zero)
    }

    val sealEntropy = if postSafroleJamState.entropy.pool.length > 3
      then postSafroleJamState.entropy.pool(3).bytes
      else new Array[Byte](32)

    val sealVrfInput = postSafroleJamState.gamma.s match
      case TicketsOrKeys.Keys(_) => SigningContext.fallbackSealInputData(sealEntropy)
      case TicketsOrKeys.Tickets(tickets) =>
        SigningContext.safroleTicketInputData(sealEntropy, tickets(slotIndex).attempt.toByte)

    val dummySeal = BandersnatchWrapper.ietfVrfSign(authorBsSecret, sealVrfInput, Array.empty)
    val sealVrfOutput = BandersnatchWrapper.getIetfVrfOutput(dummySeal)
    val entropyVrfInput = SigningContext.entropyInputData(sealVrfOutput)
    val entropyVrf = BandersnatchWrapper.ietfVrfSign(authorBsSecret, entropyVrfInput, Array.empty)

    val headerCodec = Header.headerCodec(config.validatorCount, config.epochLength)
    val headerWithEntropy = Header(
      parent = parentHash,
      parentStateRoot = preState.stateRoot,
      extrinsicHash = extrinsicHash,
      slot = Timeslot(UInt(slot.toInt)),
      epochMark = outputData.epochMark,
      ticketsMark = outputData.ticketsMark,
      offendersMark = List.empty,
      authorIndex = ValidatorIndex(authorIndex),
      entropySource = JamBytes(entropyVrf),
      seal = JamBytes.zeros(96)
    )

    val fullHeaderBytes = headerCodec.encode(headerWithEntropy).require.bytes.toArray
    val encodedUnsignedHeader = fullHeaderBytes.dropRight(96)
    val seal = BandersnatchWrapper.ietfVrfSign(authorBsSecret, sealVrfInput, encodedUnsignedHeader)
    val finalHeader = headerWithEntropy.copy(seal = JamBytes(seal))

    val finalHeaderBytes = headerCodec.encode(finalHeader).require.bytes.toArray
    lastHeaderHash = Some(Hashing.blake2b256(finalHeaderBytes))

    Block(finalHeader, extrinsic)

  // --- Extrinsic Generation ---

  private def buildExtrinsics(
    preState: JamState,
    postSafroleState: JamState,
    slot: Long
  ): (List[GuaranteeExtrinsic], List[AssuranceExtrinsic]) =
    val reports = preState.cores.reports
    val recentHistory = preState.beta.history

    val core0Pending = reports.lift(0).flatten.isDefined
    val core1Pending = reports.lift(1).flatten.isDefined

    if core0Pending || core1Pending then
      val bitfield = buildAssuranceBitfield(core0Pending, core1Pending)
      val parentHash = lastHeaderHash.getOrElse {
        recentHistory.lastOption.map(_.headerHash).getOrElse(Hash.zero)
      }
      val assurances = buildAssurances(parentHash, bitfield)
      (List.empty, assurances)
    else if recentHistory.size >= 2 then
      val guarantees = scala.collection.mutable.ListBuffer[GuaranteeExtrinsic]()

      // Core 0: always try
      buildGuarantee(preState, postSafroleState, slot, coreIndex = 0).foreach(guarantees += _)

      // Core 1: ~40% of blocks (both cores active)
      if rng.nextDouble() < 0.4 then
        buildGuarantee(preState, postSafroleState, slot, coreIndex = 1).foreach(guarantees += _)

      (guarantees.sortBy(_.report.coreIndex.toInt).toList, List.empty)
    else
      (List.empty, List.empty)

  private def buildAssuranceBitfield(core0: Boolean, core1: Boolean): Array[Byte] =
    val b = (if core0 then 1 else 0) | (if core1 then 2 else 0)
    Array(b.toByte)

  private def buildAssurances(parentHash: Hash, bitfield: Array[Byte]): List[AssuranceExtrinsic] =
    val serializedData = parentHash.bytes ++ bitfield
    val dataHash = Hashing.blake2b256(serializedData)
    val message = constants.JAM_AVAILABLE_BYTES ++ dataHash.bytes
    (0 until 5).map { vi =>
      val sig = Ed25519ZebraWrapper.sign(validators(vi).ed25519Secret, message)
      AssuranceExtrinsic(parentHash, JamBytes(bitfield), ValidatorIndex(vi), Ed25519Signature(sig))
    }.toList

  private def buildGuarantee(
    preState: JamState,
    postSafroleState: JamState,
    slot: Long,
    coreIndex: Int
  ): Option[GuaranteeExtrinsic] =
    val recentHistory = preState.beta.history
    if recentHistory.size < 2 then return None

    val coreAuthPool = preState.authPools.lift(coreIndex).getOrElse(List.empty)
    if coreAuthPool.isEmpty then return None
    val authorizerHash = coreAuthPool.head

    val anchorEntry = recentHistory(recentHistory.size - 2)
    val serviceCodeHash = preState.accumulation.serviceAccounts
      .find(_.id == 0).map(_.data.service.codeHash).getOrElse(return None)

    packageNonce += 1
    val packageHash = Hashing.blake2b256(s"stress-pkg-$packageNonce".getBytes)

    // Track for prerequisites
    recentPackageHashes.enqueue(packageHash)
    if recentPackageHashes.size > 20 then recentPackageHashes.dequeue()

    // Variable number of results (1-4)
    val numResults = 1 + rng.nextInt(4)
    val totalGas = config.reportAccGas
    val gasPerResult = totalGas / numResults

    val results = (0 until numResults).map { ri =>
      val templateIdx = rng.nextInt(resultTemplates.length)
      val resultData = resultTemplates(templateIdx).clone()
      // Randomize last few bytes to avoid identical accumulations
      if resultData.length > 4 then
        val offset = resultData.length - 4
        rng.nextBytes(resultData.slice(offset, resultData.length))
        System.arraycopy(resultData.slice(offset, resultData.length), 0, resultData, offset, 4)

      WorkResult(
        serviceId = ServiceId(UInt(0)),
        codeHash = serviceCodeHash,
        payloadHash = Hashing.blake2b256(s"payload-$packageNonce-$ri".getBytes),
        accumulateGas = Gas(gasPerResult),
        result = ExecutionResult.Ok(JamBytes(resultData)),
        refineLoad = RefineLoad(
          Gas(1000L + rng.nextInt(5000)),
          UShort(rng.nextInt(3)),
          UShort(1 + rng.nextInt(6)),
          UInt(100 + rng.nextInt(500)),
          UShort(rng.nextInt(2))
        )
      )
    }.toList

    // Prerequisites: reference earlier package hashes (~30% of guarantees)
    val prerequisites = if recentPackageHashes.size > 3 && rng.nextDouble() < 0.3 then
      // Pick 1-2 recent package hashes that are in recent history
      val historyHashes = recentHistory.flatMap(_.reported.map(_.hash)).toSet
      recentPackageHashes.toList
        .filter(historyHashes.contains)
        .take(1 + rng.nextInt(2))
    else List.empty

    val report = WorkReport(
      packageSpec = PackageSpec(packageHash, UInt(100 + rng.nextInt(5000)), Hash.zero, Hash.zero, UShort(0)),
      context = Context(
        anchor = anchorEntry.headerHash,
        stateRoot = anchorEntry.stateRoot,
        beefyRoot = anchorEntry.beefyRoot,
        lookupAnchor = anchorEntry.headerHash,
        lookupAnchorSlot = Timeslot(UInt(preState.tau.toInt)),
        prerequisites = prerequisites
      ),
      coreIndex = CoreIndex(coreIndex),
      authorizerHash = authorizerHash,
      authGasUsed = Gas(3),
      authOutput = JamBytes.empty,
      segmentRootLookup = List.empty,
      results = results
    )

    val reportEncoded = summon[Codec[WorkReport]].encode(report).require.toByteArray
    val reportHash = Hashing.blake2b256(reportEncoded)
    val sigMessage = constants.JAM_GUARANTEE_BYTES ++ reportHash.bytes

    val entropy = postSafroleState.entropy.pool
    val coreAssignments = calculateCoreAssignments(entropy(2), slot, config)
    val assignedValidators = coreAssignments.zipWithIndex
      .filter(_._1 == coreIndex).map(_._2).take(3)

    if assignedValidators.size < 2 then return None

    val signatures = assignedValidators.take(3).map { vi =>
      val sig = Ed25519ZebraWrapper.sign(validators(vi).ed25519Secret, sigMessage)
      GuaranteeSignature(ValidatorIndex(vi), Ed25519Signature(sig))
    }.sortBy(_.validatorIndex.value.toInt)

    Some(GuaranteeExtrinsic(report, Timeslot(UInt(slot.toInt)), signatures))

  // Preimage submissions (~15% of blocks after block 10)
  private def buildPreimages(preState: JamState, slot: Long): List[Preimage] =
    if slot < 10 || rng.nextDouble() > 0.15 then return List.empty
    val blob = preimageBlobs(rng.nextInt(preimageBlobs.length))
    // Preimage requires the service to have solicited it first, which is complex.
    // For now, return empty — preimage lifecycle requires PVM cooperation.
    List.empty

  // --- Helpers ---

  private def calculateCoreAssignments(randomness: Hash, slot: Long, config: ChainConfig): List[Int] =
    val source = (0 until config.validatorCount).map(i => (config.coresCount * i) / config.validatorCount).toList
    val shuffledIndices = Shuffle.jamComputeShuffle(config.validatorCount, randomness)
    val shift = (math.floorMod(slot, config.epochLength) / config.rotationPeriod).toInt
    shuffledIndices.map(idx => math.floorMod(source(idx) + shift, config.coresCount))

  private def findSealingValidator(state: JamState, slotIndex: Int): (Int, Array[Byte]) =
    state.gamma.s match
      case TicketsOrKeys.Keys(keys) =>
        val keyHex = keys(slotIndex).bytes.map("%02x".format(_)).mkString
        bsKeyToIndex.get(keyHex) match
          case Some(idx) => (idx, validators(idx).bandersnatchSecret)
          case None =>
            val fallbackIdx = validators.indices.find(i =>
              java.util.Arrays.equals(validators(i).bandersnatchPublic, keys(slotIndex).bytes)
            ).getOrElse(0)
            (fallbackIdx, validators(fallbackIdx).bandersnatchSecret)
      case TicketsOrKeys.Tickets(_) =>
        (0, validators(0).bandersnatchSecret)

  private def computeExtrinsicHash(ex: Extrinsic): Hash =
    import io.forge.jam.core.scodec.JamCodecs.{compactPrefixedList, compactInt}
    val ticketsEncoded = compactPrefixedList(summon[Codec[TicketEnvelope]]).encode(ex.tickets).require.toByteArray
    val preimagesEncoded = compactPrefixedList(summon[Codec[Preimage]]).encode(ex.preimages).require.toByteArray
    val assurancesEncoded = compactPrefixedList(AssuranceExtrinsic.codec(config.coresCount)).encode(ex.assurances).require.toByteArray
    val disputesEncoded = Dispute.codec(config.votesPerVerdict).encode(ex.disputes).require.toByteArray

    val guaranteeItems = ex.guarantees.map { g =>
      val reportEncoded = summon[Codec[WorkReport]].encode(g.report).require.toByteArray
      val reportHash = Hashing.blake2b256(reportEncoded)
      val timeslotEncoded = _root_.scodec.codecs.uint32L.encode(g.slot.value.toLong & 0xffffffffL).require.toByteArray
      val credentialEncoded = compactPrefixedList(summon[Codec[GuaranteeSignature]]).encode(g.signatures).require.toByteArray
      reportHash.bytes ++ timeslotEncoded ++ credentialEncoded
    }
    val guaranteeListLenEncoded = compactInt.encode(guaranteeItems.length).require.toByteArray
    val gEncoded = guaranteeListLenEncoded ++ guaranteeItems.foldLeft(Array.empty[Byte])(_ ++ _)

    val hashes = List(ticketsEncoded, preimagesEncoded, gEncoded, assurancesEncoded, disputesEncoded).map(Hashing.blake2b256)
    Hashing.blake2b256(hashes.flatMap(_.bytes).toArray)

  private def hexToBytes(hex: String): Array[Byte] =
    hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  def reset(): Unit =
    lastHeaderHash = None
    blockCounter = 0
    packageNonce = 0
    recentPackageHashes.clear()
