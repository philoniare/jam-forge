package io.forge.jam.protocol.accumulation

import io.forge.jam.core.JamBytes
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.core.types.work.ExecutionResult
import spire.math.{UInt, ULong}

import scala.collection.mutable

/**
 * Contains extracted work item data combined with work report context.
 *
 * @param packageHash Work package hash (32 bytes)
 * @param segmentRoot Segment root from availability spec (32 bytes)
 * @param authorizerHash Authorizer hash (32 bytes)
 * @param payloadHash Work item payload hash (32 bytes)
 * @param gasLimit Gas limit for accumulation
 * @param authTrace Authorizer trace output (variable length)
 * @param result Refinement result (blob or error)
 * @param codeHash Code hash from work result for preimage lookup (32 bytes)
 */
final case class OperandTuple(
  packageHash: JamBytes,
  segmentRoot: JamBytes,
  authorizerHash: JamBytes,
  payloadHash: JamBytes,
  gasLimit: Long,
  authTrace: JamBytes,
  result: ExecutionResult,
  codeHash: JamBytes
)

/**
 * Deferred transfer
 * Represents a transfer queued during accumulation for processing in next iteration.
 *
 * @param source Source service index
 * @param destination Destination service index
 * @param amount Balance to transfer
 * @param memo Memo (128 bytes fixed size)
 * @param gasLimit Gas limit for on_transfer handler
 */
final case class DeferredTransfer(
  source: Long,
  destination: Long,
  amount: Long,
  memo: JamBytes,
  gasLimit: Long
)

object DeferredTransfer:
  val MEMO_SIZE: Int = 128

/**
 * Accumulation input - union of OperandTuple or DeferredTransfer
 */
sealed trait AccumulationOperand:
  /**
   * Encode the operand to bytes using Gray Paper natural encoding.
   */
  def encode(): Array[Byte]

object AccumulationOperand:
  final case class WorkItem(operand: OperandTuple) extends AccumulationOperand:
    override def encode(): Array[Byte] =
      val op = operand

      val variant = JamCodecs.encodeCompactInteger(0)
      val gasLimitBytes = JamCodecs.encodeCompactInteger(op.gasLimit)
      val resultBytes = op.result match
        case ExecutionResult.Ok(output) =>
          val len = JamCodecs.encodeCompactInteger(output.length.toLong)
          Array[Byte](0) ++ len ++ output.toArray // Tag 0 for Success (UInt8)
        case ExecutionResult.Panic =>
          Array[Byte](2) // Tag 2 for Panic (UInt8)

      val authTraceLen = JamCodecs.encodeCompactInteger(op.authTrace.length.toLong)
      variant ++
        op.packageHash.toArray ++ op.segmentRoot.toArray ++ op.authorizerHash.toArray ++
        op.payloadHash.toArray ++ gasLimitBytes ++ resultBytes ++ authTraceLen ++ op.authTrace.toArray

  /**
   * Transfer operand containing a DeferredTransfer.
   * Encoded as variant 1.
   */
  final case class Transfer(transfer: DeferredTransfer) extends AccumulationOperand:
    override def encode(): Array[Byte] =
      JamCodecs.encodeCompactInteger(1) ++
        JamCodecs.encodeU32LE(UInt(transfer.source.toInt)) ++
        JamCodecs.encodeU32LE(UInt(transfer.destination.toInt)) ++
        JamCodecs.encodeU64LE(ULong(transfer.amount)) ++
        transfer.memo.toArray ++
        JamCodecs.encodeU64LE(ULong(transfer.gasLimit))

/**
 * Key for preimage requests (hash + length).
 *
 * @param hash Hash of the preimage (32 bytes)
 * @param length Expected length of the preimage
 */
final case class PreimageKey(
  hash: Hash,
  length: Int
)

/**
 * Preimage request state.
 *
 * @param requestedAt Timestamps (timeslots) when requested (0-3 entries)
 */
final case class PreimageRequest(
  requestedAt: List[Long]
)

/**
 * Service account combining service info with mutable storage and preimages.
 * Used during accumulation to track state changes.
 *
 * @param info Service info containing balance, code hash, gas limits, etc.
 * @param storage Key-value storage (mutable)
 * @param preimages Hash to blob mapping (mutable)
 * @param preimageRequests Requested preimages (mutable)
 * @param lastAccumulated Last accumulation timestamp
 */
final case class ServiceAccount(
  info: ServiceInfo,
  storage: mutable.Map[JamBytes, JamBytes],
  preimages: mutable.Map[Hash, JamBytes],
  preimageRequests: mutable.Map[PreimageKey, PreimageRequest],
  var lastAccumulated: Long = 0
):
  /**
   * Create a deep copy of this service account.
   * All mutable collections are copied to ensure independence.
   */
  def copy(): ServiceAccount =
    ServiceAccount(
      info = info.copy(),
      storage = mutable.Map.from(storage),
      preimages = mutable.Map.from(preimages),
      preimageRequests = mutable.Map.from(preimageRequests),
      lastAccumulated = lastAccumulated
    )

object ServiceAccount:
  /**
   * Create an empty service account with default values.
   */
  def empty(info: ServiceInfo): ServiceAccount =
    ServiceAccount(
      info = info,
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty,
      lastAccumulated = 0
    )

/**
 * Mutable subset of JAM state used during accumulation.
 * Contains state components both needed and mutable by the accumulation process.
 *
 * @param accounts Service accounts by index
 * @param stagingSet Validator keys for staging
 * @param authQueue Per-core authorization queues
 * @param manager Manager service ID
 * @param assigners Per-core assigners
 * @param delegator Delegator service ID
 * @param registrar Registrar service ID
 * @param alwaysAccers Always-accumulate services to gas mapping
 * @param rawServiceDataByStateKey Raw state data lookups
 * @param rawServiceAccountsByStateKey Raw account lookups
 */
final case class PartialState(
  accounts: mutable.Map[Long, ServiceAccount],
  stagingSet: mutable.ListBuffer[JamBytes],
  authQueue: mutable.ListBuffer[mutable.ListBuffer[JamBytes]],
  var manager: Long,
  assigners: mutable.ListBuffer[Long],
  var delegator: Long,
  var registrar: Long,
  alwaysAccers: mutable.Map[Long, Long],
  rawServiceDataByStateKey: mutable.Map[JamBytes, JamBytes] = mutable.Map.empty,
  rawServiceAccountsByStateKey: mutable.Map[JamBytes, JamBytes] = mutable.Map.empty
):
  /**
   * Create a deep copy of this partial state.
   * All mutable collections and contained objects are copied to ensure independence.
   */
  def deepCopy(): PartialState =
    PartialState(
      accounts = mutable.Map.from(accounts.view.mapValues(_.copy())),
      stagingSet = mutable.ListBuffer.from(stagingSet.map(b => JamBytes(b.toArray))),
      authQueue = mutable.ListBuffer.from(authQueue.map(q => mutable.ListBuffer.from(q.map(h => JamBytes(h.toArray))))),
      manager = manager,
      assigners = mutable.ListBuffer.from(assigners),
      delegator = delegator,
      registrar = registrar,
      alwaysAccers = mutable.Map.from(alwaysAccers),
      rawServiceDataByStateKey = mutable.Map.from(rawServiceDataByStateKey),
      rawServiceAccountsByStateKey = mutable.Map.from(rawServiceAccountsByStateKey)
    )

object PartialState:
  /**
   * Create an empty partial state with default values.
   */
  def empty: PartialState =
    PartialState(
      accounts = mutable.Map.empty,
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )

/**
 * Execution exit reason for PVM.
 * Determines whether to use normal state (x) or checkpoint state (y).
 */
enum ExitReason:
  /** Normal completion - use normal state x */
  case HALT

  /** Panic - use checkpoint state y */
  case PANIC

  /** Gas exhausted - use checkpoint state y */
  case OUT_OF_GAS

  /** Memory access error - use checkpoint state y */
  case PAGE_FAULT

  /** Awaiting host call response */
  case HOST_CALL

  /** Code compilation failed - use checkpoint state y */
  case INVALID_CODE

/**
 * Accumulation context managing dual state (x for normal, y for checkpoint).
 * Provides checkpoint and collapse operations for accumulation.
 *
 * @param x Normal execution state
 * @param y Checkpoint state (used on panic)
 * @param serviceIndex Current service being accumulated
 * @param timeslot Current timeslot
 * @param entropy Entropy for the epoch
 * @param deferredTransfers Transfers queued during accumulation (normal state)
 * @param deferredTransfersCheckpoint Checkpoint of deferred transfers
 * @param provisions Preimage provisions (normal state)
 * @param provisionsCheckpoint Checkpoint of provisions
 * @param yieldHash Accumulation output hash (normal state)
 * @param yieldCheckpoint Checkpoint of yield hash
 * @param nextAccountIndex Next available service account index
 * @param minPublicServiceIndex Minimum public service index (S_S from Gray Paper, 2^16)
 */
final class AccumulationContext(
  var x: PartialState,
  var y: PartialState,
  val serviceIndex: Long,
  val timeslot: Long,
  val entropy: JamBytes,
  val deferredTransfers: mutable.ListBuffer[DeferredTransfer] = mutable.ListBuffer.empty,
  val deferredTransfersCheckpoint: mutable.ListBuffer[DeferredTransfer] = mutable.ListBuffer.empty,
  val provisions: mutable.Set[(Long, JamBytes)] = mutable.Set.empty,
  val provisionsCheckpoint: mutable.Set[(Long, JamBytes)] = mutable.Set.empty,
  var yieldHash: Option[JamBytes] = None,
  var yieldCheckpoint: Option[JamBytes] = None,
  var nextAccountIndex: Long = 65536L,
  val minPublicServiceIndex: Long = 65536L
):

  /**
   * Checkpoint: copy current state x to checkpoint y, including yield, provisions, and transfers.
   */
  def checkpoint(): Unit =
    y = x.deepCopy()
    yieldCheckpoint = yieldHash
    provisionsCheckpoint.clear()
    provisionsCheckpoint ++= provisions
    deferredTransfersCheckpoint.clear()
    deferredTransfersCheckpoint ++= deferredTransfers

  /**
   * Collapse: select final state based on exit reason.
   * On panic, out of gas, page fault, or invalid code, revert to checkpoint state y.
   *
   * @param exitReason The reason for execution termination
   * @return The appropriate state (y for error conditions, x otherwise)
   */
  def collapse(exitReason: ExitReason): PartialState =
    exitReason match
      case ExitReason.PANIC | ExitReason.OUT_OF_GAS | ExitReason.PAGE_FAULT | ExitReason.INVALID_CODE => y
      case _ => x

  /**
   * Get provisions based on exit reason.
   * On panic or out of gas, use checkpoint provisions.
   *
   * @param exitReason The reason for execution termination
   * @return The appropriate provisions set
   */
  def getProvisions(exitReason: ExitReason): Set[(Long, JamBytes)] =
    exitReason match
      case ExitReason.PANIC | ExitReason.OUT_OF_GAS => provisionsCheckpoint.toSet
      case _ => provisions.toSet

  /**
   * Get deferred transfers based on exit reason.
   * On panic or out of gas, use checkpoint transfers.
   *
   * @param exitReason The reason for execution termination
   * @return The appropriate list of deferred transfers
   */
  def getDeferredTransfers(exitReason: ExitReason): List[DeferredTransfer] =
    exitReason match
      case ExitReason.PANIC | ExitReason.OUT_OF_GAS => deferredTransfersCheckpoint.toList
      case _ => deferredTransfers.toList

object AccumulationContext:
  /**
   * Create a new accumulation context with the given parameters.
   * Both x and y states are initialized with the same content (deep copied).
   */
  def apply(
    initialState: PartialState,
    serviceIndex: Long,
    timeslot: Long,
    entropy: JamBytes,
    nextAccountIndex: Long = 65536L,
    minPublicServiceIndex: Long = 65536L
  ): AccumulationContext =
    new AccumulationContext(
      x = initialState.deepCopy(),
      y = initialState.deepCopy(),
      serviceIndex = serviceIndex,
      timeslot = timeslot,
      entropy = entropy,
      nextAccountIndex = nextAccountIndex,
      minPublicServiceIndex = minPublicServiceIndex
    )

/**
 * Commitment: service index and hash pair for outputs.
 *
 * @param serviceIndex Service index
 * @param hash Output hash (32 bytes)
 */
final case class Commitment(
  serviceIndex: Long,
  hash: JamBytes
)

/**
 * Result of single-service accumulation as defined in Gray Paper equation 291.
 *
 * @param postState Modified state after accumulation
 * @param deferredTransfers Outgoing transfers queued during accumulation
 * @param yieldHash Accumulation output (32-byte hash or None)
 * @param gasUsed Actual gas consumed
 * @param provisions Service/blob pairs to provision
 */
final case class AccumulationOneResult(
  postState: PartialState,
  deferredTransfers: List[DeferredTransfer],
  yieldHash: Option[JamBytes],
  gasUsed: Long,
  provisions: Set[(Long, JamBytes)]
)

/**
 * Result of parallel accumulation.
 *
 * @param postState Combined modified state
 * @param deferredTransfers All outgoing transfers
 * @param outputs Set of (serviceIndex, hash) pairs
 * @param gasUsed Service to gas used mapping
 */
final case class AccumulationParResult(
  postState: PartialState,
  deferredTransfers: List[DeferredTransfer],
  outputs: Set[Commitment],
  gasUsed: List[(Long, Long)]
)

/**
 * Result of sequential accumulation.
 *
 * @param reportsAccumulated Number of reports accumulated
 * @param postState Final state after all accumulation
 * @param outputs Set of (serviceIndex, hash) pairs
 * @param gasUsed Service to gas used mapping
 */
final case class AccumulationSeqResult(
  reportsAccumulated: Int,
  postState: PartialState,
  outputs: Set[Commitment],
  gasUsed: List[(Long, Long)]
)
