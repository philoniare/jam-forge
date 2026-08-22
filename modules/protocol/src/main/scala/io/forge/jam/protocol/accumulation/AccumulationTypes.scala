package io.forge.jam.protocol.accumulation

import io.forge.jam.core.JamBytes
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.protocol.state.ServiceStorageView
import spire.math.{UInt, ULong}

import scala.collection.mutable

/** Contains extracted work item data combined with work report context.
  *
  * @param packageHash
  *   Work package hash (32 bytes)
  * @param segmentRoot
  *   Segment root from availability spec (32 bytes)
  * @param authorizerHash
  *   Authorizer hash (32 bytes)
  * @param payloadHash
  *   Work item payload hash (32 bytes)
  * @param gasLimit
  *   Gas limit for accumulation
  * @param authTrace
  *   Authorizer trace output (variable length)
  * @param result
  *   Refinement result (blob or error)
  */
final case class OperandTuple(
    packageHash: JamBytes,
    segmentRoot: JamBytes,
    authorizerHash: JamBytes,
    payloadHash: JamBytes,
    gasLimit: Long,
    authTrace: JamBytes,
    result: ExecutionResult
)

/** Deferred transfer Represents a transfer queued during accumulation for
  * processing in next iteration.
  *
  * @param source
  *   Source service index
  * @param destination
  *   Destination service index
  * @param amount
  *   Balance to transfer
  * @param memo
  *   Memo (128 bytes fixed size)
  * @param gasLimit
  *   Gas limit for on_transfer handler
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

/** Accumulation input - union of OperandTuple or DeferredTransfer
  */
sealed trait AccumulationOperand:
  /** Encode the operand to bytes using Gray Paper natural encoding.
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
        case ExecutionResult.OOG          => Array[Byte](1)
        case ExecutionResult.Panic        => Array[Byte](2)
        case ExecutionResult.BadExports   => Array[Byte](3)
        case ExecutionResult.Oversize     => Array[Byte](4)
        case ExecutionResult.BadCode      => Array[Byte](5)
        case ExecutionResult.CodeTooLarge => Array[Byte](6)

      val authTraceLen =
        JamCodecs.encodeCompactInteger(op.authTrace.length.toLong)
      val authTrace = op.authTrace.toArray
      val out = new java.io.ByteArrayOutputStream(
        variant.length + 4 * 32 + gasLimitBytes.length + resultBytes.length +
          authTraceLen.length + authTrace.length
      )
      out.write(variant, 0, variant.length)
      out.write(op.packageHash.toArray)
      out.write(op.segmentRoot.toArray)
      out.write(op.authorizerHash.toArray)
      out.write(op.payloadHash.toArray)
      out.write(gasLimitBytes, 0, gasLimitBytes.length)
      out.write(resultBytes, 0, resultBytes.length)
      out.write(authTraceLen, 0, authTraceLen.length)
      out.write(authTrace)
      out.toByteArray

  /** Transfer operand containing a DeferredTransfer. Encoded as variant 1.
    */
  final case class Transfer(transfer: DeferredTransfer)
      extends AccumulationOperand:
    override def encode(): Array[Byte] =
      val variant = JamCodecs.encodeCompactInteger(1)
      val source = JamCodecs.encodeU32LE(UInt(transfer.source.toInt))
      val destination = JamCodecs.encodeU32LE(UInt(transfer.destination.toInt))
      val amount = JamCodecs.encodeU64LE(ULong(transfer.amount))
      val memo = transfer.memo.toArray
      val gasLimit = JamCodecs.encodeU64LE(ULong(transfer.gasLimit))
      val out = new java.io.ByteArrayOutputStream(
        variant.length + source.length + destination.length + amount.length +
          memo.length + gasLimit.length
      )
      out.write(variant, 0, variant.length)
      out.write(source, 0, source.length)
      out.write(destination, 0, destination.length)
      out.write(amount, 0, amount.length)
      out.write(memo)
      out.write(gasLimit, 0, gasLimit.length)
      out.toByteArray

/** Key for preimage requests (hash + length).
  *
  * @param hash
  *   Hash of the preimage (32 bytes)
  * @param length
  *   Expected length of the preimage
  */
final case class PreimageKey(
    hash: Hash,
    length: Int
)

/** Preimage request state.
  *
  * @param requestedAt
  *   Timestamps (timeslots) when requested (0-3 entries)
  */
final case class PreimageRequest(
    requestedAt: List[Long]
)

/** Service account combining service info with immutable storage and preimages.
  * Used during accumulation to track state changes.
  *
  * @param info
  *   Service info containing balance, code hash, gas limits, etc.
  * @param storage
  *   Key-value storage (immutable, structurally shared)
  * @param preimages
  *   Hash to blob mapping (immutable, structurally shared)
  * @param preimageRequests
  *   Requested preimages (immutable, structurally shared)
  * @param lastAccumulated
  *   Last accumulation timestamp
  */
final case class ServiceAccount(
    info: ServiceInfo,
    storage: Map[JamBytes, JamBytes],
    preimages: Map[Hash, JamBytes],
    preimageRequests: Map[PreimageKey, PreimageRequest],
    lastAccumulated: Long = 0
)

object ServiceAccount:
  /** Create an empty service account with default values.
    */
  def empty(info: ServiceInfo): ServiceAccount =
    ServiceAccount(
      info = info,
      storage = Map.empty,
      preimages = Map.empty,
      preimageRequests = Map.empty,
      lastAccumulated = 0
    )

/** Mutable subset of JAM state used during accumulation. Contains state
  * components both needed and mutable by the accumulation process.
  *
  * @param accounts
  *   Service accounts by index
  * @param stagingSet
  *   Validator keys for staging
  * @param authQueue
  *   Per-core authorization queues
  * @param manager
  *   Manager service ID
  * @param assigners
  *   Per-core assigners
  * @param delegator
  *   Delegator service ID
  * @param registrar
  *   Registrar service ID
  * @param alwaysAccers
  *   Always-accumulate services to gas mapping
  * @param rawServiceDataByStateKey
  *   Raw state data lookups
  * @param rawServiceAccountsByStateKey
  *   Raw account lookups
  */
final case class PartialState(
    var accounts: Map[Long, ServiceAccount],
    stagingSet: mutable.ListBuffer[JamBytes],
    authQueue: mutable.ListBuffer[mutable.ListBuffer[JamBytes]],
    var manager: Long,
    assigners: mutable.ListBuffer[Long],
    var delegator: Long,
    var registrar: Long,
    alwaysAccers: mutable.Map[Long, Long],
    var rawServiceDataByStateKey: Map[JamBytes, JamBytes] = Map.empty,
    var rawServiceAccountsByStateKey: Map[JamBytes, JamBytes] = Map.empty
):
  /** Create a copy-on-write snapshot of this partial state.
    */
  def deepCopy(): PartialState =
    PartialState(
      accounts = accounts,
      stagingSet =
        mutable.ListBuffer.from(stagingSet),
      authQueue = mutable.ListBuffer.from(
        authQueue.map(q => mutable.ListBuffer.from(q))
      ),
      manager = manager,
      assigners = mutable.ListBuffer.from(assigners),
      delegator = delegator,
      registrar = registrar,
      alwaysAccers = mutable.Map.from(alwaysAccers),
      rawServiceDataByStateKey = rawServiceDataByStateKey,
      rawServiceAccountsByStateKey = rawServiceAccountsByStateKey
    )

  /** Create a shallow copy that shares all collections except accounts
    */
  def shallowCopyWithAccountUpdate(
      accountId: Long,
      updatedAccount: ServiceAccount
  ): PartialState =
    PartialState(
      accounts = accounts.updated(accountId, updatedAccount),
      stagingSet = stagingSet,
      authQueue = authQueue,
      manager = manager,
      assigners = assigners,
      delegator = delegator,
      registrar = registrar,
      alwaysAccers = alwaysAccers,
      rawServiceDataByStateKey = rawServiceDataByStateKey,
      rawServiceAccountsByStateKey = rawServiceAccountsByStateKey
    )

object PartialState:
  /** Create an empty partial state with default values.
    */
  def empty: PartialState =
    PartialState(
      accounts = Map.empty,
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )

/** Execution exit reason for PVM. Determines whether to use normal state (x) or
  * checkpoint state (y).
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

/** Accumulation context managing dual state (x for normal, y for checkpoint).
  * Provides checkpoint and collapse operations for accumulation.
  *
  * @param x
  *   Normal execution state
  * @param y
  *   Checkpoint state (used on panic)
  * @param serviceIndex
  *   Current service being accumulated
  * @param timeslot
  *   Current timeslot
  * @param entropy
  *   Entropy for the epoch
  * @param deferredTransfers
  *   Transfers queued during accumulation (normal state)
  * @param deferredTransfersCheckpoint
  *   Checkpoint of deferred transfers
  * @param provisions
  *   Preimage provisions (normal state)
  * @param provisionsCheckpoint
  *   Checkpoint of provisions
  * @param yieldHash
  *   Accumulation output hash (normal state)
  * @param yieldCheckpoint
  *   Checkpoint of yield hash
  * @param nextAccountIndex
  *   Next available service account index
  * @param minPublicServiceIndex
  *   Minimum public service index (S_S from Gray Paper, 2^16)
  */
final class AccumulationContext(
    var x: PartialState,
    initialY: PartialState,
    val serviceIndex: Long,
    val timeslot: Long,
    val entropy: JamBytes,
    val deferredTransfers: mutable.ListBuffer[DeferredTransfer] =
      mutable.ListBuffer.empty,
    val deferredTransfersCheckpoint: mutable.ListBuffer[DeferredTransfer] =
      mutable.ListBuffer.empty,
    val provisions: mutable.Set[(Long, JamBytes)] = mutable.Set.empty,
    val provisionsCheckpoint: mutable.Set[(Long, JamBytes)] = mutable.Set.empty,
    var yieldHash: Option[JamBytes] = None,
    var yieldCheckpoint: Option[JamBytes] = None,
    var nextAccountIndex: Long = 65536L,
    val minPublicServiceIndex: Long = 65536L,
    val storageView: Option[ServiceStorageView] = None
):

  storageView.foreach(_.savepoint())

  private var _y: PartialState = initialY
  private var ySnapshotPending: Boolean = initialY == null

  /** Materialise the pending checkpoint snapshot from the current
    */
  def captureCheckpointIfPending(): Unit =
    if ySnapshotPending then
      _y = x.deepCopy()
      ySnapshotPending = false

  /** Checkpoint state y
    */
  def y: PartialState =
    captureCheckpointIfPending()
    _y

  /** Replace the checkpoint snapshot wholesale
    */
  def y_=(value: PartialState): Unit =
    _y = value
    ySnapshotPending = false


  def readRawData(stateKey: JamBytes): Option[JamBytes] =
    storageView match
      case Some(v) => v.getByStateKey(stateKey)
      case None    => x.rawServiceDataByStateKey.get(stateKey)

  def readRawDataFor(
      ownerServiceId: Long,
      stateKey: JamBytes
  ): Option[JamBytes] =
    if ownerServiceId == serviceIndex then readRawData(stateKey)
    else
      storageView match
        case Some(v) => v.readTrie(stateKey)
        case None    => x.rawServiceDataByStateKey.get(stateKey)

  def writeRawData(stateKey: JamBytes, value: JamBytes): Unit =
    storageView match
      case Some(v) => v.putByStateKey(stateKey, value)
      case None    =>
        x.rawServiceDataByStateKey =
          x.rawServiceDataByStateKey.updated(stateKey, value)

  def deleteRawData(stateKey: JamBytes): Unit =
    storageView match
      case Some(v) => v.deleteByStateKey(stateKey)
      case None    =>
        x.rawServiceDataByStateKey =
          x.rawServiceDataByStateKey.removed(stateKey)

  /** Checkpoint: copy current state x to checkpoint y, including yield,
    * provisions, and transfers.
    */
  def checkpoint(): Unit =
    _y = x.deepCopy()
    ySnapshotPending = false
    yieldCheckpoint = yieldHash
    provisionsCheckpoint.clear()
    provisionsCheckpoint ++= provisions
    deferredTransfersCheckpoint.clear()
    deferredTransfersCheckpoint ++= deferredTransfers
    storageView.foreach { v =>
      v.discardCheckpoint()
      v.savepoint()
    }

  /** Collapse: select final state based on exit reason. On panic, out of gas,
    * page fault, or invalid code, revert to checkpoint state y.
    *
    * @param exitReason
    *   The reason for execution termination
    * @return
    *   The appropriate state (y for error conditions, x otherwise)
    */
  def collapse(exitReason: ExitReason): PartialState =
    exitReason match
      case ExitReason.PANIC | ExitReason.OUT_OF_GAS | ExitReason.PAGE_FAULT |
          ExitReason.INVALID_CODE =>
        storageView.foreach(_.restore())
        y
      case _ =>
        storageView.foreach(_.discardCheckpoint())
        x

  /** Get provisions based on exit reason. On panic or out of gas, use
    * checkpoint provisions.
    *
    * @param exitReason
    *   The reason for execution termination
    * @return
    *   The appropriate provisions set
    */
  def getProvisions(exitReason: ExitReason): Set[(Long, JamBytes)] =
    exitReason match
      case ExitReason.PANIC | ExitReason.OUT_OF_GAS | ExitReason.PAGE_FAULT |
          ExitReason.INVALID_CODE =>
        provisionsCheckpoint.toSet
      case _ => provisions.toSet

  /** Get deferred transfers based on exit reason. On exceptional termination
    * (panic, out of gas, page fault, invalid code), use checkpoint transfers.
    *
    * @param exitReason
    *   The reason for execution termination
    * @return
    *   The appropriate list of deferred transfers
    */
  def getDeferredTransfers(exitReason: ExitReason): List[DeferredTransfer] =
    exitReason match
      case ExitReason.PANIC | ExitReason.OUT_OF_GAS | ExitReason.PAGE_FAULT |
          ExitReason.INVALID_CODE =>
        deferredTransfersCheckpoint.toList
      case _ => deferredTransfers.toList

object AccumulationContext:
  /** Create a new accumulation context with the given parameters. Both x and y
    * states are initialized with the same content (deep copied).
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
      initialY = initialState.deepCopy(),
      serviceIndex = serviceIndex,
      timeslot = timeslot,
      entropy = entropy,
      nextAccountIndex = nextAccountIndex,
      minPublicServiceIndex = minPublicServiceIndex
    )

/** Commitment: service index and hash pair for outputs.
  *
  * @param serviceIndex
  *   Service index
  * @param hash
  *   Output hash (32 bytes)
  */
final case class Commitment(
    serviceIndex: Long,
    hash: JamBytes
)

/** Result of single-service accumulation as defined in Gray Paper equation 291.
  *
  * @param postState
  *   Modified state after accumulation
  * @param deferredTransfers
  *   Outgoing transfers queued during accumulation
  * @param yieldHash
  *   Accumulation output (32-byte hash or None)
  * @param gasUsed
  *   Actual gas consumed
  * @param provisions
  *   Service/blob pairs to provision
  */
final case class AccumulationOneResult(
    postState: PartialState,
    deferredTransfers: List[DeferredTransfer],
    yieldHash: Option[JamBytes],
    gasUsed: Long,
    provisions: Set[(Long, JamBytes)]
)
