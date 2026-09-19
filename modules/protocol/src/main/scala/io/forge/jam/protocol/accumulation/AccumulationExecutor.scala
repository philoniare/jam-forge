package io.forge.jam.protocol.accumulation

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing, constants}
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.primitives.Hash
import io.forge.jam.pvm.{ExecutionMode, MemoryResult}
import io.forge.jam.pvm.memory.Memory.{isReadable, isWritable}
import io.forge.jam.pvm.engine.{InterpretedModule, InterpretedInstance}
import io.forge.jam.protocol.refine.PvmRunner
import io.forge.jam.protocol.state.ServiceStorageView
import spire.math.{UInt, UByte}

/** Orchestrates PVM execution for accumulation.
  */
class AccumulationExecutor(val config: ChainConfig):

  private var currentStorageView: Option[ServiceStorageView] = None
  def setStorageView(view: Option[ServiceStorageView]): Unit =
    currentStorageView = view
  def storageView: Option[ServiceStorageView] = currentStorageView

  // LRU-bounded module cache to prevent unbounded memory growth
  private val MAX_MODULE_CACHE_SIZE = 256
  private val moduleCache
      : java.util.LinkedHashMap[JamBytes, InterpretedModule] =
    new java.util.LinkedHashMap[JamBytes, InterpretedModule](
      MAX_MODULE_CACHE_SIZE,
      0.75f,
      true
    ) {
      override def removeEldestEntry(
          eldest: java.util.Map.Entry[JamBytes, InterpretedModule]
      ): Boolean =
        size() > MAX_MODULE_CACHE_SIZE
    }

  /** Execute accumulation for a single service. Implements the Psi_A function
    * from Gray Paper.
    */
  def executeService(
      partialState: PartialState,
      timeslot: Long,
      serviceId: Long,
      gasLimit: Long,
      entropy: JamBytes,
      operands: List[AccumulationOperand],
      executionMode: ExecutionMode = ExecutionMode.default
  ): AccumulationOneResult =
    val account = partialState.accounts.get(serviceId)
    if account.isEmpty then
      return createEmptyResult(partialState, Some(serviceId), operands)

    val acc = account.get
    val codeHash = acc.info.codeHash

    // Look up service code from service's own preimages
    val preimage = acc.preimages.get(Hash(codeHash.bytes.toArray))

    if preimage.isEmpty then
      return createEmptyResult(partialState, Some(serviceId), operands)

    val code = extractCodeBlob(preimage.get.toArray)
    if code.isEmpty || code.get.isEmpty || code.get.length > constants.Cmaxservicecodesize
    then return createEmptyResult(partialState, Some(serviceId), operands)

    // Apply incoming transfer balances before execution
    val transferBalance = operands.collect {
      case AccumulationOperand.Transfer(t) => t.amount
    }.sum
    val postTransferState = partialState.deepCopy()
    val deepCopiedAccount = postTransferState.accounts(serviceId)
    postTransferState.accounts = postTransferState.accounts.updated(
      serviceId,
      deepCopiedAccount.copy(
        info = deepCopiedAccount.info
          .copy(balance = deepCopiedAccount.info.balance + transferBalance)
      )
    )

    // Calculate initial nextAccountIndex per Gray Paper:
    // nextfreeid = check((decode[4]{blake(encode(serviceId, entropyaccumulator', timeslot))} mod (2^32-Cminpublicindex-2^8)) + Cminpublicindex)
    val minPublicServiceIndex = config.minPublicServiceIndex
    val initialIndex = calculateInitialIndex(serviceId, entropy, timeslot)
    val s = minPublicServiceIndex
    val modValue = 0xffffffffL - s - 255 // 2^32 - Cminpublicindex - 2^8
    val candidateIndex = s + (initialIndex % modValue)
    val nextAccountIndex =
      findAvailableServiceIndex(
        candidateIndex,
        minPublicServiceIndex,
        postTransferState.accounts
      )

    // Create accumulation context with dual state
    val context = new AccumulationContext(
      x = postTransferState,
      initialY = null,
      serviceIndex = serviceId,
      timeslot = timeslot,
      entropy = entropy,
      nextAccountIndex = nextAccountIndex,
      minPublicServiceIndex = minPublicServiceIndex,
      storageView = currentStorageView
    )

    // Execute PVM
    val execResult = executePvm(context, code.get, gasLimit, operands, JamBytes(codeHash.bytes.toArray), executionMode)

    // Collapse state based on exit reason
    val finalState = context.collapse(execResult.exitReason)

    // Determine yield based on exit reason
    val yieldHash: Option[JamBytes] = execResult.exitReason match
      case ExitReason.PANIC | ExitReason.OUT_OF_GAS | ExitReason.PAGE_FAULT |
          ExitReason.INVALID_CODE =>
        context.yieldCheckpoint
      case ExitReason.HALT =>
        execResult.output match
          case Some(out) if out.length == 32 => Some(JamBytes(out))
          case _                             => context.yieldHash
      case _ => context.yieldHash

    val newDeferred = context.getDeferredTransfers(execResult.exitReason)

    AccumulationOneResult(
      postState = finalState,
      deferredTransfers = newDeferred,
      yieldHash = yieldHash,
      gasUsed = execResult.gasUsed,
      provisions = context.getProvisions(execResult.exitReason)
    )

  /** Execute PVM code with host call handling.
    */
  private def executePvm(
      context: AccumulationContext,
      code: Array[Byte],
      gasLimit: Long,
      operands: List[AccumulationOperand],
      codeHash: JamBytes,
      executionMode: ExecutionMode = ExecutionMode.default
  ): PvmExecResult =
    // Encode input data: timeslot, serviceIndex, operands count
    val inputData = JamCodecs.encodeCompactInteger(context.timeslot) ++
      JamCodecs.encodeCompactInteger(context.serviceIndex) ++
      JamCodecs.encodeCompactInteger(operands.size.toLong)

    // Get or compile module
    val moduleOpt = getOrCompileModule(code, codeHash)
    if moduleOpt.isEmpty then
      return PvmExecResult(ExitReason.INVALID_CODE, 0L, None)

    val module = moduleOpt.get

    val hostCalls = new AccumulationHostCalls(context, operands, config)

    val (exit, gasUsed, output) = PvmRunner.run(
      module = module,
      inputData = inputData,
      gasLimit = gasLimit,
      entryPc = 5,
      hostCalls = hostCalls,
      executionMode = executionMode,
      preDispatch = Some(() => context.captureCheckpointIfPending())
    )

    val exitReason = exit match
      case PvmRunner.PvmExit.Halt      => ExitReason.HALT
      case PvmRunner.PvmExit.Panic     => ExitReason.PANIC
      case PvmRunner.PvmExit.OutOfGas  => ExitReason.OUT_OF_GAS
      case PvmRunner.PvmExit.PageFault => ExitReason.PAGE_FAULT

    // Output is only meaningful on HALT; every other exit yields None.
    PvmExecResult(
      exitReason,
      gasUsed,
      if exitReason == ExitReason.HALT then Some(output) else None
    )

  /** Get or compile a module from code bytes.
    */
  private def getOrCompileModule(code: Array[Byte], codeHash: JamBytes): Option[InterpretedModule] =
    val cached = moduleCache.get(codeHash)
    if cached != null then Some(cached)
    else
      ServiceCode.parseBlob(code).flatMap { blob =>
        InterpretedModule.create(blob) match
          case Right(module) =>
            moduleCache.put(codeHash, module)
            Some(module)
          case Left(_) => None
      }

  /** Extract code blob from preimage data
    */
  private def extractCodeBlob(preimage: Array[Byte]): Option[Array[Byte]] =
    ServiceCode.extractCodeBlob(preimage)

  private def calculateInitialIndex(
      serviceId: Long,
      entropy: JamBytes,
      timeslot: Long
  ): Long =
    val encodedServiceId = JamCodecs.encodeCompactInteger(serviceId)
    val encodedTimeslot = JamCodecs.encodeCompactInteger(timeslot)
    val data = encodedServiceId ++ entropy.toArray ++ encodedTimeslot
    val hash = Hashing.blake2b256(data)
    val hashBytes = hash.bytes.toArray
    val result = (hashBytes(0).toLong & 0xff) |
      ((hashBytes(1).toLong & 0xff) << 8) |
      ((hashBytes(2).toLong & 0xff) << 16) |
      ((hashBytes(3).toLong & 0xff) << 24)
    result

  private def findAvailableServiceIndex(
      candidate: Long,
      minPublicServiceIndex: Long,
      accounts: scala.collection.Map[Long, ServiceAccount]
  ): Long =
    var i = candidate
    val s = minPublicServiceIndex
    val right = (0xffffffffL - s - 255).toLong
    while accounts.contains(i) do
      val left = i - s + 1
      i = s + (left % right)
    i

  private def createEmptyResult(
      state: PartialState,
      serviceId: Option[Long],
      operands: List[AccumulationOperand]
  ): AccumulationOneResult =
    val finalState = serviceId match
      case Some(sid) =>
        val transferBalance = operands.collect {
          case AccumulationOperand.Transfer(t) => t.amount
        }.sum
        if transferBalance > 0 then
          state.accounts.get(sid) match
            case Some(account) =>
              val shallowCopy = state.shallowCopyWithAccountUpdate(
                sid,
                account.copy(info =
                  account.info.copy(balance =
                    account.info.balance + transferBalance
                  )
                )
              )
              shallowCopy
            case None => state
        else state
      case None => state

    AccumulationOneResult(finalState, List.empty, None, 0L, Set.empty)

/** Result of PVM execution.
  */
final case class PvmExecResult(
    exitReason: ExitReason,
    gasUsed: Long,
    output: Option[Array[Byte]]
)

/** Wrapper around InterpretedInstance to implement PvmInstance trait.
  */
class InterpretedInstanceWrapper(instance: InterpretedInstance)
    extends PvmInstance:
  override def reg(regIdx: Int): Long = instance.reg(regIdx)
  override def setReg(regIdx: Int, value: Long): Unit =
    instance.setReg(regIdx, value)
  override def gas: Long = instance.gas
  override def setGas(value: Long): Unit = instance.setGas(value)
  override def forceOutOfGas(): Unit = instance.forceOutOfGas()
  override def isForcedOutOfGas: Boolean = instance.isForcedOutOfGas

  override def readByte(address: Int): Option[Byte] =
    instance.basicMemory.loadU8(UInt(address)) match
      case MemoryResult.Success(v) => Some(v.toByte)
      case _                       => None

  override def writeByte(address: Int, value: Byte): Boolean =
    instance.basicMemory.storeU8(UInt(address), UByte(value)) match
      case MemoryResult.Success(_) => true
      case _                       => false

  override def readBytes(address: Int, length: Int): Option[Array[Byte]] =
    if length < 0 then None
    else
      instance.basicMemory.getMemorySlice(UInt(address), length) match
        case MemoryResult.Success(data) => Some(data)
        case _                          => None

  override def readInto(address: Int, dest: Array[Byte], destOffset: Int, length: Int): Boolean =
    if length < 0 || destOffset < 0 || destOffset + length > dest.length then false
    else
      instance.basicMemory.getMemorySliceInto(UInt(address), dest, destOffset, length) match
        case MemoryResult.Success(_) => true
        case _                       => false

  override def writeBytes(address: Int, data: Array[Byte]): Boolean =
    instance.basicMemory.setMemorySlice(UInt(address), data) match
      case MemoryResult.Success(_) => true
      case _                       => false

  override def isMemoryAccessible(address: Int, length: Int): Boolean =
    instance.basicMemory.getMemorySlice(UInt(address), length) match
      case MemoryResult.Success(_) => true
      case _                       => false

  override def isMemoryReadable(address: Int, length: Int): Boolean =
    length >= 0 && instance.basicMemory.isReadable(UInt(address), length)

  override def isMemoryWritable(address: Int, length: Int): Boolean =
    length >= 0 && instance.basicMemory.isWritable(UInt(address), length)

  override def growHeapPageBounds: Option[(Long, Long)] =
    Some(instance.growHeapPageBounds)

  override def growHeapPages(deltaPages: Long): Unit =
    if deltaPages > 0 then
      val pageBytes = instance.pageSize.signed.toLong & 0xFFFFFFFFL
      val growBytes = deltaPages * pageBytes
      instance.basicMemory.sbrk(UInt(growBytes.toInt)) match
        case Some(_) => ()
        case None =>
          throw new IllegalStateException(
            s"grow_heap invariant violated: BasicMemory.sbrk($growBytes) failed after the " +
              "spec bound b was already checked (r7 <= b) — page-domain bound and " +
              "maxHeapSize have diverged"
          )

