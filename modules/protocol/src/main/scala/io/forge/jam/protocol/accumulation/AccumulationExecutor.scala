package io.forge.jam.protocol.accumulation


import io.forge.jam.protocol.pvmhost.{AccumulateCost, ExecCost, HostCallMeter, ServiceCode}
import io.forge.jam.protocol.state.StateKey
import io.forge.jam.protocol.pipeline.StageTimers
import io.forge.jam.core.{ChainConfig, JamBytes, Hashing, constants}
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.primitives.Hash
import io.forge.jam.pvm.ExecutionMode
import io.forge.jam.pvm.engine.InterpretedModule
import io.forge.jam.protocol.pvmhost.PvmRunner
import io.forge.jam.protocol.state.ServiceStorageView

/** Orchestrates PVM execution for accumulation.
  *
  * @param parallelServices
  *   whether the services of one accpar batch run concurrently. Both engines allow it: compiled code is immutable
  *   and shared, and everything a native run mutates lives in its thread's own execution context.
  */
class AccumulationExecutor(
    val config: ChainConfig,
    val parallelServices: Boolean = true
):

  /** Execute accumulation for a single service. Implements the Psi_A function.
    */
  def executeService(
      partialState: PartialState,
      timeslot: Long,
      serviceId: Long,
      gasLimit: Long,
      entropy: JamBytes,
      operands: List[AccumulationOperand],
      executionMode: ExecutionMode = ExecutionMode.default,
      storageView: Option[ServiceStorageView] = None
  ): AccumulationOneResult =
    val started = System.nanoTime()
    def elapsed = System.nanoTime() - started
    val account = partialState.accounts.get(serviceId)
    if account.isEmpty then
      return createEmptyResult(partialState, Some(serviceId), operands, elapsed)

    val acc = account.get
    val codeHash = acc.info.codeHash

    val preimage = acc.preimages
      .get(Hash(codeHash.bytes.toArray))
      .orElse(readOwnPreimage(partialState, serviceId, codeHash.bytes.toArray, storageView))

    if preimage.isEmpty then
      return createEmptyResult(partialState, Some(serviceId), operands, elapsed)

    // The process-wide module cache only returns a module built from identical code bytes, so the blob is read
    // without an extra copy of the preimage.
    val code = extractCodeBlob(preimage.get.toArrayUnsafe)
    if code.isEmpty || code.get.isEmpty || code.get.length > constants.Cmaxservicecodesize
    then return createEmptyResult(partialState, Some(serviceId), operands, elapsed)
    val module: Option[InterpretedModule] =
      StageTimers.time("acc/service-module")(getOrCompileModule(code.get, JamBytes(codeHash.bytes.toArray)))
    val compileNanos = elapsed

    // Apply incoming transfer balances before execution
    val transferBalance = operands.collect {
      case AccumulationOperand.Transfer(t) => t.amount
    }.sum
    val postTransferState = StageTimers.time("acc/service-deepcopy")(partialState.deepCopy())
    val deepCopiedAccount = postTransferState.accounts(serviceId)
    postTransferState.accounts = postTransferState.accounts.updated(
      serviceId,
      deepCopiedAccount.copy(
        info = deepCopiedAccount.info
          .copy(balance = deepCopiedAccount.info.balance + transferBalance)
      )
    )

    // Calculate initial nextAccountIndex:
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
      storageView = storageView
    )

    // Execute PVM
    val meter = new HostCallMeter(new AccumulationHostCalls(context, operands, config), AccumulateCost.category, 6)
    val execResult = StageTimers.time("acc/service-pvm")(executePvm(context, module, gasLimit, operands, executionMode, meter))

    // Collapse state based on exit reason
    val finalState = StageTimers.time("acc/service-collapse")(context.collapse(execResult.exitReason))

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
      provisions = context.getProvisions(execResult.exitReason),
      cost = AccumulateCost.of(ExecCost(execResult.gasUsed, elapsed), compileNanos, meter)
    )

  /** Execute PVM code with host call handling.
    */
  private def executePvm(
      context: AccumulationContext,
      moduleOpt: Option[InterpretedModule],
      gasLimit: Long,
      operands: List[AccumulationOperand],
      executionMode: ExecutionMode,
      hostCalls: HostCallMeter
  ): PvmExecResult =
    // Encode input data: timeslot, serviceIndex, operands count
    val inputData = JamCodecs.encodeCompactInteger(context.timeslot) ++
      JamCodecs.encodeCompactInteger(context.serviceIndex) ++
      JamCodecs.encodeCompactInteger(operands.size.toLong)

    if moduleOpt.isEmpty then
      return PvmExecResult(ExitReason.INVALID_CODE, 0L, None)

    val module = moduleOpt.get

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

  /** The preimage blob `serviceId` holds under `hash` in the state this run starts from */
  private def readOwnPreimage(
      partialState: PartialState,
      serviceId: Long,
      hash: Array[Byte],
      storageView: Option[ServiceStorageView]
  ): Option[JamBytes] =
    val blobStateKey = StateKey.computeServiceDataStateKey(serviceId, 0xfffffffeL, JamBytes(hash))
    storageView match
      case Some(v) => v.getByStateKey(blobStateKey)
      case None    => partialState.rawServiceDataByStateKey.get(blobStateKey)

  /** Get or compile a module from code bytes (process-wide cache).
    */
  private def getOrCompileModule(code: Array[Byte], codeHash: JamBytes): Option[InterpretedModule] =
    ServiceCode.module(codeHash, code)

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
      operands: List[AccumulationOperand],
      elapsedNanos: Long
  ): AccumulationOneResult =
    val finalState = serviceId match
      case Some(sid) =>
        val transferBalance = operands.collect {
          case AccumulationOperand.Transfer(t) => t.amount
        }.sum
        if transferBalance != 0 then
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

    AccumulationOneResult(
      finalState,
      List.empty,
      None,
      0L,
      Set.empty,
      AccumulateCost.zero.copy(calls = 1, total = ExecCost(0L, elapsedNanos))
    )

/** Result of PVM execution.
  */
final case class PvmExecResult(
    exitReason: ExitReason,
    gasUsed: Long,
    output: Option[Array[Byte]]
)
