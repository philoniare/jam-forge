package io.forge.jam.protocol.accumulation

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.primitives.Hash
import io.forge.jam.pvm.{InterruptKind, MemoryResult}
import io.forge.jam.pvm.memory.Memory.{isReadable, isWritable}
import io.forge.jam.pvm.engine.{InterpretedModule, InterpretedInstance}
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.types.ProgramCounter
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
  private val MAX_SERVICE_CODE_SIZE: Int = 4_000_000
  private val JAM_PAGE_SIZE = 4096

  /** Execute accumulation for a single service. Implements the Psi_A function
    * from Gray Paper.
    */
  def executeService(
      partialState: PartialState,
      timeslot: Long,
      serviceId: Long,
      gasLimit: Long,
      entropy: JamBytes,
      operands: List[AccumulationOperand]
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
    if code.isEmpty || code.get.isEmpty || code.get.length > MAX_SERVICE_CODE_SIZE
    then return createEmptyResult(partialState, Some(serviceId), operands)

    // Apply incoming transfer balances before execution
    val transferBalance = operands.collect {
      case AccumulationOperand.Transfer(t) => t.amount
    }.sum
    val postTransferState = partialState.deepCopy()
    val deepCopiedAccount = postTransferState.accounts(serviceId)
    postTransferState.accounts(serviceId) = ServiceAccount(
      info = deepCopiedAccount.info
        .copy(balance = deepCopiedAccount.info.balance + transferBalance),
      storage = deepCopiedAccount.storage,
      preimages = deepCopiedAccount.preimages,
      preimageRequests = deepCopiedAccount.preimageRequests,
      lastAccumulated = deepCopiedAccount.lastAccumulated
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
      y = postTransferState.deepCopy(),
      serviceIndex = serviceId,
      timeslot = timeslot,
      entropy = entropy,
      nextAccountIndex = nextAccountIndex,
      minPublicServiceIndex = minPublicServiceIndex,
      storageView = currentStorageView
    )

    // Execute PVM
    val execResult = executePvm(context, code.get, gasLimit, operands, JamBytes(codeHash.bytes.toArray))

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
      codeHash: JamBytes
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

    val instance = InterpretedInstance.fromModule(
      module,
      inputData,
      forceStepTracing = false
    )

    // Create PvmInstance wrapper for host calls
    val pvmWrapper = new InterpretedInstanceWrapper(instance)
    val hostCalls = new AccumulationHostCalls(context, operands, config)

    // Set initial gas
    instance.setGas(gasLimit)
    val initialGas = gasLimit

    // Entry point is PC=5 for accumulate function
    val entryPointPc = ProgramCounter(5)

    // Standard register setup (using PVM ABI)
    val RA_INIT = 0xffff0000L
    val SP_INIT = 0xfefe0000L
    val INPUT_ADDR = 0xfeff0000L

    // Set registers
    instance.setReg(0, RA_INIT) // RA = r0
    instance.setReg(1, SP_INIT) // SP = r1
    instance.setReg(7, INPUT_ADDR) // A0 = r7
    instance.setReg(8, inputData.length.toLong) // A1 = r8
    instance.setReg(9, 0L) // A2 = r9
    instance.setReg(10, 0L) // A3 = r10
    instance.setReg(11, 0L) // A4 = r11
    instance.setReg(12, 0L) // A5 = r12

    // Set initial PC
    instance.setNextProgramCounter(entryPointPc)

    // Execute loop
    var exitReason = ExitReason.HALT
    var continueExecution = true

    while continueExecution do
      val result = instance.run()

      result match
        case Right(InterruptKind.Finished) =>
          exitReason = ExitReason.HALT
          continueExecution = false

        case Right(InterruptKind.Panic) =>
          exitReason = ExitReason.PANIC
          continueExecution = false

        case Right(InterruptKind.OutOfGas) =>
          exitReason = ExitReason.OUT_OF_GAS
          continueExecution = false

        case Right(InterruptKind.Ecalli(hostId)) =>
          // Deduct host call gas cost BEFORE execution
          val gasCost = hostCalls.getGasCost(hostId.signed, pvmWrapper)
          val gasBefore = instance.gas
          val newGas = gasBefore - gasCost
          instance.setGas(newGas)

          if newGas < 0 then
            exitReason = ExitReason.OUT_OF_GAS
            continueExecution = false
          else
            try hostCalls.dispatch(hostId.signed, pvmWrapper)
            catch
              case e: RuntimeException =>
                exitReason = ExitReason.PANIC
                continueExecution = false

        case Right(InterruptKind.Segfault(_)) =>
          exitReason = ExitReason.PAGE_FAULT
          continueExecution = false

        case Right(InterruptKind.Step) =>
        // Continue for step tracing

        case Left(_) =>
          exitReason = ExitReason.PANIC
          continueExecution = false

    val finalGas = instance.gas
    val gasUsed = if finalGas >= 0 then initialGas - finalGas else initialGas

    // Extract output on halt
    val output = if exitReason == ExitReason.HALT then
      val addr = instance.reg(7).toInt
      val len = instance.reg(8).toInt
      if len >= 0 && instance.basicMemory.isReadable(spire.math.UInt(addr), len)
      then readMemoryBulk(instance, addr, len)
      else Some(Array.empty[Byte])
    else None

    PvmExecResult(exitReason, gasUsed, output)

  /** Get or compile a module from code bytes.
    */
  private def getOrCompileModule(code: Array[Byte], codeHash: JamBytes): Option[InterpretedModule] =
    val cached = moduleCache.get(codeHash)
    if cached != null then Some(cached)
    else
      // Try JAM format first
      var blobOpt = parseJamFormat(code)

      // If that fails, try raw code+jumptable format
      if blobOpt.isEmpty then
        blobOpt = ProgramBlob.fromCodeAndJumpTable(
          data = code,
          roData = Array.empty,
          rwData = new Array[Byte](262144),
          stackSize = 65536,
          is64Bit = true
        )

      // If that fails too, try generic PVM format
      if blobOpt.isEmpty then blobOpt = ProgramBlob.parse(code)

      blobOpt.flatMap { blob =>
        InterpretedModule.create(blob) match
          case Right(module) =>
            moduleCache.put(codeHash, module)
            Some(module)
          case Left(_) => None
      }

  /** Parse JAM blob format.
    */
  private def parseJamFormat(data: Array[Byte]): Option[ProgramBlob] =
    if data.length < 15 then return None

    var offset = 0
    val roDataLen = readLE3(data, offset); offset += 3
    val rwDataLen = readLE3(data, offset); offset += 3
    val heapPages = readLE2(data, offset); offset += 2
    val stackSize = readLE3(data, offset); offset += 3

    if roDataLen > data.length || rwDataLen > data.length || stackSize > 1000000
    then return None

    if offset + roDataLen > data.length then return None
    val roData = new Array[Byte](roDataLen)
    System.arraycopy(data, offset, roData, 0, roDataLen)
    offset += roDataLen

    if offset + rwDataLen > data.length then return None
    val rwData = new Array[Byte](rwDataLen)
    System.arraycopy(data, offset, rwData, 0, rwDataLen)
    offset += rwDataLen

    if offset + 4 > data.length then return None
    val codeLen = readLE4(data, offset); offset += 4

    if offset + codeLen > data.length then return None
    val codeAndJumpTable = new Array[Byte](codeLen)
    System.arraycopy(data, offset, codeAndJumpTable, 0, codeLen)

    val heapSize = heapPages * JAM_PAGE_SIZE
    val totalRwSize = rwDataLen + heapSize
    val minRwSize = Math.max(totalRwSize, 262144)
    val paddedRwData = new Array[Byte](minRwSize)
    System.arraycopy(rwData, 0, paddedRwData, 0, rwDataLen)

    ProgramBlob.fromCodeAndJumpTable(
      data = codeAndJumpTable,
      roData = roData,
      rwData = paddedRwData,
      stackSize = stackSize,
      is64Bit = true,
      heapPages = heapPages,
      originalRwDataLen = rwDataLen
    )

  private def readLE2(data: Array[Byte], offset: Int): Int =
    (data(offset) & 0xff) | ((data(offset + 1) & 0xff) << 8)

  private def readLE3(data: Array[Byte], offset: Int): Int =
    (data(offset) & 0xff) | ((data(offset + 1) & 0xff) << 8) | ((data(
      offset + 2
    ) & 0xff) << 16)

  private def readLE4(data: Array[Byte], offset: Int): Int =
    (data(offset) & 0xff) | ((data(offset + 1) & 0xff) << 8) |
      ((data(offset + 2) & 0xff) << 16) | ((data(offset + 3) & 0xff) << 24)

  private def readMemoryBulk(
      instance: InterpretedInstance,
      address: Int,
      length: Int
  ): Option[Array[Byte]] =
    instance.basicMemory.getMemorySlice(UInt(address), length) match
      case MemoryResult.Success(data) => Some(data)
      case _                          => None

  /** Extract code blob from preimage data. Format: metaLength (Gray Paper
    * natural) + metadata + codeBlob
    */
  private def extractCodeBlob(preimage: Array[Byte]): Option[Array[Byte]] =
    if preimage.isEmpty then return None

    val firstByte = preimage(0).toInt & 0xff
    if firstByte == 0 then return Some(preimage.drop(1))

    val inverted = (~firstByte) & 0xff
    var byteLength = 0
    var i = 0
    while i < 8 && (inverted & (0x80 >> i)) == 0 do
      byteLength += 1
      i += 1

    if preimage.length < 1 + byteLength then return None

    var res: Long = 0
    i = 0
    while i < byteLength do
      res = res | ((preimage(1 + i).toLong & 0xff) << (8 * i))
      i += 1

    val mask = (1 << (8 - byteLength)) - 1
    val topBits = firstByte & mask
    val metaLength = (res + (topBits.toLong << (8 * byteLength))).toInt
    val metaLengthSize = 1 + byteLength
    val codeStart = metaLengthSize + metaLength

    if codeStart > preimage.length then return None
    Some(preimage.drop(codeStart))

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

  override def readByte(address: Int): Option[Byte] =
    instance.basicMemory.loadU8(UInt(address)) match
      case MemoryResult.Success(v) => Some(v.toByte)
      case _                       => None

  override def writeByte(address: Int, value: Byte): Boolean =
    instance.basicMemory.storeU8(UInt(address), UByte(value)) match
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

