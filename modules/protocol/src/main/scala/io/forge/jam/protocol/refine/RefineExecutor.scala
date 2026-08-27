package io.forge.jam.protocol.refine

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.pvm.engine.InterpretedModule
import io.forge.jam.protocol.accumulation.ServiceCode

/** Result of a refine invocation Psi_R: the execution result (Ok(output) or a
  * work error), the exported segments (empty unless successful) and the gas
  * used.
  */
final case class RefineResult(
    result: ExecutionResult,
    exports: List[Array[Byte]],
    gasUsed: Long
)

/** Executes the Refine invocation function Psi_R: resolves the work item's code via
  * historical lookup, runs the service's refine entry point (instruction
  * counter 0) with argument encode(c, i, s, var(payload), blake(p)), and maps
  * the machine outcome to a work execution result.
  */
class RefineExecutor(val config: ChainConfig):

  // Cmaxservicecodesize
  private val MAX_SERVICE_CODE_SIZE: Int = 4_000_000

  // LRU-bounded module cache keyed by the work item's code hash, mirroring the
  // accumulate executor's cache.
  private val MAX_MODULE_CACHE_SIZE = 64
  private val moduleCache: java.util.LinkedHashMap[JamBytes, InterpretedModule] =
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

  /** Run refine for work item `workItemIndex` of `workPackage`.
    *
    * @param coreIndex core doing the refining (c)
    * @param authorizerTrace output of the is-authorized invocation (r)
    * @param importSegments all work items' imported segments (ī)
    * @param extrinsicData all work items' extrinsic blobs (x̄)
    * @param exportSegmentOffset segments already exported by prior items (ς)
    * @param accounts recent service-account state for historical lookups
    */
  def executeRefine(
      coreIndex: Int,
      workItemIndex: Int,
      workPackage: WorkPackage,
      authorizerTrace: Array[Byte],
      importSegments: IndexedSeq[IndexedSeq[Array[Byte]]],
      extrinsicData: IndexedSeq[IndexedSeq[Array[Byte]]],
      exportSegmentOffset: Long,
      accounts: HistoricalLookupService
  ): RefineResult =
    val workItem = workPackage.items(workItemIndex)
    val serviceId = workItem.service.value.toLong
    val lookupAnchorTimeslot =
      workPackage.context.lookupAnchorSlot.value.toLong

    // BAD: unknown service or unavailable code preimage at the lookup anchor.
    if !accounts.serviceExists(serviceId) then
      return RefineResult(ExecutionResult.BadCode, Nil, 0L)

    val preimage = accounts.historicalLookup(
      serviceId,
      lookupAnchorTimeslot,
      workItem.codeHash
    )
    if preimage.isEmpty then
      return RefineResult(ExecutionResult.BadCode, Nil, 0L)

    // BIG: oversized code.
    if preimage.get.length > MAX_SERVICE_CODE_SIZE then
      return RefineResult(ExecutionResult.CodeTooLarge, Nil, 0L)

    // The preimage is encode(var(metadata), code); extract the code part.
    val code = ServiceCode.extractCodeBlob(preimage.get)
    if code.isEmpty || code.get.isEmpty then
      return RefineResult(ExecutionResult.BadCode, Nil, 0L)

    // a = encode(c, i, s, var(payload), blake(p))
    val payload = workItem.payload.toArray
    val argsBuffer = new java.io.ByteArrayOutputStream(64 + payload.length)
    argsBuffer.write(JamCodecs.encodeCompactInteger(coreIndex.toLong))
    argsBuffer.write(JamCodecs.encodeCompactInteger(workItemIndex.toLong))
    argsBuffer.write(JamCodecs.encodeCompactInteger(serviceId))
    argsBuffer.write(JamCodecs.encodeCompactInteger(payload.length.toLong))
    argsBuffer.write(payload)
    argsBuffer.write(RefineFetch.workPackageHash(workPackage).bytes.toArray)
    val inputData = argsBuffer.toByteArray

    val moduleOpt = getOrCompileModule(
      code.get,
      JamBytes(workItem.codeHash.bytes.toArray)
    )
    if moduleOpt.isEmpty then
      // Psi_M: Y(p, a) = none → panic with zero gas used.
      return RefineResult(ExecutionResult.Panic, Nil, 0L)

    val context = new RefineContext(
      config = config,
      workPackage = workPackage,
      workItemIndex = workItemIndex,
      coreIndex = coreIndex,
      authorizerTrace = authorizerTrace,
      importSegments = importSegments,
      extrinsicData = extrinsicData,
      exportSegmentOffset = exportSegmentOffset,
      accounts = accounts
    )

    val gasLimit = workItem.refineGasLimit.toLong
    val hostCalls = new RefineHostCalls(context)
    val (exitReason, gasUsed, output) =
      PvmRunner.run(moduleOpt.get, inputData, gasLimit, entryPc = 0, hostCalls)

    exitReason match
      case PvmRunner.PvmExit.OutOfGas =>
        RefineResult(ExecutionResult.OOG, Nil, gasUsed)
      case PvmRunner.PvmExit.Panic =>
        RefineResult(ExecutionResult.Panic, Nil, gasUsed)
      case PvmRunner.PvmExit.Halt =>
        RefineResult(
          ExecutionResult.Ok(JamBytes(output)),
          context.exports.toList,
          gasUsed
        )

  private def getOrCompileModule(
      code: Array[Byte],
      codeHash: JamBytes
  ): Option[InterpretedModule] =
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
