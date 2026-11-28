package io.forge.jam.protocol.refine


import io.forge.jam.protocol.pvmhost.{HostCallDispatcher, PvmRunner}
import io.forge.jam.core.{ChainConfig, JamBytes, Hashing, constants}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.pvm.ExecutionMode
import io.forge.jam.protocol.pvmhost.{GrowHeapHostCall, HostCall, HostCallGas, HostCallResult, PvmInstance, ServiceCode}
import spire.math.ULong

/** Result of the Is-Authorized invocation Psi_I: Ok(authorizer trace) or a
  * work error, plus the gas used.
  */
final case class IsAuthorizedResult(
    result: ExecutionResult,
    gasUsed: Long,
    cost: io.forge.jam.protocol.pvmhost.IsAuthorizedCost = io.forge.jam.protocol.pvmhost.IsAuthorizedCost.zero
)

/** Host-call dispatcher for the is-authorized invocation only `gas` and
  * a work-package-restricted `fetch`
  * are available; everything else reports WHAT.
  */
private final class IsAuthorizedHostCalls(
    config: ChainConfig,
    workPackage: WorkPackage
) extends HostCallDispatcher:

  private def setReg(instance: PvmInstance, reg: Int, value: ULong): Unit =
    instance.setReg(reg, value.signed)

  def getGasCost(hostCallId: Int, instance: PvmInstance): Long =
    hostCallId match
      case HostCall.GROW_HEAP => 0L
      case HostCall.GAS => HostCallGas.CgasG
      case HostCall.FETCH =>
        HostCallGas.fetchGas(ULong(instance.reg(10)), ULong(instance.reg(9)))
      case HostCall.LOG => HostCallGas.Cgasunknown
      case _ => HostCallGas.Cgasunknown

  def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
    hostCallId match
      case HostCall.GAS =>
        setReg(instance, 7, ULong(instance.gas))
      case HostCall.GROW_HEAP =>
        GrowHeapHostCall.handle(instance)
      case HostCall.FETCH =>
        handleFetch(instance)
      case _ =>
        setReg(instance, 7, HostCallResult.WHAT)

  private def handleFetch(instance: PvmInstance): Unit =
    val selector = ULong(instance.reg(10))
    val outputAddr = PvmRunner.memAddr(instance.reg(7))
    val r11 = ULong(instance.reg(11))
    val r12 = ULong(instance.reg(12))

    // Omega_Y with only p present: n, r, i, ī, x̄ and the operand list are all
    // none in the is-authorized context.
    RefineFetch.resolveFetch(
      instance,
      selector,
      outputAddr,
      r11,
      r12,
      config,
      Some(workPackage),
      None,
      None,
      None,
      None,
      None
    )(value => setReg(instance, 7, value))

/** Executes the Is-Authorized invocation Psi_I : resolves the work package's
  * authorization code via historical lookup of `authCodeHash` from
  * `authCodeHost`, runs it at instruction counter 0 with args encode[2](core)
  * and gas Cpackageauthgas, and returns the authorizer trace.
  */
class IsAuthorizedExecutor(val config: ChainConfig):

  /** The work-package's implied authorizer hash:
    * blake(authCodeHash ++ authorizerConfig).
    */
  def authorizerHash(workPackage: WorkPackage): Hash = IsAuthorizedExecutor.authorizerHashOf(workPackage)

  def execute(
      workPackage: WorkPackage,
      coreIndex: Int,
      accounts: HistoricalLookupService,
      executionMode: ExecutionMode = ExecutionMode.default
  ): IsAuthorizedResult =
    val probe = new io.forge.jam.protocol.pvmhost.RunProbe
    val r = run(workPackage, coreIndex, accounts, executionMode, probe)
    IsAuthorizedResult(r.result, r.gasUsed,
      io.forge.jam.protocol.pvmhost.IsAuthorizedCost(
        io.forge.jam.protocol.pvmhost.ExecCost(r.gasUsed, probe.elapsedNanos),
        probe.compileNanos,
        probe.hostCalls
      )
    )

  private def run(
      workPackage: WorkPackage,
      coreIndex: Int,
      accounts: HistoricalLookupService,
      executionMode: ExecutionMode,
      probe: io.forge.jam.protocol.pvmhost.RunProbe
  ): IsAuthorizedResult =
    val hostService = workPackage.authCodeHost.toLong
    val lookupAnchorTimeslot =
      workPackage.context.lookupAnchorSlot.toLong

    if !accounts.serviceExists(hostService) then return IsAuthorizedResult(ExecutionResult.BadCode, 0L)

    val codeKey = JamBytes(workPackage.authCodeHash.bytes.toArray)
    val storedHash = accounts.storedPreimageHash(hostService, workPackage.authCodeHash)
    val module = storedHash.flatMap(ServiceCode.stored(codeKey, _)) match
      case Some(known) =>
        if !accounts.preimageAvailable(hostService, lookupAnchorTimeslot, workPackage.authCodeHash, known.sourceSize) then
          return IsAuthorizedResult(ExecutionResult.BadCode, 0L)
        if known.codeLength > constants.Cmaxauthcodesize then
          return IsAuthorizedResult(ExecutionResult.CodeTooLarge, 0L)
        known.module
      case None =>
        // p_authcode: encode(var(metadata), code) = histlookup(δ[authCodeHost], t, authCodeHash)
        val preimage = accounts.historicalLookup(hostService, lookupAnchorTimeslot, workPackage.authCodeHash) match
          case None =>
            return IsAuthorizedResult(ExecutionResult.BadCode, 0L)
          case Some(bytes) => bytes

        val code = ServiceCode.extractCodeBlob(preimage) match
          case Some(bytes) if bytes.nonEmpty => bytes
          case _ =>
            return IsAuthorizedResult(ExecutionResult.BadCode, 0L)

        if code.length > constants.Cmaxauthcodesize then
          return IsAuthorizedResult(ExecutionResult.CodeTooLarge, 0L)

        val built = storedHash match
          case Some(h) => ServiceCode.module(codeKey, code, h, preimage.length.toLong)
          case None    => ServiceCode.module(codeKey, code)
        built match
          case None    => return IsAuthorizedResult(ExecutionResult.Panic, 0L)
          case Some(m) => m
    probe.codeReady()

    // a = encode[2](c)
    val args = Array[Byte](
      (coreIndex & 0xff).toByte,
      ((coreIndex >> 8) & 0xff).toByte
    )

    val hostCalls = new io.forge.jam.protocol.pvmhost.HostCallMeter(new IsAuthorizedHostCalls(config, workPackage), _ => 0, 1)
    probe.meter = hostCalls
    val (exit, gasUsed, output) =
      PvmRunner.run(
        module, args, constants.Cpackageauthgas, entryPc = 0, hostCalls, executionMode,
        memoryBudget = io.forge.jam.pvm.memory.InstanceMemoryBudget.inCore,
        abstainOnInternalError = true
      )

    exit match
      case PvmRunner.PvmExit.OutOfGas =>
        IsAuthorizedResult(ExecutionResult.OOG, gasUsed)
      case PvmRunner.PvmExit.Panic =>
        IsAuthorizedResult(ExecutionResult.Panic, gasUsed)
      case PvmRunner.PvmExit.PageFault =>
        IsAuthorizedResult(ExecutionResult.Panic, gasUsed)
      case PvmRunner.PvmExit.Halt =>
        IsAuthorizedResult(ExecutionResult.Ok(JamBytes(output)), gasUsed)

object IsAuthorizedExecutor:
  /** The work-package's implied authorizer hash: blake(authCodeHash ++ authorizerConfig). */
  def authorizerHashOf(workPackage: WorkPackage): Hash =
    Hashing.blake2b256(workPackage.authCodeHash.bytes.toArray ++ workPackage.authorizerConfig.toArray)
