package io.forge.jam.protocol.refine

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.pvm.engine.InterpretedModule
import io.forge.jam.protocol.accumulation.{
  HostCall,
  HostCallResult,
  PvmInstance,
  ServiceCode
}
import spire.math.ULong

/** Result of the Is-Authorized invocation Psi_I: Ok(authorizer trace) or a
  * work error, plus the gas used.
  */
final case class IsAuthorizedResult(
    result: ExecutionResult,
    gasUsed: Long
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

  def getGasCost(hostCallId: Int, instance: PvmInstance): Long = 10L

  def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
    hostCallId match
      case HostCall.GAS =>
        setReg(instance, 7, ULong(instance.gas))
      case HostCall.FETCH =>
        handleFetch(instance)
      case _ =>
        setReg(instance, 7, HostCallResult.WHAT)

  private def handleFetch(instance: PvmInstance): Unit =
    val selector = ULong(instance.reg(10))
    val outputAddr = instance.reg(7).toInt
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

  /** Cmaxauthcodesize (gp 0.7.2 definitions.tex). */
  private val MAX_AUTH_CODE_SIZE: Int = 64_000

  /** Cpackageauthgas (gp 0.7.2 definitions.tex). */
  private val PACKAGE_AUTH_GAS: Long = 50_000_000L

  private val MAX_MODULE_CACHE_SIZE = 16
  private val moduleCache = new BoundedModuleCache(MAX_MODULE_CACHE_SIZE)

  /** The work-package's implied authorizer hash:
    * blake(authCodeHash ++ authorizerConfig).
    */
  def authorizerHash(workPackage: WorkPackage): Hash =
    Hashing.blake2b256(
      workPackage.authCodeHash.bytes.toArray ++ workPackage.authorizerConfig.toArray
    )

  def execute(
      workPackage: WorkPackage,
      coreIndex: Int,
      accounts: HistoricalLookupService
  ): IsAuthorizedResult =
    val hostService = workPackage.authCodeHost.value.toLong
    val lookupAnchorTimeslot =
      workPackage.context.lookupAnchorSlot.value.toLong

    // p_authcode: encode(var(metadata), code) = histlookup(δ[authCodeHost], t, authCodeHash)
    val preimageOpt =
      if accounts.serviceExists(hostService) then
        accounts.historicalLookup(
          hostService,
          lookupAnchorTimeslot,
          workPackage.authCodeHash
        )
      else None

    val preimage = preimageOpt match
      case None =>
        return IsAuthorizedResult(ExecutionResult.BadCode, 0L)
      case Some(bytes) => bytes

    val code = ServiceCode.extractCodeBlob(preimage) match
      case Some(bytes) if bytes.nonEmpty => bytes
      case _ =>
        return IsAuthorizedResult(ExecutionResult.BadCode, 0L)

    if code.length > MAX_AUTH_CODE_SIZE then
      return IsAuthorizedResult(ExecutionResult.CodeTooLarge, 0L)

    val module = getOrCompileModule(
      code,
      JamBytes(workPackage.authCodeHash.bytes.toArray)
    ) match
      case None =>
        return IsAuthorizedResult(ExecutionResult.Panic, 0L)
      case Some(m) => m

    // a = encode[2](c)
    val args = Array[Byte](
      (coreIndex & 0xff).toByte,
      ((coreIndex >> 8) & 0xff).toByte
    )

    val hostCalls = new IsAuthorizedHostCalls(config, workPackage)
    val (exit, gasUsed, output) =
      PvmRunner.run(module, args, PACKAGE_AUTH_GAS, entryPc = 0, hostCalls)

    exit match
      case PvmRunner.PvmExit.OutOfGas =>
        IsAuthorizedResult(ExecutionResult.OOG, gasUsed)
      case PvmRunner.PvmExit.Panic =>
        IsAuthorizedResult(ExecutionResult.Panic, gasUsed)
      case PvmRunner.PvmExit.Halt =>
        IsAuthorizedResult(ExecutionResult.Ok(JamBytes(output)), gasUsed)

  private def getOrCompileModule(
      code: Array[Byte],
      codeHash: JamBytes
  ): Option[InterpretedModule] =
    moduleCache.getOrCompile(codeHash) {
      ServiceCode.parseBlob(code).flatMap { blob =>
        InterpretedModule.create(blob).toOption
      }
    }
