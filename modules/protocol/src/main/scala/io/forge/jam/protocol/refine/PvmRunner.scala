package io.forge.jam.protocol.refine

import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.engine.{InterpretedInstance, InterpretedModule}
import io.forge.jam.pvm.types.ProgramCounter
import io.forge.jam.protocol.accumulation.{InterpretedInstanceWrapper, PvmInstance}

/** Host-call dispatch surface consumed by [[PvmRunner]] (implemented by the
  * refine and is-authorized dispatchers).
  */
trait HostCallDispatcher:
  def getGasCost(hostCallId: Int, instance: PvmInstance): Long
  def dispatch(hostCallId: Int, instance: PvmInstance): Unit

object PvmRunner:

  enum PvmExit:
    case Halt, Panic, OutOfGas

  private val RA_INIT = 0xffff0000L
  private val SP_INIT = 0xfefe0000L
  private val INPUT_ADDR = 0xfeff0000L

  def run(
      module: InterpretedModule,
      inputData: Array[Byte],
      gasLimit: Long,
      entryPc: Int,
      hostCalls: HostCallDispatcher
  ): (PvmExit, Long, Array[Byte]) =
    val instance = InterpretedInstance.fromModule(
      module,
      inputData,
      forceStepTracing = false
    )

    val pvmWrapper = new InterpretedInstanceWrapper(instance)

    instance.setGas(gasLimit)
    val initialGas = gasLimit

    instance.setNextProgramCounter(ProgramCounter(entryPc))

    instance.setReg(0, RA_INIT)
    instance.setReg(1, SP_INIT)
    instance.setReg(7, INPUT_ADDR)
    instance.setReg(8, inputData.length.toLong)
    instance.setReg(9, 0L)
    instance.setReg(10, 0L)
    instance.setReg(11, 0L)
    instance.setReg(12, 0L)

    var exit = PvmExit.Halt
    var continueExecution = true

    while continueExecution do
      instance.run() match
        case Right(InterruptKind.Finished) =>
          exit = PvmExit.Halt
          continueExecution = false

        case Right(InterruptKind.Panic) =>
          exit = PvmExit.Panic
          continueExecution = false

        case Right(InterruptKind.OutOfGas) =>
          exit = PvmExit.OutOfGas
          continueExecution = false

        case Right(InterruptKind.Ecalli(hostId)) =>
          val gasCost = hostCalls.getGasCost(hostId.signed, pvmWrapper)
          val newGas = instance.gas - gasCost
          instance.setGas(newGas)
          if newGas < 0 then
            exit = PvmExit.OutOfGas
            continueExecution = false
          else
            try hostCalls.dispatch(hostId.signed, pvmWrapper)
            catch
              case _: RuntimeException =>
                exit = PvmExit.Panic
                continueExecution = false

        case Right(InterruptKind.Segfault(_)) =>
          exit = PvmExit.Panic
          continueExecution = false

        case Right(InterruptKind.Step) =>
        // step tracing only; continue

        case Left(_) =>
          exit = PvmExit.Panic
          continueExecution = false

    val finalGas = instance.gas
    val gasUsed = if finalGas >= 0 then initialGas - finalGas else initialGas

    val output =
      if exit == PvmExit.Halt then
        val addr = instance.reg(7).toInt
        val len = instance.reg(8).toInt
        if len >= 0 && pvmWrapper.isMemoryReadable(addr, len) then
          pvmWrapper.readBytes(addr, len).getOrElse(Array.empty[Byte])
        else Array.empty[Byte]
      else Array.empty[Byte]

    (exit, gasUsed, output)
