package io.forge.jam.protocol.refine

import io.forge.jam.pvm.ExecutionMode
import io.forge.jam.pvm.engine.{InterpretedModule, PvmInternalError}
import io.forge.jam.protocol.pvmhost.{HostCallDispatcher, PvmInstance, PvmRunner}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InternalErrorAbstainSpec extends AnyFunSuite with Matchers:

  private val module = InterpretedModule.createForTest(code = Array[Byte](10, 0), bitmask = Array[Byte](0x03))
  private val noHostCalls = new HostCallDispatcher:
    def getGasCost(hostCallId: Int, instance: PvmInstance): Long = 0L
    def dispatch(hostCallId: Int, instance: PvmInstance): Unit = ()
  private val bug = new IllegalStateException("simulated interpreter bug")

  private def run(abstain: Boolean) =
    PvmRunner.run(
      module, Array.emptyByteArray, 100000L, 0, noHostCalls,
      executionMode = ExecutionMode.Interpreted,
      abstainOnInternalError = abstain,
      interpreterStep = _ => Left(PvmInternalError(bug))
    )

  test("a run that abstains throws on an internal interpreter error instead of exiting with Panic") {
    val thrown = intercept[PvmRunner.InternalFailure](run(abstain = true))
    thrown.getCause shouldBe theSameInstanceAs(bug)
  }

  test("a run that does not abstain (accumulation) still exits with Panic") {
    run(abstain = false)._1 shouldBe PvmRunner.PvmExit.Panic
  }
