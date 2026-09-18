package io.forge.jam.protocol.refine

import io.forge.jam.protocol.HostCallPanic
import io.forge.jam.protocol.accumulation.PvmInstance
import io.forge.jam.pvm.engine.InterpretedModule
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HostCallPanicPropagationSpec extends AnyFunSuite with Matchers:

  private def ecalliModule: InterpretedModule =
    InterpretedModule.createForTest(
      code = Array[Byte](10, 0),
      bitmask = Array[Byte](0x03)
    )

  private def dispatcher(
      onDispatch: => Unit,
      dispatched: () => Unit = () => ()
  ): HostCallDispatcher =
    new HostCallDispatcher:
      def getGasCost(hostCallId: Int, instance: PvmInstance): Long = 0L
      def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
        dispatched()
        onDispatch

  test("HostCallPanic from a handler maps to PvmExit.Panic") {
    var wasDispatched = false
    val (exit, _, _) = PvmRunner.run(
      module = ecalliModule,
      inputData = Array.empty,
      gasLimit = 100000L,
      entryPc = 0,
      hostCalls = dispatcher(
        throw new HostCallPanic("Test PANIC"),
        () => wasDispatched = true
      )
    )
    // Guards against a Panic produced by anything other than the handler.
    wasDispatched shouldBe true
    exit shouldBe PvmRunner.PvmExit.Panic
  }

  test("an implementation bug (NPE) propagates out of the run loop") {
    intercept[NullPointerException] {
      PvmRunner.run(
        module = ecalliModule,
        inputData = Array.empty,
        gasLimit = 100000L,
        entryPc = 0,
        hostCalls = dispatcher(throw new NullPointerException("simulated bug"))
      )
    }
  }
