package io.forge.jam.protocol.accumulation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.protocol.generators.StfGenerators.*


/** Checkpoint/collapse semantics of AccumulationContext over generated partial states. */
class AccumulationSTFSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  private val testConfig = ChainConfig.TINY

  // Override default ScalaCheck configuration for faster tests
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  test("property: collapse returns y state on PANIC, x state on HALT") {
    forAll(genPartialState(testConfig)) { initialState =>
      val ctx = AccumulationContext(
        initialState = initialState,
        serviceIndex = 100L,
        timeslot = 1000L,
        entropy = JamBytes.zeros(32)
      )

      // Set distinct values for x and y
      ctx.x.manager = 1L
      ctx.checkpoint() // y now has manager = 1
      ctx.x.manager = 2L // x now has manager = 2

      // Property: HALT should return x state
      val haltState = ctx.collapse(ExitReason.HALT)
      haltState.manager shouldBe 2L

      // Property: PANIC should return y state
      val panicState = ctx.collapse(ExitReason.PANIC)
      panicState.manager shouldBe 1L

      // Property: OUT_OF_GAS should return y state
      val oogState = ctx.collapse(ExitReason.OUT_OF_GAS)
      oogState.manager shouldBe 1L
    }
  }
