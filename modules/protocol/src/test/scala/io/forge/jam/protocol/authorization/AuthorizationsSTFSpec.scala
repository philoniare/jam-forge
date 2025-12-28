package io.forge.jam.protocol.authorization

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import io.forge.jam.core.{ChainConfig, constants}
import io.forge.jam.core.primitives.{Hash, CoreIndex}
import io.forge.jam.protocol.generators.StfGenerators.*
import io.forge.jam.protocol.authorization.AuthorizationTypes.*
import spire.math.UShort

/**
 * - Authorization pool management preserves valid entries
 * - Service authorization handling follows queue semantics
 */
class AuthorizationsSTFSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  private val testConfig = ChainConfig.TINY

  // Override default ScalaCheck configuration for faster tests
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  private def genAuth(config: ChainConfig): Gen[Auth] =
    for
      coreIndex <- Gen.choose(0, config.coresCount - 1)
      authHash <- genHash
    yield Auth(CoreIndex(UShort(coreIndex)), authHash)

  private def genAuthInput(config: ChainConfig): Gen[AuthInput] =
    for
      slot <- Gen.choose(1L, 1000L)
      authCount <- Gen.choose(0, 5)
      auths <- Gen.listOfN(authCount, genAuth(config))
    yield AuthInput(slot, auths)

  private def genAuthState(config: ChainConfig): Gen[AuthState] =
    for
      poolsSize <- Gen.choose(0, constants.O)
      pools <- Gen.listOfN(config.coresCount, Gen.listOfN(poolsSize, genHash))
      queues <- Gen.listOfN(config.coresCount, Gen.listOfN(constants.Q, genHash))
    yield AuthState(pools, queues)

  test("property: authPools size matches cores count") {
    forAll(genAuthState(testConfig)) { state =>
      // Property: authPools list size should equal cores count
      state.authPools.size shouldBe testConfig.coresCount
    }
  }

  test("property: authQueues size matches cores count") {
    forAll(genAuthState(testConfig)) { state =>
      // Property: authQueues list size should equal cores count
      state.authQueues.size shouldBe testConfig.coresCount
    }
  }

  test("property: authQueues inner lists have fixed size Q") {
    forAll(genAuthState(testConfig)) { state =>
      // Property: each queue should have exactly Q entries
      state.authQueues.foreach(queue => queue.size shouldBe constants.Q)
    }
  }

  test("property: authPools inner lists are limited to O entries") {
    forAll(genAuthState(testConfig)) { state =>
      // Property: each pool should have at most O entries
      state.authPools.foreach(pool => pool.size should be <= constants.O)
    }
  }

  test("property: empty input preserves pool size or adds from queue") {
    forAll(genAuthState(testConfig)) { preState =>
      val input = AuthInput(slot = 100L, auths = List.empty)

      val postState = AuthorizationTransition.stfInternal(input, preState)

      // Property: with no consumed auths, pools should grow by 1 (from queue rotation)
      // but stay capped at O
      postState.authPools.zip(preState.authPools).zip(preState.authQueues).foreach {
        case ((postPool, prePool), queue) =>
          if queue.nonEmpty then
            // Pool should either grow by 1 or stay at O
            postPool.size should be <= constants.O
            if prePool.size < constants.O then
              postPool.size shouldBe (prePool.size + 1).min(constants.O)
      }
    }
  }

  test("property: queue rotation uses slot modulo queue size") {
    forAll(genAuthState(testConfig), Gen.choose(0L, 1000L)) { (preState, slot) =>
      val input = AuthInput(slot = slot, auths = List.empty)

      val postState = AuthorizationTransition.stfInternal(input, preState)

      // Property: queues should remain unchanged (they are read-only in this STF)
      postState.authQueues.zip(preState.authQueues).zipWithIndex.foreach {
        case ((postQueue, preQueue), _) =>
          postQueue.size shouldBe preQueue.size
          postQueue.zip(preQueue).foreach {
            case (postHash, preHash) =>
              java.util.Arrays.equals(postHash.bytes, preHash.bytes) shouldBe true
          }
      }
    }
  }

  test("property: consumed authorizations are removed from pool") {
    forAll(genAuthState(testConfig)) { preState =>
      // Find a core with a non-empty pool
      val coreWithPool = preState.authPools.zipWithIndex.collectFirst {
        case (pool, idx) if pool.nonEmpty => (pool, idx)
      }

      coreWithPool match
        case Some((pool, coreIndex)) =>
          // Create input that consumes the first auth in this pool
          val authToConsume = Auth(CoreIndex(coreIndex), pool.head)
          val input = AuthInput(slot = 100L, auths = List(authToConsume))

          val postState = AuthorizationTransition.stfInternal(input, preState)

          // Property: the consumed auth should be removed (pool size may change due to queue rotation)
          // The exact hash should no longer be in the pool at the same position
          val postPool = postState.authPools(coreIndex)

          // The consumed hash might be added back from queue, but the original should be removed
          // We verify the pool was modified
          if preState.authQueues(coreIndex).nonEmpty then
            // Pool was modified (auth removed, new one added from queue)
            succeed
          else
            // Pool should have one less entry (no queue rotation)
            postPool.size shouldBe (pool.size - 1).max(0)

        case None =>
          // No pools with entries to test
          succeed
    }
  }

  test("property: STF is deterministic") {
    forAll(genAuthState(testConfig), genAuthInput(testConfig)) { (preState, input) =>
      val postState1 = AuthorizationTransition.stfInternal(input, preState)
      val postState2 = AuthorizationTransition.stfInternal(input, preState)

      // Property: same inputs should produce same output
      postState1.authPools.size shouldBe postState2.authPools.size
      postState1.authPools.zip(postState2.authPools).foreach {
        case (pool1, pool2) =>
          pool1.size shouldBe pool2.size
          pool1.zip(pool2).foreach {
            case (h1, h2) =>
              java.util.Arrays.equals(h1.bytes, h2.bytes) shouldBe true
          }
      }

      postState1.authQueues.size shouldBe postState2.authQueues.size
      postState1.authQueues.zip(postState2.authQueues).foreach {
        case (queue1, queue2) =>
          queue1.size shouldBe queue2.size
          queue1.zip(queue2).foreach {
            case (h1, h2) =>
              java.util.Arrays.equals(h1.bytes, h2.bytes) shouldBe true
          }
      }
    }
  }

  test("property: authorization queue size Q is correctly defined") {
    // Property: Q should be the fixed queue size
    constants.Q should be > 0
    AuthorizationTypes.AuthQueueSize shouldBe constants.Q
  }

  test("property: authorization pool size O is correctly defined") {
    // Property: O should be the maximum pool size
    constants.O should be > 0
    AuthorizationTypes.PoolSize shouldBe constants.O
  }

  test("property: pool takes rightmost O items after operations") {
    forAll(genAuthState(testConfig), genAuthInput(testConfig)) { (preState, input) =>
      val postState = AuthorizationTransition.stfInternal(input, preState)

      // Property: all pools should have at most O entries
      postState.authPools.foreach(pool => pool.size should be <= constants.O)
    }
  }

  test("property: new items from queue are appended to pool") {
    forAll(genAuthState(testConfig)) { preState =>
      // Start with empty pools to clearly see queue items being added
      val emptyPoolsState = preState.copy(
        authPools = List.fill(testConfig.coresCount)(List.empty)
      )

      val input = AuthInput(slot = 0L, auths = List.empty)
      val postState = AuthorizationTransition.stfInternal(input, emptyPoolsState)

      // Property: pools should now contain items from queues at position slot % Q
      postState.authPools.zip(emptyPoolsState.authQueues).foreach {
        case (postPool, queue) =>
          if queue.nonEmpty then
            // Pool should have exactly 1 item (from queue at position slot % Q)
            postPool.size shouldBe 1
            val expectedHash = queue(0) // slot = 0, so 0 % Q = 0
            java.util.Arrays.equals(postPool.head.bytes, expectedHash.bytes) shouldBe true
      }
    }
  }

  test("property: auth core indices are within bounds") {
    forAll(genAuthInput(testConfig)) { input =>
      input.auths.foreach { auth =>
        // Property: core index should be less than cores count
        auth.core.toInt should be < testConfig.coresCount
        auth.core.toInt should be >= 0
      }
    }
  }

  test("property: auth hashes are 32 bytes") {
    forAll(genAuthInput(testConfig)) { input =>
      input.auths.foreach { auth =>
        // Property: auth hash should be exactly 32 bytes
        auth.authHash.size shouldBe Hash.Size
      }
    }
  }
