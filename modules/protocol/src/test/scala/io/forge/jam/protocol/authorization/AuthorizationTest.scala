package io.forge.jam.protocol.authorization

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.AppendedClues.convertToClueful
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.encode
import io.forge.jam.core.primitives.{Hash, CoreIndex}
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.TestHelpers.hashFilled
import io.forge.jam.protocol.authorization.AuthorizationTypes.*
import io.forge.jam.protocol.authorization.AuthorizationTransition

/**
 * Tests for the Authorization State Transition Function.
 */
class AuthorizationTest extends AnyFunSuite with Matchers:

  // Config for single-core unit tests
  private val singleCoreConfig = ChainConfig.TINY.copy(coresCount = 1)
  private val twoCoreConfig = ChainConfig.TINY.copy(coresCount = 2)

  test("authorization consumption from pool") {
    // Create a simple state with one core
    val pool = List(hashFilled(1), hashFilled(2), hashFilled(3))
    val queue = (1 to 80).map(i => hashFilled(100 + i)).toList

    val preState = AuthState(
      authPools = List(pool),
      authQueues = List(queue)
    )

    // Consume hashFilled(2) from core 0
    val input = AuthInput(
      slot = 5L,
      auths = List(Auth(CoreIndex(0), hashFilled(2)))
    )

    val postState = AuthorizationTransition.stf(input, preState, singleCoreConfig)

    // hashFilled(2) should be removed, and a new item from queue added
    postState.authPools(0) should not contain hashFilled(2)
    // Pool should have original items minus consumed plus new one from queue
    postState.authPools(0).size shouldBe 3 // 3 - 1 + 1 = 3
    // The new item should be from queue at index (5 % 80 = 5)
    postState.authPools(0) should contain(queue(5))
  }

  test("queue rotation logic") {
    // Create state with two cores - both will get rotation because both have non-zero pools
    val pool1 = List(hashFilled(1), hashFilled(2))
    val pool2 = List(hashFilled(10), hashFilled(20))
    val queue1 = (0 until 80).map(i => hashFilled(100 + i)).toList
    val queue2 = (0 until 80).map(i => hashFilled(200 + i)).toList

    val preState = AuthState(
      authPools = List(pool1, pool2),
      authQueues = List(queue1, queue2)
    )

    // Consume one from core 0 only
    val input42 = AuthInput(
      slot = 42L,
      auths = List(Auth(CoreIndex(0), hashFilled(1)))
    )

    val postState = AuthorizationTransition.stf(input42, preState, twoCoreConfig)

    // Core 0 should get item from queue at index 42 % 80 = 42
    postState.authPools(0) should contain(queue1(42))

    // Core 1 should ALSO get rotation (pool is processed for every core)
    // Pool was [hashFilled(10), hashFilled(20)] -> add queue2(42) -> [10, 20, queue2(42)]
    postState.authPools(1) should contain(queue2(42))
    postState.authPools(1).size shouldBe 3
  }

  test("zero-hash padding behavior") {
    // Create a pool with only one item
    val pool = List(hashFilled(1))
    val queue = (0 until 80).map(i => hashFilled(100 + i)).toList

    val preState = AuthState(
      authPools = List(pool),
      authQueues = List(queue)
    )

    // Consume the only item
    val input = AuthInput(
      slot = 10L,
      auths = List(Auth(CoreIndex(0), hashFilled(1)))
    )

    val postState = AuthorizationTransition.stf(input, preState, singleCoreConfig)

    // Pool should have a zero hash since it became empty
    postState.authPools(0) should contain(Hash.zero)
  }

  test("pool size enforcement") {
    // Create a pool that's already at max size
    val pool = (1 to 8).map(i => hashFilled(i)).toList
    val queue = (0 until 80).map(i => hashFilled(100 + i)).toList

    val preState = AuthState(
      authPools = List(pool),
      authQueues = List(queue)
    )

    // Consume first item
    val input = AuthInput(
      slot = 25L,
      auths = List(Auth(CoreIndex(0), hashFilled(1)))
    )

    val postState = AuthorizationTransition.stf(input, preState, singleCoreConfig)

    // Pool should still be at max size 8
    postState.authPools(0).size shouldBe 8
    // First item (hashFilled(1)) should be removed
    postState.authPools(0) should not contain hashFilled(1)
    // New item from queue at index 25 should be added at end
    postState.authPools(0) should contain(queue(25))
  }

  test("tiny config (2 cores) state transition") {
    val folderPath = "stf/authorizations/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadTestDataFromTestVectors[AuthCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right((testCase, expectedBinaryData)) =>
          // Test encoding
          val encoded = testCase.encode
          encoded.toArray shouldBe expectedBinaryData withClue s"Encoding mismatch for $testCaseName"

          // Test state transition
          val postState = AuthorizationTransition.stf(
            testCase.input,
            testCase.preState,
            ChainConfig.TINY
          )
          assertAuthStateEquals(testCase.postState, postState, testCaseName)
  }

  test("full config (341 cores) state transition") {
    val folderPath = "stf/authorizations/full"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadTestDataFromTestVectors[AuthCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right((testCase, expectedBinaryData)) =>
          // Test encoding
          val encoded = testCase.encode
          encoded.toArray shouldBe expectedBinaryData withClue s"Encoding mismatch for $testCaseName"

          // Test state transition
          val postState = AuthorizationTransition.stf(
            testCase.input,
            testCase.preState,
            ChainConfig.FULL
          )
          assertAuthStateEquals(testCase.postState, postState, testCaseName)
  }

  // Helper method to compare AuthState instances with detailed error messages
  private def assertAuthStateEquals(
    expected: AuthState,
    actual: AuthState,
    testCaseName: String
  ): Unit =
    // Compare auth pools
    assertAuthPoolsEqual(expected.authPools, actual.authPools, testCaseName)
    // Compare auth queues
    assertAuthQueuesEqual(expected.authQueues, actual.authQueues, testCaseName)

  private def assertAuthPoolsEqual(
    expected: List[List[Hash]],
    actual: List[List[Hash]],
    testCaseName: String
  ): Unit =
    expected.size shouldBe actual.size withClue
      s"Auth pools size mismatch in test case: $testCaseName"

    expected.zip(actual).zipWithIndex.foreach { case ((expectedPool, actualPool), index) =>
      expectedPool.size shouldBe actualPool.size withClue
        s"Auth pool $index size mismatch in test case: $testCaseName"
      expectedPool.zip(actualPool).zipWithIndex.foreach { case ((exp, act), hashIdx) =>
        exp shouldBe act withClue
          s"Auth pool $index hash $hashIdx mismatch in test case: $testCaseName\nExpected: ${exp.toHex}\nActual: ${act.toHex}"
      }
    }

  private def assertAuthQueuesEqual(
    expected: List[List[Hash]],
    actual: List[List[Hash]],
    testCaseName: String
  ): Unit =
    expected.size shouldBe actual.size withClue
      s"Auth queues size mismatch in test case: $testCaseName"

    expected.zip(actual).zipWithIndex.foreach { case ((expectedQueue, actualQueue), index) =>
      expectedQueue.size shouldBe actualQueue.size withClue
        s"Auth queue $index size mismatch in test case: $testCaseName"
      expectedQueue.zip(actualQueue).zipWithIndex.foreach { case ((exp, act), hashIdx) =>
        exp shouldBe act withClue
          s"Auth queue $index hash $hashIdx mismatch in test case: $testCaseName"
      }
    }
