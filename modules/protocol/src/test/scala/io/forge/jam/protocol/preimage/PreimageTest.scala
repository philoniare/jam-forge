package io.forge.jam.protocol.preimage

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.AppendedClues.convertToClueful
import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.primitives.{Hash, ServiceId}
import io.forge.jam.core.types.extrinsic.Preimage
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.preimage.PreimageTypes.*
import io.forge.jam.protocol.preimage.PreimageTransition
import spire.math.UInt

/**
 * Tests for the Preimages State Transition Function.
 */
class PreimageTest extends AnyFunSuite with Matchers:

  // Tiny config: 6 validators, 2 cores
  val TinyConfig = ChainConfig.TINY

  // Full config: 1023 validators, 341 cores
  val FullConfig = ChainConfig.FULL

  test("preimage solicitation validation") {
    // Test that unsolicited preimages are rejected
    val account = PreimageAccount(
      id = 1,
      data = AccountInfo(
        preimages = List.empty,
        lookupMeta = List(
          PreimageHistory(
            key = PreimageHistoryKey(Hash(Array.fill(32)(0x01.toByte)), 100),
            value = List.empty // Solicited - empty timestamp list
          )
        )
      )
    )
    val preState = PreimageState(accounts = List(account))

    // Create a preimage with different hash - should be rejected
    val blob = JamBytes(Array.fill(50)(0x42.toByte))
    val wrongPreimage = Preimage(requester = ServiceId(UInt(1)), blob = blob)
    val input = PreimageInput(preimages = List(wrongPreimage), slot = 100)

    val (postState, output) = PreimageTransition.stfInternal(input, preState)

    output.isLeft shouldBe true
    output.left.toOption.get shouldBe PreimageErrorCode.PreimageUnneeded
    postState shouldBe preState // State should not change on error
  }

  test("sorted/unique ordering validation") {
    // Test that preimages not in sorted order by (requester, hash) are rejected
    // Create two accounts
    val account1 = PreimageAccount(
      id = 1,
      data = AccountInfo(
        preimages = List.empty,
        lookupMeta = List.empty
      )
    )
    val account2 = PreimageAccount(
      id = 2,
      data = AccountInfo(
        preimages = List.empty,
        lookupMeta = List.empty
      )
    )
    val preState = PreimageState(accounts = List(account1, account2))

    // Preimages in wrong order (requester 2 before requester 1)
    val blob1 = JamBytes(Array.fill(10)(0x01.toByte))
    val blob2 = JamBytes(Array.fill(10)(0x02.toByte))
    val preimage1 = Preimage(requester = ServiceId(UInt(2)), blob = blob1)
    val preimage2 = Preimage(requester = ServiceId(UInt(1)), blob = blob2)
    val input = PreimageInput(preimages = List(preimage1, preimage2), slot = 100)

    val (_, output) = PreimageTransition.stfInternal(input, preState)

    // Expected to fail because preimages are not sorted by (requester, blob)
    output.isLeft shouldBe true
    output.left.toOption.get shouldBe PreimageErrorCode.PreimagesNotSortedUnique
  }

  test("preimage storage by service") {
    // Test that preimages are stored correctly for a service account
    val blob = JamBytes(Array.fill(20)(0xAB.toByte))
    val hash = Hashing.blake2b256(blob)
    val length = blob.length.toLong

    val account = PreimageAccount(
      id = 5,
      data = AccountInfo(
        preimages = List.empty,
        lookupMeta = List(
          PreimageHistory(
            key = PreimageHistoryKey(hash, length),
            value = List.empty // Solicited - empty timestamp list
          )
        )
      )
    )
    val preState = PreimageState(accounts = List(account))

    val preimage = Preimage(requester = ServiceId(UInt(5)), blob = blob)
    val input = PreimageInput(preimages = List(preimage), slot = 42)

    val (postState, output) = PreimageTransition.stfInternal(input, preState)

    output.isRight shouldBe true

    // Verify preimage was stored
    val updatedAccount = postState.accounts.find(_.id == 5).get
    updatedAccount.data.preimages.size shouldBe 1
    updatedAccount.data.preimages.head.hash shouldBe hash
    updatedAccount.data.preimages.head.blob shouldBe blob
  }

  test("lookup metadata update") {
    // Test that lookup metadata is updated with submission timestamp
    val blob = JamBytes(Array.fill(30)(0xCD.toByte))
    val hash = Hashing.blake2b256(blob)
    val length = blob.length.toLong

    val account = PreimageAccount(
      id = 7,
      data = AccountInfo(
        preimages = List.empty,
        lookupMeta = List(
          PreimageHistory(
            key = PreimageHistoryKey(hash, length),
            value = List.empty // Solicited - empty timestamp list
          )
        )
      )
    )
    val preState = PreimageState(accounts = List(account))

    val preimage = Preimage(requester = ServiceId(UInt(7)), blob = blob)
    val slot = 123L
    val input = PreimageInput(preimages = List(preimage), slot = slot)

    val (postState, output) = PreimageTransition.stfInternal(input, preState)

    output.isRight shouldBe true

    // Verify lookup metadata was updated with timestamp
    val updatedAccount = postState.accounts.find(_.id == 7).get
    val updatedMeta = updatedAccount.data.lookupMeta.find(_.key.hash == hash).get

    updatedMeta.value.size shouldBe 1
    updatedMeta.value.head shouldBe slot
  }

  test("tiny config state transition") {
    val folderPath = "stf/preimages/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadJsonFromTestVectors[PreimageCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right(testCase) =>
          // Test state transition
          val (postState, output) = PreimageTransition.stfInternal(
            testCase.input,
            testCase.preState
          )
          assertPreimageOutputEquals(testCase.output, output, testCaseName)
          assertPreimageStateEquals(testCase.postState, postState, testCaseName)
  }

  test("full config state transition") {
    val folderPath = "stf/preimages/full"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadJsonFromTestVectors[PreimageCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right(testCase) =>
          // Test state transition
          val (postState, output) = PreimageTransition.stfInternal(
            testCase.input,
            testCase.preState
          )
          assertPreimageOutputEquals(testCase.output, output, testCaseName)
          assertPreimageStateEquals(testCase.postState, postState, testCaseName)
  }

  // Helper method to compare PreimageOutput instances
  private def assertPreimageOutputEquals(
    expected: PreimageOutput,
    actual: PreimageOutput,
    testCaseName: String
  ): Unit =
    (expected, actual) match
      case (Left(expectedErr), Left(actualErr)) =>
        expectedErr shouldBe actualErr withClue s"Error code mismatch in test case: $testCaseName"
      case (Left(expectedErr), Right(_)) =>
        fail(s"Expected error $expectedErr but got success in test case: $testCaseName")
      case (Right(_), Left(actualErr)) =>
        fail(s"Expected success but got error $actualErr in test case: $testCaseName")
      case (Right(_), Right(_)) =>
        // Both are success - nothing to compare for Unit output
        ()

  // Helper method to compare PreimageState instances
  private def assertPreimageStateEquals(
    expected: PreimageState,
    actual: PreimageState,
    testCaseName: String
  ): Unit =
    expected.accounts.size shouldBe actual.accounts.size withClue
      s"Accounts size mismatch in test case: $testCaseName"

    expected.accounts.zip(actual.accounts).zipWithIndex.foreach { case ((exp, act), idx) =>
      exp.id shouldBe act.id withClue
        s"Account ID mismatch at index $idx in test case: $testCaseName"

      // Compare preimages
      exp.data.preimages.size shouldBe act.data.preimages.size withClue
        s"Preimages size mismatch for account ${exp.id} in test case: $testCaseName"

      exp.data.preimages.zip(act.data.preimages).zipWithIndex.foreach { case ((expP, actP), pIdx) =>
        expP.hash.bytes shouldBe actP.hash.bytes withClue
          s"Preimage hash mismatch at index $pIdx for account ${exp.id} in test case: $testCaseName"
        expP.blob.toArray shouldBe actP.blob.toArray withClue
          s"Preimage blob mismatch at index $pIdx for account ${exp.id} in test case: $testCaseName"
      }

      // Compare lookup metadata
      exp.data.lookupMeta.size shouldBe act.data.lookupMeta.size withClue
        s"LookupMeta size mismatch for account ${exp.id} in test case: $testCaseName"

      exp.data.lookupMeta.zip(act.data.lookupMeta).zipWithIndex.foreach { case ((expM, actM), mIdx) =>
        expM.key.hash.bytes shouldBe actM.key.hash.bytes withClue
          s"LookupMeta hash mismatch at index $mIdx for account ${exp.id} in test case: $testCaseName"
        expM.key.length shouldBe actM.key.length withClue
          s"LookupMeta length mismatch at index $mIdx for account ${exp.id} in test case: $testCaseName"
        expM.value shouldBe actM.value withClue
          s"LookupMeta value mismatch at index $mIdx for account ${exp.id} in test case: $testCaseName"
      }
    }

    expected.statistics.size shouldBe actual.statistics.size withClue
      s"Statistics size mismatch in test case: $testCaseName"
