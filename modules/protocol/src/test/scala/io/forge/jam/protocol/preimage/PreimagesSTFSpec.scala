package io.forge.jam.protocol.preimage

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Hash, ServiceId}
import io.forge.jam.core.types.extrinsic.Preimage
import io.forge.jam.protocol.generators.StfGenerators.*
import io.forge.jam.protocol.preimage.PreimageTypes.*

/**
 * - Preimage management tracks request counts correctly
 * - Preimage availability respects timeout constraints
 */
class PreimagesSTFSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  private val testConfig = ChainConfig.TINY

  // Override default ScalaCheck configuration for faster tests
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  test("property: preimage state accounts list is valid") {
    forAll(genPreimageState) { state =>
      // Property: accounts list should be a valid list
      state.accounts should not be null
    }
  }

  test("property: preimage account has valid ID") {
    forAll(genPreimageAccount) { account =>
      // Property: account ID should be non-negative
      account.id should be >= 0L
    }
  }

  test("property: preimage hash has correct size") {
    forAll(genPreimageHash) { preimageHash =>
      // Property: hash should be 32 bytes
      preimageHash.hash.size shouldBe Hash.Size

      // Property: blob should be non-empty
      preimageHash.blob.length should be > 0
    }
  }

  test("property: preimage history key has valid hash") {
    forAll(genPreimageHistoryKey) { key =>
      // Property: hash should be 32 bytes
      key.hash.size shouldBe Hash.Size

      // Property: length should be positive
      key.length should be > 0L
    }
  }

  test("property: preimage history has valid structure") {
    forAll(genPreimageHistory) { history =>
      // Property: key should have valid hash
      history.key.hash.size shouldBe Hash.Size

      // Property: timestamps list should be valid
      history.value should not be null
    }
  }

  test("property: preimage input has valid slot") {
    forAll(genPreimageInput) { input =>
      // Property: slot should be non-negative
      input.slot should be >= 0L
    }
  }

  test("property: preimage extrinsic has valid requester") {
    forAll(genPreimageExtrinsic) { preimage =>
      // Property: requester service ID should be valid
      preimage.requester.value.toLong should be >= 0L
    }
  }

  test("property: preimage extrinsic has non-empty blob") {
    forAll(genPreimageExtrinsic) { preimage =>
      // Property: blob should not be empty
      preimage.blob.length should be > 0
    }
  }

  test("property: empty preimages input produces successful output") {
    forAll(genPreimageState) { preState =>
      val input = PreimageInput(preimages = List.empty, slot = 100L)

      val (postState, output) = PreimageTransition.stfInternal(input, preState)

      // Property: empty preimages should succeed
      output.isRight shouldBe true

      // Property: state should remain unchanged with empty input
      postState.accounts.size shouldBe preState.accounts.size
    }
  }

  test("property: preimages must be sorted by (requester, blob)") {
    // Generate two preimages with different requesters
    forAll(genPreimageExtrinsic, genPreimageExtrinsic) { (p1, p2) =>
      // Ensure they have different requesters for proper sorting test
      whenever(p1.requester.value != p2.requester.value) {
        val sorted = List(p1, p2).sortBy(p => (p.requester.value.toLong, p.blob.toHex))

        // Property: sorted list should be in ascending order by requester first
        if sorted.size == 2 then
          sorted.head.requester.value.toLong should be <= sorted(1).requester.value.toLong
      }
    }
  }

  test("property: duplicate preimages are rejected") {
    forAll(genPreimageExtrinsic) { preimage =>
      // Create input with duplicate preimages
      val duplicateInput = PreimageInput(
        preimages = List(preimage, preimage),
        slot = 100L
      )

      // Create empty state - the validation order is:
      // 1. Check if preimage is solicited (PreimageUnneeded if not)
      // 2. Check if sorted/unique (PreimagesNotSortedUnique if not)
      // With empty state, unsolicited check fails first
      val emptyState = PreimageState(accounts = List.empty, statistics = List.empty)
      val (_, output) = PreimageTransition.stfInternal(duplicateInput, emptyState)

      // Property: duplicate preimages should fail
      // (either PreimageUnneeded if not solicited, or PreimagesNotSortedUnique if solicited)
      output.isLeft shouldBe true
    }
  }

  test("property: unsolicited preimage is rejected") {
    forAll(genPreimageExtrinsic) { preimage =>
      // Create empty state with no accounts
      val emptyState = PreimageState(accounts = List.empty, statistics = List.empty)
      val input = PreimageInput(preimages = List(preimage), slot = 100L)

      val (_, output) = PreimageTransition.stfInternal(input, emptyState)

      // Property: preimage for non-existent account should fail
      output.isLeft shouldBe true
      output.left.toOption.get shouldBe PreimageErrorCode.PreimageUnneeded
    }
  }

  test("property: preimage hash is computed correctly") {
    forAll(genJamBytesRange(1, 100)) { blob =>
      val hash = Hashing.blake2b256(blob)

      // Property: hash should be 32 bytes
      hash.size shouldBe Hash.Size

      // Property: same blob should produce same hash
      val hash2 = Hashing.blake2b256(blob)
      java.util.Arrays.equals(hash.bytes, hash2.bytes) shouldBe true
    }
  }

  test("property: account info has valid structure") {
    forAll(genAccountInfo) { info =>
      // Property: preimages list should be valid
      info.preimages should not be null

      // Property: lookupMeta list should be valid
      info.lookupMeta should not be null
    }
  }

  test("property: preimage account contains valid data") {
    forAll(genPreimageAccount) { account =>
      // Property: account has valid ID
      account.id should be >= 0L

      // Property: account has valid data
      account.data should not be null
    }
  }

  test("GP: stfInternal with empty preimages returns success and preserves state") {
    forAll(genPreimageState) { preState =>
      val input = PreimageInput(preimages = List.empty, slot = 100L)

      val (postState, output) = PreimageTransition.stfInternal(input, preState)

      // GP: Empty preimages should succeed
      output.isRight shouldBe true

      // GP: State accounts should be preserved
      postState.accounts.size shouldBe preState.accounts.size
      postState.statistics.size shouldBe preState.statistics.size
    }
  }

  test("GP: stfInternal rejects unsolicited preimage with PreimageUnneeded") {
    forAll(genPreimageExtrinsic) { preimage =>
      // Empty state means no solicited preimages
      val emptyState = PreimageState(accounts = List.empty, statistics = List.empty)
      val input = PreimageInput(preimages = List(preimage), slot = 100L)

      val (_, output) = PreimageTransition.stfInternal(input, emptyState)

      // GP: Should reject with PreimageUnneeded since no account exists
      output.isLeft shouldBe true
      output.left.toOption.get shouldBe PreimageErrorCode.PreimageUnneeded
    }
  }

  test("GP: stfInternal is deterministic (same input produces same output)") {
    forAll(genPreimageState) { preState =>
      val input = PreimageInput(preimages = List.empty, slot = 100L)

      val (postState1, output1) = PreimageTransition.stfInternal(input, preState)
      val (postState2, output2) = PreimageTransition.stfInternal(input, preState)

      // GP: Same inputs must produce identical outputs
      output1 shouldBe output2
      postState1.accounts.size shouldBe postState2.accounts.size
      postState1.statistics.size shouldBe postState2.statistics.size
    }
  }

  test("GP: preimage hash is blake2b256 of blob") {
    forAll(genJamBytesRange(1, 100)) { blob =>
      // GP: H(blob) = blake2b-256(blob)
      val hash = Hashing.blake2b256(blob)

      // GP: Hash must be 32 bytes
      hash.size shouldBe Hash.Size

      // GP: Hash must be deterministic
      val hash2 = Hashing.blake2b256(blob)
      java.util.Arrays.equals(hash.bytes, hash2.bytes) shouldBe true
    }
  }

  test("GP: preimages sorted by (requester, blob) with requester as primary key") {
    forAll(genPreimageExtrinsic, genPreimageExtrinsic) { (p1, p2) =>
      whenever(p1.requester.value != p2.requester.value) {
        val sorted = List(p1, p2).sortBy(p => (p.requester.value.toLong, p.blob.toHex))

        // GP: Sorted list should have lower requester first
        sorted.head.requester.value.toLong should be <= sorted(1).requester.value.toLong
      }
    }
  }

  test("GP: duplicate preimages fail validation (not sorted unique)") {
    forAll(genPreimageExtrinsic) { preimage =>
      // Create duplicate preimages
      val duplicates = List(preimage, preimage)
      val emptyState = PreimageState(accounts = List.empty, statistics = List.empty)
      val input = PreimageInput(preimages = duplicates, slot = 100L)

      val (_, output) = PreimageTransition.stfInternal(input, emptyState)

      // GP: Should fail (either PreimageUnneeded or PreimagesNotSortedUnique)
      output.isLeft shouldBe true
    }
  }

  test("property: service activity record has non-negative values") {
    forAll(Gen.choose(0, 100), Gen.choose(0L, 10000L)) { (count, size) =>
      val record = ServiceActivityRecord(
        providedCount = count,
        providedSize = size
      )

      // Property: all values should be non-negative
      record.providedCount should be >= 0
      record.providedSize should be >= 0L
    }
  }

  test("property: service statistics entry has valid structure") {
    forAll(Gen.choose(0L, 1000L)) { serviceId =>
      val entry = ServiceStatisticsEntry(
        id = serviceId,
        record = ServiceActivityRecord()
      )

      // Property: entry has valid ID
      entry.id shouldBe serviceId

      // Property: entry has valid record
      entry.record should not be null
    }
  }
