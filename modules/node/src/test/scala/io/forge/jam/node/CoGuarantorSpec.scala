package io.forge.jam.node

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.Hash
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Who may share a work-package for a core (CE 134): the validators assigned to it in the rotation of the slot, and
  * those of the rotation before, who may still be finishing a package.
  */
class CoGuarantorSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY // 6 validators, 2 cores, rotation of 4 slots, epochs of 12
  private val entropy = (0 to 3).map(i => Hash(Array.fill(32)((i + 1).toByte))).toList
  private val previous = (0 until 6).map(i => s"previous-$i")
  private val current = (0 until 6).map(i => s"current-$i")
  private val upcoming = (0 until 6).map(i => s"next-$i")

  private def assigned(stateSlot: Long, slot: Long, core: Int): Set[String] =
    GuarantorService.guarantorsOf(stateSlot, entropy, previous, current, upcoming, core, slot, config)

  /** The validators of `set` on `core` at `slot`, from the assignment the guarantor itself uses. */
  private def onCore(set: IndexedSeq[String], randomness: Hash, slot: Long, core: Int): Set[String] =
    val cores = io.forge.jam.protocol.report.GuarantorAssignment.coreAssignments(randomness, slot, set.size, config)
    set.zip(cores).collect { case (v, c) if c == core => v }.toSet

  test("the guarantors of a core are those of the slot's rotation and of the one before") {
    for core <- 0 until 2 do
      // Slot 22 is in the rotation of slots 20 to 23; the one before is 16 to 19, in the same epoch (12 to 23).
      assigned(stateSlot = 21, slot = 22, core) shouldBe
        onCore(current, entropy(2), 22, core) ++ onCore(current, entropy(2), 18, core)
    (assigned(21, 22, 0) ++ assigned(21, 22, 1)) shouldBe current.toSet
  }

  test("across an epoch change the rotation before is the previous validator set's") {
    for core <- 0 until 2 do
      // Slot 13 is in the first rotation of the state's epoch: the rotation before it belonged to the epoch before.
      assigned(stateSlot = 12, slot = 13, core) shouldBe
        onCore(current, entropy(2), 13, core) ++ onCore(previous, entropy(3), 9, core)
      // The next block opens a new epoch: its guarantors are the next set, and the current one's just before.
      assigned(stateSlot = 23, slot = 24, core) shouldBe
        onCore(upcoming, entropy(1), 24, core) ++ onCore(current, entropy(2), 20, core)
  }

  test("no rotation precedes the first") {
    assigned(stateSlot = 0, slot = 1, core = 0) shouldBe onCore(current, entropy(2), 1, 0)
  }
