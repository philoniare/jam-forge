package io.forge.jam.protocol.report

import io.forge.jam.core.{ChainConfig, Shuffle}
import io.forge.jam.core.primitives.Hash

object GuarantorAssignment:

  /** A guarantee signed in the rotation of `reportSlot` may be reported in a block at `blockSlot` when that rotation
    * is the block's or the one before (GP eq:guarantorsig).
    */
  def rotationInWindow(reportSlot: Long, blockSlot: Long, config: ChainConfig): Boolean =
    reportSlot / config.rotationPeriod >= blockSlot / config.rotationPeriod - 1

  /** The first slot whose guarantees a block at `blockSlot` may still report. */
  def oldestReportableSlot(blockSlot: Long, config: ChainConfig): Long =
    (blockSlot / config.rotationPeriod - 1) * config.rotationPeriod

  def coreAssignments(
    randomness: Hash,
    slot: Long,
    validatorCount: Int,
    config: ChainConfig
  ): Array[Int] =
    val activeCores = math.max(1, validatorCount / 3)
    val source = new Array[Int](validatorCount)
    var i = 0
    while i < validatorCount do
      source(i) = i / 3
      i += 1
    val shuffledIndices = Shuffle.jamComputeShuffle(validatorCount, randomness)
    val shift = (math.floorMod(slot, config.epochLength) / config.rotationPeriod).toInt
    val out = new Array[Int](shuffledIndices.length)
    var j = 0
    val it = shuffledIndices.iterator
    while it.hasNext do
      val idx = it.next()
      out(j) = math.floorMod(source(idx) + shift, activeCores)
      j += 1
    out
