package io.forge.jam.protocol.report

import io.forge.jam.core.{ChainConfig, Shuffle}
import io.forge.jam.core.primitives.Hash

object GuarantorAssignment:

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
