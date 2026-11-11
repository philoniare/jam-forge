package io.forge.jam.node

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.JamBytes.compareUnsigned
import io.forge.jam.core.primitives.ServiceId
import io.forge.jam.core.types.extrinsic.Preimage
import io.forge.jam.protocol.preimage.PreimageTransition
import io.forge.jam.protocol.preimage.PreimageTypes.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ExtrinsicPoolsSpec extends AnyFunSuite with Matchers:

  /** Two single-byte blobs b1 < b2 whose blake2b hashes sort the other way. */
  private def hashOrderDiffersFromByteOrder: (Array[Byte], Array[Byte]) =
    val pairs =
      for
        i <- 0 until 256
        j <- (i + 1) until 256
        a = Array(i.toByte)
        b = Array(j.toByte)
        if compareUnsigned(Hashing.blake2b256(a).bytes, Hashing.blake2b256(b).bytes) > 0
      yield (a, b)
    pairs.head

  test("takePreimages orders one service's preimages by blob bytes, as block import requires") {
    val (low, high) = hashOrderDiffersFromByteOrder
    val service = 42L
    val pLow = Preimage(ServiceId(service.toInt), JamBytes(low))
    val pHigh = Preimage(ServiceId(service.toInt), JamBytes(high))

    val pools = new ExtrinsicPools
    pools.addPreimage(pHigh)
    pools.addPreimage(pLow)
    val taken = pools.takePreimages()

    taken shouldBe List(pLow, pHigh)
    PreimageTransition.arePreimagesSortedAndUnique(taken) shouldBe true

    // Run the authored extrinsic through the import-side STF with both blobs solicited.
    def solicited(blob: Array[Byte]) =
      PreimageHistory(PreimageHistoryKey(Hashing.blake2b256(blob), blob.length.toLong), Nil)
    val preState = PreimageState(
      accounts = List(PreimageAccount(service, AccountInfo(Nil, List(solicited(low), solicited(high)))))
    )
    val (_, output) = PreimageTransition.stfInternal(PreimageInput(taken, slot = 1L), preState)
    output shouldBe Right(())
  }

  test("takePreimages orders by requester before blob") {
    val (low, high) = hashOrderDiffersFromByteOrder
    val pools = new ExtrinsicPools
    val a = Preimage(ServiceId(7), JamBytes(low))
    val b = Preimage(ServiceId(3), JamBytes(high))
    pools.addPreimage(a)
    pools.addPreimage(b)
    pools.takePreimages() shouldBe List(b, a)
  }
