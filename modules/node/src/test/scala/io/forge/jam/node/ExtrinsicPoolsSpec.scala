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

  private def guarantee(pkg: String, core: Int = 0): io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic =
    import io.forge.jam.core.primitives.{CoreIndex, Gas, Hash, Timeslot}
    import spire.math.{UInt, UShort}
    val report = io.forge.jam.core.types.workpackage.WorkReport(
      packageSpec = io.forge.jam.core.types.work.PackageSpec(
        Hashing.blake2b256(pkg.getBytes("UTF-8")), UInt(0), Hash.zero, UShort(0), Hash.zero, UShort(0)),
      context = io.forge.jam.core.types.context.Context(Hash.zero, Timeslot(UInt(0)), Hash.zero, Hash.zero, Hash.zero,
        Timeslot(UInt(0)), Hash.zero, List.empty),
      coreIndex = CoreIndex(core),
      authorizerHash = Hash.zero,
      authGasUsed = Gas(0),
      authOutput = JamBytes.empty,
      segmentRootLookup = List.empty,
      results = List.empty
    )
    io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic(report, Timeslot(UInt(5)), List.empty)

  private def extrinsic(
      preimages: List[Preimage] = Nil,
      guarantees: List[io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic] = Nil
  ): io.forge.jam.core.types.block.Extrinsic =
    io.forge.jam.core.types.block.Extrinsic(
      Nil, preimages, guarantees, Nil, io.forge.jam.core.types.extrinsic.Dispute(Nil, Nil, Nil))

  test("guarantees and preimages of blocks a reorg left behind are pooled again, unless the new chain has them") {
    val pools = new ExtrinsicPools
    val (lost, kept) = (Preimage(ServiceId(1), JamBytes(Array[Byte](1))), Preimage(ServiceId(1), JamBytes(Array[Byte](2))))
    val (gLost, gKept) = (guarantee("lost"), guarantee("kept", core = 1))
    pools.restoreAbandoned(
      abandoned = List(extrinsic(List(lost), List(gLost)), extrinsic(List(kept), List(gKept))),
      adopted = List(extrinsic(List(kept), List(gKept)))
    )
    pools.pendingPreimages shouldBe List(lost)
    pools.pendingGuarantees shouldBe List(gLost)
    // Again changes nothing: both are pooled already.
    pools.restoreAbandoned(List(extrinsic(List(lost), List(gLost))), Nil)
    (pools.pendingPreimages.size, pools.guaranteeCount) shouldBe ((1, 1))
  }
