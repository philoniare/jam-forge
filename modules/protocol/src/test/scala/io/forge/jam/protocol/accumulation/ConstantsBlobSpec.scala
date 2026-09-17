package io.forge.jam.protocol.accumulation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.ChainConfig

import java.nio.{ByteBuffer, ByteOrder}

class ConstantsBlobSpec extends AnyFunSuite with Matchers:

  private def le(value: Long, byteCount: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(byteCount).order(ByteOrder.LITTLE_ENDIAN)
    byteCount match
      case 2 => buf.putShort(value.toShort)
      case 4 => buf.putInt(value.toInt)
      case 8 => buf.putLong(value)
      case n => fail(s"unsupported byteCount $n")
    buf.array()

  private def expectedBlob(config: ChainConfig): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream(256)
    out.write(le(config.additionalMinBalancePerStateItem, 8)) // Citemdeposit
    out.write(le(config.additionalMinBalancePerStateByte, 8)) // Cbytedeposit
    out.write(le(config.serviceMinBalance, 8))                // Cbasedeposit
    out.write(le(config.coresCount.toLong, 2))                // Ccorecount
    out.write(le(config.preimageExpungePeriod.toLong, 4))     // Cexpungeperiod
    out.write(le(config.epochLength.toLong, 4))               // Cepochlen
    out.write(le(config.reportAccGas, 8))                     // Creportaccgas
    out.write(le(50_000_000L, 8))                             // Cpackageauthgas
    out.write(le(config.maxRefineGas, 8))                     // Cpackagerefgas
    out.write(le(config.maxBlockGas, 8))                      // Cblockaccgas
    out.write(le(config.maxBlockHistory.toLong, 2))           // Crecenthistorylen
    out.write(le(config.maxWorkItems.toLong, 2))              // Cmaxpackageitems
    out.write(le(config.maxDependencies.toLong, 2))           // Cmaxreportdeps
    out.write(le(config.maxTicketsPerExtrinsic.toLong, 2))    // Cmaxblocktickets
    out.write(le(config.maxLookupAnchorAge, 4))               // Cmaxlookupanchorage
    // Cticketentries REMOVED (#514)
    out.write(le(8L, 2))                                      // Cauthpoolsize
    out.write(le(config.slotDuration.toLong, 2))               // Cslotseconds
    out.write(le(config.authQueueSize.toLong, 2))              // Cauthqueuesize
    out.write(le(config.rotationPeriod.toLong, 2))            // Crotationperiod
    out.write(le(128L, 2))                                    // Cmaxpackagexts
    out.write(le(5L, 2))                                       // Cassurancetimeoutperiod
    // Cvalcount REMOVED (#514)
    out.write(le(64_000L, 4))                                 // Cmaxauthcodesize
    out.write(le(13_791_360L, 4))                              // Cmaxbundlesize
    out.write(le(4_000_000L, 4))                               // Cmaxservicecodesize
    // Cecpiecesize REMOVED (#514)
    out.write(le(3072L, 4))                                    // Cmaxpackageimports
    // Csegmentecpieces REMOVED (#514)
    out.write(le(48L * 1024L, 4))                               // Cmaxreportvarsize
    out.write(le(128L, 4))                                       // Cmemosize
    out.write(le(3072L, 4))                                     // Cmaxpackageexports
    out.write(le(config.ticketCutoff.toLong, 4))                // Cepochtailstart
    out.toByteArray

  for (name, config) <- Seq("TINY" -> ChainConfig.TINY, "FULL" -> ChainConfig.FULL) do
    test(s"$name: constants blob is 122 bytes and matches the independent gp 0.8.0 encoding") {
      val actual = ConstantsBlob.build(config)
      actual.length shouldBe 122
      actual.toList shouldBe expectedBlob(config).toList
    }
