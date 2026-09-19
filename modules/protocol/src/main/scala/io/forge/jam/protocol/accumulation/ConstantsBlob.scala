package io.forge.jam.protocol.accumulation

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.LittleEndian
import io.forge.jam.core.constants

/** Builds the FETCH(CONSTANTS) blob */
object ConstantsBlob:

  private inline def encodeShort(value: Int): Array[Byte] = LittleEndian.encode(value.toLong, 2)
  private inline def encodeIntLE(value: Int): Array[Byte] = LittleEndian.encode(value.toLong, 4)
  private inline def encodeLong(value: Long): Array[Byte] = LittleEndian.encode(value, 8)

  /** Build the 134-byte constants blob for `config`. */
  def build(config: ChainConfig): Array[Byte] =
    val buffer = new java.io.ByteArrayOutputStream(256)

    buffer.write(
      encodeLong(config.additionalMinBalancePerStateItem)
    ) // additionalMinBalancePerStateItem (UInt64)
    buffer.write(
      encodeLong(config.additionalMinBalancePerStateByte)
    ) // additionalMinBalancePerStateByte (UInt64)
    buffer.write(
      encodeLong(config.serviceMinBalance)
    ) // serviceMinBalance (UInt64)
    buffer.write(encodeShort(config.coresCount)) // totalNumberOfCores (UInt16)
    buffer.write(
      encodeIntLE(config.preimageExpungePeriod)
    ) // preimagePurgePeriod (UInt32)
    buffer.write(encodeIntLE(config.epochLength)) // epochLength (UInt32)
    buffer.write(
      encodeLong(config.reportAccGas)
    ) // workReportAccumulationGas (UInt64)
    buffer.write(
      encodeLong(constants.Cpackageauthgas)
    ) // workPackageIsAuthorizedGas (UInt64) - same for both configs
    buffer.write(
      encodeLong(config.maxRefineGas)
    ) // workPackageRefineGas (UInt64)
    buffer.write(
      encodeLong(config.maxBlockGas)
    ) // totalAccumulationGas (UInt64)
    buffer.write(
      encodeShort(config.maxBlockHistory)
    ) // recentHistorySize (UInt16)
    buffer.write(
      encodeShort(config.maxWorkItems)
    ) // maxWorkItems (UInt16)
    buffer.write(
      encodeShort(config.maxDependencies)
    ) // maxDepsInWorkReport (UInt16)
    buffer.write(
      encodeShort(config.maxTicketsPerExtrinsic)
    ) // maxTicketsPerExtrinsic (UInt16)
    buffer.write(
      encodeIntLE(config.maxLookupAnchorAge.toInt)
    ) // maxLookupAnchorAge (UInt32)
    buffer.write(
      encodeShort(8)
    ) // maxAuthorizationsPoolItems (UInt16) - same for both configs
    buffer.write(encodeShort(config.slotDuration)) // slotPeriodSeconds (UInt16)
    buffer.write(
      encodeShort(config.authQueueSize)
    ) // maxAuthorizationsQueueItems (UInt16)
    buffer.write(
      encodeShort(config.rotationPeriod)
    ) // coreAssignmentRotationPeriod (UInt16)
    buffer.write(
      encodeShort(128)
    ) // maxWorkPackageExtrinsics (UInt16) - same for both configs
    buffer.write(
      encodeShort(5)
    ) // preimageReplacementPeriod / Cassurancetimeoutperiod (UInt16) - same for both configs
    buffer.write(encodeIntLE(constants.Cmaxauthcodesize)) // Cmaxauthcodesize (UInt32)
    buffer.write(encodeIntLE(13_791_360)) // Cmaxbundlesize (UInt32)
    buffer.write(encodeIntLE(constants.Cmaxservicecodesize)) // Cmaxservicecodesize (UInt32)
    buffer.write(encodeIntLE(constants.Cmaxpackageimports)) // maxWorkPackageImports (UInt32)
    buffer.write(encodeIntLE(constants.Cmaxreportvarsize)) // maxWorkReportBlobSize (UInt32) 48KB
    buffer.write(encodeIntLE(constants.Cmemosize)) // transferMemoSize (UInt32)
    buffer.write(encodeIntLE(constants.Cmaxpackageexports)) // maxWorkPackageExports (UInt32)
    buffer.write(
      encodeIntLE(config.ticketCutoff)
    ) // ticketSubmissionEndSlot (UInt32)

    buffer.toByteArray
