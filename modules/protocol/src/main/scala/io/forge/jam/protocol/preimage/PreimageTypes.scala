package io.forge.jam.protocol.preimage

import io.forge.jam.core.{JamBytes, codec, CodecDerivation, StfResult}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode, decodeAs}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.extrinsic.Preimage
import io.forge.jam.core.types.preimage.PreimageHash
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder
import spire.math.UInt
import io.forge.jam.core.StfResult.given_JamEncoder_Unit
import io.forge.jam.core.StfResult.given_JamDecoder_Unit

/**
 * Types for the Preimages State Transition Function.
 *
 * The Preimages STF manages preimage storage and retrieval by service account.
 * It validates that preimages were solicited and updates lookup metadata with
 * submission timestamps.
 */
object PreimageTypes:

  /**
   * Key for preimage history lookup - consists of hash and length.
   */
  final case class PreimageHistoryKey(
    hash: Hash,
    length: Long
  )

  object PreimageHistoryKey:
    given JamEncoder[PreimageHistoryKey] with
      def encode(a: PreimageHistoryKey): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.hash.encode
        builder ++= codec.encodeU32LE(UInt(a.length.toInt))
        builder.result()

    given JamDecoder[PreimageHistoryKey] with
      def decode(bytes: JamBytes, offset: Int): (PreimageHistoryKey, Int) =
        val arr = bytes.toArray
        val (hash, _) = bytes.decodeAs[Hash](offset)
        val length = codec.decodeU32LE(arr, offset + Hash.Size).toLong
        (PreimageHistoryKey(hash, length), Hash.Size + 4)

    given Decoder[PreimageHistoryKey] =
      Decoder.instance { cursor =>
        for
          hash <- cursor.get[String]("hash").map(h => Hash(parseHex(h)))
          length <- cursor.get[Long]("length")
        yield PreimageHistoryKey(hash, length)
      }

  /**
   * History entry for a preimage - contains key and list of timestamps.
   */
  final case class PreimageHistory(
    key: PreimageHistoryKey,
    value: List[Long]
  )

  object PreimageHistory:
    given JamEncoder[PreimageHistory] with
      def encode(a: PreimageHistory): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.key.encode
        // value is a fixed-width 1-byte length followed by 4-byte timestamps
        builder += a.value.size.toByte
        for ts <- a.value do
          builder ++= codec.encodeU32LE(UInt(ts.toInt))
        builder.result()

    given JamDecoder[PreimageHistory] with
      def decode(bytes: JamBytes, offset: Int): (PreimageHistory, Int) =
        val arr = bytes.toArray
        var pos = offset
        val (key, keyBytes) = bytes.decodeAs[PreimageHistoryKey](pos)
        pos += keyBytes
        val length = (arr(pos).toInt & 0xff)
        pos += 1
        val value = (0 until length).map { _ =>
          val ts = codec.decodeU32LE(arr, pos).toLong
          pos += 4
          ts
        }.toList
        (PreimageHistory(key, value), pos - offset)

    given Decoder[PreimageHistory] =
      Decoder.instance { cursor =>
        for
          key <- cursor.get[PreimageHistoryKey]("key")
          value <- cursor.get[List[Long]]("value")
        yield PreimageHistory(key, value)
      }

  /**
   * Account info containing preimages and lookup metadata.
   */
  final case class AccountInfo(
    preimages: List[PreimageHash],
    lookupMeta: List[PreimageHistory]
  )

  object AccountInfo:
    given JamEncoder[AccountInfo] with
      def encode(a: AccountInfo): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.preimages.encode
        builder ++= a.lookupMeta.encode
        builder.result()

    given JamDecoder[AccountInfo] with
      def decode(bytes: JamBytes, offset: Int): (AccountInfo, Int) =
        var pos = offset
        val (preimages, preimagesBytes) = bytes.decodeAs[List[PreimageHash]](pos)
        pos += preimagesBytes
        val (lookupMeta, lookupMetaBytes) = bytes.decodeAs[List[PreimageHistory]](pos)
        pos += lookupMetaBytes
        (AccountInfo(preimages, lookupMeta), pos - offset)

    given Decoder[AccountInfo] =
      Decoder.instance { cursor =>
        for
          preimages <- cursor.get[List[PreimageHash]]("preimages")
          lookupMeta <- cursor.get[List[PreimageHistory]]("lookup_meta")
        yield AccountInfo(preimages, lookupMeta)
      }

  /**
   * Service account with preimage data.
   */
  final case class PreimageAccount(
    id: Long,
    data: AccountInfo
  )

  object PreimageAccount:
    given JamEncoder[PreimageAccount] with
      def encode(a: PreimageAccount): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeU32LE(UInt(a.id.toInt))
        builder ++= a.data.encode
        builder.result()

    given JamDecoder[PreimageAccount] with
      def decode(bytes: JamBytes, offset: Int): (PreimageAccount, Int) =
        val arr = bytes.toArray
        var pos = offset
        val id = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        val (data, consumed) = bytes.decodeAs[AccountInfo](pos)
        pos += consumed
        (PreimageAccount(id, data), pos - offset)

    given Decoder[PreimageAccount] =
      Decoder.instance { cursor =>
        for
          id <- cursor.get[Long]("id")
          data <- cursor.get[AccountInfo]("data")
        yield PreimageAccount(id, data)
      }

  /**
   * Service activity record for statistics.
   */
  final case class ServiceActivityRecord(
    providedCount: Int = 0,
    providedSize: Long = 0,
    refinementCount: Long = 0,
    refinementGasUsed: Long = 0,
    imports: Long = 0,
    extrinsicCount: Long = 0,
    extrinsicSize: Long = 0,
    exports: Long = 0,
    accumulateCount: Long = 0,
    accumulateGasUsed: Long = 0
  )

  object ServiceActivityRecord:
    given JamEncoder[ServiceActivityRecord] with
      def encode(a: ServiceActivityRecord): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeCompactInteger(a.providedCount.toLong)
        builder ++= codec.encodeCompactInteger(a.providedSize)
        builder ++= codec.encodeCompactInteger(a.refinementCount)
        builder ++= codec.encodeCompactInteger(a.refinementGasUsed)
        builder ++= codec.encodeCompactInteger(a.imports)
        builder ++= codec.encodeCompactInteger(a.extrinsicCount)
        builder ++= codec.encodeCompactInteger(a.extrinsicSize)
        builder ++= codec.encodeCompactInteger(a.exports)
        builder ++= codec.encodeCompactInteger(a.accumulateCount)
        builder ++= codec.encodeCompactInteger(a.accumulateGasUsed)
        builder.result()

    given JamDecoder[ServiceActivityRecord] with
      def decode(bytes: JamBytes, offset: Int): (ServiceActivityRecord, Int) =
        val arr = bytes.toArray
        var pos = offset
        val (providedCount, pc) = codec.decodeCompactInteger(arr, pos); pos += pc
        val (providedSize, ps) = codec.decodeCompactInteger(arr, pos); pos += ps
        val (refinementCount, rc) = codec.decodeCompactInteger(arr, pos); pos += rc
        val (refinementGasUsed, rgu) = codec.decodeCompactInteger(arr, pos); pos += rgu
        val (imports, imp) = codec.decodeCompactInteger(arr, pos); pos += imp
        val (extrinsicCount, ec) = codec.decodeCompactInteger(arr, pos); pos += ec
        val (extrinsicSize, es) = codec.decodeCompactInteger(arr, pos); pos += es
        val (exports, exp) = codec.decodeCompactInteger(arr, pos); pos += exp
        val (accumulateCount, ac) = codec.decodeCompactInteger(arr, pos); pos += ac
        val (accumulateGasUsed, agu) = codec.decodeCompactInteger(arr, pos); pos += agu
        (
          ServiceActivityRecord(
            providedCount.toInt,
            providedSize,
            refinementCount,
            refinementGasUsed,
            imports,
            extrinsicCount,
            extrinsicSize,
            exports,
            accumulateCount,
            accumulateGasUsed
          ),
          pos - offset
        )

    given Decoder[ServiceActivityRecord] =
      Decoder.instance { cursor =>
        for
          providedCount <- cursor.getOrElse[Int]("provided_count")(0)
          providedSize <- cursor.getOrElse[Long]("provided_size")(0)
          refinementCount <- cursor.getOrElse[Long]("refinement_count")(0)
          refinementGasUsed <- cursor.getOrElse[Long]("refinement_gas_used")(0)
          imports <- cursor.getOrElse[Long]("imports")(0)
          extrinsicCount <- cursor.getOrElse[Long]("extrinsic_count")(0)
          extrinsicSize <- cursor.getOrElse[Long]("extrinsic_size")(0)
          exports <- cursor.getOrElse[Long]("exports")(0)
          accumulateCount <- cursor.getOrElse[Long]("accumulate_count")(0)
          accumulateGasUsed <- cursor.getOrElse[Long]("accumulate_gas_used")(0)
        yield ServiceActivityRecord(
          providedCount,
          providedSize,
          refinementCount,
          refinementGasUsed,
          imports,
          extrinsicCount,
          extrinsicSize,
          exports,
          accumulateCount,
          accumulateGasUsed
        )
      }

  /**
   * Service statistics entry.
   */
  final case class ServiceStatisticsEntry(
    id: Long,
    record: ServiceActivityRecord
  )

  object ServiceStatisticsEntry:
    given JamEncoder[ServiceStatisticsEntry] with
      def encode(a: ServiceStatisticsEntry): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeU32LE(UInt(a.id.toInt))
        builder ++= a.record.encode
        builder.result()

    given JamDecoder[ServiceStatisticsEntry] with
      def decode(bytes: JamBytes, offset: Int): (ServiceStatisticsEntry, Int) =
        val arr = bytes.toArray
        var pos = offset
        val id = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        val (record, consumed) = bytes.decodeAs[ServiceActivityRecord](pos)
        pos += consumed
        (ServiceStatisticsEntry(id, record), pos - offset)

    given Decoder[ServiceStatisticsEntry] =
      Decoder.instance { cursor =>
        for
          id <- cursor.get[Long]("id")
          record <- cursor.get[ServiceActivityRecord]("record")
        yield ServiceStatisticsEntry(id, record)
      }

  /**
   * Preimage state containing service accounts and statistics.
   */
  final case class PreimageState(
    accounts: List[PreimageAccount],
    statistics: List[ServiceStatisticsEntry] = List.empty
  )

  object PreimageState:
    given JamEncoder[PreimageState] with
      def encode(a: PreimageState): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.accounts.encode
        builder ++= a.statistics.encode
        builder.result()

    given JamDecoder[PreimageState] with
      def decode(bytes: JamBytes, offset: Int): (PreimageState, Int) =
        var pos = offset
        val (accounts, accountsBytes) = bytes.decodeAs[List[PreimageAccount]](pos)
        pos += accountsBytes
        val (statistics, statisticsBytes) = bytes.decodeAs[List[ServiceStatisticsEntry]](pos)
        pos += statisticsBytes
        (PreimageState(accounts, statistics), pos - offset)

    given Decoder[PreimageState] =
      Decoder.instance { cursor =>
        for
          accounts <- cursor.get[List[PreimageAccount]]("accounts")
          statistics <- cursor.getOrElse[List[ServiceStatisticsEntry]]("statistics")(List.empty)
        yield PreimageState(accounts, statistics)
      }

  /**
   * Input to the Preimages STF.
   */
  final case class PreimageInput(
    preimages: List[Preimage],
    slot: Long
  )

  object PreimageInput:
    given JamEncoder[PreimageInput] with
      def encode(a: PreimageInput): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.preimages.encode
        builder ++= codec.encodeU32LE(UInt(a.slot.toInt))
        builder.result()

    given JamDecoder[PreimageInput] with
      def decode(bytes: JamBytes, offset: Int): (PreimageInput, Int) =
        val arr = bytes.toArray
        var pos = offset
        val (preimages, preimagesBytes) = bytes.decodeAs[List[Preimage]](pos)
        pos += preimagesBytes
        val slot = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        (PreimageInput(preimages, slot), pos - offset)

    given Decoder[PreimageInput] =
      Decoder.instance { cursor =>
        for
          preimages <- cursor.get[List[Preimage]]("preimages")
          slot <- cursor.get[Long]("slot")
        yield PreimageInput(preimages, slot)
      }

  /**
   * Error codes for the Preimages STF.
   */
  enum PreimageErrorCode:
    case PreimageUnneeded
    case PreimagesNotSortedUnique

  object PreimageErrorCode:
    given JamEncoder[PreimageErrorCode] = CodecDerivation.enumEncoder(_.ordinal)
    given JamDecoder[PreimageErrorCode] = CodecDerivation.enumDecoder(PreimageErrorCode.fromOrdinal)

    given Decoder[PreimageErrorCode] =
      Decoder.instance { cursor =>
        cursor.as[String].map {
          case "preimage_unneeded" => PreimageErrorCode.PreimageUnneeded
          case "preimages_not_sorted_unique" => PreimageErrorCode.PreimagesNotSortedUnique
        }
      }

  /**
   * Output from the Preimages STF.
   */
  type PreimageOutput = StfResult[Unit, PreimageErrorCode]

  object PreimageOutput:
    given JamEncoder[PreimageOutput] = StfResult.stfResultEncoder[Unit, PreimageErrorCode]
    given JamDecoder[PreimageOutput] = StfResult.stfResultDecoder[Unit, PreimageErrorCode]
    given circeDecoder: Decoder[PreimageOutput] =
      Decoder.instance { cursor =>
        val okResult = cursor.downField("ok").focus
        val errResult = cursor.get[PreimageErrorCode]("err")

        if okResult.isDefined then
          Right(StfResult.success(()))
        else
          errResult.map(err => StfResult.error(err))
      }

  /**
   * Test case for Preimages STF.
   */
  final case class PreimageCase(
    input: PreimageInput,
    preState: PreimageState,
    output: PreimageOutput,
    postState: PreimageState
  )

  object PreimageCase:
    import PreimageOutput.circeDecoder

    given JamEncoder[PreimageCase] with
      def encode(a: PreimageCase): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.input.encode
        builder ++= a.preState.encode
        builder ++= a.output.encode
        builder ++= a.postState.encode
        builder.result()

    given JamDecoder[PreimageCase] with
      def decode(bytes: JamBytes, offset: Int): (PreimageCase, Int) =
        var pos = offset
        val (input, inputBytes) = bytes.decodeAs[PreimageInput](pos)
        pos += inputBytes
        val (preState, preStateBytes) = bytes.decodeAs[PreimageState](pos)
        pos += preStateBytes
        val (output, outputBytes) = bytes.decodeAs[PreimageOutput](pos)
        pos += outputBytes
        val (postState, postStateBytes) = bytes.decodeAs[PreimageState](pos)
        pos += postStateBytes
        (PreimageCase(input, preState, output, postState), pos - offset)

    given Decoder[PreimageCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[PreimageInput]("input")
          preState <- cursor.get[PreimageState]("pre_state")
          output <- cursor.get[PreimageOutput]("output")
          postState <- cursor.get[PreimageState]("post_state")
        yield PreimageCase(input, preState, output, postState)
      }
