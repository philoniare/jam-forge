package io.forge.jam.protocol.preimage

import _root_.scodec.Codec
import _root_.scodec.codecs.*
import io.forge.jam.core.StfResult
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.extrinsic.Preimage
import io.forge.jam.core.types.preimage.PreimageHash
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.scodec.JamCodecs.{hashCodec, compactInteger, compactPrefixedList}
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder

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
    given Codec[PreimageHistoryKey] =
      (hashCodec :: uint32L).xmap(
        { case (hash, length) => PreimageHistoryKey(hash, length & 0xFFFFFFFFL) },
        key => (key.hash, key.length & 0xFFFFFFFFL)
      )

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
    given Codec[PreimageHistory] =
      (summon[Codec[PreimageHistoryKey]] :: listOfN(uint8, uint32L)).xmap(
        { case (key, timestamps) => PreimageHistory(key, timestamps.map(_ & 0xFFFFFFFFL)) },
        hist => (hist.key, hist.value.map(_ & 0xFFFFFFFFL))
      )

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
    given Codec[AccountInfo] =
      (compactPrefixedList(summon[Codec[PreimageHash]]) :: compactPrefixedList(summon[Codec[PreimageHistory]])).xmap(
        { case (preimages, lookupMeta) => AccountInfo(preimages, lookupMeta) },
        ai => (ai.preimages, ai.lookupMeta)
      )

    given Decoder[AccountInfo] =
      Decoder.instance { cursor =>
        for
          preimages <- cursor.get[List[PreimageHash]]("preimage_blobs")
          lookupMeta <- cursor.get[List[PreimageHistory]]("preimage_requests")
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
    given Codec[PreimageAccount] =
      (uint32L :: summon[Codec[AccountInfo]]).xmap(
        { case (id, data) => PreimageAccount(id & 0xFFFFFFFFL, data) },
        pa => (pa.id & 0xFFFFFFFFL, pa.data)
      )

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
    given Codec[ServiceActivityRecord] =
      (compactInteger :: compactInteger :: compactInteger :: compactInteger :: compactInteger ::
       compactInteger :: compactInteger :: compactInteger :: compactInteger :: compactInteger).xmap(
        { case (pc, ps, rc, rgu, imp, ec, es, exp, ac, agu) =>
          ServiceActivityRecord(pc.toInt, ps, rc, rgu, imp, ec, es, exp, ac, agu)
        },
        sar => (sar.providedCount.toLong, sar.providedSize, sar.refinementCount, sar.refinementGasUsed,
                sar.imports, sar.extrinsicCount, sar.extrinsicSize, sar.exports,
                sar.accumulateCount, sar.accumulateGasUsed)
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
    given Codec[ServiceStatisticsEntry] =
      (uint32L :: summon[Codec[ServiceActivityRecord]]).xmap(
        { case (id, record) => ServiceStatisticsEntry(id & 0xFFFFFFFFL, record) },
        sse => (sse.id & 0xFFFFFFFFL, sse.record)
      )

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
    given Codec[PreimageState] =
      (compactPrefixedList(summon[Codec[PreimageAccount]]) :: compactPrefixedList(summon[Codec[ServiceStatisticsEntry]])).xmap(
        { case (accounts, statistics) => PreimageState(accounts, statistics) },
        ps => (ps.accounts, ps.statistics)
      )

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
    given Codec[PreimageInput] =
      (compactPrefixedList(summon[Codec[Preimage]]) :: uint32L).xmap(
        { case (preimages, slot) => PreimageInput(preimages, slot & 0xFFFFFFFFL) },
        pi => (pi.preimages, pi.slot & 0xFFFFFFFFL)
      )

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
    given Codec[PreimageErrorCode] = uint8.xmap(
      ordinal => PreimageErrorCode.fromOrdinal(ordinal),
      err => err.ordinal
    )

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
    // Unit codec encodes to zero bytes
    given unitCodec: Codec[Unit] = provide(())

    given Codec[PreimageOutput] = JamCodecs.stfResultCodec[Unit, PreimageErrorCode]

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
    import PreimageOutput.{circeDecoder, given}

    given Codec[PreimageCase] =
      (summon[Codec[PreimageInput]] :: summon[Codec[PreimageState]] ::
       summon[Codec[PreimageOutput]] :: summon[Codec[PreimageState]]).xmap(
        { case (input, preState, output, postState) => PreimageCase(input, preState, output, postState) },
        pc => (pc.input, pc.preState, pc.output, pc.postState)
      )

    given Decoder[PreimageCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[PreimageInput]("input")
          preState <- cursor.get[PreimageState]("pre_state")
          output <- cursor.get[PreimageOutput]("output")
          postState <- cursor.get[PreimageState]("post_state")
        yield PreimageCase(input, preState, output, postState)
      }
