package io.forge.jam.core.types

import scodec.*
import scodec.bits.*
import scodec.codecs.*
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder

/**
 * Service-related types.
 */
object service:

  // Helper codec for Hash (32 bytes)
  private val hashCodec: Codec[Hash] = fixedSizeBytes(Hash.Size.toLong, bytes).xmap(
    bv => Hash.fromByteVectorUnsafe(bv),
    h => h.toByteVector
  )

  /**
   * Service info for validation.
   */
  final case class ServiceInfo(
    version: Int = 0,
    codeHash: Hash,
    balance: Long,
    minItemGas: Long,
    minMemoGas: Long,
    bytesUsed: Long,
    depositOffset: Long = 0,
    items: Int,
    creationSlot: Long = 0,
    lastAccumulationSlot: Long = 0,
    parentService: Long = 0
  )

  object ServiceInfo:
    val Size: Int = 89

    given Codec[ServiceInfo] =
      (byte ::
       hashCodec ::
       int64L ::
       int64L ::
       int64L ::
       int64L ::
       int64L ::
       uint32L ::
       uint32L ::
       uint32L ::
       uint32L).xmap(
        { case (version, codeHash, balance, minItemGas, minMemoGas, bytesUsed, depositOffset, items, creationSlot, lastAccumulationSlot, parentService) =>
          ServiceInfo(
            version = version.toInt & 0xFF,
            codeHash = codeHash,
            balance = balance,
            minItemGas = minItemGas,
            minMemoGas = minMemoGas,
            bytesUsed = bytesUsed,
            depositOffset = depositOffset,
            items = items.toInt,
            creationSlot = creationSlot & 0xFFFFFFFFL,
            lastAccumulationSlot = lastAccumulationSlot & 0xFFFFFFFFL,
            parentService = parentService & 0xFFFFFFFFL
          )
        },
        si => (
          si.version.toByte,
          si.codeHash,
          si.balance,
          si.minItemGas,
          si.minMemoGas,
          si.bytesUsed,
          si.depositOffset,
          si.items.toLong & 0xFFFFFFFFL,
          si.creationSlot & 0xFFFFFFFFL,
          si.lastAccumulationSlot & 0xFFFFFFFFL,
          si.parentService & 0xFFFFFFFFL
        )
      )

    given Decoder[ServiceInfo] =
      Decoder.instance { cursor =>
        for
          version <- cursor.getOrElse[Int]("version")(0)
          codeHash <- cursor.get[String]("code_hash").map(h => Hash(parseHex(h)))
          balance <- cursor.get[Long]("balance")
          minItemGas <- cursor.get[Long]("min_item_gas")
          minMemoGas <- cursor.get[Long]("min_memo_gas")
          bytesUsed <- cursor.get[Long]("bytes")
          depositOffset <- cursor.getOrElse[Long]("deposit_offset")(0)
          items <- cursor.get[Int]("items")
          creationSlot <- cursor.getOrElse[Long]("creation_slot")(0)
          lastAccumulationSlot <- cursor.getOrElse[Long]("last_accumulation_slot")(0)
          parentService <- cursor.getOrElse[Long]("parent_service")(0)
        yield ServiceInfo(
          version,
          codeHash,
          balance,
          minItemGas,
          minMemoGas,
          bytesUsed,
          depositOffset,
          items,
          creationSlot,
          lastAccumulationSlot,
          parentService
        )
      }

  /**
   * Service data wrapper containing service info.
   */
  final case class ServiceData(
    service: ServiceInfo
  )

  object ServiceData:
    val Size: Int = ServiceInfo.Size

    given Codec[ServiceData] = summon[Codec[ServiceInfo]].xmap(
      ServiceData.apply,
      _.service
    )

    given Decoder[ServiceData] =
      Decoder.instance(cursor => cursor.get[ServiceInfo]("service").map(ServiceData.apply))

  /**
   * Service account with ID and data.
   */
  final case class ServiceAccount(
    id: Long,
    data: ServiceData
  )

  object ServiceAccount:
    val Size: Int = 4 + ServiceData.Size

    given Codec[ServiceAccount] =
      (uint32L :: summon[Codec[ServiceData]]).xmap(
        { case (id, data) => ServiceAccount(id & 0xFFFFFFFFL, data) },
        sa => (sa.id & 0xFFFFFFFFL, sa.data)
      )

    given Decoder[ServiceAccount] =
      Decoder.instance { cursor =>
        for
          id <- cursor.get[Long]("id")
          data <- cursor.get[ServiceData]("data")
        yield ServiceAccount(id, data)
      }
