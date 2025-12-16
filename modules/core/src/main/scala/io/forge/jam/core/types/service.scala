package io.forge.jam.core.types

import io.forge.jam.core.{JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode, decodeAs}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder
import spire.math.{UInt, ULong}

/**
 * Service-related types.
 */
object service:

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

    given JamEncoder[ServiceInfo] with
      def encode(a: ServiceInfo): JamBytes =
        val builder = JamBytes.newBuilder
        builder += a.version.toByte
        builder ++= a.codeHash.bytes
        builder ++= codec.encodeU64LE(ULong(a.balance))
        builder ++= codec.encodeU64LE(ULong(a.minItemGas))
        builder ++= codec.encodeU64LE(ULong(a.minMemoGas))
        builder ++= codec.encodeU64LE(ULong(a.bytesUsed))
        builder ++= codec.encodeU64LE(ULong(a.depositOffset))
        builder ++= codec.encodeU32LE(UInt(a.items))
        builder ++= codec.encodeU32LE(UInt(a.creationSlot.toInt))
        builder ++= codec.encodeU32LE(UInt(a.lastAccumulationSlot.toInt))
        builder ++= codec.encodeU32LE(UInt(a.parentService.toInt))
        builder.result()

    given JamDecoder[ServiceInfo] with
      def decode(bytes: JamBytes, offset: Int): (ServiceInfo, Int) =
        val arr = bytes.toArray
        var pos = offset
        val version = arr(pos).toInt & 0xff
        pos += 1
        val codeHash = Hash(arr.slice(pos, pos + 32))
        pos += 32
        val balance = codec.decodeU64LE(arr, pos).signed
        pos += 8
        val minItemGas = codec.decodeU64LE(arr, pos).signed
        pos += 8
        val minMemoGas = codec.decodeU64LE(arr, pos).signed
        pos += 8
        val bytesUsed = codec.decodeU64LE(arr, pos).signed
        pos += 8
        val depositOffset = codec.decodeU64LE(arr, pos).signed
        pos += 8
        val items = codec.decodeU32LE(arr, pos).signed
        pos += 4
        val creationSlot = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        val lastAccumulationSlot = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        val parentService = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        (
          ServiceInfo(
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
          ),
          Size
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

    given JamEncoder[ServiceData] with
      def encode(a: ServiceData): JamBytes = a.service.encode

    given JamDecoder[ServiceData] with
      def decode(bytes: JamBytes, offset: Int): (ServiceData, Int) =
        val (service, consumed) = bytes.decodeAs[ServiceInfo](offset)
        (ServiceData(service), consumed)

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

    given JamEncoder[ServiceAccount] with
      def encode(a: ServiceAccount): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeU32LE(UInt(a.id.toInt))
        builder ++= a.data.encode
        builder.result()

    given JamDecoder[ServiceAccount] with
      def decode(bytes: JamBytes, offset: Int): (ServiceAccount, Int) =
        val arr = bytes.toArray
        val id = codec.decodeU32LE(arr, offset).toLong
        val (data, dataBytes) = bytes.decodeAs[ServiceData](offset + 4)
        (ServiceAccount(id, data), 4 + dataBytes)

    given Decoder[ServiceAccount] =
      Decoder.instance { cursor =>
        for
          id <- cursor.get[Long]("id")
          data <- cursor.get[ServiceData]("data")
        yield ServiceAccount(id, data)
      }
