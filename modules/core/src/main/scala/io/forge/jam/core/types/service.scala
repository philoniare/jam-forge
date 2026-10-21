package io.forge.jam.core.types

import io.forge.jam.core.codec.Codec
import io.forge.jam.core.codec.codecs.uint32L
import io.forge.jam.core.primitives.Hash
import io.circe.Decoder

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

    /** Service account info as stored in state (GP 0.8.0 appendix D, C(255, s)). */
    given Codec[ServiceInfo] = Codec.of(
      (w, si) =>
        w.u8(si.version)
        w.bytes(si.codeHash.toArrayUnsafe)
        w.u64(si.balance)
        w.u64(si.minItemGas)
        w.u64(si.minMemoGas)
        w.u64(si.bytesUsed)
        w.u64(si.depositOffset)
        w.i32(si.items)
        w.u32(si.creationSlot)
        w.u32(si.lastAccumulationSlot)
        w.u32(si.parentService),
      r =>
        val version = r.u8()
        val codeHash = Hash.wrap(r.bytes(Hash.Size))
        ServiceInfo(
          version = version,
          codeHash = codeHash,
          balance = r.u64(),
          minItemGas = r.u64(),
          minMemoGas = r.u64(),
          bytesUsed = r.u64(),
          depositOffset = r.u64(),
          items = r.i32(),
          creationSlot = r.u32(),
          lastAccumulationSlot = r.u32(),
          parentService = r.u32()
        ),
      Size
    )

    given Decoder[ServiceInfo] =
      Decoder.instance { cursor =>
        for
          version <- cursor.getOrElse[Int]("version")(0)
          codeHash <- cursor.get[Hash]("code_hash")
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
        // Ensure service ID is treated as unsigned 32-bit
        yield ServiceAccount(id & 0xFFFFFFFFL, data)
      }
