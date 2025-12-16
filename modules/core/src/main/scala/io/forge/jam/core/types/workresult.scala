package io.forge.jam.core.types

import scodec.*
import scodec.bits.*
import scodec.codecs.*
import io.forge.jam.core.primitives.{Hash, ServiceId, Gas}
import io.forge.jam.core.scodec.JamCodecs.compactInteger
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder
import spire.math.{UShort, UInt}

/**
 * Work result related types
 */
object workresult:

  // Helper codec for Hash (32 bytes)
  private val hashCodec: Codec[Hash] = fixedSizeBytes(Hash.Size.toLong, bytes).xmap(
    bv => Hash.fromByteVectorUnsafe(bv),
    h => h.toByteVector
  )

  /**
   * Refine load statistics for a work result.
   */
  final case class RefineLoad(
    gasUsed: Gas,
    imports: UShort,
    extrinsicCount: UShort,
    extrinsicSize: UInt,
    exports: UShort
  )

  object RefineLoad:
    given Codec[RefineLoad] =
      (compactInteger :: compactInteger :: compactInteger :: compactInteger :: compactInteger).xmap(
        { case (gasUsed, imports, extrinsicCount, extrinsicSize, exports) =>
          RefineLoad(
            Gas(gasUsed),
            UShort(imports.toInt),
            UShort(extrinsicCount.toInt),
            UInt(extrinsicSize.toInt),
            UShort(exports.toInt)
          )
        },
        rl => (rl.gasUsed.toLong, rl.imports.toLong, rl.extrinsicCount.toLong, rl.extrinsicSize.toLong, rl.exports.toLong)
      )

    given Decoder[RefineLoad] = Decoder.instance { cursor =>
      for
        gasUsed <- cursor.get[Long]("gas_used")
        imports <- cursor.get[Int]("imports")
        extrinsicCount <- cursor.get[Int]("extrinsic_count")
        extrinsicSize <- cursor.get[Long]("extrinsic_size")
        exports <- cursor.get[Int]("exports")
      yield RefineLoad(Gas(gasUsed), UShort(imports), UShort(extrinsicCount), UInt(extrinsicSize.toInt), UShort(exports))
    }

  /**
   * Result of executing a work item.
   *
   * Encoding order:
   * - serviceId: 4 bytes
   * - codeHash: 32 bytes
   * - payloadHash: 32 bytes
   * - accumulateGas: 8 bytes
   * - result: ExecutionResult
   * - refineLoad: RefineLoad
   */
  final case class WorkResult(
    serviceId: ServiceId,
    codeHash: Hash,
    payloadHash: Hash,
    accumulateGas: Gas,
    result: ExecutionResult,
    refineLoad: RefineLoad
  )

  object WorkResult:
    given Codec[WorkResult] =
      (uint32L :: hashCodec :: hashCodec :: int64L :: summon[Codec[ExecutionResult]] :: summon[Codec[RefineLoad]]).xmap(
        { case (serviceId, codeHash, payloadHash, gas, result, refineLoad) =>
          WorkResult(
            ServiceId(UInt(serviceId.toInt)),
            codeHash,
            payloadHash,
            Gas(gas),
            result,
            refineLoad
          )
        },
        wr => (wr.serviceId.value.toLong & 0xFFFFFFFFL, wr.codeHash, wr.payloadHash, wr.accumulateGas.toLong, wr.result, wr.refineLoad)
      )

    given Decoder[WorkResult] = Decoder.instance { cursor =>
      for
        serviceId <- cursor.get[Long]("service_id")
        codeHash <- cursor.get[String]("code_hash")
        payloadHash <- cursor.get[String]("payload_hash")
        accumulateGas <- cursor.get[Long]("accumulate_gas")
        result <- cursor.get[ExecutionResult]("result")
        refineLoad <- cursor.get[RefineLoad]("refine_load")
      yield WorkResult(
        ServiceId(UInt(serviceId.toInt)),
        Hash(parseHex(codeHash)),
        Hash(parseHex(payloadHash)),
        Gas(accumulateGas),
        result,
        refineLoad
      )
    }
