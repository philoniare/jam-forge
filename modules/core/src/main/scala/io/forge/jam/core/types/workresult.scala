package io.forge.jam.core.types

import io.forge.jam.core.{JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode, decodeAs}
import io.forge.jam.core.primitives.{Hash, ServiceId, Gas}
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder
import spire.math.{UShort, UInt}

/**
 * Work result related types
 */
object workresult:

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
    given JamEncoder[RefineLoad] with
      def encode(a: RefineLoad): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeCompactInteger(a.gasUsed.toLong)
        builder ++= codec.encodeCompactInteger(a.imports.toLong)
        builder ++= codec.encodeCompactInteger(a.extrinsicCount.toLong)
        builder ++= codec.encodeCompactInteger(a.extrinsicSize.toLong)
        builder ++= codec.encodeCompactInteger(a.exports.toLong)
        builder.result()

    given JamDecoder[RefineLoad] with
      def decode(bytes: JamBytes, offset: Int): (RefineLoad, Int) =
        val arr = bytes.toArray
        var pos = offset

        val (gasUsed, gasUsedBytes) = codec.decodeCompactInteger(arr, pos)
        pos += gasUsedBytes

        val (imports, importsBytes) = codec.decodeCompactInteger(arr, pos)
        pos += importsBytes

        val (extrinsicCount, extrinsicCountBytes) = codec.decodeCompactInteger(arr, pos)
        pos += extrinsicCountBytes

        val (extrinsicSize, extrinsicSizeBytes) = codec.decodeCompactInteger(arr, pos)
        pos += extrinsicSizeBytes

        val (exports, exportsBytes) = codec.decodeCompactInteger(arr, pos)
        pos += exportsBytes

        (RefineLoad(
          Gas(gasUsed),
          UShort(imports.toInt),
          UShort(extrinsicCount.toInt),
          UInt(extrinsicSize.toInt),
          UShort(exports.toInt)
        ), pos - offset)

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
    given JamEncoder[WorkResult] with
      def encode(a: WorkResult): JamBytes =
        val builder = JamBytes.newBuilder
        // serviceId - 4 bytes
        builder ++= codec.encodeU32LE(a.serviceId.value)
        // codeHash - 32 bytes
        builder ++= a.codeHash.bytes
        // payloadHash - 32 bytes
        builder ++= a.payloadHash.bytes
        // accumulateGas - 8 bytes
        builder ++= codec.encodeU64LE(a.accumulateGas.value)
        // result - ExecutionResult (variable)
        builder ++= a.result.encode
        // refineLoad - RefineLoad (variable)
        builder ++= a.refineLoad.encode
        builder.result()

    given JamDecoder[WorkResult] with
      def decode(bytes: JamBytes, offset: Int): (WorkResult, Int) =
        val arr = bytes.toArray
        var pos = offset

        // serviceId - 4 bytes
        val serviceId = ServiceId(codec.decodeU32LE(arr, pos))
        pos += 4

        // codeHash - 32 bytes
        val codeHash = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // payloadHash - 32 bytes
        val payloadHash = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // accumulateGas - 8 bytes
        val accumulateGas = Gas(codec.decodeU64LE(arr, pos))
        pos += 8

        // result - ExecutionResult (variable)
        val (result, resultBytes) = bytes.decodeAs[ExecutionResult](pos)
        pos += resultBytes

        // refineLoad - RefineLoad (variable)
        val (refineLoad, refineLoadBytes) = bytes.decodeAs[RefineLoad](pos)
        pos += refineLoadBytes

        (WorkResult(serviceId, codeHash, payloadHash, accumulateGas, result, refineLoad), pos - offset)

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
