package io.forge.jam.core.types

import io.forge.jam.core.{JamBytes, codec, encoding}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder}
import io.forge.jam.core.primitives.{Hash, ServiceId, Gas}
import io.forge.jam.core.types.work.ExecutionResult
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
        builder ++= encoding.encodeCompactInteger(a.gasUsed.toLong)
        builder ++= encoding.encodeCompactInteger(a.imports.toLong)
        builder ++= encoding.encodeCompactInteger(a.extrinsicCount.toLong)
        builder ++= encoding.encodeCompactInteger(a.extrinsicSize.toLong)
        builder ++= encoding.encodeCompactInteger(a.exports.toLong)
        builder.result()

    given JamDecoder[RefineLoad] with
      def decode(bytes: JamBytes, offset: Int): (RefineLoad, Int) =
        val arr = bytes.toArray
        var pos = offset

        val (gasUsed, gasUsedBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += gasUsedBytes

        val (imports, importsBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += importsBytes

        val (extrinsicCount, extrinsicCountBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += extrinsicCountBytes

        val (extrinsicSize, extrinsicSizeBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += extrinsicSizeBytes

        val (exports, exportsBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += exportsBytes

        (RefineLoad(
          Gas(gasUsed),
          UShort(imports.toInt),
          UShort(extrinsicCount.toInt),
          UInt(extrinsicSize.toInt),
          UShort(exports.toInt)
        ), pos - offset)

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
        builder ++= encoding.encodeU32LE(a.serviceId.value)
        // codeHash - 32 bytes
        builder ++= a.codeHash.bytes
        // payloadHash - 32 bytes
        builder ++= a.payloadHash.bytes
        // accumulateGas - 8 bytes
        builder ++= encoding.encodeU64LE(a.accumulateGas.value)
        // result - ExecutionResult (variable)
        builder ++= ExecutionResult.given_JamEncoder_ExecutionResult.encode(a.result)
        // refineLoad - RefineLoad (variable)
        builder ++= RefineLoad.given_JamEncoder_RefineLoad.encode(a.refineLoad)
        builder.result()

    given JamDecoder[WorkResult] with
      def decode(bytes: JamBytes, offset: Int): (WorkResult, Int) =
        val arr = bytes.toArray
        var pos = offset

        // serviceId - 4 bytes
        val serviceId = ServiceId(encoding.decodeU32LE(arr, pos))
        pos += 4

        // codeHash - 32 bytes
        val codeHash = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // payloadHash - 32 bytes
        val payloadHash = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // accumulateGas - 8 bytes
        val accumulateGas = Gas(encoding.decodeU64LE(arr, pos))
        pos += 8

        // result - ExecutionResult (variable)
        val (result, resultBytes) = ExecutionResult.given_JamDecoder_ExecutionResult.decode(bytes, pos)
        pos += resultBytes

        // refineLoad - RefineLoad (variable)
        val (refineLoad, refineLoadBytes) = RefineLoad.given_JamDecoder_RefineLoad.decode(bytes, pos)
        pos += refineLoadBytes

        (WorkResult(serviceId, codeHash, payloadHash, accumulateGas, result, refineLoad), pos - offset)
