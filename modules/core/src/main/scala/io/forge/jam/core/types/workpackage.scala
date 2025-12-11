package io.forge.jam.core.types

import io.forge.jam.core.{JamBytes, codec, encoding}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder}
import io.forge.jam.core.primitives.{Hash, ServiceId, CoreIndex, Gas}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workresult.WorkResult
import io.forge.jam.core.types.work.PackageSpec
import spire.math.UInt

/**
 * Work package and work report related types
 */
object workpackage:

  /**
   * Segment root lookup entry.
   * Fixed size: 64 bytes (32-byte work package hash + 32-byte segment tree root)
   */
  final case class SegmentRootLookup(
    workPackageHash: Hash,
    segmentTreeRoot: Hash
  )

  object SegmentRootLookup:
    val Size: Int = Hash.Size * 2 // 64 bytes

    given JamEncoder[SegmentRootLookup] with
      def encode(a: SegmentRootLookup): JamBytes =
        JamBytes(a.workPackageHash.bytes ++ a.segmentTreeRoot.bytes)

    given JamDecoder[SegmentRootLookup] with
      def decode(bytes: JamBytes, offset: Int): (SegmentRootLookup, Int) =
        val arr = bytes.toArray
        val workPackageHash = Hash(arr.slice(offset, offset + Hash.Size))
        val segmentTreeRoot = Hash(arr.slice(offset + Hash.Size, offset + Size))
        (SegmentRootLookup(workPackageHash, segmentTreeRoot), Size)

  /**
   * A work package containing authorization and work items.
   *
   * Encoding order:
   * - authCodeHost: 4 bytes
   * - authCodeHash: 32 bytes
   * - context: variable size
   * - authorization: compact length prefix + bytes
   * - authorizerConfig: compact length prefix + bytes
   * - items: compact length prefix + variable-size items
   */
  final case class WorkPackage(
    authCodeHost: ServiceId,
    authCodeHash: Hash,
    context: Context,
    authorization: JamBytes,
    authorizerConfig: JamBytes,
    items: List[WorkItem]
  )

  object WorkPackage:
    given JamEncoder[WorkPackage] with
      def encode(a: WorkPackage): JamBytes =
        val builder = JamBytes.newBuilder
        // authCodeHost - 4 bytes
        builder ++= encoding.encodeU32LE(a.authCodeHost.value)
        // authCodeHash - 32 bytes
        builder ++= a.authCodeHash.bytes
        // context - variable size
        builder ++= Context.given_JamEncoder_Context.encode(a.context)
        // authorization - compact length prefix + bytes
        builder ++= encoding.encodeCompactInteger(a.authorization.length.toLong)
        builder ++= a.authorization
        // authorizerConfig - compact length prefix + bytes
        builder ++= encoding.encodeCompactInteger(a.authorizerConfig.length.toLong)
        builder ++= a.authorizerConfig
        // items - compact length prefix + variable-size items
        builder ++= encoding.encodeCompactInteger(a.items.length.toLong)
        for item <- a.items do
          builder ++= WorkItem.given_JamEncoder_WorkItem.encode(item)
        builder.result()

    given JamDecoder[WorkPackage] with
      def decode(bytes: JamBytes, offset: Int): (WorkPackage, Int) =
        val arr = bytes.toArray
        var pos = offset

        // authCodeHost - 4 bytes
        val authCodeHost = ServiceId(encoding.decodeU32LE(arr, pos))
        pos += 4

        // authCodeHash - 32 bytes
        val authCodeHash = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // context - variable size
        val (context, contextBytes) = Context.given_JamDecoder_Context.decode(bytes, pos)
        pos += contextBytes

        // authorization - compact length prefix + bytes
        val (authorizationLength, authorizationLengthBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += authorizationLengthBytes
        val authorization = bytes.slice(pos, pos + authorizationLength.toInt)
        pos += authorizationLength.toInt

        // authorizerConfig - compact length prefix + bytes
        val (authorizerConfigLength, authorizerConfigLengthBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += authorizerConfigLengthBytes
        val authorizerConfig = bytes.slice(pos, pos + authorizerConfigLength.toInt)
        pos += authorizerConfigLength.toInt

        // items - compact length prefix + variable-size items
        val (itemsLength, itemsLengthBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += itemsLengthBytes
        val items = (0 until itemsLength.toInt).map { _ =>
          val (item, itemBytes) = WorkItem.given_JamDecoder_WorkItem.decode(bytes, pos)
          pos += itemBytes
          item
        }.toList

        (WorkPackage(authCodeHost, authCodeHash, context, authorization, authorizerConfig, items), pos - offset)

  /**
   * A work report containing results of executing a work package.
   *
   * Encoding order:
   * - packageSpec: 102 bytes (fixed)
   * - context: variable size
   * - coreIndex: compact integer
   * - authorizerHash: 32 bytes
   * - authGasUsed: compact integer
   * - authOutput: compact length prefix + bytes
   * - segmentRootLookup: compact length prefix + fixed-size items
   * - results: compact length prefix + variable-size items
   */
  final case class WorkReport(
    packageSpec: PackageSpec,
    context: Context,
    coreIndex: CoreIndex,
    authorizerHash: Hash,
    authGasUsed: Gas,
    authOutput: JamBytes,
    segmentRootLookup: List[SegmentRootLookup],
    results: List[WorkResult]
  )

  object WorkReport:
    given JamEncoder[WorkReport] with
      def encode(a: WorkReport): JamBytes =
        val builder = JamBytes.newBuilder
        // packageSpec - 102 bytes
        builder ++= PackageSpec.given_JamEncoder_PackageSpec.encode(a.packageSpec)
        // context - variable size
        builder ++= Context.given_JamEncoder_Context.encode(a.context)
        // coreIndex - compact integer
        builder ++= encoding.encodeCompactInteger(a.coreIndex.toInt.toLong)
        // authorizerHash - 32 bytes
        builder ++= a.authorizerHash.bytes
        // authGasUsed - compact integer
        builder ++= encoding.encodeCompactInteger(a.authGasUsed.toLong)
        // authOutput - compact length prefix + bytes
        builder ++= encoding.encodeCompactInteger(a.authOutput.length.toLong)
        builder ++= a.authOutput
        // segmentRootLookup - compact length prefix + fixed-size items
        builder ++= encoding.encodeCompactInteger(a.segmentRootLookup.length.toLong)
        for lookup <- a.segmentRootLookup do
          builder ++= SegmentRootLookup.given_JamEncoder_SegmentRootLookup.encode(lookup)
        // results - compact length prefix + variable-size items
        builder ++= encoding.encodeCompactInteger(a.results.length.toLong)
        for result <- a.results do
          builder ++= WorkResult.given_JamEncoder_WorkResult.encode(result)
        builder.result()

    given JamDecoder[WorkReport] with
      def decode(bytes: JamBytes, offset: Int): (WorkReport, Int) =
        val arr = bytes.toArray
        var pos = offset

        // packageSpec - 102 bytes
        val (packageSpec, packageSpecBytes) = PackageSpec.given_JamDecoder_PackageSpec.decode(bytes, pos)
        pos += packageSpecBytes

        // context - variable size
        val (context, contextBytes) = Context.given_JamDecoder_Context.decode(bytes, pos)
        pos += contextBytes

        // coreIndex - compact integer
        val (coreIndex, coreIndexBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += coreIndexBytes

        // authorizerHash - 32 bytes
        val authorizerHash = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // authGasUsed - compact integer
        val (authGasUsed, authGasUsedBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += authGasUsedBytes

        // authOutput - compact length prefix + bytes
        val (authOutputLength, authOutputLengthBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += authOutputLengthBytes
        val authOutput = bytes.slice(pos, pos + authOutputLength.toInt)
        pos += authOutputLength.toInt

        // segmentRootLookup - compact length prefix + fixed-size items
        val (segmentRootLookupLength, segmentRootLookupLengthBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += segmentRootLookupLengthBytes
        val segmentRootLookup = (0 until segmentRootLookupLength.toInt).map { _ =>
          val (lookup, consumed) = SegmentRootLookup.given_JamDecoder_SegmentRootLookup.decode(bytes, pos)
          pos += consumed
          lookup
        }.toList

        // results - compact length prefix + variable-size items
        val (resultsLength, resultsLengthBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += resultsLengthBytes
        val results = (0 until resultsLength.toInt).map { _ =>
          val (result, resultBytes) = WorkResult.given_JamDecoder_WorkResult.decode(bytes, pos)
          pos += resultBytes
          result
        }.toList

        (WorkReport(
          packageSpec,
          context,
          CoreIndex(coreIndex.toInt),
          authorizerHash,
          Gas(authGasUsed),
          authOutput,
          segmentRootLookup,
          results
        ), pos - offset)
