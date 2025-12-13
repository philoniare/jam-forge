package io.forge.jam.core.types

import io.forge.jam.core.{JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode, decodeAs}
import io.forge.jam.core.primitives.{Hash, ServiceId, CoreIndex, Gas}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workresult.WorkResult
import io.forge.jam.core.types.work.PackageSpec
import io.circe.Decoder
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

    given Decoder[SegmentRootLookup] = Decoder.instance { cursor =>
      for
        workPackageHash <- cursor.get[Hash]("work_package_hash")
        segmentTreeRoot <- cursor.get[Hash]("segment_tree_root")
      yield SegmentRootLookup(workPackageHash, segmentTreeRoot)
    }

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
    given Decoder[WorkPackage] = Decoder.instance { cursor =>
      for
        authCodeHost <- cursor.get[Long]("auth_code_host")
        authCodeHash <- cursor.get[Hash]("auth_code_hash")
        context <- cursor.get[Context]("context")
        authorization <- cursor.get[JamBytes]("authorization")
        authorizerConfig <- cursor.get[JamBytes]("authorizer_config")
        items <- cursor.get[List[WorkItem]]("items")
      yield WorkPackage(
        ServiceId(authCodeHost.toInt),
        authCodeHash,
        context,
        authorization,
        authorizerConfig,
        items
      )
    }

    given JamEncoder[WorkPackage] with
      def encode(a: WorkPackage): JamBytes =
        val builder = JamBytes.newBuilder
        // authCodeHost - 4 bytes
        builder ++= codec.encodeU32LE(a.authCodeHost.value)
        // authCodeHash - 32 bytes
        builder ++= a.authCodeHash.bytes
        // context - variable size
        builder ++= a.context.encode
        // authorization - compact length prefix + bytes
        builder ++= codec.encodeCompactInteger(a.authorization.length.toLong)
        builder ++= a.authorization
        // authorizerConfig - compact length prefix + bytes
        builder ++= codec.encodeCompactInteger(a.authorizerConfig.length.toLong)
        builder ++= a.authorizerConfig
        // items - compact length prefix + variable-size items
        builder ++= codec.encodeCompactInteger(a.items.length.toLong)
        for item <- a.items do
          builder ++= item.encode
        builder.result()

    given JamDecoder[WorkPackage] with
      def decode(bytes: JamBytes, offset: Int): (WorkPackage, Int) =
        val arr = bytes.toArray
        var pos = offset

        // authCodeHost - 4 bytes
        val authCodeHost = ServiceId(codec.decodeU32LE(arr, pos))
        pos += 4

        // authCodeHash - 32 bytes
        val authCodeHash = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // context - variable size
        val (context, contextBytes) = bytes.decodeAs[Context](pos)
        pos += contextBytes

        // authorization - compact length prefix + bytes
        val (authorizationLength, authorizationLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += authorizationLengthBytes
        val authorization = bytes.slice(pos, pos + authorizationLength.toInt)
        pos += authorizationLength.toInt

        // authorizerConfig - compact length prefix + bytes
        val (authorizerConfigLength, authorizerConfigLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += authorizerConfigLengthBytes
        val authorizerConfig = bytes.slice(pos, pos + authorizerConfigLength.toInt)
        pos += authorizerConfigLength.toInt

        // items - compact length prefix + variable-size items
        val (itemsLength, itemsLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += itemsLengthBytes
        val items = (0 until itemsLength.toInt).map { _ =>
          val (item, itemBytes) = bytes.decodeAs[WorkItem](pos)
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
        builder ++= a.packageSpec.encode
        // context - variable size
        builder ++= a.context.encode
        // coreIndex - compact integer
        builder ++= codec.encodeCompactInteger(a.coreIndex.toInt.toLong)
        // authorizerHash - 32 bytes
        builder ++= a.authorizerHash.bytes
        // authGasUsed - compact integer
        builder ++= codec.encodeCompactInteger(a.authGasUsed.toLong)
        // authOutput - compact length prefix + bytes
        builder ++= codec.encodeCompactInteger(a.authOutput.length.toLong)
        builder ++= a.authOutput
        // segmentRootLookup - compact length prefix + fixed-size items
        builder ++= codec.encodeCompactInteger(a.segmentRootLookup.length.toLong)
        for lookup <- a.segmentRootLookup do
          builder ++= lookup.encode
        // results - compact length prefix + variable-size items
        builder ++= codec.encodeCompactInteger(a.results.length.toLong)
        for result <- a.results do
          builder ++= result.encode
        builder.result()

    given JamDecoder[WorkReport] with
      def decode(bytes: JamBytes, offset: Int): (WorkReport, Int) =
        val arr = bytes.toArray
        var pos = offset

        // packageSpec - 102 bytes
        val (packageSpec, packageSpecBytes) = bytes.decodeAs[PackageSpec](pos)
        pos += packageSpecBytes

        // context - variable size
        val (context, contextBytes) = bytes.decodeAs[Context](pos)
        pos += contextBytes

        // coreIndex - compact integer
        val (coreIndex, coreIndexBytes) = codec.decodeCompactInteger(arr, pos)
        pos += coreIndexBytes

        // authorizerHash - 32 bytes
        val authorizerHash = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // authGasUsed - compact integer
        val (authGasUsed, authGasUsedBytes) = codec.decodeCompactInteger(arr, pos)
        pos += authGasUsedBytes

        // authOutput - compact length prefix + bytes
        val (authOutputLength, authOutputLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += authOutputLengthBytes
        val authOutput = bytes.slice(pos, pos + authOutputLength.toInt)
        pos += authOutputLength.toInt

        // segmentRootLookup - compact length prefix + fixed-size items
        val (segmentRootLookupLength, segmentRootLookupLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += segmentRootLookupLengthBytes
        val segmentRootLookup = (0 until segmentRootLookupLength.toInt).map { _ =>
          val (lookup, consumed) = bytes.decodeAs[SegmentRootLookup](pos)
          pos += consumed
          lookup
        }.toList

        // results - compact length prefix + variable-size items
        val (resultsLength, resultsLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += resultsLengthBytes
        val results = (0 until resultsLength.toInt).map { _ =>
          val (result, resultBytes) = bytes.decodeAs[WorkResult](pos)
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

    given Decoder[WorkReport] = Decoder.instance { cursor =>
      for
        packageSpec <- cursor.get[PackageSpec]("package_spec")
        context <- cursor.get[Context]("context")
        coreIndex <- cursor.get[Int]("core_index")
        authorizerHash <- cursor.get[Hash]("authorizer_hash")
        authGasUsed <- cursor.get[Long]("auth_gas_used")
        authOutput <- cursor.get[JamBytes]("auth_output")
        segmentRootLookup <- cursor.get[List[SegmentRootLookup]]("segment_root_lookup")
        results <- cursor.get[List[WorkResult]]("results")
      yield WorkReport(
        packageSpec,
        context,
        CoreIndex(coreIndex),
        authorizerHash,
        Gas(authGasUsed),
        authOutput,
        segmentRootLookup,
        results
      )
    }

  /**
   * Availability assignment for a core.
   * Contains a work report and a timeout slot.
   */
  final case class AvailabilityAssignment(
    report: WorkReport,
    timeout: Long
  )

  object AvailabilityAssignment:
    given JamEncoder[AvailabilityAssignment] with
      def encode(a: AvailabilityAssignment): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.report.encode
        builder ++= codec.encodeU32LE(UInt(a.timeout.toInt))
        builder.result()

    given JamDecoder[AvailabilityAssignment] with
      def decode(bytes: JamBytes, offset: Int): (AvailabilityAssignment, Int) =
        var pos = offset
        val (report, reportBytes) = bytes.decodeAs[WorkReport](pos)
        pos += reportBytes
        val timeout = codec.decodeU32LE(bytes.toArray, pos).toLong
        pos += 4
        (AvailabilityAssignment(report, timeout), pos - offset)

    given Decoder[AvailabilityAssignment] =
      Decoder.instance { cursor =>
        for
          report <- cursor.get[WorkReport]("report")
          timeout <- cursor.get[Long]("timeout")
        yield AvailabilityAssignment(report, timeout)
      }
