package io.forge.jam.core.types

import scodec.*
import scodec.bits.*
import scodec.codecs.*
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Hash, ServiceId, CoreIndex, Gas}
import io.forge.jam.core.scodec.JamCodecs.{hashCodec, compactInteger, compactInt, compactPrefixedList}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workresult.WorkResult
import io.forge.jam.core.types.work.PackageSpec
import io.forge.jam.core.json.JsonHelpers.parseHex
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

    given Codec[SegmentRootLookup] =
      (hashCodec :: hashCodec).xmap(
        { case (wpHash, segRoot) => SegmentRootLookup(wpHash, segRoot) },
        srl => (srl.workPackageHash, srl.segmentTreeRoot)
      )

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
    private val jamBytesWithCompactPrefix: Codec[JamBytes] =
      variableSizeBytesLong(compactInteger, bytes).xmap(
        bv => JamBytes.fromByteVector(bv),
        jb => jb.toByteVector
      )

    given Codec[WorkPackage] =
      (uint32L ::                             // authCodeHost - 4 bytes unsigned
       hashCodec ::                           // authCodeHash - 32 bytes
       Codec[Context] ::                      // context - variable size
       jamBytesWithCompactPrefix ::           // authorization - compact length + bytes
       jamBytesWithCompactPrefix ::           // authorizerConfig - compact length + bytes
       compactPrefixedList(Codec[WorkItem])   // items - compact length + variable items
      ).xmap(
        { case (authCodeHost, authCodeHash, context, authorization, authorizerConfig, items) =>
          WorkPackage(
            ServiceId(authCodeHost.toInt),
            authCodeHash,
            context,
            authorization,
            authorizerConfig,
            items
          )
        },
        wp => (wp.authCodeHost.value.toLong & 0xFFFFFFFFL, wp.authCodeHash, wp.context, wp.authorization, wp.authorizerConfig, wp.items)
      )

    given Decoder[WorkPackage] = Decoder.instance { cursor =>
      for
        authCodeHost <- cursor.get[Long]("auth_code_host")
        authCodeHash <- cursor.get[Hash]("auth_code_hash")
        context <- cursor.get[Context]("context")
        authorization <- cursor.get[String]("authorization")
        authorizerConfig <- cursor.get[String]("authorizer_config")
        items <- cursor.get[List[WorkItem]]("items")
      yield WorkPackage(
        ServiceId(authCodeHost.toInt),
        authCodeHash,
        context,
        JamBytes(parseHex(authorization)),
        JamBytes(parseHex(authorizerConfig)),
        items
      )
    }

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
    private val jamBytesWithCompactPrefix: Codec[JamBytes] =
      variableSizeBytesLong(compactInteger, bytes).xmap(
        bv => JamBytes.fromByteVector(bv),
        jb => jb.toByteVector
      )

    given Codec[WorkReport] =
      (Codec[PackageSpec] ::                           // packageSpec - 102 bytes
       Codec[Context] ::                               // context - variable size
       compactInt ::                                   // coreIndex - compact integer
       hashCodec ::                                    // authorizerHash - 32 bytes
       compactInteger ::                               // authGasUsed - compact integer
       jamBytesWithCompactPrefix ::                    // authOutput - compact length + bytes
       compactPrefixedList(Codec[SegmentRootLookup]) :: // segmentRootLookup - compact length + items
       compactPrefixedList(Codec[WorkResult])          // results - compact length + items
      ).xmap(
        { case (packageSpec, context, coreIndex, authorizerHash, authGasUsed, authOutput, segmentRootLookup, results) =>
          WorkReport(
            packageSpec,
            context,
            CoreIndex(coreIndex),
            authorizerHash,
            Gas(authGasUsed),
            authOutput,
            segmentRootLookup,
            results
          )
        },
        wr => {
          // Sort segment root lookup by work package hash (as in original encoder)
          val sortedLookup = wr.segmentRootLookup.sortBy(_.workPackageHash.toHex)
          (wr.packageSpec, wr.context, wr.coreIndex.toInt, wr.authorizerHash, wr.authGasUsed.toLong, wr.authOutput, sortedLookup, wr.results)
        }
      )

    given Decoder[WorkReport] = Decoder.instance { cursor =>
      for
        packageSpec <- cursor.get[PackageSpec]("package_spec")
        context <- cursor.get[Context]("context")
        coreIndex <- cursor.get[Int]("core_index")
        authorizerHash <- cursor.get[Hash]("authorizer_hash")
        authGasUsed <- cursor.get[Long]("auth_gas_used")
        authOutput <- cursor.get[String]("auth_output")
        segmentRootLookup <- cursor.get[List[SegmentRootLookup]]("segment_root_lookup")
        results <- cursor.get[List[WorkResult]]("results")
      yield WorkReport(
        packageSpec,
        context,
        CoreIndex(coreIndex),
        authorizerHash,
        Gas(authGasUsed),
        JamBytes(parseHex(authOutput)),
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
    given Codec[AvailabilityAssignment] =
      (Codec[WorkReport] :: uint32L).xmap(
        { case (report, timeout) =>
          AvailabilityAssignment(report, timeout & 0xFFFFFFFFL)
        },
        aa => (aa.report, aa.timeout & 0xFFFFFFFFL)
      )

    given Decoder[AvailabilityAssignment] =
      Decoder.instance { cursor =>
        for
          report <- cursor.get[WorkReport]("report")
          timeout <- cursor.get[Long]("timeout")
        yield AvailabilityAssignment(report, timeout)
      }
