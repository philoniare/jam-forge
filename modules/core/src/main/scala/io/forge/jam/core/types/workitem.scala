package io.forge.jam.core.types

import io.circe.Decoder
import scodec.*
import scodec.bits.*
import scodec.codecs.*
import spire.math.{UShort, UInt}
import io.forge.jam.core.{JamBytes, primitives}
import io.forge.jam.core.primitives.{Hash, ServiceId, Gas}
import io.forge.jam.core.scodec.JamCodecs.{hashCodec, compactInteger, compactInt}

/**
 * Work item related types.
 */
object workitem:

  /**
   * Import segment reference for a work item.
   * Fixed size: 34 bytes (32-byte tree root + 2-byte index)
   */
  final case class WorkItemImportSegment(
    treeRoot: Hash,
    index: UShort
  )

  object WorkItemImportSegment:
    val Size: Int = Hash.Size + 2 // 34 bytes

    given Codec[WorkItemImportSegment] =
      (hashCodec :: uint16L).xmap(
        { case (root, index) => WorkItemImportSegment(root, UShort(index)) },
        s => (s.treeRoot, s.index.toInt)
      )

    given Decoder[WorkItemImportSegment] = Decoder.instance { cursor =>
      for
        treeRoot <- cursor.get[Hash]("tree_root")
        index <- cursor.get[Int]("index")
      yield WorkItemImportSegment(treeRoot, UShort(index))
    }

  /**
   * Extrinsic reference for a work item.
   * Fixed size: 36 bytes (32-byte hash + 4-byte length)
   */
  final case class WorkItemExtrinsic(
    hash: Hash,
    len: UInt
  )

  object WorkItemExtrinsic:
    val Size: Int = Hash.Size + 4 // 36 bytes

    given Codec[WorkItemExtrinsic] =
      (hashCodec :: uint32L).xmap(
        { case (hash, len) => WorkItemExtrinsic(hash, UInt(len.toInt)) },
        e => (e.hash, e.len.toLong & 0xFFFFFFFFL)
      )

    given Decoder[WorkItemExtrinsic] = Decoder.instance { cursor =>
      for
        hash <- cursor.get[Hash]("hash")
        len <- cursor.get[Long]("len")
      yield WorkItemExtrinsic(hash, UInt(len.toInt))
    }

  /**
   * A work item in a work package.
   *
   * Encoding order:
   * - service: 4 bytes
   * - codeHash: 32 bytes
   * - refineGasLimit: 8 bytes
   * - accumulateGasLimit: 8 bytes
   * - exportCount: 2 bytes
   * - payload: compact length prefix + bytes
   * - importSegments: compact length prefix + fixed-size items
   * - extrinsic: compact length prefix + fixed-size items
   */
  final case class WorkItem(
    service: ServiceId,
    codeHash: Hash,
    payload: JamBytes,
    refineGasLimit: Gas,
    accumulateGasLimit: Gas,
    importSegments: List[WorkItemImportSegment],
    extrinsic: List[WorkItemExtrinsic],
    exportCount: UShort
  )

  object WorkItem:
    private val jamBytesWithCompactLength: Codec[JamBytes] =
      variableSizeBytes(compactInt, bytes).xmap(
        bv => JamBytes.fromByteVector(bv),
        jb => jb.toByteVector
      )

    given Codec[WorkItem] =
      (uint32L ::                                             // service - 4 bytes
       hashCodec ::                                           // codeHash - 32 bytes
       int64L ::                                              // refineGasLimit - 8 bytes (signed for Gas)
       int64L ::                                              // accumulateGasLimit - 8 bytes (signed for Gas)
       uint16L ::                                             // exportCount - 2 bytes
       jamBytesWithCompactLength ::                           // payload with compact length
       listOfN(compactInt, Codec[WorkItemImportSegment]) ::   // importSegments
       listOfN(compactInt, Codec[WorkItemExtrinsic])          // extrinsic
      ).xmap(
        { case (sid, code, refineGas, accGas, exports, payload, imports, extrinsics) =>
          WorkItem(ServiceId(UInt(sid.toInt)), code, payload, Gas(refineGas), Gas(accGas), imports, extrinsics, UShort(exports))
        },
        wi => (wi.service.value.toLong & 0xFFFFFFFFL, wi.codeHash, wi.refineGasLimit.toLong, wi.accumulateGasLimit.toLong, wi.exportCount.toInt, wi.payload, wi.importSegments, wi.extrinsic)
      )

    given Decoder[WorkItem] = Decoder.instance { cursor =>
      for
        service <- cursor.get[Long]("service")
        codeHash <- cursor.get[Hash]("code_hash")
        payload <- cursor.get[JamBytes]("payload")
        refineGasLimit <- cursor.get[Long]("refine_gas_limit")
        accumulateGasLimit <- cursor.get[Long]("accumulate_gas_limit")
        importSegments <- cursor.get[List[WorkItemImportSegment]]("import_segments")
        extrinsic <- cursor.get[List[WorkItemExtrinsic]]("extrinsic")
        exportCount <- cursor.get[Int]("export_count")
      yield WorkItem(
        ServiceId(service.toInt),
        codeHash,
        payload,
        Gas(refineGasLimit),
        Gas(accumulateGasLimit),
        importSegments,
        extrinsic,
        UShort(exportCount)
      )
    }
