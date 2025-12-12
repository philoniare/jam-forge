package io.forge.jam.core.types

import io.forge.jam.core.{JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode, decodeAs}
import io.forge.jam.core.primitives.{Hash, ServiceId, Gas}
import spire.math.{UShort, UInt}

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

    given JamEncoder[WorkItemImportSegment] with
      def encode(a: WorkItemImportSegment): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.treeRoot.bytes
        builder ++= codec.encodeU16LE(a.index)
        builder.result()

    given JamDecoder[WorkItemImportSegment] with
      def decode(bytes: JamBytes, offset: Int): (WorkItemImportSegment, Int) =
        val arr = bytes.toArray
        val treeRoot = Hash(arr.slice(offset, offset + Hash.Size))
        val index = codec.decodeU16LE(arr, offset + Hash.Size)
        (WorkItemImportSegment(treeRoot, index), Size)

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

    given JamEncoder[WorkItemExtrinsic] with
      def encode(a: WorkItemExtrinsic): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.hash.bytes
        builder ++= codec.encodeU32LE(a.len)
        builder.result()

    given JamDecoder[WorkItemExtrinsic] with
      def decode(bytes: JamBytes, offset: Int): (WorkItemExtrinsic, Int) =
        val arr = bytes.toArray
        val hash = Hash(arr.slice(offset, offset + Hash.Size))
        val len = codec.decodeU32LE(arr, offset + Hash.Size)
        (WorkItemExtrinsic(hash, len), Size)

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
    given JamEncoder[WorkItem] with
      def encode(a: WorkItem): JamBytes =
        val builder = JamBytes.newBuilder
        // service - 4 bytes
        builder ++= codec.encodeU32LE(a.service.value)
        // codeHash - 32 bytes
        builder ++= a.codeHash.bytes
        // refineGasLimit - 8 bytes
        builder ++= codec.encodeU64LE(a.refineGasLimit.value)
        // accumulateGasLimit - 8 bytes
        builder ++= codec.encodeU64LE(a.accumulateGasLimit.value)
        // exportCount - 2 bytes
        builder ++= codec.encodeU16LE(a.exportCount)
        // payload - compact length prefix + bytes
        builder ++= codec.encodeCompactInteger(a.payload.length.toLong)
        builder ++= a.payload
        // importSegments - compact length prefix + fixed-size items
        builder ++= codec.encodeCompactInteger(a.importSegments.length.toLong)
        for segment <- a.importSegments do
          builder ++= segment.encode
        // extrinsic - compact length prefix + fixed-size items
        builder ++= codec.encodeCompactInteger(a.extrinsic.length.toLong)
        for ext <- a.extrinsic do
          builder ++= ext.encode
        builder.result()

    given JamDecoder[WorkItem] with
      def decode(bytes: JamBytes, offset: Int): (WorkItem, Int) =
        val arr = bytes.toArray
        var pos = offset

        // service - 4 bytes
        val service = ServiceId(codec.decodeU32LE(arr, pos))
        pos += 4

        // codeHash - 32 bytes
        val codeHash = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // refineGasLimit - 8 bytes
        val refineGasLimit = Gas(codec.decodeU64LE(arr, pos))
        pos += 8

        // accumulateGasLimit - 8 bytes
        val accumulateGasLimit = Gas(codec.decodeU64LE(arr, pos))
        pos += 8

        // exportCount - 2 bytes
        val exportCount = codec.decodeU16LE(arr, pos)
        pos += 2

        // payload - compact length prefix + bytes
        val (payloadLength, payloadLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += payloadLengthBytes
        val payload = bytes.slice(pos, pos + payloadLength.toInt)
        pos += payloadLength.toInt

        // importSegments - compact length prefix + fixed-size items
        val (segmentsLength, segmentsLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += segmentsLengthBytes
        val importSegments = (0 until segmentsLength.toInt).map { _ =>
          val (segment, consumed) = bytes.decodeAs[WorkItemImportSegment](pos)
          pos += consumed
          segment
        }.toList

        // extrinsic - compact length prefix + fixed-size items
        val (extrinsicLength, extrinsicLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += extrinsicLengthBytes
        val extrinsic = (0 until extrinsicLength.toInt).map { _ =>
          val (ext, consumed) = bytes.decodeAs[WorkItemExtrinsic](pos)
          pos += consumed
          ext
        }.toList

        (WorkItem(service, codeHash, payload, refineGasLimit, accumulateGasLimit, importSegments, extrinsic, exportCount), pos - offset)
