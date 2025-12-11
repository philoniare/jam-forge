package io.forge.jam.core.types

import io.forge.jam.core.{JamBytes, codec, encoding}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder}
import io.forge.jam.core.primitives.{Hash, Timeslot}
import spire.math.UInt

/**
 * Refinement context types.
 */
object context:

  /**
   * Refinement context containing anchors, state roots, and prerequisites.
   *
   * Encoding order:
   * - anchor: 32 bytes
   * - stateRoot: 32 bytes
   * - beefyRoot: 32 bytes
   * - lookupAnchor: 32 bytes
   * - lookupAnchorSlot: 4 bytes (little-endian)
   * - prerequisites: compact length prefix + list of 32-byte hashes
   */
  final case class Context(
    anchor: Hash,
    stateRoot: Hash,
    beefyRoot: Hash,
    lookupAnchor: Hash,
    lookupAnchorSlot: Timeslot,
    prerequisites: List[Hash]
  )

  object Context:
    /** Fixed part size (without prerequisites) */
    val FixedSize: Int = Hash.Size * 4 + 4 // 132 bytes

    given JamEncoder[Context] with
      def encode(a: Context): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.anchor.bytes
        builder ++= a.stateRoot.bytes
        builder ++= a.beefyRoot.bytes
        builder ++= a.lookupAnchor.bytes
        builder ++= encoding.encodeU32LE(a.lookupAnchorSlot.value)
        // prerequisites - compact length prefix followed by hashes
        builder ++= encoding.encodeCompactInteger(a.prerequisites.length.toLong)
        for prereq <- a.prerequisites do
          builder ++= prereq.bytes
        builder.result()

    given JamDecoder[Context] with
      def decode(bytes: JamBytes, offset: Int): (Context, Int) =
        val arr = bytes.toArray
        var pos = offset

        val anchor = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        val stateRoot = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        val beefyRoot = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        val lookupAnchor = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        val lookupAnchorSlot = Timeslot(encoding.decodeU32LE(arr, pos))
        pos += 4

        // prerequisites - compact length prefix followed by hashes
        val (prereqsLength, prereqsLengthBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += prereqsLengthBytes

        val prerequisites = (0 until prereqsLength.toInt).map { _ =>
          val hash = Hash(arr.slice(pos, pos + Hash.Size))
          pos += Hash.Size
          hash
        }.toList

        (Context(anchor, stateRoot, beefyRoot, lookupAnchor, lookupAnchorSlot, prerequisites), pos - offset)
