package io.forge.jam.core.types

import io.forge.jam.core.{JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder}
import io.forge.jam.core.primitives.{Hash, Timeslot}
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder
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
        builder ++= codec.encodeU32LE(a.lookupAnchorSlot.value)
        // prerequisites - compact length prefix followed by hashes
        builder ++= codec.encodeCompactInteger(a.prerequisites.length.toLong)
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

        val lookupAnchorSlot = Timeslot(codec.decodeU32LE(arr, pos))
        pos += 4

        // prerequisites - compact length prefix followed by hashes
        val (prereqsLength, prereqsLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += prereqsLengthBytes

        val prerequisites = (0 until prereqsLength.toInt).map { _ =>
          val hash = Hash(arr.slice(pos, pos + Hash.Size))
          pos += Hash.Size
          hash
        }.toList

        (Context(anchor, stateRoot, beefyRoot, lookupAnchor, lookupAnchorSlot, prerequisites), pos - offset)

    given Decoder[Context] = Decoder.instance { cursor =>
      for
        anchor <- cursor.get[String]("anchor")
        stateRoot <- cursor.get[String]("state_root")
        beefyRoot <- cursor.get[String]("beefy_root")
        lookupAnchor <- cursor.get[String]("lookup_anchor")
        lookupAnchorSlot <- cursor.get[Long]("lookup_anchor_slot")
        prerequisites <- cursor.get[List[String]]("prerequisites")
      yield Context(
        Hash(parseHex(anchor)),
        Hash(parseHex(stateRoot)),
        Hash(parseHex(beefyRoot)),
        Hash(parseHex(lookupAnchor)),
        Timeslot(lookupAnchorSlot.toInt),
        prerequisites.map(h => Hash(parseHex(h)))
      )
    }
