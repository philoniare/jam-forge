package io.forge.jam.core.types

import scodec.*
import scodec.codecs.*
import io.forge.jam.core.primitives.{Hash, Timeslot}
import io.forge.jam.core.scodec.JamCodecs.{hashCodec, compactInt}
import io.circe.Decoder

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

    given Codec[Context] =
      (hashCodec ::               // anchor
       hashCodec ::               // stateRoot
       hashCodec ::               // beefyRoot
       hashCodec ::               // lookupAnchor
       uint32L ::                 // lookupAnchorSlot (4 bytes LE unsigned)
       listOfN(compactInt, hashCodec)  // prerequisites with compact length prefix
      ).xmap(
        { case (anchor, stateRoot, beefyRoot, lookupAnchor, slot, prereqs) =>
          Context(anchor, stateRoot, beefyRoot, lookupAnchor, Timeslot(slot.toInt), prereqs)
        },
        c => (c.anchor, c.stateRoot, c.beefyRoot, c.lookupAnchor, c.lookupAnchorSlot.value.toLong & 0xFFFFFFFFL, c.prerequisites)
      )

    given Decoder[Context] = Decoder.instance { cursor =>
      for
        anchor <- cursor.get[Hash]("anchor")
        stateRoot <- cursor.get[Hash]("state_root")
        beefyRoot <- cursor.get[Hash]("beefy_root")
        lookupAnchor <- cursor.get[Hash]("lookup_anchor")
        lookupAnchorSlot <- cursor.get[Long]("lookup_anchor_slot")
        prerequisites <- cursor.get[List[Hash]]("prerequisites")
      yield Context(
        anchor,
        stateRoot,
        beefyRoot,
        lookupAnchor,
        Timeslot(lookupAnchorSlot.toInt),
        prerequisites
      )
    }
