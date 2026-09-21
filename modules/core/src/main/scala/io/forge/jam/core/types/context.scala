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
   * - anchorSlot: 4 bytes (little-endian)      [new in 0.8.0, GP #526]
   * - stateRoot: 32 bytes
   * - beefyRoot: 32 bytes
   * - lookupAnchor: 32 bytes
   * - lookupAnchorSlot: 4 bytes (little-endian)
   * - lookupAnchorStateRoot: 32 bytes          [new in 0.8.0, GP #526]
   * - prerequisites: compact length prefix + list of 32-byte hashes
   */
  final case class Context(
    anchor: Hash,
    anchorSlot: Timeslot,
    stateRoot: Hash,
    beefyRoot: Hash,
    lookupAnchor: Hash,
    lookupAnchorSlot: Timeslot,
    lookupAnchorStateRoot: Hash,
    prerequisites: List[Hash]
  )

  object Context:
    given Codec[Context] =
      (hashCodec ::               // anchor
       uint32L ::                 // anchorSlot (4 bytes LE unsigned)
       hashCodec ::               // stateRoot
       hashCodec ::               // beefyRoot
       hashCodec ::               // lookupAnchor
       uint32L ::                 // lookupAnchorSlot (4 bytes LE unsigned)
       hashCodec ::               // lookupAnchorStateRoot
       listOfN(compactInt, hashCodec)  // prerequisites with compact length prefix
      ).xmap(
        { case (anchor, anchorSlot, stateRoot, beefyRoot, lookupAnchor, slot, lookupAnchorStateRoot, prereqs) =>
          Context(anchor, Timeslot(anchorSlot.toInt), stateRoot, beefyRoot, lookupAnchor,
            Timeslot(slot.toInt), lookupAnchorStateRoot, prereqs)
        },
        c => (
          c.anchor,
          c.anchorSlot.value.toLong & 0xFFFFFFFFL,
          c.stateRoot,
          c.beefyRoot,
          c.lookupAnchor,
          c.lookupAnchorSlot.value.toLong & 0xFFFFFFFFL,
          c.lookupAnchorStateRoot,
          c.prerequisites
        )
      )

    given Decoder[Context] = Decoder.instance { cursor =>
      for
        anchor <- cursor.get[Hash]("anchor")
        anchorSlot <- cursor.get[Long]("anchor_slot")
        stateRoot <- cursor.get[Hash]("state_root")
        beefyRoot <- cursor.get[Hash]("beefy_root")
        lookupAnchor <- cursor.get[Hash]("lookup_anchor")
        lookupAnchorSlot <- cursor.get[Long]("lookup_anchor_slot")
        lookupAnchorStateRoot <- cursor.get[Hash]("lookup_anchor_state_root")
        prerequisites <- cursor.get[List[Hash]]("prerequisites")
      yield Context(
        anchor,
        Timeslot(anchorSlot.toInt),
        stateRoot,
        beefyRoot,
        lookupAnchor,
        Timeslot(lookupAnchorSlot.toInt),
        lookupAnchorStateRoot,
        prerequisites
      )
    }
