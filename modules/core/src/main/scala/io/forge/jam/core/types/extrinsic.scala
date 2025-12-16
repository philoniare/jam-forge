package io.forge.jam.core.types

import _root_.scodec.*
import _root_.scodec.bits.*
import _root_.scodec.codecs.*
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Hash, ServiceId, ValidatorIndex, Timeslot, Ed25519Signature}
import io.forge.jam.core.scodec.JamCodecs.{hashCodec, compactInteger, compactInt, compactPrefixedList}
import io.forge.jam.core.types.work.Vote
import io.forge.jam.core.types.dispute.{Culprit, Fault, GuaranteeSignature}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder
import spire.math.UInt

/**
 * Extrinsic sub-types (preimage, assurance, verdict, dispute, guarantee).
 */
object extrinsic:

  // ============================================================================
  // Private Codec Helpers
  // ============================================================================

  private val ed25519SigCodec: Codec[Ed25519Signature] =
    fixedSizeBytes(Ed25519Signature.Size.toLong, bytes).xmap(
      bv => Ed25519Signature(bv.toArray),
      sig => ByteVector(sig.bytes)
    )

  private val serviceIdCodec: Codec[ServiceId] =
    uint32L.xmap(
      i => ServiceId(UInt(i.toInt)),
      sid => sid.value.toLong & 0xFFFFFFFFL
    )

  private val timeslotCodec: Codec[Timeslot] =
    uint32L.xmap(
      i => Timeslot(UInt(i.toInt)),
      ts => ts.value.toLong & 0xFFFFFFFFL
    )

  private val validatorIndexCodec: Codec[ValidatorIndex] =
    uint16L.xmap(
      i => ValidatorIndex(i),
      vi => vi.value.toInt
    )

  // ============================================================================
  // Preimage
  // ============================================================================

  /**
   * A preimage request.
   * Variable size: 4-byte requester + compact length prefix + blob bytes
   */
  final case class Preimage(
    requester: ServiceId,
    blob: JamBytes
  )

  object Preimage:
    private val jamBytesWithCompactPrefix: Codec[JamBytes] =
      variableSizeBytes(compactInt, bytes).xmap(
        bv => JamBytes.fromByteVector(bv),
        jb => jb.toByteVector
      )

    given Codec[Preimage] =
      (serviceIdCodec :: jamBytesWithCompactPrefix).xmap(
        { case (requester, blob) => Preimage(requester, blob) },
        p => (p.requester, p.blob)
      )

    given Decoder[Preimage] = Decoder.instance { cursor =>
      for
        requester <- cursor.get[Long]("requester")
        blob <- cursor.get[String]("blob")
      yield Preimage(ServiceId(UInt(requester.toInt)), JamBytes(parseHex(blob)))
    }

  // ============================================================================
  // AssuranceExtrinsic
  // ============================================================================

  /**
   * An assurance extrinsic from a validator.
   * Size depends on coresCount: 32 + ceil(coresCount/8) + 2 + 64 bytes
   */
  final case class AssuranceExtrinsic(
    anchor: Hash,
    bitfield: JamBytes,
    validatorIndex: ValidatorIndex,
    signature: Ed25519Signature
  )

  object AssuranceExtrinsic:
    val SignatureSize: Int = 64 // Ed25519

    /** Calculate size based on cores count */
    def size(coresCount: Int): Int =
      val bitfieldSize = (coresCount + 7) / 8
      Hash.Size + bitfieldSize + 2 + SignatureSize

    /** Create a codec that knows the cores count */
    def codec(coresCount: Int): Codec[AssuranceExtrinsic] =
      val bitfieldSize = (coresCount + 7) / 8
      (hashCodec :: fixedSizeBytes(bitfieldSize.toLong, bytes) :: validatorIndexCodec :: ed25519SigCodec).xmap(
        { case (anchor, bitfield, idx, sig) =>
          AssuranceExtrinsic(anchor, JamBytes.fromByteVector(bitfield), idx, sig)
        },
        ae => (ae.anchor, ae.bitfield.toByteVector, ae.validatorIndex, ae.signature)
      )

    given Decoder[AssuranceExtrinsic] = Decoder.instance { cursor =>
      for
        anchor <- cursor.get[String]("anchor")
        bitfield <- cursor.get[String]("bitfield")
        validatorIndex <- cursor.get[Int]("validator_index")
        signature <- cursor.get[String]("signature")
      yield AssuranceExtrinsic(
        Hash(parseHex(anchor)),
        JamBytes(parseHex(bitfield)),
        ValidatorIndex(validatorIndex),
        Ed25519Signature(parseHex(signature))
      )
    }

  // ============================================================================
  // Verdict
  // ============================================================================

  /**
   * A verdict in a dispute.
   * Size depends on votes count: 32 + 4 + votesPerVerdict * 67 bytes
   */
  final case class Verdict(
    target: Hash,
    age: Timeslot,
    votes: List[Vote]
  )

  object Verdict:
    /** Calculate size based on votes count */
    def size(votesPerVerdict: Int): Int =
      Hash.Size + 4 + votesPerVerdict * Vote.Size

    /** Create a codec that knows the votes per verdict */
    def codec(votesPerVerdict: Int): Codec[Verdict] =
      import io.forge.jam.core.scodec.JamCodecs.fixedSizeList
      (hashCodec :: timeslotCodec :: fixedSizeList(summon[Codec[Vote]], votesPerVerdict)).xmap(
        { case (target, age, votes) =>
          Verdict(target, age, votes)
        },
        v => (v.target, v.age, v.votes)
      )

    given Decoder[Verdict] = Decoder.instance { cursor =>
      for
        target <- cursor.get[String]("target")
        age <- cursor.get[Long]("age")
        votes <- cursor.get[List[Vote]]("votes")
      yield Verdict(Hash(parseHex(target)), Timeslot(age.toInt), votes)
    }

  // ============================================================================
  // Dispute
  // ============================================================================

  /**
   * A dispute containing verdicts, culprits, and faults.
   */
  final case class Dispute(
    verdicts: List[Verdict],
    culprits: List[Culprit],
    faults: List[Fault]
  )

  object Dispute:
    /** Create a codec that knows the votes per verdict */
    def codec(votesPerVerdict: Int): Codec[Dispute] =
      val verdictCodec = Verdict.codec(votesPerVerdict)
      (compactPrefixedList(verdictCodec) ::
       compactPrefixedList(summon[Codec[Culprit]]) ::
       compactPrefixedList(summon[Codec[Fault]])).xmap(
        { case (verdicts, culprits, faults) =>
          Dispute(verdicts, culprits, faults)
        },
        d => (d.verdicts, d.culprits, d.faults)
      )

    given Decoder[Dispute] = Decoder.instance { cursor =>
      for
        verdicts <- cursor.get[List[Verdict]]("verdicts")
        culprits <- cursor.get[List[Culprit]]("culprits")
        faults <- cursor.get[List[Fault]]("faults")
      yield Dispute(verdicts, culprits, faults)
    }

  // ============================================================================
  // GuaranteeExtrinsic
  // ============================================================================

  /**
   * A guarantee extrinsic containing a work report and signatures.
   */
  final case class GuaranteeExtrinsic(
    report: WorkReport,
    slot: Timeslot,
    signatures: List[GuaranteeSignature]
  )

  object GuaranteeExtrinsic:
    given Codec[GuaranteeExtrinsic] =
      (summon[Codec[WorkReport]] :: timeslotCodec :: compactPrefixedList(summon[Codec[GuaranteeSignature]])).xmap(
        { case (report, slot, signatures) =>
          GuaranteeExtrinsic(report, slot, signatures)
        },
        ge => (ge.report, ge.slot, ge.signatures)
      )

    given Decoder[GuaranteeExtrinsic] = Decoder.instance { cursor =>
      for
        report <- cursor.get[WorkReport]("report")
        slot <- cursor.get[Long]("slot")
        signatures <- cursor.get[List[GuaranteeSignature]]("signatures")
      yield GuaranteeExtrinsic(report, Timeslot(slot.toInt), signatures)
    }
