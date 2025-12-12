package io.forge.jam.core.types

import io.forge.jam.core.{ChainConfig, JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode}
import io.forge.jam.core.primitives.{Hash, ServiceId, ValidatorIndex, Timeslot, Ed25519Signature, Ed25519PublicKey}
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

  /**
   * A preimage request.
   * Variable size: 4-byte requester + compact length prefix + blob bytes
   */
  final case class Preimage(
    requester: ServiceId,
    blob: JamBytes
  )

  object Preimage:
    given JamEncoder[Preimage] with
      def encode(a: Preimage): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeU32LE(a.requester.value)
        builder ++= codec.encodeCompactInteger(a.blob.length.toLong)
        builder ++= a.blob
        builder.result()

    given JamDecoder[Preimage] with
      def decode(bytes: JamBytes, offset: Int): (Preimage, Int) =
        val arr = bytes.toArray
        var pos = offset

        val requester = ServiceId(codec.decodeU32LE(arr, pos))
        pos += 4

        val (blobLength, blobLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += blobLengthBytes
        val blob = bytes.slice(pos, pos + blobLength.toInt)
        pos += blobLength.toInt

        (Preimage(requester, blob), pos - offset)

    given Decoder[Preimage] = Decoder.instance { cursor =>
      for
        requester <- cursor.get[Long]("requester")
        blob <- cursor.get[String]("blob")
      yield Preimage(ServiceId(UInt(requester.toInt)), JamBytes(parseHex(blob)))
    }

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

    given JamEncoder[AssuranceExtrinsic] with
      def encode(a: AssuranceExtrinsic): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.anchor.bytes
        builder ++= a.bitfield
        builder ++= codec.encodeU16LE(a.validatorIndex.value)
        builder ++= a.signature.bytes
        builder.result()

    /** Create a decoder that knows the cores count */
    def decoder(coresCount: Int): JamDecoder[AssuranceExtrinsic] = new JamDecoder[AssuranceExtrinsic]:
      def decode(bytes: JamBytes, offset: Int): (AssuranceExtrinsic, Int) =
        val arr = bytes.toArray
        var pos = offset
        val bitfieldSize = (coresCount + 7) / 8

        val anchor = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        val bitfield = bytes.slice(pos, pos + bitfieldSize)
        pos += bitfieldSize

        val validatorIndex = ValidatorIndex(codec.decodeU16LE(arr, pos))
        pos += 2

        val signature = Ed25519Signature(arr.slice(pos, pos + SignatureSize))
        pos += SignatureSize

        (AssuranceExtrinsic(anchor, bitfield, validatorIndex, signature), pos - offset)

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

    given JamEncoder[Verdict] with
      def encode(a: Verdict): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.target.bytes
        builder ++= codec.encodeU32LE(a.age.value)
        // votes are encoded without length prefix (fixed count)
        for vote <- a.votes do
          builder ++= vote.encode
        builder.result()

    /** Create a decoder that knows the votes per verdict */
    def decoder(votesPerVerdict: Int): JamDecoder[Verdict] = new JamDecoder[Verdict]:
      def decode(bytes: JamBytes, offset: Int): (Verdict, Int) =
        val arr = bytes.toArray
        var pos = offset

        val target = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        val age = Timeslot(codec.decodeU32LE(arr, pos))
        pos += 4

        val votes = (0 until votesPerVerdict).map { _ =>
          val (vote, consumed) = Vote.given_JamDecoder_Vote.decode(bytes, pos)
          pos += consumed
          vote
        }.toList

        (Verdict(target, age, votes), pos - offset)

    given Decoder[Verdict] = Decoder.instance { cursor =>
      for
        target <- cursor.get[String]("target")
        age <- cursor.get[Long]("age")
        votes <- cursor.get[List[Vote]]("votes")
      yield Verdict(Hash(parseHex(target)), Timeslot(age.toInt), votes)
    }

  /**
   * A dispute containing verdicts, culprits, and faults.
   */
  final case class Dispute(
    verdicts: List[Verdict],
    culprits: List[Culprit],
    faults: List[Fault]
  )

  object Dispute:
    given JamEncoder[Dispute] with
      def encode(a: Dispute): JamBytes =
        val builder = JamBytes.newBuilder
        // verdicts - compact length prefix + items
        builder ++= codec.encodeCompactInteger(a.verdicts.length.toLong)
        for verdict <- a.verdicts do
          builder ++= verdict.encode
        // culprits - compact length prefix + items
        builder ++= codec.encodeCompactInteger(a.culprits.length.toLong)
        for culprit <- a.culprits do
          builder ++= culprit.encode
        // faults - compact length prefix + items
        builder ++= codec.encodeCompactInteger(a.faults.length.toLong)
        for fault <- a.faults do
          builder ++= fault.encode
        builder.result()

    /** Create a decoder that knows the votes per verdict */
    def decoder(votesPerVerdict: Int): JamDecoder[Dispute] = new JamDecoder[Dispute]:
      def decode(bytes: JamBytes, offset: Int): (Dispute, Int) =
        val arr = bytes.toArray
        var pos = offset

        val verdictDecoder = Verdict.decoder(votesPerVerdict)

        // verdicts - compact length prefix + items
        val (verdictsLength, verdictsLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += verdictsLengthBytes
        val verdicts = (0 until verdictsLength.toInt).map { _ =>
          val (verdict, consumed) = verdictDecoder.decode(bytes, pos)
          pos += consumed
          verdict
        }.toList

        // culprits - compact length prefix + fixed-size items
        val (culpritsLength, culpritsLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += culpritsLengthBytes
        val culprits = (0 until culpritsLength.toInt).map { _ =>
          val (culprit, consumed) = Culprit.given_JamDecoder_Culprit.decode(bytes, pos)
          pos += consumed
          culprit
        }.toList

        // faults - compact length prefix + fixed-size items
        val (faultsLength, faultsLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += faultsLengthBytes
        val faults = (0 until faultsLength.toInt).map { _ =>
          val (fault, consumed) = Fault.given_JamDecoder_Fault.decode(bytes, pos)
          pos += consumed
          fault
        }.toList

        (Dispute(verdicts, culprits, faults), pos - offset)

    given Decoder[Dispute] = Decoder.instance { cursor =>
      for
        verdicts <- cursor.get[List[Verdict]]("verdicts")
        culprits <- cursor.get[List[Culprit]]("culprits")
        faults <- cursor.get[List[Fault]]("faults")
      yield Dispute(verdicts, culprits, faults)
    }

  /**
   * A guarantee extrinsic containing a work report and signatures.
   */
  final case class GuaranteeExtrinsic(
    report: WorkReport,
    slot: Timeslot,
    signatures: List[GuaranteeSignature]
  )

  object GuaranteeExtrinsic:
    given JamEncoder[GuaranteeExtrinsic] with
      def encode(a: GuaranteeExtrinsic): JamBytes =
        val builder = JamBytes.newBuilder
        // report - variable size
        builder ++= a.report.encode
        // slot - 4 bytes
        builder ++= codec.encodeU32LE(a.slot.value)
        // signatures - compact length prefix + items
        builder ++= codec.encodeCompactInteger(a.signatures.length.toLong)
        for sig <- a.signatures do
          builder ++= sig.encode
        builder.result()

    given JamDecoder[GuaranteeExtrinsic] with
      def decode(bytes: JamBytes, offset: Int): (GuaranteeExtrinsic, Int) =
        val arr = bytes.toArray
        var pos = offset

        // report - variable size
        val (report, reportBytes) = workpackage.WorkReport.given_JamDecoder_WorkReport.decode(bytes, pos)
        pos += reportBytes

        // slot - 4 bytes
        val slot = Timeslot(codec.decodeU32LE(arr, pos))
        pos += 4

        // signatures - compact length prefix + fixed-size items
        val (signaturesLength, signaturesLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += signaturesLengthBytes
        val signatures = (0 until signaturesLength.toInt).map { _ =>
          val (sig, consumed) = GuaranteeSignature.given_JamDecoder_GuaranteeSignature.decode(bytes, pos)
          pos += consumed
          sig
        }.toList

        (GuaranteeExtrinsic(report, slot, signatures), pos - offset)

    given Decoder[GuaranteeExtrinsic] = Decoder.instance { cursor =>
      for
        report <- cursor.get[WorkReport]("report")
        slot <- cursor.get[Long]("slot")
        signatures <- cursor.get[List[GuaranteeSignature]]("signatures")
      yield GuaranteeExtrinsic(report, Timeslot(slot.toInt), signatures)
    }
