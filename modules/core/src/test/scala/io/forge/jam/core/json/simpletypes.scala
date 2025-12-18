package io.forge.jam.core.json

import io.circe.Decoder
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.*
import io.forge.jam.core.types.tickets.{TicketEnvelope, TicketMark}
import io.forge.jam.core.types.epoch.{EpochValidatorKey, EpochMark}
import io.forge.jam.core.types.work.{PackageSpec, Vote}
import io.forge.jam.core.types.dispute.{Culprit, Fault, GuaranteeSignature}
import spire.math.{UByte, UShort, UInt}
import decoders.given

/**
 * Circe decoders for simple protocol types.
 * Uses snake_case field mapping for JSON test vectors.
 */
object simpletypes:

  // ════════════════════════════════════════════════════════════════════════════
  // Ticket Types
  // ════════════════════════════════════════════════════════════════════════════

  given Decoder[TicketEnvelope] = Decoder.instance { cursor =>
    for
      attempt <- cursor.downField("attempt").as[Int]
      signature <- cursor.downField("signature").as[JamBytes]
    yield TicketEnvelope(UByte(attempt), signature)
  }

  given Decoder[TicketMark] = Decoder.instance { cursor =>
    for
      id <- cursor.downField("id").as[JamBytes]
      attempt <- cursor.downField("attempt").as[Int]
    yield TicketMark(id, UByte(attempt))
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Epoch Types
  // ════════════════════════════════════════════════════════════════════════════

  given Decoder[EpochValidatorKey] = Decoder.instance { cursor =>
    for
      bandersnatch <- cursor.downField("bandersnatch").as[BandersnatchPublicKey]
      ed25519 <- cursor.downField("ed25519").as[Ed25519PublicKey]
    yield EpochValidatorKey(bandersnatch, ed25519)
  }

  given Decoder[EpochMark] = Decoder.instance { cursor =>
    for
      entropy <- cursor.downField("entropy").as[Hash]
      ticketsEntropy <- cursor.downField("tickets_entropy").as[Hash]
      validators <- cursor.downField("validators").as[List[EpochValidatorKey]]
    yield EpochMark(entropy, ticketsEntropy, validators)
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Work Types
  // ════════════════════════════════════════════════════════════════════════════

  given Decoder[PackageSpec] = Decoder.instance { cursor =>
    for
      hash <- cursor.downField("hash").as[Hash]
      length <- cursor.downField("length").as[Long]
      erasureRoot <- cursor.downField("erasure_root").as[Hash]
      exportsRoot <- cursor.downField("exports_root").as[Hash]
      exportsCount <- cursor.downField("exports_count").as[Int]
    yield PackageSpec(hash, UInt(length.toInt), erasureRoot, exportsRoot, UShort(exportsCount))
  }

  given Decoder[Vote] = Decoder.instance { cursor =>
    for
      vote <- cursor.downField("vote").as[Boolean]
      validatorIndex <- cursor.downField("index").as[Int]
      signature <- cursor.downField("signature").as[Ed25519Signature]
    yield Vote(vote, ValidatorIndex(validatorIndex), signature)
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Dispute Types
  // ════════════════════════════════════════════════════════════════════════════

  given Decoder[Culprit] = Decoder.instance { cursor =>
    for
      target <- cursor.downField("target").as[Hash]
      key <- cursor.downField("key").as[Ed25519PublicKey]
      signature <- cursor.downField("signature").as[Ed25519Signature]
    yield Culprit(target, key, signature)
  }

  given Decoder[Fault] = Decoder.instance { cursor =>
    for
      target <- cursor.downField("target").as[Hash]
      vote <- cursor.downField("vote").as[Boolean]
      key <- cursor.downField("key").as[Ed25519PublicKey]
      signature <- cursor.downField("signature").as[Ed25519Signature]
    yield Fault(target, vote, key, signature)
  }

  given Decoder[GuaranteeSignature] = Decoder.instance { cursor =>
    for
      validatorIndex <- cursor.downField("validator_index").as[Int]
      signature <- cursor.downField("signature").as[Ed25519Signature]
    yield GuaranteeSignature(ValidatorIndex(validatorIndex), signature)
  }
