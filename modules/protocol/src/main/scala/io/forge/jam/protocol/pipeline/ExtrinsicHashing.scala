package io.forge.jam.protocol.pipeline

import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.scodec.JamCodecs.{compactInt, compactPrefixedList}
import io.forge.jam.core.types.block.Extrinsic
import io.forge.jam.core.types.dispute.GuaranteeSignature
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, Dispute}
import io.forge.jam.core.types.tickets.TicketEnvelope
import io.forge.jam.core.types.workpackage.WorkReport
import scodec.Codec

/** H_extrinsichash computation: blake2b over the concatenated
  * hashes of the five encoded extrinsic groups, with each preimage item
  * contributing service ++ hash(data) and each guarantee item contributing
  * hash(report) ++ slot ++ credentials. Shared by block
  * validation (pipeline) and block authoring.
  */
object ExtrinsicHashing:

  def computeExtrinsicHash(ex: Extrinsic, config: ChainConfig): Hash =
    val ticketsEncoded =
      compactPrefixedList(summon[Codec[TicketEnvelope]]).encode(ex.tickets).require.toByteArray
    // Each preimage contributes E_4(service) ++ hash(data)
    val preimageItems = ex.preimages.map { p =>
      _root_.scodec.codecs.uint32L.encode(p.requester.value.toLong & 0xffffffffL).require.toByteArray ++
        Hashing.blake2b256(p.blob.toArray).bytes
    }
    val preimagesEncoded =
      Array.concat(compactInt.encode(preimageItems.length).require.toByteArray +: preimageItems*)
    val assurancesEncoded =
      compactPrefixedList(AssuranceExtrinsic.codec(config.coresCount))
        .encode(ex.assurances)
        .require
        .toByteArray
    val disputesEncoded =
      Dispute.codec(config.votesPerVerdict).encode(ex.disputes).require.toByteArray

    val guaranteeItems = ex.guarantees.map { g =>
      val reportEncoded = summon[Codec[WorkReport]].encode(g.report).require.toByteArray
      val reportHash = Hashing.blake2b256(reportEncoded)
      val timeslotEncoded =
        _root_.scodec.codecs.uint32L.encode(g.slot.value.toLong & 0xffffffffL).require.toByteArray
      val credentialEncoded =
        compactPrefixedList(summon[Codec[GuaranteeSignature]])
          .encode(g.signatures)
          .require
          .toByteArray
      reportHash.bytes ++ timeslotEncoded ++ credentialEncoded
    }

    val guaranteeListLenEncoded = compactInt.encode(guaranteeItems.length).require.toByteArray
    val gEncoded = Array.concat(guaranteeListLenEncoded +: guaranteeItems*)

    val hashes =
      List(ticketsEncoded, preimagesEncoded, gEncoded, assurancesEncoded, disputesEncoded)
        .map(Hashing.blake2b256)
    Hashing.blake2b256(hashes.flatMap(_.bytes).toArray)
