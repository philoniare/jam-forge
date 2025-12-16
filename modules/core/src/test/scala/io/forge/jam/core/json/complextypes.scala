package io.forge.jam.core.json

import io.circe.{Decoder, HCursor, Json}
import io.forge.jam.core.JamBytes
import _root_.scodec.bits.ByteVector
import io.forge.jam.core.primitives.*
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.{WorkItem, WorkItemImportSegment, WorkItemExtrinsic}
import io.forge.jam.core.types.workresult.{WorkResult, RefineLoad}
import io.forge.jam.core.types.work.{PackageSpec, ExecutionResult, Vote}
import io.forge.jam.core.types.workpackage.{WorkPackage, WorkReport, SegmentRootLookup}
import io.forge.jam.core.types.header.Header
import io.forge.jam.core.types.block.{Block, Extrinsic}
import io.forge.jam.core.types.extrinsic.{Preimage, AssuranceExtrinsic, Verdict, Dispute, GuaranteeExtrinsic}
import io.forge.jam.core.types.tickets.{TicketEnvelope, TicketMark}
import io.forge.jam.core.types.epoch.EpochMark
import io.forge.jam.core.types.dispute.{Culprit, Fault, GuaranteeSignature}
import spire.math.{UByte, UShort, UInt}
import decoders.given
import simpletypes.given

/**
 * Circe decoders for complex protocol types.
 * Uses snake_case field mapping for JSON test vectors.
 */
object complextypes:

  // ════════════════════════════════════════════════════════════════════════════
  // Context Type
  // ════════════════════════════════════════════════════════════════════════════

  given Decoder[Context] = Decoder.instance { cursor =>
    for
      anchor <- cursor.downField("anchor").as[Hash]
      stateRoot <- cursor.downField("state_root").as[Hash]
      beefyRoot <- cursor.downField("beefy_root").as[Hash]
      lookupAnchor <- cursor.downField("lookup_anchor").as[Hash]
      lookupAnchorSlot <- cursor.downField("lookup_anchor_slot").as[Long]
      prerequisites <- cursor.downField("prerequisites").as[List[Hash]]
    yield Context(
      anchor,
      stateRoot,
      beefyRoot,
      lookupAnchor,
      Timeslot(lookupAnchorSlot.toInt),
      prerequisites
    )
  }

  // ════════════════════════════════════════════════════════════════════════════
  // WorkItem Types
  // ════════════════════════════════════════════════════════════════════════════

  given Decoder[WorkItemImportSegment] = Decoder.instance { cursor =>
    for
      treeRoot <- cursor.downField("tree_root").as[Hash]
      index <- cursor.downField("index").as[Int]
    yield WorkItemImportSegment(treeRoot, UShort(index))
  }

  given Decoder[WorkItemExtrinsic] = Decoder.instance { cursor =>
    for
      hash <- cursor.downField("hash").as[Hash]
      len <- cursor.downField("len").as[Long]
    yield WorkItemExtrinsic(hash, UInt(len.toInt))
  }

  given Decoder[WorkItem] = Decoder.instance { cursor =>
    for
      service <- cursor.downField("service").as[Long]
      codeHash <- cursor.downField("code_hash").as[Hash]
      payload <- cursor.downField("payload").as[JamBytes]
      refineGasLimit <- cursor.downField("refine_gas_limit").as[Long]
      accumulateGasLimit <- cursor.downField("accumulate_gas_limit").as[Long]
      importSegments <- cursor.downField("import_segments").as[List[WorkItemImportSegment]]
      extrinsic <- cursor.downField("extrinsic").as[List[WorkItemExtrinsic]]
      exportCount <- cursor.downField("export_count").as[Int]
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

  // ════════════════════════════════════════════════════════════════════════════
  // WorkResult Types
  // ════════════════════════════════════════════════════════════════════════════

  given Decoder[RefineLoad] = Decoder.instance { cursor =>
    for
      gasUsed <- cursor.downField("gas_used").as[Long]
      imports <- cursor.downField("imports").as[Int]
      extrinsicCount <- cursor.downField("extrinsic_count").as[Int]
      extrinsicSize <- cursor.downField("extrinsic_size").as[Long]
      exports <- cursor.downField("exports").as[Int]
    yield RefineLoad(
      Gas(gasUsed),
      UShort(imports),
      UShort(extrinsicCount),
      UInt(extrinsicSize.toInt),
      UShort(exports)
    )
  }

  given Decoder[ExecutionResult] = Decoder.instance { cursor =>
    // Check for "ok" field first
    cursor.downField("ok").as[JamBytes] match
      case Right(output) => Right(ExecutionResult.Ok(output))
      case Left(_) =>
        // Check for "panic" field
        cursor.downField("panic").focus match
          case Some(_) => Right(ExecutionResult.Panic)
          case None => Left(io.circe.DecodingFailure("Expected 'ok' or 'panic' in result", cursor.history))
  }

  given Decoder[WorkResult] = Decoder.instance { cursor =>
    for
      serviceId <- cursor.downField("service_id").as[Long]
      codeHash <- cursor.downField("code_hash").as[Hash]
      payloadHash <- cursor.downField("payload_hash").as[Hash]
      accumulateGas <- cursor.downField("accumulate_gas").as[Long]
      result <- cursor.downField("result").as[ExecutionResult]
      refineLoad <- cursor.downField("refine_load").as[RefineLoad]
    yield WorkResult(
      ServiceId(serviceId.toInt),
      codeHash,
      payloadHash,
      Gas(accumulateGas),
      result,
      refineLoad
    )
  }

  // ════════════════════════════════════════════════════════════════════════════
  // WorkPackage and WorkReport Types
  // ════════════════════════════════════════════════════════════════════════════

  given Decoder[SegmentRootLookup] = Decoder.instance { cursor =>
    for
      workPackageHash <- cursor.downField("work_package_hash").as[Hash]
      segmentTreeRoot <- cursor.downField("segment_tree_root").as[Hash]
    yield SegmentRootLookup(workPackageHash, segmentTreeRoot)
  }

  given Decoder[WorkPackage] = Decoder.instance { cursor =>
    for
      authCodeHost <- cursor.downField("auth_code_host").as[Long]
      authCodeHash <- cursor.downField("auth_code_hash").as[Hash]
      context <- cursor.downField("context").as[Context]
      authorization <- cursor.downField("authorization").as[JamBytes]
      authorizerConfig <- cursor.downField("authorizer_config").as[JamBytes]
      items <- cursor.downField("items").as[List[WorkItem]]
    yield WorkPackage(
      ServiceId(authCodeHost.toInt),
      authCodeHash,
      context,
      authorization,
      authorizerConfig,
      items
    )
  }

  given Decoder[WorkReport] = Decoder.instance { cursor =>
    for
      packageSpec <- cursor.downField("package_spec").as[PackageSpec]
      context <- cursor.downField("context").as[Context]
      coreIndex <- cursor.downField("core_index").as[Int]
      authorizerHash <- cursor.downField("authorizer_hash").as[Hash]
      authGasUsed <- cursor.downField("auth_gas_used").as[Long]
      authOutput <- cursor.downField("auth_output").as[JamBytes]
      segmentRootLookup <- cursor.downField("segment_root_lookup").as[List[SegmentRootLookup]]
      results <- cursor.downField("results").as[List[WorkResult]]
    yield WorkReport(
      packageSpec,
      context,
      CoreIndex(coreIndex),
      authorizerHash,
      Gas(authGasUsed),
      authOutput,
      segmentRootLookup,
      results
    )
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Extrinsic Sub-types
  // ════════════════════════════════════════════════════════════════════════════

  given Decoder[Preimage] = Decoder.instance { cursor =>
    for
      requester <- cursor.downField("requester").as[Long]
      blob <- cursor.downField("blob").as[JamBytes]
    yield Preimage(ServiceId(requester.toInt), blob)
  }

  given Decoder[AssuranceExtrinsic] = Decoder.instance { cursor =>
    for
      anchor <- cursor.downField("anchor").as[Hash]
      bitfield <- cursor.downField("bitfield").as[JamBytes]
      validatorIndex <- cursor.downField("validator_index").as[Int]
      signature <- cursor.downField("signature").as[Ed25519Signature]
    yield AssuranceExtrinsic(anchor, bitfield, ValidatorIndex(validatorIndex), signature)
  }

  given Decoder[Verdict] = Decoder.instance { cursor =>
    for
      target <- cursor.downField("target").as[Hash]
      age <- cursor.downField("age").as[Long]
      votes <- cursor.downField("votes").as[List[Vote]]
    yield Verdict(target, Timeslot(age.toInt), votes)
  }

  given Decoder[Dispute] = Decoder.instance { cursor =>
    for
      verdicts <- cursor.downField("verdicts").as[List[Verdict]]
      culprits <- cursor.downField("culprits").as[List[Culprit]]
      faults <- cursor.downField("faults").as[List[Fault]]
    yield Dispute(verdicts, culprits, faults)
  }

  given Decoder[GuaranteeExtrinsic] = Decoder.instance { cursor =>
    for
      report <- cursor.downField("report").as[WorkReport]
      slot <- cursor.downField("slot").as[Long]
      signatures <- cursor.downField("signatures").as[List[GuaranteeSignature]]
    yield GuaranteeExtrinsic(report, Timeslot(slot.toInt), signatures)
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Header Type
  // ════════════════════════════════════════════════════════════════════════════

  given Decoder[Header] = Decoder.instance { cursor =>
    for
      parent <- cursor.downField("parent").as[Hash]
      parentStateRoot <- cursor.downField("parent_state_root").as[Hash]
      extrinsicHash <- cursor.downField("extrinsic_hash").as[Hash]
      slot <- cursor.downField("slot").as[Long]
      epochMark <- cursor.downField("epoch_mark").as[Option[EpochMark]]
      ticketsMark <- cursor.downField("tickets_mark").as[Option[List[TicketMark]]]
      authorIndex <- cursor.downField("author_index").as[Int]
      entropySource <- cursor.downField("entropy_source").as[JamBytes]
      offendersMark <- cursor.downField("offenders_mark").as[List[Hash]]
      seal <- cursor.downField("seal").as[JamBytes]
    yield Header(
      parent,
      parentStateRoot,
      extrinsicHash,
      Timeslot(slot.toInt),
      epochMark,
      ticketsMark,
      ValidatorIndex(authorIndex),
      entropySource,
      offendersMark,
      seal
    )
  }

  // ════════════════════════════════════════════════════════════════════════════
  // Extrinsic and Block Types
  // ════════════════════════════════════════════════════════════════════════════

  given Decoder[Extrinsic] = Decoder.instance { cursor =>
    for
      tickets <- cursor.downField("tickets").as[List[TicketEnvelope]]
      preimages <- cursor.downField("preimages").as[List[Preimage]]
      guarantees <- cursor.downField("guarantees").as[List[GuaranteeExtrinsic]]
      assurances <- cursor.downField("assurances").as[List[AssuranceExtrinsic]]
      disputes <- cursor.downField("disputes").as[Dispute]
    yield Extrinsic(tickets, preimages, guarantees, assurances, disputes)
  }

  given Decoder[Block] = Decoder.instance { cursor =>
    for
      header <- cursor.downField("header").as[Header]
      extrinsic <- cursor.downField("extrinsic").as[Extrinsic]
    yield Block(header, extrinsic)
  }
