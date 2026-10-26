package io.forge.jam.protocol.pipeline

import io.forge.jam.protocol.safrole.SafroleTypes.SafroleErrorCode
import io.forge.jam.protocol.dispute.DisputeTypes.DisputeErrorCode
import io.forge.jam.protocol.assurance.AssuranceTypes.AssuranceErrorCode
import io.forge.jam.protocol.report.ReportTypes.ReportErrorCode
import io.forge.jam.protocol.preimage.PreimageTypes.PreimageErrorCode

enum HeaderErrorCode:
  case InvalidExtrinsicHash
  case InvalidAuthorIndex
  case UnexpectedAuthor
  case BadSealSignature
  case InvalidAuthorTicket
  case BadEntropySignature
  case InvalidEpochMark
  case InvalidTicketsMark

/**
 * Unified error type for pipeline composition.
 * Wraps individual STF error codes and adds pipeline-specific errors.
 */
enum PipelineError:
  case SafroleErr(code: SafroleErrorCode)
  case DisputeErr(code: DisputeErrorCode)
  case AssuranceErr(code: AssuranceErrorCode)
  case ReportErr(code: ReportErrorCode)
  case PreimageErr(code: PreimageErrorCode)
  case HeaderErr(code: HeaderErrorCode)
  case InvalidOffendersMark
  case BadSignatureBatch

  def message: String = this match
    case SafroleErr(code) => s"Safrole: $code"
    case DisputeErr(code) => s"Dispute: $code"
    case AssuranceErr(code) => s"Assurance: $code"
    case ReportErr(code) => s"Report: $code"
    case PreimageErr(code) => s"Preimage: $code"
    case HeaderErr(code) => s"Header: $code"
    case InvalidOffendersMark => "Invalid offenders mark"
    case BadSignatureBatch => "Bad signature batch"
