package io.forge.jam.protocol.pipeline

import io.forge.jam.protocol.safrole.SafroleTypes.SafroleErrorCode
import io.forge.jam.protocol.dispute.DisputeTypes.DisputeErrorCode
import io.forge.jam.protocol.assurance.AssuranceTypes.AssuranceErrorCode
import io.forge.jam.protocol.report.ReportTypes.ReportErrorCode
import io.forge.jam.protocol.preimage.PreimageTypes.PreimageErrorCode

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
  case AccumulationErr(msg: String)
  case HeaderVerificationErr(msg: String)
  case InvalidEpochMark
  case InvalidTicketsMark
  case InvalidBlockSeal

  def message: String = this match
    case SafroleErr(code) => s"Safrole: $code"
    case DisputeErr(code) => s"Dispute: $code"
    case AssuranceErr(code) => s"Assurance: $code"
    case ReportErr(code) => s"Report: $code"
    case PreimageErr(code) => s"Preimage: $code"
    case AccumulationErr(msg) => s"Accumulation: $msg"
    case HeaderVerificationErr(msg) => s"Header: $msg"
    case InvalidEpochMark => "Invalid epoch mark"
    case InvalidTicketsMark => "Invalid tickets mark"
    case InvalidBlockSeal => "Invalid block seal"
