package io.forge.jam.conformance

import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.header.Header
import io.forge.jam.protocol.assurance.AssuranceTypes.AssuranceErrorCode
import io.forge.jam.protocol.dispute.DisputeTypes.DisputeErrorCode
import io.forge.jam.protocol.pipeline.PipelineError
import io.forge.jam.protocol.preimage.PreimageTypes.PreimageErrorCode
import io.forge.jam.protocol.report.ReportTypes.ReportErrorCode
import io.forge.jam.protocol.safrole.SafroleTypes.SafroleErrorCode
import io.forge.jam.protocol.traces.{ImportError, ImportResult}

object ImportErrorText:

  def apply(block: Block, parentStateRoot: Hash, failure: ImportResult.Failure)(using config: ChainConfig): String =
    failure.cause match
      case Some(PipelineError.SafroleErr(SafroleErrorCode.BadSlot)) =>
        val headerHash = Hashing.blake2b256(Header.headerCodec(config.epochLength).encode(block.header))
        s"Local chain error: block ${short(headerHash)} is not part of the finalized chain"
      case Some(PipelineError.HeaderErr(code)) => s"Local chain error: block header verification failure: $code"
      case Some(error)                         => s"Local chain error: block execution failure: ${execution(error)}"
      case None if failure.error == ImportError.InvalidStateRoot =>
        s"Local chain error: invalid parent state root (expected: ${short(block.header.parentStateRoot)}, " +
          s"actual: ${short(parentStateRoot)})"
      case None => s"Local chain error: block execution failure: ${failure.message}"

  private def short(hash: Hash): String = s"0x${hash.toHex.take(16)}..."

  private def execution(error: PipelineError): String = error match
    case PipelineError.SafroleErr(code)    => s"safrole error: ${safrole(code)}"
    case PipelineError.DisputeErr(code)    => s"disputes error: ${disputes(code)}"
    case PipelineError.AssuranceErr(code)  => s"assurances error: ${assurances(code)}"
    case PipelineError.ReportErr(code)     => s"reports error: ${reports(code)}"
    case PipelineError.PreimageErr(code)   => s"preimages error: ${preimages(code)}"
    case PipelineError.InvalidOffendersMark => "bad offenders mark"
    case PipelineError.BadSignatureBatch   => "bad signature batch"
    case PipelineError.HeaderErr(code)     => s"$code"

  private def safrole(code: SafroleErrorCode): String = code match
    case SafroleErrorCode.BadSlot          => "bad slot"
    case SafroleErrorCode.UnexpectedTicket => "tickets were found outside of lottery period"
    case SafroleErrorCode.BadTicketOrder   => "out-of-order tickets are not allowed"
    case SafroleErrorCode.BadTicketProof   => "invalid ticket signature"
    case SafroleErrorCode.BadTicketAttempt => "invalid ticket attempt"
    case SafroleErrorCode.Reserved         => "too many tickets"
    case SafroleErrorCode.DuplicateTicket  => "duplicate ticket"

  private def assurances(code: AssuranceErrorCode): String = code match
    case AssuranceErrorCode.BadAttestationParent      => "bad attestation parent"
    case AssuranceErrorCode.BadValidatorIndex         => "bad attestation validator index"
    case AssuranceErrorCode.CoreNotEngaged            => "core not engaged"
    case AssuranceErrorCode.BadSignature              => "bad signature"
    case AssuranceErrorCode.NotSortedOrUniqueAssurers => "not sorted or unique assurers"

  private def disputes(code: DisputeErrorCode): String = code match
    case DisputeErrorCode.JudgementsNotSortedUnique => "not sorted or unique judgements"
    case DisputeErrorCode.AlreadyJudged             => "already judged"
    case DisputeErrorCode.BadVoteSplit              => "bad vote split"
    case DisputeErrorCode.VerdictsNotSortedUnique   => "not sorted or unique verdicts"
    case DisputeErrorCode.CulpritsNotSortedUnique   => "not sorted or unique culprits"
    case DisputeErrorCode.FaultsNotSortedUnique     => "not sorted or unique faults"
    case DisputeErrorCode.NotEnoughFaults           => "not enough faults"
    case DisputeErrorCode.CulpritsVerdictNotBad     => "culprits verdict is not bad"
    case DisputeErrorCode.FaultVerdictWrong         => "fault verdict is wrong"
    case DisputeErrorCode.OffenderAlreadyReported   => "offender already reported"
    case DisputeErrorCode.BadJudgementAge           => "bad judgement age"
    case DisputeErrorCode.BadValidatorIndex         => "bad validator index"
    case DisputeErrorCode.BadSignature              => "bad signature"
    case DisputeErrorCode.BadGuarantorKey           => "bad guarantor key"
    case DisputeErrorCode.BadAuditorKey             => "bad auditor key"
    case DisputeErrorCode.BadVotesCount             => "bad votes count"

  private def reports(code: ReportErrorCode): String = code match
    case ReportErrorCode.BadCoreIndex                => "bad core index for work report"
    case ReportErrorCode.FutureReportSlot            => "report refers to slot in the future"
    case ReportErrorCode.ReportEpochBeforeLast       => "report rotation before last"
    case ReportErrorCode.InsufficientGuarantees      => "not enough guarantees"
    case ReportErrorCode.NotSortedOrUniqueGuarantors => "not sorted or unique guarantors"
    case ReportErrorCode.WrongAssignment             => "wrong core assignment"
    case ReportErrorCode.CoreEngaged                 => "core not available"
    case ReportErrorCode.BadCodeHash                 => "bad code hash"
    case ReportErrorCode.DuplicatePackage            => "duplicate package"
    case ReportErrorCode.BadBeefyMmrRoot             => "bad beefy mmr root"
    case ReportErrorCode.CoreUnauthorized            => "code unauthorized"
    case ReportErrorCode.BadValidatorIndex           => "bad validator index"
    case ReportErrorCode.LookupAnchorNotRecent       => "lookup anchor is not recent"
    case ReportErrorCode.MissingWorkResults          => "work report with no work results"
    case ReportErrorCode.BadAnchorSlot               => "bad anchor slot"
    case ReportErrorCode.BadErasureShards            => "erasure-coded shard count differs from the active validator count"
    case ReportErrorCode.OutOfOrderGuarantee         => "not sorted or unique guarantees"
    case ReportErrorCode.AnchorNotRecent             => "anchor is not recent"
    case ReportErrorCode.BadServiceId                => "bad service id"
    case ReportErrorCode.DependencyMissing           => "dependency missing"
    case ReportErrorCode.BadStateRoot                => "bad state root"
    case ReportErrorCode.WorkReportGasTooHigh        => "work report gas too high"
    case ReportErrorCode.ServiceItemGasTooLow        => "service item gas too low"
    case ReportErrorCode.TooManyDependencies         => "too many dependencies"
    case ReportErrorCode.SegmentRootLookupInvalid    => "segment root lookup invalid"
    case ReportErrorCode.BadSignature                => "bad signature"
    case ReportErrorCode.WorkReportTooBig            => "work report too big"
    case ReportErrorCode.BannedValidator             => "banned validator"
    case ReportErrorCode.RefineLoadTooHigh           => "refine load too high"
    case ReportErrorCode.BadExportsCount             => "bad exports count"
    case ReportErrorCode.BundleTooBig                => "bundle too big"
    case ReportErrorCode.AuthGasTooHigh              => "authorizer gas too high"
    case ReportErrorCode.DuplicateGuarantors         => "duplicate guarantors"

  private def preimages(code: PreimageErrorCode): String = code match
    case PreimageErrorCode.PreimageUnneeded          => "preimage not required"
    case PreimageErrorCode.PreimagesNotSortedUnique  => "preimages not sorted or unique"
