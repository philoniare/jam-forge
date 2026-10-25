package io.forge.jam.protocol.report

import io.forge.jam.core.StfResult
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.workpackage.{SegmentRootLookup, AvailabilityAssignment}
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.service.ServiceAccount
import io.forge.jam.core.types.history.HistoricalBetaContainer
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.forge.jam.core.json.JsonHelpers
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.codec.FullJamStateCodecs
import io.circe.Decoder
import io.forge.jam.core.codec.Codec
import io.forge.jam.core.codec.codecs.*

/**
 * Types for the Reports State Transition Function.
 *
 * The Reports STF validates work reports, verifies guarantor signatures,
 * and updates availability assignments.
 */
object ReportTypes:

  /**
   * Core statistics record.
   */
  final case class CoreStatisticsRecord(
    daLoad: Long = 0,
    popularity: Long = 0,
    imports: Long = 0,
    extrinsicCount: Long = 0,
    extrinsicSize: Long = 0,
    exports: Long = 0,
    bundleSize: Long = 0,
    gasUsed: Long = 0
  )

  object CoreStatisticsRecord:
    def zero: CoreStatisticsRecord = CoreStatisticsRecord()

    /** Per-core record: 8 compact naturals d, p, i, x, z, e, b, u (GP 0.8.0 appendix D, pi_C). */
    given Codec[CoreStatisticsRecord] = Codec.of(
      (w, r) =>
        w.compact(r.daLoad)
        w.compact(r.popularity)
        w.compact(r.imports)
        w.compact(r.extrinsicCount)
        w.compact(r.extrinsicSize)
        w.compact(r.exports)
        w.compact(r.bundleSize)
        w.compact(r.gasUsed),
      r => CoreStatisticsRecord(r.compact(), r.compact(), r.compact(), r.compact(), r.compact(), r.compact(),
        r.compact(), r.compact())
    )

    given Decoder[CoreStatisticsRecord] =
      Decoder.instance { cursor =>
        for
          daLoad <- cursor.getOrElse[Long]("da_load")(0)
          popularity <- cursor.getOrElse[Long]("popularity")(0)
          imports <- cursor.getOrElse[Long]("imports")(0)
          extrinsicCount <- cursor.getOrElse[Long]("extrinsic_count")(0)
          extrinsicSize <- cursor.getOrElse[Long]("extrinsic_size")(0)
          exports <- cursor.getOrElse[Long]("exports")(0)
          bundleSize <- cursor.getOrElse[Long]("bundle_size")(0)
          gasUsed <- cursor.getOrElse[Long]("gas_used")(0)
        yield CoreStatisticsRecord(
          daLoad,
          popularity,
          imports,
          extrinsicCount,
          extrinsicSize,
          exports,
          bundleSize,
          gasUsed
        )
      }

  /**
   * Service activity record for statistics.
   */
  final case class ServiceActivityRecord(
    providedCount: Int = 0,
    providedSize: Long = 0,
    refinementCount: Long = 0,
    refinementGasUsed: Long = 0,
    extrinsicCount: Long = 0,
    extrinsicSize: Long = 0,
    imports: Long = 0,
    exports: Long = 0,
    accumulateCount: Long = 0,
    accumulateTransferCount: Long = 0,
    accumulateGasUsed: Long = 0
  )

  object ServiceActivityRecord:
    given Codec[ServiceActivityRecord] = Codec.of(
      (w, r) =>
        w.compact(r.providedCount.toLong)
        w.compact(r.providedSize)
        w.compact(r.refinementCount)
        w.compact(r.refinementGasUsed)
        w.compact(r.extrinsicCount)
        w.compact(r.extrinsicSize)
        w.compact(r.imports)
        w.compact(r.exports)
        w.compact(r.accumulateCount)
        w.compact(r.accumulateTransferCount)
        w.compact(r.accumulateGasUsed),
      r =>
        ServiceActivityRecord(
          r.compact().toInt,
          r.compact(),
          r.compact(),
          r.compact(),
          r.compact(),
          r.compact(),
          r.compact(),
          r.compact(),
          r.compact(),
          r.compact(),
          r.compact()
        )
    )

    given Decoder[ServiceActivityRecord] =
      Decoder.instance { cursor =>
        for
          providedCount <- cursor.getOrElse[Int]("provided_count")(0)
          providedSize <- cursor.getOrElse[Long]("provided_size")(0)
          refinementCount <- cursor.getOrElse[Long]("refinement_count")(0)
          refinementGasUsed <- cursor.getOrElse[Long]("refinement_gas_used")(0)
          extrinsicCount <- cursor.getOrElse[Long]("extrinsic_count")(0)
          extrinsicSize <- cursor.getOrElse[Long]("extrinsic_size")(0)
          imports <- cursor.getOrElse[Long]("imports")(0)
          exports <- cursor.getOrElse[Long]("exports")(0)
          accumulateCount <- cursor.getOrElse[Long]("accumulate_count")(0)
          accumulateTransferCount <- cursor.getOrElse[Long]("accumulate_transfer_count")(0)
          accumulateGasUsed <- cursor.getOrElse[Long]("accumulate_gas_used")(0)
        yield ServiceActivityRecord(
          providedCount,
          providedSize,
          refinementCount,
          refinementGasUsed,
          extrinsicCount,
          extrinsicSize,
          imports,
          exports,
          accumulateCount,
          accumulateTransferCount,
          accumulateGasUsed
        )
      }

  /**
   * Service statistics entry.
   */
  final case class ServiceStatisticsEntry(
    id: Long,
    record: ServiceActivityRecord
  )

  object ServiceStatisticsEntry:
    given Codec[ServiceStatisticsEntry] =
      (uint32L :: summon[Codec[ServiceActivityRecord]]).xmap(
        {
          case (id, record) =>
            ServiceStatisticsEntry((id & 0xffffffffL), record)
        },
        entry => (entry.id & 0xffffffffL, entry.record)
      )

    given Decoder[ServiceStatisticsEntry] =
      Decoder.instance { cursor =>
        for
          id <- cursor.get[Long]("id")
          record <- cursor.get[ServiceActivityRecord]("record")
        yield ServiceStatisticsEntry(id, record)
      }

  /**
   * Report state containing all state needed for reports validation.
   */
  final case class ReportState(
    availAssignments: List[Option[AvailabilityAssignment]],
    currValidators: List[ValidatorKey],
    prevValidators: List[ValidatorKey],
    entropy: List[Hash],
    offenders: List[Hash],
    recentBlocks: HistoricalBetaContainer,
    authPools: List[List[Hash]],
    accounts: List[ServiceAccount],
    coresStatistics: List[CoreStatisticsRecord],
    servicesStatistics: List[ServiceStatisticsEntry],
    readyQueuePackageHashes: Set[Hash] = Set.empty,
    accumulatedPackageHashes: Set[Hash] = Set.empty
  )

  object ReportState:
    def codec(coresCount: Int, validatorsCount: Int): Codec[ReportState] =
      val availAssignmentsCodec =
        JamCodecs.fixedSizeList(JamCodecs.optionCodec(summon[Codec[AvailabilityAssignment]]), coresCount)
      val currValidatorsCodec = FullJamStateCodecs.validatorListCodec(validatorsCount)
      val prevValidatorsCodec = FullJamStateCodecs.validatorListCodec(validatorsCount)
      val entropyCodec = JamCodecs.fixedSizeList(JamCodecs.hashCodec, 4)
      val offendersCodec = JamCodecs.compactPrefixedList(JamCodecs.hashCodec)
      val recentBlocksCodec = summon[Codec[HistoricalBetaContainer]]
      val authPoolsCodec = JamCodecs.fixedSizeList(JamCodecs.compactPrefixedList(JamCodecs.hashCodec), coresCount)
      val accountsCodec = JamCodecs.compactPrefixedList(summon[Codec[ServiceAccount]])
      val coresStatisticsCodec = JamCodecs.fixedSizeList(summon[Codec[CoreStatisticsRecord]], coresCount)
      val servicesStatisticsCodec = JamCodecs.compactPrefixedList(summon[Codec[ServiceStatisticsEntry]])

      (availAssignmentsCodec :: currValidatorsCodec :: prevValidatorsCodec ::
        entropyCodec :: offendersCodec :: recentBlocksCodec :: authPoolsCodec ::
        accountsCodec :: coresStatisticsCodec :: servicesStatisticsCodec).xmap(
        {
          case (
                availAssignments,
                currValidators,
                prevValidators,
                entropy,
                offenders,
                recentBlocks,
                authPools,
                accounts,
                coresStatistics,
                servicesStatistics
              ) =>
            ReportState(
              availAssignments,
              currValidators,
              prevValidators,
              entropy,
              offenders,
              recentBlocks,
              authPools,
              accounts,
              coresStatistics,
              servicesStatistics
            )
        },
        state =>
          (
            state.availAssignments,
            state.currValidators,
            state.prevValidators,
            state.entropy,
            state.offenders,
            state.recentBlocks,
            state.authPools,
            state.accounts,
            state.coresStatistics,
            state.servicesStatistics
          )
      )

    given Decoder[ReportState] =
      Decoder.instance { cursor =>
        for
          availAssignments <- cursor.get[List[Option[AvailabilityAssignment]]]("avail_assignments")
          currValidators <- cursor.get[List[ValidatorKey]]("curr_validators")
          prevValidators <- cursor.get[List[ValidatorKey]]("prev_validators")
          entropy <- cursor.get[List[String]]("entropy").map(_.map(h => Hash(parseHex(h))))
          offenders <- cursor.get[List[String]]("offenders").map(_.map(h => Hash(parseHex(h))))
          recentBlocks <- cursor.get[HistoricalBetaContainer]("recent_blocks")
          authPools <- cursor.get[List[List[String]]]("auth_pools").map(_.map(_.map(h => Hash(parseHex(h)))))
          accounts <- cursor.get[List[ServiceAccount]]("accounts")
          coresStatistics <- cursor.get[List[CoreStatisticsRecord]]("cores_statistics")
          servicesStatistics <- cursor.getOrElse[List[ServiceStatisticsEntry]]("services_statistics")(List.empty)
        yield ReportState(
          availAssignments,
          currValidators,
          prevValidators,
          entropy,
          offenders,
          recentBlocks,
          authPools,
          accounts,
          coresStatistics,
          servicesStatistics
        )
      }

  /**
   * A single entry of the ancestor set A: slot, header hash and, when known, the block's posterior state root (its
   * child's prior state root, h'_r in GP 0.8.0 §reporting).
   */
  final case class AncestorHeader(slot: Long, headerHash: Hash, postStateRoot: Option[Hash] = None)

  /**
   * Report input containing guarantees and slot.
   */
  final case class ReportInput(
    guarantees: List[GuaranteeExtrinsic],
    slot: Long,
    knownPackages: List[Hash] = List.empty
  )

  object ReportInput:
    given Codec[ReportInput] =
      (JamCodecs.compactPrefixedList(summon[Codec[GuaranteeExtrinsic]]) ::
        uint32L ::
        JamCodecs.compactPrefixedList(JamCodecs.hashCodec)).xmap(
        {
          case (guarantees, slot, knownPackages) =>
            ReportInput(guarantees, slot & 0xffffffffL, knownPackages)
        },
        input => (input.guarantees, input.slot & 0xffffffffL, input.knownPackages)
      )

    given Decoder[ReportInput] =
      Decoder.instance { cursor =>
        for
          guarantees <- cursor.get[List[GuaranteeExtrinsic]]("guarantees")
          slot <- cursor.get[Long]("slot")
          knownPackages <-
            cursor.getOrElse[List[String]]("known_packages")(List.empty).map(_.map(h => Hash(parseHex(h))))
        yield ReportInput(guarantees, slot, knownPackages)
      }

  /**
   * Error codes for the Reports STF.
   */
  /** Ordinals 0-31 follow jam-test-vectors reports.asn; DuplicateGuarantors is ours. */
  enum ReportErrorCode:
    case BadCoreIndex
    case FutureReportSlot
    case ReportEpochBeforeLast
    case InsufficientGuarantees
    case OutOfOrderGuarantee
    case NotSortedOrUniqueGuarantors
    case WrongAssignment
    case CoreEngaged
    case AnchorNotRecent
    case BadServiceId
    case BadCodeHash
    case DependencyMissing
    case DuplicatePackage
    case BadStateRoot
    case BadBeefyMmrRoot
    case CoreUnauthorized
    case BadValidatorIndex
    case WorkReportGasTooHigh
    case ServiceItemGasTooLow
    case TooManyDependencies
    case SegmentRootLookupInvalid
    case BadSignature
    case WorkReportTooBig
    case BannedValidator
    case LookupAnchorNotRecent
    case MissingWorkResults
    case BadAnchorSlot
    case BadErasureShards
    case RefineLoadTooHigh
    case BadExportsCount
    case BundleTooBig
    case AuthGasTooHigh
    case DuplicateGuarantors

  object ReportErrorCode:
    given Codec[ReportErrorCode] = byte.emap(
      b =>
        val ordinal = b.toInt & 0xff
        if ordinal < ReportErrorCode.values.length then Right(ReportErrorCode.fromOrdinal(ordinal))
        else Left(s"invalid ReportErrorCode ordinal: $ordinal"),
      e => e.ordinal.toByte
    )

    given Decoder[ReportErrorCode] =
      Decoder.instance { cursor =>
        cursor.as[String].map {
          case "bad_core_index" => ReportErrorCode.BadCoreIndex
          case "future_report_slot" => ReportErrorCode.FutureReportSlot
          case "report_epoch_before_last" => ReportErrorCode.ReportEpochBeforeLast
          case "insufficient_guarantees" => ReportErrorCode.InsufficientGuarantees
          case "out_of_order_guarantee" => ReportErrorCode.OutOfOrderGuarantee
          case "not_sorted_or_unique_guarantors" => ReportErrorCode.NotSortedOrUniqueGuarantors
          case "wrong_assignment" => ReportErrorCode.WrongAssignment
          case "core_engaged" => ReportErrorCode.CoreEngaged
          case "anchor_not_recent" => ReportErrorCode.AnchorNotRecent
          case "bad_service_id" => ReportErrorCode.BadServiceId
          case "bad_code_hash" => ReportErrorCode.BadCodeHash
          case "dependency_missing" => ReportErrorCode.DependencyMissing
          case "duplicate_package" => ReportErrorCode.DuplicatePackage
          case "bad_state_root" => ReportErrorCode.BadStateRoot
          case "bad_beefy_mmr_root" => ReportErrorCode.BadBeefyMmrRoot
          case "core_unauthorized" => ReportErrorCode.CoreUnauthorized
          case "bad_validator_index" => ReportErrorCode.BadValidatorIndex
          case "work_report_gas_too_high" => ReportErrorCode.WorkReportGasTooHigh
          case "service_item_gas_too_low" => ReportErrorCode.ServiceItemGasTooLow
          case "too_many_dependencies" => ReportErrorCode.TooManyDependencies
          case "segment_root_lookup_invalid" => ReportErrorCode.SegmentRootLookupInvalid
          case "bad_signature" => ReportErrorCode.BadSignature
          case "work_report_too_big" => ReportErrorCode.WorkReportTooBig
          case "banned_validator" => ReportErrorCode.BannedValidator
          case "lookup_anchor_not_recent" => ReportErrorCode.LookupAnchorNotRecent
          case "missing_work_results" => ReportErrorCode.MissingWorkResults
          case "bad_anchor_slot" => ReportErrorCode.BadAnchorSlot
          case "bad_erasure_shards" => ReportErrorCode.BadErasureShards
          case "refine_load_too_high" => ReportErrorCode.RefineLoadTooHigh
          case "bad_exports_count" => ReportErrorCode.BadExportsCount
          case "bundle_too_big" => ReportErrorCode.BundleTooBig
          case "auth_gas_too_high" => ReportErrorCode.AuthGasTooHigh
          case "duplicate_guarantors" => ReportErrorCode.DuplicateGuarantors
          case other => throw new IllegalArgumentException(s"Unknown error code: $other")
        }
      }

  /**
   * Output marks for successful report processing.
   */
  final case class ReportOutputMarks(
    reported: List[SegmentRootLookup],
    reporters: List[Hash]
  )

  object ReportOutputMarks:
    given Codec[ReportOutputMarks] =
      (JamCodecs.compactPrefixedList(summon[Codec[SegmentRootLookup]]) ::
        JamCodecs.compactPrefixedList(JamCodecs.hashCodec)).xmap(
        {
          case (reported, reporters) =>
            ReportOutputMarks(reported, reporters)
        },
        marks => (marks.reported, marks.reporters)
      )

    given Decoder[ReportOutputMarks] =
      Decoder.instance { cursor =>
        for
          reported <- cursor.get[List[SegmentRootLookup]]("reported")
          reporters <- cursor.get[List[String]]("reporters").map(_.map(h => Hash(parseHex(h))))
        yield ReportOutputMarks(reported, reporters)
      }

  /**
   * Report output - success or error.
   */
  type ReportOutput = StfResult[ReportOutputMarks, ReportErrorCode]

  object ReportOutput:
    given Codec[ReportOutput] = JamCodecs.stfResultCodec[ReportOutputMarks, ReportErrorCode]

    given circeDecoder: Decoder[ReportOutput] = JsonHelpers.stfResultDecoder[ReportOutputMarks, ReportErrorCode]

  /**
   * Test case for Reports STF.
   */
  final case class ReportCase(
    input: ReportInput,
    preState: ReportState,
    output: ReportOutput,
    postState: ReportState
  )

  object ReportCase:
    import ReportOutput.{circeDecoder, given}

    def codec(coresCount: Int, validatorsCount: Int): Codec[ReportCase] =
      val stateCodec = ReportState.codec(coresCount, validatorsCount)
      (summon[Codec[ReportInput]] :: stateCodec ::
        summon[Codec[ReportOutput]] :: stateCodec).xmap(
        {
          case (input, preState, output, postState) =>
            ReportCase(input, preState, output, postState)
        },
        testCase => (testCase.input, testCase.preState, testCase.output, testCase.postState)
      )

    given Decoder[ReportCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[ReportInput]("input")
          preState <- cursor.get[ReportState]("pre_state")
          output <- cursor.get[ReportOutput]("output")
          postState <- cursor.get[ReportState]("post_state")
        yield ReportCase(input, preState, output, postState)
      }
