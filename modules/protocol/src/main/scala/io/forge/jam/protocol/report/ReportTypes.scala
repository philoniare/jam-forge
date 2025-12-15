package io.forge.jam.protocol.report

import io.forge.jam.core.{JamBytes, codec, CodecDerivation, StfResult}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode, decodeAs}
import io.forge.jam.core.primitives.{Hash, Gas}
import io.forge.jam.core.types.workpackage.{SegmentRootLookup, AvailabilityAssignment}
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.service.ServiceAccount
import io.forge.jam.core.types.history.HistoricalBetaContainer
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder
import spire.math.{UInt, ULong}

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

    given JamEncoder[CoreStatisticsRecord] with
      def encode(a: CoreStatisticsRecord): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeCompactInteger(a.daLoad)
        builder ++= codec.encodeCompactInteger(a.popularity)
        builder ++= codec.encodeCompactInteger(a.imports)
        builder ++= codec.encodeCompactInteger(a.extrinsicCount)
        builder ++= codec.encodeCompactInteger(a.extrinsicSize)
        builder ++= codec.encodeCompactInteger(a.exports)
        builder ++= codec.encodeCompactInteger(a.bundleSize)
        builder ++= codec.encodeCompactInteger(a.gasUsed)
        builder.result()

    given JamDecoder[CoreStatisticsRecord] with
      def decode(bytes: JamBytes, offset: Int): (CoreStatisticsRecord, Int) =
        val arr = bytes.toArray
        var pos = offset
        val (daLoad, d1) = codec.decodeCompactInteger(arr, pos); pos += d1
        val (popularity, d2) = codec.decodeCompactInteger(arr, pos); pos += d2
        val (imports, d3) = codec.decodeCompactInteger(arr, pos); pos += d3
        val (extrinsicCount, d4) = codec.decodeCompactInteger(arr, pos); pos += d4
        val (extrinsicSize, d5) = codec.decodeCompactInteger(arr, pos); pos += d5
        val (exports, d6) = codec.decodeCompactInteger(arr, pos); pos += d6
        val (bundleSize, d7) = codec.decodeCompactInteger(arr, pos); pos += d7
        val (gasUsed, d8) = codec.decodeCompactInteger(arr, pos); pos += d8
        (
          CoreStatisticsRecord(
            daLoad,
            popularity,
            imports,
            extrinsicCount,
            extrinsicSize,
            exports,
            bundleSize,
            gasUsed
          ),
          pos - offset
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
    refinementCount: Long = 0,
    refinementGasUsed: Long = 0,
    extrinsicCount: Long = 0,
    extrinsicSize: Long = 0,
    imports: Long = 0,
    exports: Long = 0,
    accumulateCount: Long = 0,
    accumulateGasUsed: Long = 0,
    transferCount: Long = 0,
    transferGasUsed: Long = 0
  )

  object ServiceActivityRecord:
    given JamEncoder[ServiceActivityRecord] with
      def encode(a: ServiceActivityRecord): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeCompactInteger(a.refinementCount)
        builder ++= codec.encodeCompactInteger(a.refinementGasUsed)
        builder ++= codec.encodeCompactInteger(a.extrinsicCount)
        builder ++= codec.encodeCompactInteger(a.extrinsicSize)
        builder ++= codec.encodeCompactInteger(a.imports)
        builder ++= codec.encodeCompactInteger(a.exports)
        builder ++= codec.encodeCompactInteger(a.accumulateCount)
        builder ++= codec.encodeCompactInteger(a.accumulateGasUsed)
        builder ++= codec.encodeCompactInteger(a.transferCount)
        builder ++= codec.encodeCompactInteger(a.transferGasUsed)
        builder.result()

    given JamDecoder[ServiceActivityRecord] with
      def decode(bytes: JamBytes, offset: Int): (ServiceActivityRecord, Int) =
        val arr = bytes.toArray
        var pos = offset
        val (refinementCount, d1) = codec.decodeCompactInteger(arr, pos); pos += d1
        val (refinementGasUsed, d2) = codec.decodeCompactInteger(arr, pos); pos += d2
        val (extrinsicCount, d3) = codec.decodeCompactInteger(arr, pos); pos += d3
        val (extrinsicSize, d4) = codec.decodeCompactInteger(arr, pos); pos += d4
        val (imports, d5) = codec.decodeCompactInteger(arr, pos); pos += d5
        val (exports, d6) = codec.decodeCompactInteger(arr, pos); pos += d6
        val (accumulateCount, d7) = codec.decodeCompactInteger(arr, pos); pos += d7
        val (accumulateGasUsed, d8) = codec.decodeCompactInteger(arr, pos); pos += d8
        val (transferCount, d9) = codec.decodeCompactInteger(arr, pos); pos += d9
        val (transferGasUsed, d10) = codec.decodeCompactInteger(arr, pos); pos += d10
        (
          ServiceActivityRecord(
            refinementCount,
            refinementGasUsed,
            extrinsicCount,
            extrinsicSize,
            imports,
            exports,
            accumulateCount,
            accumulateGasUsed,
            transferCount,
            transferGasUsed
          ),
          pos - offset
        )

    given Decoder[ServiceActivityRecord] =
      Decoder.instance { cursor =>
        for
          refinementCount <- cursor.getOrElse[Long]("refinement_count")(0)
          refinementGasUsed <- cursor.getOrElse[Long]("refinement_gas_used")(0)
          extrinsicCount <- cursor.getOrElse[Long]("extrinsic_count")(0)
          extrinsicSize <- cursor.getOrElse[Long]("extrinsic_size")(0)
          imports <- cursor.getOrElse[Long]("imports")(0)
          exports <- cursor.getOrElse[Long]("exports")(0)
          accumulateCount <- cursor.getOrElse[Long]("accumulate_count")(0)
          accumulateGasUsed <- cursor.getOrElse[Long]("accumulate_gas_used")(0)
          transferCount <- cursor.getOrElse[Long]("transfer_count")(0)
          transferGasUsed <- cursor.getOrElse[Long]("transfer_gas_used")(0)
        yield ServiceActivityRecord(
          refinementCount,
          refinementGasUsed,
          extrinsicCount,
          extrinsicSize,
          imports,
          exports,
          accumulateCount,
          accumulateGasUsed,
          transferCount,
          transferGasUsed
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
    given JamEncoder[ServiceStatisticsEntry] with
      def encode(a: ServiceStatisticsEntry): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeU32LE(UInt(a.id.toInt))
        builder ++= a.record.encode
        builder.result()

    given JamDecoder[ServiceStatisticsEntry] with
      def decode(bytes: JamBytes, offset: Int): (ServiceStatisticsEntry, Int) =
        val arr = bytes.toArray
        val id = codec.decodeU32LE(arr, offset).toLong
        val (record, recordBytes) = bytes.decodeAs[ServiceActivityRecord](offset + 4)
        (ServiceStatisticsEntry(id, record), 4 + recordBytes)

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
    servicesStatistics: List[ServiceStatisticsEntry]
  )

  object ReportState:
    def decoder(coresCount: Int, validatorsCount: Int): JamDecoder[ReportState] = new JamDecoder[ReportState]:
      def decode(bytes: JamBytes, offset: Int): (ReportState, Int) =
        val arr = bytes.toArray
        var pos = offset

        // availAssignments - fixed size (coresCount), optional items
        val availAssignments = (0 until coresCount).map { _ =>
          val optionByte = arr(pos).toInt & 0xff
          pos += 1
          if optionByte == 0 then None
          else
            val (assignment, consumed) = bytes.decodeAs[AvailabilityAssignment](pos)
            pos += consumed
            Some(assignment)
        }.toList

        // currValidators - fixed size (validatorsCount)
        val (currValidators, currValidatorsBytes) = codec.decodeFixedList[ValidatorKey](bytes, pos, validatorsCount)
        pos += currValidatorsBytes

        // prevValidators - fixed size (validatorsCount)
        val (prevValidators, prevValidatorsBytes) = codec.decodeFixedList[ValidatorKey](bytes, pos, validatorsCount)
        pos += prevValidatorsBytes

        // entropy - 4 hashes
        val (entropy, entropyBytes) = codec.decodeFixedList[Hash](bytes, pos, 4)
        pos += entropyBytes

        // offenders - variable size list
        val (offenders, offendersBytes) = bytes.decodeAs[List[Hash]](pos)
        pos += offendersBytes

        // recentBlocks
        val (recentBlocks, recentBlocksBytes) = bytes.decodeAs[HistoricalBetaContainer](pos)
        pos += recentBlocksBytes

        // authPools - fixed size outer (coresCount), variable inner
        val authPools = (0 until coresCount).map { _ =>
          val (pool, poolBytes) = bytes.decodeAs[List[Hash]](pos)
          pos += poolBytes
          pool
        }.toList

        // accounts - variable size
        val (accounts, accountsBytes) = bytes.decodeAs[List[ServiceAccount]](pos)
        pos += accountsBytes

        // coresStatistics - fixed size (coresCount)
        val (coresStatistics, coreStatsBytes) = codec.decodeFixedList[CoreStatisticsRecord](bytes, pos, coresCount)
        pos += coreStatsBytes

        // servicesStatistics - variable size
        val (servicesStatistics, servicesStatsBytes) = bytes.decodeAs[List[ServiceStatisticsEntry]](pos)
        pos += servicesStatsBytes

        (
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
          ),
          pos - offset
        )

    def encoder(coresCount: Int): JamEncoder[ReportState] = new JamEncoder[ReportState]:
      def encode(a: ReportState): JamBytes =
        val builder = JamBytes.newBuilder

        // availAssignments - no length prefix for fixed-size outer list
        for assignment <- a.availAssignments do
          assignment match
            case None => builder += 0.toByte
            case Some(av) =>
              builder += 1.toByte
              builder ++= av.encode

        // currValidators - no length prefix
        builder ++= codec.encodeFixedList(a.currValidators)

        // prevValidators - no length prefix
        builder ++= codec.encodeFixedList(a.prevValidators)

        // entropy - no length prefix (4 fixed hashes)
        builder ++= codec.encodeFixedList(a.entropy)

        // offenders - variable size
        builder ++= a.offenders.encode

        // recentBlocks
        builder ++= a.recentBlocks.encode

        // authPools - fixed outer, variable inner
        for pool <- a.authPools do
          builder ++= pool.encode

        // accounts - variable size
        builder ++= a.accounts.encode

        // coresStatistics - no length prefix (fixed size)
        builder ++= codec.encodeFixedList(a.coresStatistics)

        // servicesStatistics - variable size
        builder ++= a.servicesStatistics.encode

        builder.result()

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
   * Report input containing guarantees and slot.
   */
  final case class ReportInput(
    guarantees: List[GuaranteeExtrinsic],
    slot: Long,
    knownPackages: List[Hash] = List.empty
  )

  object ReportInput:
    given JamEncoder[ReportInput] with
      def encode(a: ReportInput): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.guarantees.encode
        builder ++= codec.encodeU32LE(UInt(a.slot.toInt))
        builder ++= a.knownPackages.encode
        builder.result()

    given JamDecoder[ReportInput] with
      def decode(bytes: JamBytes, offset: Int): (ReportInput, Int) =
        val arr = bytes.toArray
        var pos = offset
        val (guarantees, guaranteesBytes) = bytes.decodeAs[List[GuaranteeExtrinsic]](pos)
        pos += guaranteesBytes
        val slot = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        val (knownPackages, knownPackagesBytes) = bytes.decodeAs[List[Hash]](pos)
        pos += knownPackagesBytes
        (ReportInput(guarantees, slot, knownPackages), pos - offset)

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
    case DuplicateGuarantors

  object ReportErrorCode:
    given JamEncoder[ReportErrorCode] = CodecDerivation.enumEncoder(_.ordinal)
    given JamDecoder[ReportErrorCode] = CodecDerivation.enumDecoder(ReportErrorCode.fromOrdinal)

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
    given JamEncoder[ReportOutputMarks] with
      def encode(a: ReportOutputMarks): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.reported.encode
        builder ++= a.reporters.encode
        builder.result()

    given JamDecoder[ReportOutputMarks] with
      def decode(bytes: JamBytes, offset: Int): (ReportOutputMarks, Int) =
        var pos = offset
        val (reported, reportedBytes) = bytes.decodeAs[List[SegmentRootLookup]](pos)
        pos += reportedBytes
        val (reporters, reportersBytes) = bytes.decodeAs[List[Hash]](pos)
        pos += reportersBytes
        (ReportOutputMarks(reported, reporters), pos - offset)

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
    given JamEncoder[ReportOutput] = StfResult.stfResultEncoder[ReportOutputMarks, ReportErrorCode]
    given JamDecoder[ReportOutput] = StfResult.stfResultDecoder[ReportOutputMarks, ReportErrorCode]

    given circeDecoder: Decoder[ReportOutput] =
      Decoder.instance { cursor =>
        val okResult = cursor.downField("ok").focus
        val errResult = cursor.get[ReportErrorCode]("err")

        if okResult.isDefined then
          cursor.get[ReportOutputMarks]("ok").map(marks => StfResult.success(marks))
        else
          errResult.map(err => StfResult.error(err))
      }

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
    import ReportOutput.circeDecoder

    def decoder(coresCount: Int, validatorsCount: Int): JamDecoder[ReportCase] = new JamDecoder[ReportCase]:
      def decode(bytes: JamBytes, offset: Int): (ReportCase, Int) =
        var pos = offset
        val (input, inputBytes) = bytes.decodeAs[ReportInput](pos)
        pos += inputBytes
        val stateDecoder = ReportState.decoder(coresCount, validatorsCount)
        val (preState, preStateBytes) = stateDecoder.decode(bytes, pos)
        pos += preStateBytes
        val (output, outputBytes) = bytes.decodeAs[ReportOutput](pos)
        pos += outputBytes
        val (postState, postStateBytes) = stateDecoder.decode(bytes, pos)
        pos += postStateBytes
        (ReportCase(input, preState, output, postState), pos - offset)

    def encoder(coresCount: Int): JamEncoder[ReportCase] = new JamEncoder[ReportCase]:
      def encode(a: ReportCase): JamBytes =
        val stateEncoder = ReportState.encoder(coresCount)
        val builder = JamBytes.newBuilder
        builder ++= a.input.encode
        builder ++= stateEncoder.encode(a.preState)
        builder ++= a.output.encode
        builder ++= stateEncoder.encode(a.postState)
        builder.result()

    given Decoder[ReportCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[ReportInput]("input")
          preState <- cursor.get[ReportState]("pre_state")
          output <- cursor.get[ReportOutput]("output")
          postState <- cursor.get[ReportState]("post_state")
        yield ReportCase(input, preState, output, postState)
      }
