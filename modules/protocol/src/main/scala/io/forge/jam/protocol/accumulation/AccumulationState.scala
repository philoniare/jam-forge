package io.forge.jam.protocol.accumulation

import io.forge.jam.core.{JamBytes, StfResult}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.preimage.PreimageHash
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder
import spire.math.{UInt, ULong}
import _root_.scodec.{Codec, Attempt, DecodeResult}
import _root_.scodec.bits.BitVector
import _root_.scodec.codecs.*
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.scodec.JamCodecs.jamBytesCodec

import scala.collection.mutable

/**
 * Accumulation state serialization types.
 */

/**
 * Always-accumulate service item with service ID and gas allocation.
 *
 * @param id Service ID (4 bytes)
 * @param gas Gas allocation (8 bytes)
 */
final case class AlwaysAccItem(
  id: Long,
  gas: Long
)

object AlwaysAccItem:
  val Size: Int = 12 // 4 bytes id + 8 bytes gas

  given Codec[AlwaysAccItem] =
    (uint32L :: int64L).xmap(
      { case (id, gas) => AlwaysAccItem(id & 0xFFFFFFFFL, gas) },
      a => (a.id & 0xFFFFFFFFL, a.gas)
    )

  given Decoder[AlwaysAccItem] = Decoder.instance { cursor =>
    for
      id <- cursor.get[Long]("id")
      gas <- cursor.get[Long]("gas")
    yield AlwaysAccItem(id, gas)
  }

/**
 * Privileged service configuration.
 *
 * @param bless Manager service ID
 * @param assign Per-core assigner service IDs
 * @param designate Delegator service ID
 * @param register Registrar service ID (v0.7.1+, defaults to 0 for v0.7.0 compatibility)
 * @param alwaysAcc Always-accumulate services with gas allocations
 */
final case class Privileges(
  bless: Long,
  assign: List[Long],
  designate: Long,
  register: Long,
  alwaysAcc: List[AlwaysAccItem]
)

object Privileges:
  def codec(coresCount: Int): Codec[Privileges] =
    (uint32L ::
     JamCodecs.fixedSizeList(uint32L, coresCount) ::
     uint32L ::
     uint32L ::
     JamCodecs.compactPrefixedList(summon[Codec[AlwaysAccItem]])).xmap(
      { case (bless, assign, designate, register, alwaysAcc) =>
        Privileges(bless & 0xFFFFFFFFL, assign.map(_ & 0xFFFFFFFFL), designate & 0xFFFFFFFFL, register & 0xFFFFFFFFL, alwaysAcc)
      },
      p => (p.bless & 0xFFFFFFFFL, p.assign.map(_ & 0xFFFFFFFFL), p.designate & 0xFFFFFFFFL, p.register & 0xFFFFFFFFL, p.alwaysAcc)
    )

  given Decoder[Privileges] = Decoder.instance { cursor =>
    for
      bless <- cursor.get[Long]("bless")
      assign <- cursor.get[List[Long]]("assign")
      designate <- cursor.get[Long]("designate")
      register <- cursor.getOrElse[Long]("register")(0L)
      alwaysAcc <- cursor.get[List[AlwaysAccItem]]("always_acc")
    yield Privileges(bless, assign, designate, register, alwaysAcc)
  }

/**
 * Service activity record for statistics tracking.
 */
final case class ServiceActivityRecord(
  providedCount: Int = 0,
  providedSize: Long = 0,
  refinementCount: Long = 0,
  refinementGasUsed: Long = 0,
  imports: Long = 0,
  extrinsicCount: Long = 0,
  extrinsicSize: Long = 0,
  exports: Long = 0,
  accumulateCount: Long = 0,
  accumulateGasUsed: Long = 0
)

object ServiceActivityRecord:
  given Codec[ServiceActivityRecord] =
    (JamCodecs.compactInteger :: JamCodecs.compactInteger :: JamCodecs.compactInteger ::
     JamCodecs.compactInteger :: JamCodecs.compactInteger :: JamCodecs.compactInteger ::
     JamCodecs.compactInteger :: JamCodecs.compactInteger :: JamCodecs.compactInteger ::
     JamCodecs.compactInteger).xmap(
      { case (pc, ps, rc, rg, imp, xc, xs, exp, ac, ag) =>
        ServiceActivityRecord(pc.toInt, ps, rc, rg, imp, xc, xs, exp, ac, ag)
      },
      r => (r.providedCount.toLong, r.providedSize, r.refinementCount, r.refinementGasUsed,
            r.imports, r.extrinsicCount, r.extrinsicSize, r.exports, r.accumulateCount, r.accumulateGasUsed)
    )

  given Decoder[ServiceActivityRecord] = Decoder.instance { cursor =>
    for
      providedCount <- cursor.getOrElse[Int]("provided_count")(0)
      providedSize <- cursor.getOrElse[Long]("provided_size")(0)
      refinementCount <- cursor.getOrElse[Long]("refinement_count")(0)
      refinementGasUsed <- cursor.getOrElse[Long]("refinement_gas_used")(0)
      imports <- cursor.getOrElse[Long]("imports")(0)
      extrinsicCount <- cursor.getOrElse[Long]("extrinsic_count")(0)
      extrinsicSize <- cursor.getOrElse[Long]("extrinsic_size")(0)
      exports <- cursor.getOrElse[Long]("exports")(0)
      accumulateCount <- cursor.getOrElse[Long]("accumulate_count")(0)
      accumulateGasUsed <- cursor.getOrElse[Long]("accumulate_gas_used")(0)
    yield ServiceActivityRecord(
      providedCount,
      providedSize,
      refinementCount,
      refinementGasUsed,
      imports,
      extrinsicCount,
      extrinsicSize,
      exports,
      accumulateCount,
      accumulateGasUsed
    )
  }

/**
 * Service statistics entry pairing service ID with activity record.
 */
final case class ServiceStatisticsEntry(
  id: Long,
  record: ServiceActivityRecord
)

object ServiceStatisticsEntry:
  given Codec[ServiceStatisticsEntry] =
    (uint32L :: summon[Codec[ServiceActivityRecord]]).xmap(
      { case (id, record) => ServiceStatisticsEntry(id & 0xFFFFFFFFL, record) },
      e => (e.id & 0xFFFFFFFFL, e.record)
    )

  given Decoder[ServiceStatisticsEntry] = Decoder.instance { cursor =>
    for
      id <- cursor.get[Long]("id")
      record <- cursor.get[ServiceActivityRecord]("record")
    yield ServiceStatisticsEntry(id, record)
  }

/**
 * Storage map entry with key and value.
 */
final case class StorageMapEntry(
  key: JamBytes,
  value: JamBytes
)

object StorageMapEntry:
  given Codec[StorageMapEntry] =
    (variableSizeBytesLong(JamCodecs.compactInteger, summon[Codec[JamBytes]]) ::
     variableSizeBytesLong(JamCodecs.compactInteger, summon[Codec[JamBytes]])).xmap(
      { case (key, value) => StorageMapEntry(key, value) },
      e => (e.key, e.value)
    )

  given Decoder[StorageMapEntry] = Decoder.instance { cursor =>
    for
      key <- cursor.get[String]("key")
      value <- cursor.get[String]("value")
    yield StorageMapEntry(JamBytes(parseHex(key)), JamBytes(parseHex(value)))
  }

/**
 * Preimage status entry with hash and status list.
 */
final case class PreimagesStatusMapEntry(
  hash: Hash,
  status: List[Long]
)

object PreimagesStatusMapEntry:
  given Codec[PreimagesStatusMapEntry] =
    (JamCodecs.hashCodec :: JamCodecs.compactPrefixedList(uint32L)).xmap(
      { case (hash, status) => PreimagesStatusMapEntry(hash, status.map(_ & 0xFFFFFFFFL)) },
      e => (e.hash, e.status.map(_ & 0xFFFFFFFFL))
    )


  given Decoder[PreimagesStatusMapEntry] = Decoder.instance { cursor =>
    for
      hash <- cursor.get[String]("hash")
      status <- cursor.get[List[Long]]("status")
    yield PreimagesStatusMapEntry(Hash(parseHex(hash)), status)
  }

/**
 * Full account data for accumulation state.
 */
final case class AccumulationServiceData(
  service: ServiceInfo,
  storage: List[StorageMapEntry] = List.empty,
  preimages: List[PreimageHash] = List.empty,
  preimagesStatus: List[PreimagesStatusMapEntry] = List.empty
)

object AccumulationServiceData:
  given Codec[AccumulationServiceData] =
    (summon[Codec[ServiceInfo]] ::
     JamCodecs.compactPrefixedList(summon[Codec[StorageMapEntry]]) ::
     JamCodecs.compactPrefixedList(summon[Codec[PreimageHash]]) ::
     JamCodecs.compactPrefixedList(summon[Codec[PreimagesStatusMapEntry]])).xmap(
      { case (service, storage, preimages, preimagesStatus) =>
        AccumulationServiceData(service, storage, preimages, preimagesStatus)
      },
      a => (a.service, a.storage, a.preimages, a.preimagesStatus)
    )

  given Decoder[AccumulationServiceData] = Decoder.instance { cursor =>
    for
      service <- cursor.get[ServiceInfo]("service")
      storage <- cursor.getOrElse[List[StorageMapEntry]]("storage")(List.empty)
      preimages <- cursor.getOrElse[List[PreimageHash]]("preimages_blob")(List.empty)
      preimagesStatus <- cursor.getOrElse[List[PreimagesStatusMapEntry]]("preimages_status")(List.empty)
    yield AccumulationServiceData(service, storage, preimages, preimagesStatus)
  }

/**
 * Service item for accumulation state.
 */
final case class AccumulationServiceItem(
  id: Long,
  data: AccumulationServiceData
)

object AccumulationServiceItem:
  given Codec[AccumulationServiceItem] =
    (uint32L :: summon[Codec[AccumulationServiceData]]).xmap(
      { case (id, data) => AccumulationServiceItem(id & 0xFFFFFFFFL, data) },
      item => (item.id & 0xFFFFFFFFL, item.data)
    )

  given Decoder[AccumulationServiceItem] = Decoder.instance { cursor =>
    for
      id <- cursor.get[Long]("id")
      data <- cursor.get[AccumulationServiceData]("data")
    yield AccumulationServiceItem(id, data)
  }

/**
 * Ready record containing a work report and its dependencies.
 * Used in test vector state serialization.
 */
final case class AccumulationReadyRecord(
  report: WorkReport,
  dependencies: List[JamBytes]
)

object AccumulationReadyRecord:
  given Codec[AccumulationReadyRecord] =
    (summon[Codec[WorkReport]] :: JamCodecs.compactPrefixedList(summon[Codec[JamBytes]])).xmap(
      { case (report, dependencies) => AccumulationReadyRecord(report, dependencies) },
      r => (r.report, r.dependencies)
    )


  given Decoder[AccumulationReadyRecord] = Decoder.instance { cursor =>
    for
      report <- cursor.get[WorkReport]("report")
      dependencies <- cursor.get[List[String]]("dependencies")
    yield AccumulationReadyRecord(report, dependencies.map(h => JamBytes(parseHex(h))))
  }

/**
 * Accumulation state for test vector serialization.
 *
 * @param slot Current timeslot
 * @param entropy Entropy for the epoch (32 bytes)
 * @param readyQueue Ready queue (epochLength lists of ReadyRecords)
 * @param accumulated Accumulated hashes (epochLength lists of 32-byte hashes)
 * @param privileges Privileged service configuration
 * @param statistics Service statistics entries
 * @param accounts Service accounts with full data
 * @param rawServiceDataByStateKey Raw state data for lookups (transient)
 * @param rawServiceAccountsByStateKey Raw account data for lookups (transient)
 */
final case class AccumulationState(
  slot: Long,
  entropy: JamBytes,
  readyQueue: List[List[AccumulationReadyRecord]],
  accumulated: List[List[JamBytes]],
  privileges: Privileges,
  statistics: List[ServiceStatisticsEntry] = List.empty,
  accounts: List[AccumulationServiceItem],
  rawServiceDataByStateKey: mutable.Map[JamBytes, JamBytes] = mutable.Map.empty,
  rawServiceAccountsByStateKey: mutable.Map[JamBytes, JamBytes] = mutable.Map.empty
):
  /**
   * Deep copy of this state.
   */
  def deepCopy(): AccumulationState =
    AccumulationState(
      slot = slot,
      entropy = JamBytes(entropy.toArray),
      readyQueue =
        readyQueue.map(_.map(r => AccumulationReadyRecord(r.report, r.dependencies.map(d => JamBytes(d.toArray))))),
      accumulated = accumulated.map(_.map(h => JamBytes(h.toArray))),
      privileges = privileges.copy(),
      statistics = statistics.map(_.copy()),
      accounts = accounts.map(_.copy()),
      rawServiceDataByStateKey = mutable.Map.from(rawServiceDataByStateKey),
      rawServiceAccountsByStateKey = mutable.Map.from(rawServiceAccountsByStateKey)
    )

  /**
   * Convert to PartialState for PVM execution.
   */
  def toPartialState(): PartialState =
    PartialState(
      accounts = mutable.Map.from(accounts.map { item =>
        val preimagesMap = mutable.Map.from(item.data.preimages.map(p => p.hash -> p.blob))
        item.id -> ServiceAccount(
          info = item.data.service,
          storage = mutable.Map.from(item.data.storage.map(e => e.key -> e.value)),
          preimages = preimagesMap,
          preimageRequests = mutable.Map.from(item.data.preimagesStatus.flatMap { status =>
            // Look up the preimage blob to get its length
            preimagesMap.get(status.hash).map { blob =>
              PreimageKey(status.hash, blob.length) -> PreimageRequest(status.status)
            }
          }),
          lastAccumulated = item.data.service.lastAccumulationSlot
        )
      }),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = privileges.bless,
      assigners = mutable.ListBuffer.from(privileges.assign),
      delegator = privileges.designate,
      registrar = privileges.register,
      alwaysAccers = mutable.Map.from(privileges.alwaysAcc.map(a => a.id -> a.gas)),
      rawServiceDataByStateKey = mutable.Map.from(rawServiceDataByStateKey),
      rawServiceAccountsByStateKey = mutable.Map.from(rawServiceAccountsByStateKey)
    )

object AccumulationState:
  def codec(coresCount: Int, epochLength: Int): Codec[AccumulationState] =
    val readyQueueCodec: Codec[List[List[AccumulationReadyRecord]]] =
      JamCodecs.fixedSizeList(JamCodecs.compactPrefixedList(summon[Codec[AccumulationReadyRecord]]), epochLength)

    val accumulatedCodec: Codec[List[List[JamBytes]]] =
      JamCodecs.fixedSizeList(JamCodecs.compactPrefixedList(summon[Codec[JamBytes]]), epochLength)

    (uint32L ::
     JamCodecs.hashCodec ::
     readyQueueCodec ::
     accumulatedCodec ::
     Privileges.codec(coresCount) ::
     JamCodecs.compactPrefixedList(summon[Codec[ServiceStatisticsEntry]]) ::
     JamCodecs.compactPrefixedList(summon[Codec[AccumulationServiceItem]])).xmap(
      { case (slot, entropy, readyQueue, accumulated, privileges, statistics, accounts) =>
        AccumulationState(slot & 0xFFFFFFFFL, JamBytes.fromByteVector(entropy.toByteVector), readyQueue, accumulated, privileges, statistics, accounts)
      },
      a => (a.slot & 0xFFFFFFFFL, Hash(a.entropy.toArray), a.readyQueue, a.accumulated, a.privileges, a.statistics, a.accounts)
    )

  given Decoder[AccumulationState] = Decoder.instance { cursor =>
    for
      slot <- cursor.get[Long]("slot")
      entropy <- cursor.get[String]("entropy")
      readyQueue <- cursor.get[List[List[AccumulationReadyRecord]]]("ready_queue")
      accumulated <- cursor.get[List[List[String]]]("accumulated")
      privileges <- cursor.get[Privileges]("privileges")
      statistics <- cursor.getOrElse[List[ServiceStatisticsEntry]]("statistics")(List.empty)
      accounts <- cursor.get[List[AccumulationServiceItem]]("accounts")
    yield AccumulationState(
      slot,
      JamBytes(parseHex(entropy)),
      readyQueue,
      accumulated.map(_.map(h => JamBytes(parseHex(h)))),
      privileges,
      statistics,
      accounts
    )
  }

/**
 * Extension methods for PartialState to convert back to AccumulationServiceItems.
 */
extension (state: PartialState)
  /**
   * Convert PartialState back to sorted list of AccumulationServiceItems.
   */
  def toAccumulationServiceItems(): List[AccumulationServiceItem] =
    state.accounts.toList.map {
      case (id, account) =>
        AccumulationServiceItem(
          id = id,
          data = AccumulationServiceData(
            service = account.info,
            storage = account.storage.toList
              .sortBy(_._1.toHex)
              .map { case (key, value) => StorageMapEntry(key, value) },
            preimages = account.preimages.toList
              .sortBy(_._1.toHex)
              .map { case (hash, blob) => PreimageHash(hash, blob) },
            preimagesStatus = account.preimageRequests.toList
              .sortBy(_._1.hash.toHex)
              .map {
                case (key, request) =>
                  PreimagesStatusMapEntry(key.hash, request.requestedAt)
              }
          )
        )
    }.sortBy(_.id)

/**
 * Input to the accumulation STF.
 */
final case class AccumulationInput(
  slot: Long,
  reports: List[WorkReport]
)

object AccumulationInput:
  given Codec[AccumulationInput] =
    (uint32L :: JamCodecs.compactPrefixedList(summon[Codec[WorkReport]])).xmap(
      { case (slot, reports) => AccumulationInput(slot & 0xFFFFFFFFL, reports) },
      i => (i.slot & 0xFFFFFFFFL, i.reports)
    )

  given Decoder[AccumulationInput] = Decoder.instance { cursor =>
    for
      slot <- cursor.get[Long]("slot")
      reports <- cursor.get[List[WorkReport]]("reports")
    yield AccumulationInput(slot, reports)
  }

/**
 * Output data from the accumulation STF.
 *
 * @param ok The accumulation root hash
 * @param accumulationStats Per-service accumulation statistics: serviceId -> (gasUsed, workItemCount)
 * @param transferStats Per-service transfer statistics: serviceId -> (count, gasUsed)
 */
final case class AccumulationOutputData(
  ok: JamBytes,
  accumulationStats: Map[Long, (Long, Int)] = Map.empty,
  transferStats: Map[Long, (Long, Long)] = Map.empty
)

object AccumulationOutputData:
  // AccumulationOutputData codec - only used for decoding in tests
  // Encoding is handled via custom logic in AccumulationOutput
  given Codec[AccumulationOutputData] =
    variableSizeBytesLong(JamCodecs.compactInteger, summon[Codec[JamBytes]]).xmap(
      ok => AccumulationOutputData(ok),
      a => a.ok
    )

  given Decoder[AccumulationOutputData] = Decoder.instance { cursor =>
    for
      ok <- cursor.get[String]("ok")
    yield AccumulationOutputData(JamBytes(parseHex(ok)))
  }

/**
 * Output from the accumulation STF.
 * Uses generic StfResult type with AccumulationOutputData as success type.
 * Note: Nothing as error type since no error case currently exists.
 */
type AccumulationOutput = StfResult[AccumulationOutputData, Nothing]

object AccumulationOutput:
  given Decoder[AccumulationOutput] = Decoder.instance { cursor =>
    for
      ok <- cursor.get[String]("ok")
    yield StfResult.success(AccumulationOutputData(JamBytes(parseHex(ok))))
  }

// Custom codec for AccumulationOutput (StfResult[AccumulationOutputData, Nothing])
// Since error type is Nothing, we only encode/decode success case with discriminator 0
given accumulationOutputCodec: Codec[AccumulationOutput] = new Codec[AccumulationOutput]:
  def sizeBound = _root_.scodec.SizeBound.unknown

  def encode(value: AccumulationOutput) =
    val dataCodec = summon[Codec[AccumulationOutputData]]
    value match
      case Right(data) =>
        for
          discriminator <- uint8.encode(0)
          encoded <- dataCodec.encode(data)
        yield discriminator ++ encoded
      case Left(_) =>
        // This should never happen since error type is Nothing
        Attempt.failure(_root_.scodec.Err("Cannot encode error in AccumulationOutput"))

  def decode(bits: BitVector) =
    for
      discriminator <- uint8.decode(bits)
      _ <- if discriminator.value == 0 then Attempt.successful(())
           else Attempt.failure(_root_.scodec.Err(s"Invalid discriminator: ${discriminator.value}"))
      result <- summon[Codec[AccumulationOutputData]].decode(discriminator.remainder)
    yield result.map(data => Right(data))

/**
 * Test case for accumulation STF.
 */
final case class AccumulationCase(
  input: AccumulationInput,
  preState: AccumulationState,
  output: AccumulationOutput,
  postState: AccumulationState
)

object AccumulationCase:
  import AccumulationOutput.given

  /** Create a config-aware codec for AccumulationCase */
  def codec(coresCount: Int, epochLength: Int): Codec[AccumulationCase] =
    val stateCodec = AccumulationState.codec(coresCount, epochLength)
    (summon[Codec[AccumulationInput]] ::
     stateCodec ::
     accumulationOutputCodec ::
     stateCodec).xmap(
      { case (input, preState, output, postState) =>
        AccumulationCase(input, preState, output, postState)
      },
      c => (c.input, c.preState, c.output, c.postState)
    )

  given Decoder[AccumulationCase] = Decoder.instance { cursor =>
    for
      input <- cursor.get[AccumulationInput]("input")
      preState <- cursor.get[AccumulationState]("pre_state")
      output <- cursor.get[AccumulationOutput]("output")
      postState <- cursor.get[AccumulationState]("post_state")
    yield AccumulationCase(input, preState, output, postState)
  }
