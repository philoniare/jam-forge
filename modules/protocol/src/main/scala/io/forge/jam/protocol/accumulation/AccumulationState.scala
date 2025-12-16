package io.forge.jam.protocol.accumulation

import io.forge.jam.core.{JamBytes, codec, StfResult}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode, decodeAs}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.preimage.PreimageHash
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder
import spire.math.{UInt, ULong}

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

  given JamEncoder[AlwaysAccItem] with
    def encode(a: AlwaysAccItem): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= codec.encodeU32LE(UInt(a.id.toInt))
      builder ++= codec.encodeU64LE(ULong(a.gas))
      builder.result()

  given JamDecoder[AlwaysAccItem] with
    def decode(bytes: JamBytes, offset: Int): (AlwaysAccItem, Int) =
      val arr = bytes.toArray
      val id = codec.decodeU32LE(arr, offset).toLong
      val gas = codec.decodeU64LE(arr, offset + 4).signed
      (AlwaysAccItem(id, gas), Size)

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
  /** Create a config-aware decoder for Privileges */
  def decoder(coresCount: Int): JamDecoder[Privileges] = new JamDecoder[Privileges]:
    def decode(bytes: JamBytes, offset: Int): (Privileges, Int) =
      val arr = bytes.toArray
      var pos = offset

      // bless - 4 bytes
      val bless = codec.decodeU32LE(arr, pos).toLong
      pos += 4

      // assign - fixed size list (coresCount)
      val assign = (0 until coresCount).map { _ =>
        val v = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        v
      }.toList

      // designate - 4 bytes
      val designate = codec.decodeU32LE(arr, pos).toLong
      pos += 4

      // register - 4 bytes
      val register = codec.decodeU32LE(arr, pos).toLong
      pos += 4

      // alwaysAcc - compact length prefix + fixed-size items
      val (alwaysAccLength, alwaysAccLengthBytes) = codec.decodeCompactInteger(arr, pos)
      pos += alwaysAccLengthBytes
      val alwaysAcc = (0 until alwaysAccLength.toInt).map { _ =>
        val (item, _) = bytes.decodeAs[AlwaysAccItem](pos)
        pos += AlwaysAccItem.Size
        item
      }.toList

      (Privileges(bless, assign, designate, register, alwaysAcc), pos - offset)

  given JamEncoder[Privileges] with
    def encode(a: Privileges): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= codec.encodeU32LE(UInt(a.bless.toInt))
      for assigner <- a.assign do
        builder ++= codec.encodeU32LE(UInt(assigner.toInt))
      builder ++= codec.encodeU32LE(UInt(a.designate.toInt))
      builder ++= codec.encodeU32LE(UInt(a.register.toInt))
      builder ++= codec.encodeCompactInteger(a.alwaysAcc.size.toLong)
      for item <- a.alwaysAcc do
        builder ++= item.encode
      builder.result()

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
  given JamEncoder[ServiceActivityRecord] with
    def encode(a: ServiceActivityRecord): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= codec.encodeCompactInteger(a.providedCount.toLong)
      builder ++= codec.encodeCompactInteger(a.providedSize)
      builder ++= codec.encodeCompactInteger(a.refinementCount)
      builder ++= codec.encodeCompactInteger(a.refinementGasUsed)
      builder ++= codec.encodeCompactInteger(a.imports)
      builder ++= codec.encodeCompactInteger(a.extrinsicCount)
      builder ++= codec.encodeCompactInteger(a.extrinsicSize)
      builder ++= codec.encodeCompactInteger(a.exports)
      builder ++= codec.encodeCompactInteger(a.accumulateCount)
      builder ++= codec.encodeCompactInteger(a.accumulateGasUsed)
      builder.result()

  given JamDecoder[ServiceActivityRecord] with
    def decode(bytes: JamBytes, offset: Int): (ServiceActivityRecord, Int) =
      val arr = bytes.toArray
      var pos = offset

      val (providedCount, providedCountBytes) = codec.decodeCompactInteger(arr, pos)
      pos += providedCountBytes
      val (providedSize, providedSizeBytes) = codec.decodeCompactInteger(arr, pos)
      pos += providedSizeBytes
      val (refinementCount, refinementCountBytes) = codec.decodeCompactInteger(arr, pos)
      pos += refinementCountBytes
      val (refinementGasUsed, refinementGasUsedBytes) = codec.decodeCompactInteger(arr, pos)
      pos += refinementGasUsedBytes
      val (imports, importsBytes) = codec.decodeCompactInteger(arr, pos)
      pos += importsBytes
      val (extrinsicCount, extrinsicCountBytes) = codec.decodeCompactInteger(arr, pos)
      pos += extrinsicCountBytes
      val (extrinsicSize, extrinsicSizeBytes) = codec.decodeCompactInteger(arr, pos)
      pos += extrinsicSizeBytes
      val (exports, exportsBytes) = codec.decodeCompactInteger(arr, pos)
      pos += exportsBytes
      val (accumulateCount, accumulateCountBytes) = codec.decodeCompactInteger(arr, pos)
      pos += accumulateCountBytes
      val (accumulateGasUsed, accumulateGasUsedBytes) = codec.decodeCompactInteger(arr, pos)
      pos += accumulateGasUsedBytes

      (
        ServiceActivityRecord(
          providedCount.toInt,
          providedSize,
          refinementCount,
          refinementGasUsed,
          imports,
          extrinsicCount,
          extrinsicSize,
          exports,
          accumulateCount,
          accumulateGasUsed
        ),
        pos - offset
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
  given JamEncoder[ServiceStatisticsEntry] with
    def encode(a: ServiceStatisticsEntry): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= codec.encodeU32LE(UInt(a.id.toInt))
      builder ++= a.record.encode
      builder.result()

  given JamDecoder[ServiceStatisticsEntry] with
    def decode(bytes: JamBytes, offset: Int): (ServiceStatisticsEntry, Int) =
      val arr = bytes.toArray
      var pos = offset
      val id = codec.decodeU32LE(arr, pos).toLong
      pos += 4
      val (record, recordBytes) = bytes.decodeAs[ServiceActivityRecord](pos)
      pos += recordBytes
      (ServiceStatisticsEntry(id, record), pos - offset)

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
  given JamEncoder[StorageMapEntry] with
    def encode(a: StorageMapEntry): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= codec.encodeCompactInteger(a.key.length.toLong)
      builder ++= a.key
      builder ++= codec.encodeCompactInteger(a.value.length.toLong)
      builder ++= a.value
      builder.result()

  given JamDecoder[StorageMapEntry] with
    def decode(bytes: JamBytes, offset: Int): (StorageMapEntry, Int) =
      val arr = bytes.toArray
      var pos = offset
      val (keyLength, keyLengthBytes) = codec.decodeCompactInteger(arr, pos)
      pos += keyLengthBytes
      val key = JamBytes(arr.slice(pos, pos + keyLength.toInt))
      pos += keyLength.toInt
      val (valueLength, valueLengthBytes) = codec.decodeCompactInteger(arr, pos)
      pos += valueLengthBytes
      val value = JamBytes(arr.slice(pos, pos + valueLength.toInt))
      pos += valueLength.toInt
      (StorageMapEntry(key, value), pos - offset)

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
  given JamEncoder[PreimagesStatusMapEntry] with
    def encode(a: PreimagesStatusMapEntry): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= a.hash.bytes
      builder ++= codec.encodeCompactInteger(a.status.size.toLong)
      for ts <- a.status do
        builder ++= codec.encodeU32LE(UInt(ts.toInt))
      builder.result()

  given JamDecoder[PreimagesStatusMapEntry] with
    def decode(bytes: JamBytes, offset: Int): (PreimagesStatusMapEntry, Int) =
      val arr = bytes.toArray
      var pos = offset
      val hash = Hash(arr.slice(pos, pos + Hash.Size))
      pos += Hash.Size
      val (statusCount, statusCountBytes) = codec.decodeCompactInteger(arr, pos)
      pos += statusCountBytes
      val statusValues = (0 until statusCount.toInt).map { _ =>
        val v = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        v
      }.toList
      (PreimagesStatusMapEntry(hash, statusValues), pos - offset)

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
  given JamEncoder[AccumulationServiceData] with
    def encode(a: AccumulationServiceData): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= a.service.encode
      builder ++= codec.encodeCompactInteger(a.storage.size.toLong)
      for entry <- a.storage do
        builder ++= entry.encode
      builder ++= codec.encodeCompactInteger(a.preimages.size.toLong)
      for preimage <- a.preimages do
        builder ++= preimage.encode
      builder ++= codec.encodeCompactInteger(a.preimagesStatus.size.toLong)
      for status <- a.preimagesStatus do
        builder ++= status.encode
      builder.result()

  given JamDecoder[AccumulationServiceData] with
    def decode(bytes: JamBytes, offset: Int): (AccumulationServiceData, Int) =
      val arr = bytes.toArray
      var pos = offset

      val (service, _) = bytes.decodeAs[ServiceInfo](pos)
      pos += ServiceInfo.Size

      val (storageLength, storageLengthBytes) = codec.decodeCompactInteger(arr, pos)
      pos += storageLengthBytes
      val storage = (0 until storageLength.toInt).map { _ =>
        val (entry, entryBytes) = bytes.decodeAs[StorageMapEntry](pos)
        pos += entryBytes
        entry
      }.toList

      val (preimagesLength, preimagesLengthBytes) = codec.decodeCompactInteger(arr, pos)
      pos += preimagesLengthBytes
      val preimages = (0 until preimagesLength.toInt).map { _ =>
        val (preimage, preimageBytes) = bytes.decodeAs[PreimageHash](pos)
        pos += preimageBytes
        preimage
      }.toList

      val (statusLength, statusLengthBytes) = codec.decodeCompactInteger(arr, pos)
      pos += statusLengthBytes
      val preimagesStatus = (0 until statusLength.toInt).map { _ =>
        val (status, statusBytes) = bytes.decodeAs[PreimagesStatusMapEntry](pos)
        pos += statusBytes
        status
      }.toList

      (AccumulationServiceData(service, storage, preimages, preimagesStatus), pos - offset)

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
  given JamEncoder[AccumulationServiceItem] with
    def encode(a: AccumulationServiceItem): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= codec.encodeU32LE(UInt(a.id.toInt))
      builder ++= a.data.encode
      builder.result()

  given JamDecoder[AccumulationServiceItem] with
    def decode(bytes: JamBytes, offset: Int): (AccumulationServiceItem, Int) =
      val arr = bytes.toArray
      var pos = offset
      val id = codec.decodeU32LE(arr, pos).toLong
      pos += 4
      val (serviceData, serviceDataBytes) = bytes.decodeAs[AccumulationServiceData](pos)
      pos += serviceDataBytes
      (AccumulationServiceItem(id, serviceData), pos - offset)

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
  given JamEncoder[AccumulationReadyRecord] with
    def encode(a: AccumulationReadyRecord): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= a.report.encode
      builder ++= codec.encodeCompactInteger(a.dependencies.size.toLong)
      for dep <- a.dependencies do
        builder ++= dep
      builder.result()

  given JamDecoder[AccumulationReadyRecord] with
    def decode(bytes: JamBytes, offset: Int): (AccumulationReadyRecord, Int) =
      val arr = bytes.toArray
      var pos = offset

      // report - variable size
      val (report, reportBytes) = bytes.decodeAs[WorkReport](pos)
      pos += reportBytes

      // dependencies - compact length + 32-byte hashes
      val (depsLength, depsLengthBytes) = codec.decodeCompactInteger(arr, pos)
      pos += depsLengthBytes
      val dependencies = (0 until depsLength.toInt).map { _ =>
        val dep = JamBytes(arr.slice(pos, pos + 32))
        pos += 32
        dep
      }.toList

      (AccumulationReadyRecord(report, dependencies), pos - offset)

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
  /** Create a config-aware decoder for AccumulationState */
  def decoder(coresCount: Int, epochLength: Int): JamDecoder[AccumulationState] = new JamDecoder[AccumulationState]:
    def decode(bytes: JamBytes, offset: Int): (AccumulationState, Int) =
      val arr = bytes.toArray
      var pos = offset

      // slot - 4 bytes
      val slot = codec.decodeU32LE(arr, pos).toLong
      pos += 4

      // entropy - 32 bytes
      val entropy = JamBytes(arr.slice(pos, pos + 32))
      pos += 32

      // readyQueue - fixed size list (epochLength), each inner list is variable
      val readyQueue = (0 until epochLength).map { _ =>
        val (innerLength, innerLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += innerLengthBytes
        (0 until innerLength.toInt).map { _ =>
          val (record, recordBytes) = bytes.decodeAs[AccumulationReadyRecord](pos)
          pos += recordBytes
          record
        }.toList
      }.toList

      // accumulated - fixed size list (epochLength), each inner list is variable 32-byte hashes
      val accumulated = (0 until epochLength).map { _ =>
        val (innerLength, innerLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += innerLengthBytes
        (0 until innerLength.toInt).map { _ =>
          val hash = JamBytes(arr.slice(pos, pos + 32))
          pos += 32
          hash
        }.toList
      }.toList

      // privileges - variable size
      val privilegesDecoder = Privileges.decoder(coresCount)
      val (privileges, privilegesBytes) = privilegesDecoder.decode(bytes, pos)
      pos += privilegesBytes

      // statistics - compact length + variable-size items
      val (statsLength, statsLengthBytes) = codec.decodeCompactInteger(arr, pos)
      pos += statsLengthBytes
      val statistics = (0 until statsLength.toInt).map { _ =>
        val (entry, entryBytes) = bytes.decodeAs[ServiceStatisticsEntry](pos)
        pos += entryBytes
        entry
      }.toList

      // accounts - compact length + variable-size items
      val (accountsLength, accountsLengthBytes) = codec.decodeCompactInteger(arr, pos)
      pos += accountsLengthBytes
      val accounts = (0 until accountsLength.toInt).map { _ =>
        val (item, itemBytes) = bytes.decodeAs[AccumulationServiceItem](pos)
        pos += itemBytes
        item
      }.toList

      (AccumulationState(slot, entropy, readyQueue, accumulated, privileges, statistics, accounts), pos - offset)

  given JamEncoder[AccumulationState] with
    def encode(a: AccumulationState): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= codec.encodeU32LE(UInt(a.slot.toInt))
      builder ++= a.entropy
      // readyQueue - nested list with fixed outer size
      for inner <- a.readyQueue do
        builder ++= codec.encodeCompactInteger(inner.size.toLong)
        for record <- inner do
          builder ++= record.encode
      // accumulated - nested list with fixed outer size
      for inner <- a.accumulated do
        builder ++= codec.encodeCompactInteger(inner.size.toLong)
        for hash <- inner do
          builder ++= hash
      builder ++= a.privileges.encode
      builder ++= codec.encodeCompactInteger(a.statistics.size.toLong)
      for stat <- a.statistics do
        builder ++= stat.encode
      builder ++= codec.encodeCompactInteger(a.accounts.size.toLong)
      for account <- a.accounts do
        builder ++= account.encode
      builder.result()

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
  given JamEncoder[AccumulationInput] with
    def encode(a: AccumulationInput): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= codec.encodeU32LE(UInt(a.slot.toInt))
      builder ++= codec.encodeCompactInteger(a.reports.size.toLong)
      for report <- a.reports do
        builder ++= report.encode
      builder.result()

  given JamDecoder[AccumulationInput] with
    def decode(bytes: JamBytes, offset: Int): (AccumulationInput, Int) =
      val arr = bytes.toArray
      var pos = offset

      // slot - 4 bytes
      val slot = codec.decodeU32LE(arr, pos).toLong
      pos += 4

      // reports - compact length + variable-size items
      val (reportsLength, reportsLengthBytes) = codec.decodeCompactInteger(arr, pos)
      pos += reportsLengthBytes
      val reports = (0 until reportsLength.toInt).map { _ =>
        val (report, reportBytes) = bytes.decodeAs[WorkReport](pos)
        pos += reportBytes
        report
      }.toList

      (AccumulationInput(slot, reports), pos - offset)

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
  given JamEncoder[AccumulationOutputData] with
    def encode(a: AccumulationOutputData): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= codec.encodeCompactInteger(a.ok.length.toLong)
      builder ++= a.ok
      builder.result()

  given JamDecoder[AccumulationOutputData] with
    def decode(bytes: JamBytes, offset: Int): (AccumulationOutputData, Int) =
      val arr = bytes.toArray
      var pos = offset

      // ok - compact length + bytes
      val (okLength, okLengthBytes) = codec.decodeCompactInteger(arr, pos)
      pos += okLengthBytes
      val ok = JamBytes(arr.slice(pos, pos + okLength.toInt))
      pos += okLength.toInt

      (AccumulationOutputData(ok), pos - offset)

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
  given jamEncoder: JamEncoder[AccumulationOutput] with
    def encode(a: AccumulationOutput): JamBytes =
      val builder = JamBytes.newBuilder
      builder += 0.toByte // discriminator for ok
      a.foreach(data => builder ++= data.encode)
      builder.result()

  given jamDecoder: JamDecoder[AccumulationOutput] with
    def decode(bytes: JamBytes, offset: Int): (AccumulationOutput, Int) =
      val arr = bytes.toArray
      var pos = offset

      // discriminator - 0 = ok, 1 = err
      val discriminator = arr(pos).toInt & 0xff
      pos += 1

      if discriminator == 0 then
        val (data, dataBytes) = bytes.decodeAs[AccumulationOutputData](pos)
        (StfResult.success(data), 1 + dataBytes)
      else
        (StfResult.success(AccumulationOutputData(JamBytes.empty)), pos - offset)

  given circeDecoder: Decoder[AccumulationOutput] = Decoder.instance { cursor =>
    for
      ok <- cursor.get[String]("ok")
    yield StfResult.success(AccumulationOutputData(JamBytes(parseHex(ok))))
  }

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
  // Import the custom codecs from AccumulationOutput companion
  import AccumulationOutput.{jamEncoder, jamDecoder, circeDecoder}

  /** Create a config-aware decoder for AccumulationCase */
  def decoder(coresCount: Int, epochLength: Int): JamDecoder[AccumulationCase] = new JamDecoder[AccumulationCase]:
    def decode(bytes: JamBytes, offset: Int): (AccumulationCase, Int) =
      var pos = offset

      val (input, inputBytes) = bytes.decodeAs[AccumulationInput](pos)
      pos += inputBytes

      val stateDecoder = AccumulationState.decoder(coresCount, epochLength)
      val (preState, preStateBytes) = stateDecoder.decode(bytes, pos)
      pos += preStateBytes

      val (output, outputBytes) = jamDecoder.decode(bytes, pos)
      pos += outputBytes

      val (postState, postStateBytes) = stateDecoder.decode(bytes, pos)
      pos += postStateBytes

      (AccumulationCase(input, preState, output, postState), pos - offset)

  given JamEncoder[AccumulationCase] with
    def encode(a: AccumulationCase): JamBytes =
      val builder = JamBytes.newBuilder
      builder ++= a.input.encode
      builder ++= a.preState.encode
      builder ++= jamEncoder.encode(a.output)
      builder ++= a.postState.encode
      builder.result()

  given Decoder[AccumulationCase] = Decoder.instance { cursor =>
    for
      input <- cursor.get[AccumulationInput]("input")
      preState <- cursor.get[AccumulationState]("pre_state")
      output <- cursor.get[AccumulationOutput]("output")(circeDecoder)
      postState <- cursor.get[AccumulationState]("post_state")
    yield AccumulationCase(input, preState, output, postState)
  }
