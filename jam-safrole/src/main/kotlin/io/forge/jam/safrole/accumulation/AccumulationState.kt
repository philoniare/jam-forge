package io.forge.jam.safrole.accumulation

import io.forge.jam.core.*
import io.forge.jam.core.serializers.ByteArrayNestedListSerializer
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import io.forge.jam.safrole.preimage.PreimageHash
import io.forge.jam.safrole.report.AccumulationServiceData
import io.forge.jam.safrole.report.AccumulationServiceItem
import io.forge.jam.safrole.report.ServiceInfo
import io.forge.jam.safrole.report.StorageMapEntry
import io.forge.jam.safrole.report.PreimagesStatusMapEntry
import io.forge.jam.safrole.report.PreimageStatusKey
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class AccumulationState(
    var slot: Long,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val entropy: JamByteArray,
    @SerialName("ready_queue")
    var readyQueue: MutableList<List<ReadyRecord>>,
    @SerialName("accumulated")
    @Serializable(with = ByteArrayNestedListSerializer::class)
    var accumulated: MutableList<List<JamByteArray>>,
    val privileges: Privileges,
    val statistics: List<ServiceStatisticsEntry> = emptyList(),
    val accounts: List<AccumulationServiceItem>,
    // Raw service data keyvals indexed by hashed state key (for storage lookups)
    // Transient field - not serialized
    @kotlinx.serialization.Transient
    val rawServiceDataByStateKey: MutableMap<JamByteArray, JamByteArray> = mutableMapOf()
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0, coresCount: Int, epochLength: Int): Pair<AccumulationState, Int> {
            var currentOffset = offset

            // slot - 4 bytes
            val slot = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4

            // entropy - 32 bytes
            val entropy = JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32))
            currentOffset += 32

            // readyQueue - fixed size list (epochLength), each inner list is variable
            val readyQueue = mutableListOf<List<ReadyRecord>>()
            for (i in 0 until epochLength) {
                val (innerLength, innerLengthBytes) = decodeCompactInteger(data, currentOffset)
                currentOffset += innerLengthBytes
                val innerList = mutableListOf<ReadyRecord>()
                for (j in 0 until innerLength.toInt()) {
                    val (record, recordBytes) = ReadyRecord.fromBytes(data, currentOffset)
                    innerList.add(record)
                    currentOffset += recordBytes
                }
                readyQueue.add(innerList)
            }

            // accumulated - fixed size list (epochLength), each inner list is variable 32-byte hashes
            val accumulated = mutableListOf<List<JamByteArray>>()
            for (i in 0 until epochLength) {
                val (innerLength, innerLengthBytes) = decodeCompactInteger(data, currentOffset)
                currentOffset += innerLengthBytes
                val innerList = mutableListOf<JamByteArray>()
                for (j in 0 until innerLength.toInt()) {
                    innerList.add(JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32)))
                    currentOffset += 32
                }
                accumulated.add(innerList)
            }

            // privileges - variable size
            val (privileges, privilegesBytes) = Privileges.fromBytes(data, currentOffset, coresCount)
            currentOffset += privilegesBytes

            // statistics - compact length + variable-size items
            val (statsLength, statsLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += statsLengthBytes
            val statistics = mutableListOf<ServiceStatisticsEntry>()
            for (i in 0 until statsLength.toInt()) {
                val (entry, entryBytes) = ServiceStatisticsEntry.fromBytes(data, currentOffset)
                statistics.add(entry)
                currentOffset += entryBytes
            }

            // accounts - compact length + variable-size items
            val (accountsLength, accountsLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += accountsLengthBytes
            val accounts = mutableListOf<AccumulationServiceItem>()
            for (i in 0 until accountsLength.toInt()) {
                val (item, itemBytes) = AccumulationServiceItem.fromBytes(data, currentOffset)
                accounts.add(item)
                currentOffset += itemBytes
            }

            return Pair(
                AccumulationState(slot, entropy, readyQueue, accumulated, privileges, statistics, accounts),
                currentOffset - offset
            )
        }
    }

    override fun encode(): ByteArray {
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        return slotBytes + entropy.bytes + encodeNestedList(readyQueue, false) + encodeNestedList(
            accumulated,
            false
        ) + privileges.encode() + encodeList(statistics) + encodeList(
            accounts
        )
    }

    fun deepCopy(): AccumulationState {
        return AccumulationState(
            slot = slot,
            entropy = entropy.copy(),
            readyQueue = readyQueue.map { it.map { it.copy() } }.toMutableList(),
            accumulated = accumulated.map { it.map { it.copy() } }.toMutableList(),
            privileges = privileges.copy(),
            statistics = statistics.map { it.copy() },
            accounts = accounts.map { it.copy() }
        )
    }

    /**
     * Convert AccumulationState to PartialState for PVM execution.
     */
    fun toPartialState(): PartialState {
        return PartialState(
            accounts = accounts.associate { item ->
                item.id to ServiceAccount(
                    info = item.data.service,
                    storage = item.data.storage.associate { entry ->
                        entry.key to entry.value
                    }.toMutableMap(),
                    preimages = item.data.preimages.associate { preimage ->
                        preimage.hash to preimage.blob
                    }.toMutableMap(),
                    preimageRequests = item.data.preimagesStatus.associate { status ->
                        PreimageKey(status.hash, status.length) to PreimageRequest(status.status)
                    }.toMutableMap(),
                    lastAccumulated = item.data.service.lastAccumulationSlot
                )
            }.toMutableMap(),
            stagingSet = mutableListOf(),
            authQueue = mutableListOf(),
            manager = privileges.bless,
            assigners = privileges.assign.toMutableList(),
            delegator = privileges.designate,
            registrar = privileges.register,
            alwaysAccers = privileges.alwaysAcc.associate { it.id to it.gas }.toMutableMap(),
            rawServiceDataByStateKey = rawServiceDataByStateKey.toMutableMap()
        )
    }
}

/**
 * Convert PartialState back to list of AccumulationServiceItems.
 */
fun PartialState.toAccumulationServiceItems(): List<AccumulationServiceItem> {
    return accounts.map { (id, account) ->
        AccumulationServiceItem(
            id = id,
            data = AccumulationServiceData(
                service = account.info,
                storage = account.storage.entries
                    .sortedBy { it.key.toHex() }
                    .map { (key, value) -> StorageMapEntry(key = key, value = value) },
                preimages = account.preimages.entries
                    .sortedBy { it.key.toHex() }
                    .map { (hash, blob) -> PreimageHash(hash = hash, blob = blob) },
                preimagesStatus = account.preimageRequests.entries
                    .sortedBy { it.key.hash.toHex() }
                    .map { (key, request) ->
                        PreimagesStatusMapEntry(
                            hash = key.hash,
                            status = request.requestedAt
                        )
                    }
            )
        )
    }.sortedBy { it.id }
}
