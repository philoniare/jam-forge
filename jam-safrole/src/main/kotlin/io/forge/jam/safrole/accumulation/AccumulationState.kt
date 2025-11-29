package io.forge.jam.safrole.accumulation

import io.forge.jam.core.*
import io.forge.jam.core.serializers.ByteArrayNestedListSerializer
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import io.forge.jam.safrole.preimage.PreimageHash
import io.forge.jam.safrole.report.ServiceData
import io.forge.jam.safrole.report.ServiceInfo
import io.forge.jam.safrole.report.ServiceItem
import io.forge.jam.safrole.report.StorageMapEntry
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
    val accounts: List<ServiceItem>
) : Encodable {
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
                    preimageRequests = mutableMapOf(),
                    lastAccumulated = 0L
                )
            }.toMutableMap(),
            stagingSet = mutableListOf(),
            authQueue = mutableListOf(),
            manager = privileges.bless,
            assigners = privileges.assign.toMutableList(),
            delegator = privileges.designate,
            registrar = 0L,
            alwaysAccers = privileges.alwaysAcc.associate { it.id to it.gas }.toMutableMap()
        )
    }
}

/**
 * Convert PartialState back to list of ServiceItems.
 */
fun PartialState.toServiceItems(): List<ServiceItem> {
    return accounts.map { (id, account) ->
        ServiceItem(
            id = id,
            data = ServiceData(
                service = account.info,
                storage = account.storage.map { (key, value) ->
                    StorageMapEntry(key = key, value = value)
                },
                preimages = account.preimages.map { (hash, blob) ->
                    PreimageHash(hash = hash, blob = blob)
                }
            )
        )
    }.sortedBy { it.id }
}
