package io.forge.jam.safrole.accumulation

import io.forge.jam.core.*
import io.forge.jam.core.serializers.ByteArrayNestedListSerializer
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import io.forge.jam.safrole.report.ServiceItem
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
    val accumulated: MutableList<List<JamByteArray>>,
    val privileges: Privileges,
    val accounts: List<ServiceItem>
) : Encodable {
    override fun encode(): ByteArray {
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        return slotBytes + entropy.bytes + encodeNestedList(readyQueue, false) + encodeNestedList(
            accumulated,
            false
        ) + privileges.encode() + encodeList(
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
            accounts = accounts.map { it.copy() }
        )
    }
}
