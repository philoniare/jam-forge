package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
class ServiceInfo(
    @SerialName("version")
    val version: Int = 0,
    @SerialName("code_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val codeHash: JamByteArray,
    val balance: Long,
    @SerialName("min_item_gas")
    val minItemGas: Long,
    @SerialName("min_memo_gas")
    val minMemoGas: Long,
    val bytes: Long,
    @SerialName("deposit_offset")
    val depositOffset: Long = 0,
    val items: Int,
    @SerialName("creation_slot")
    val creationSlot: Long = 0,
    @SerialName("last_accumulation_slot")
    val lastAccumulationSlot: Long = 0,
    @SerialName("parent_service")
    val parentService: Long = 0
) : Encodable {
    companion object {
        // 1 (version) + 32 (codeHash) + 8 (balance) + 8 (minItemGas) + 8 (minMemoGas)
        // + 8 (bytes) + 8 (depositOffset) + 4 (items) + 4 (creationSlot) + 4 (lastAccumulationSlot) + 4 (parentService) = 89
        const val SIZE = 89

        fun fromBytes(data: ByteArray, offset: Int = 0): ServiceInfo {
            var currentOffset = offset

            val version = (data[currentOffset].toInt() and 0xFF)
            currentOffset += 1

            val codeHash = JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32))
            currentOffset += 32

            val balance = decodeFixedWidthInteger(data, currentOffset, 8, false)
            currentOffset += 8

            val minItemGas = decodeFixedWidthInteger(data, currentOffset, 8, false)
            currentOffset += 8

            val minMemoGas = decodeFixedWidthInteger(data, currentOffset, 8, false)
            currentOffset += 8

            val bytes = decodeFixedWidthInteger(data, currentOffset, 8, false)
            currentOffset += 8

            val depositOffset = decodeFixedWidthInteger(data, currentOffset, 8, false)
            currentOffset += 8

            val items = decodeFixedWidthInteger(data, currentOffset, 4, false).toInt()
            currentOffset += 4

            val creationSlot = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4

            val lastAccumulationSlot = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4

            val parentService = decodeFixedWidthInteger(data, currentOffset, 4, false)

            return ServiceInfo(
                version, codeHash, balance, minItemGas, minMemoGas, bytes,
                depositOffset, items, creationSlot, lastAccumulationSlot, parentService
            )
        }
    }

    override fun encode(): ByteArray {
        val versionBytes = encodeFixedWidthInteger(version, 1, false)
        val balanceBytes = encodeFixedWidthInteger(balance, 8, false)
        val minItemGasBytes = encodeFixedWidthInteger(minItemGas, 8, false)
        val minMemoGasBytes = encodeFixedWidthInteger(minMemoGas, 8, false)
        val bytesBytes = encodeFixedWidthInteger(bytes, 8, false)
        val depositOffsetBytes = encodeFixedWidthInteger(depositOffset, 8, false)
        val itemsBytes = encodeFixedWidthInteger(items, 4, false)
        val creationSlotBytes = encodeFixedWidthInteger(creationSlot, 4, false)
        val lastAccumulationSlotBytes = encodeFixedWidthInteger(lastAccumulationSlot, 4, false)
        val parentServiceBytes = encodeFixedWidthInteger(parentService, 4, false)
        return versionBytes + codeHash.bytes + balanceBytes + minItemGasBytes + minMemoGasBytes + bytesBytes + depositOffsetBytes + itemsBytes + creationSlotBytes + lastAccumulationSlotBytes + parentServiceBytes
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is ServiceInfo) return false

        return version == other.version &&
            codeHash == other.codeHash &&
            balance == other.balance &&
            minItemGas == other.minItemGas &&
            minMemoGas == other.minMemoGas &&
            bytes == other.bytes &&
            depositOffset == other.depositOffset &&
            items == other.items &&
            creationSlot == other.creationSlot &&
            lastAccumulationSlot == other.lastAccumulationSlot &&
            parentService == other.parentService
    }

    override fun hashCode(): Int {
        var result = version.hashCode()
        result = 31 * result + codeHash.hashCode()
        result = 31 * result + balance.hashCode()
        result = 31 * result + minItemGas.hashCode()
        result = 31 * result + minMemoGas.hashCode()
        result = 31 * result + bytes.hashCode()
        result = 31 * result + depositOffset.hashCode()
        result = 31 * result + items.hashCode()
        result = 31 * result + creationSlot.hashCode()
        result = 31 * result + lastAccumulationSlot.hashCode()
        result = 31 * result + parentService.hashCode()
        return result
    }

    override fun toString(): String {
        return "ServiceInfo(version=$version, codeHash=$codeHash, balance=$balance, minItemGas=$minItemGas, minMemoGas=$minMemoGas, bytes=$bytes, depositOffset=$depositOffset, items=$items, creationSlot=$creationSlot, lastAccumulationSlot=$lastAccumulationSlot, parentService=$parentService)"
    }

    fun copy(
        version: Int = this.version,
        codeHash: JamByteArray = this.codeHash,
        balance: Long = this.balance,
        minItemGas: Long = this.minItemGas,
        minMemoGas: Long = this.minMemoGas,
        bytes: Long = this.bytes,
        depositOffset: Long = this.depositOffset,
        items: Int = this.items,
        creationSlot: Long = this.creationSlot,
        lastAccumulationSlot: Long = this.lastAccumulationSlot,
        parentService: Long = this.parentService
    ): ServiceInfo {
        return ServiceInfo(version, codeHash, balance, minItemGas, minMemoGas, bytes, depositOffset, items, creationSlot, lastAccumulationSlot, parentService)
    }
}
