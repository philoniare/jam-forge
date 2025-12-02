package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.core.encodeFixedWidthInteger
import kotlinx.serialization.Serializable

/**
 * ServiceItem for Reports STF - wraps ServiceData (service only).
 */
@Serializable
class ServiceItem(
    val id: Long,
    val data: ServiceData
) : Encodable {
    companion object {
        const val SIZE = 4 + ServiceData.SIZE // 4 (id) + 89 (ServiceData) = 93

        fun fromBytes(data: ByteArray, offset: Int = 0): ServiceItem {
            val id = decodeFixedWidthInteger(data, offset, 4, false)
            val serviceData = ServiceData.fromBytes(data, offset + 4)
            return ServiceItem(id, serviceData)
        }
    }

    override fun encode(): ByteArray {
        val idBytes = encodeFixedWidthInteger(id, 4, false)
        return idBytes + data.encode()
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is ServiceItem) return false

        return id == other.id && data == other.data
    }

    override fun hashCode(): Int {
        var result = id.hashCode()
        result = 31 * result + data.hashCode()
        return result
    }

    override fun toString(): String {
        return "ServiceItem(id=$id, info=$data)"
    }

    fun copy(
        id: Long = this.id,
        data: ServiceData = this.data
    ): ServiceItem {
        return ServiceItem(id, data)
    }
}

/**
 * ServiceItem for Accumulation STF - wraps AccumulationServiceData (full account data).
 */
@Serializable
class AccumulationServiceItem(
    val id: Long,
    val data: AccumulationServiceData
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<AccumulationServiceItem, Int> {
            var currentOffset = offset

            val id = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4

            val (serviceData, serviceDataBytes) = AccumulationServiceData.fromBytes(data, currentOffset)
            currentOffset += serviceDataBytes

            return Pair(AccumulationServiceItem(id, serviceData), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val idBytes = encodeFixedWidthInteger(id, 4, false)
        return idBytes + data.encode()
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is AccumulationServiceItem) return false

        return id == other.id && data == other.data
    }

    override fun hashCode(): Int {
        var result = id.hashCode()
        result = 31 * result + data.hashCode()
        return result
    }

    override fun toString(): String {
        return "AccumulationServiceItem(id=$id, info=$data)"
    }

    fun copy(
        id: Long = this.id,
        data: AccumulationServiceData = this.data
    ): AccumulationServiceItem {
        return AccumulationServiceItem(id, data)
    }
}
