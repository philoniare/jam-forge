package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeFixedWidthInteger
import kotlinx.serialization.Serializable

@Serializable
class ServiceItem(
    val id: Long,
    val data: ServiceData
) : Encodable {
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
