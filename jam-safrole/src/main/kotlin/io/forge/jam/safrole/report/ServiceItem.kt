package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeFixedWidthInteger
import kotlinx.serialization.Serializable

@Serializable
class ServiceItem(
    val id: Long,
    val info: ServiceInfo
) : Encodable {
    override fun encode(): ByteArray {
        val idBytes = encodeFixedWidthInteger(id, 4, false)
        return idBytes + info.encode()
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is ServiceItem) return false

        return id == other.id && info == other.info
    }

    override fun hashCode(): Int {
        var result = id.hashCode()
        result = 31 * result + info.hashCode()
        return result
    }

    override fun toString(): String {
        return "ServiceItem(id=$id, info=$info)"
    }

    fun copy(
        id: Long = this.id,
        info: ServiceInfo = this.info
    ): ServiceItem {
        return ServiceItem(id, info)
    }
}
