package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import kotlinx.serialization.Serializable

@Serializable
class ServiceData(
    val service: ServiceInfo
) : Encodable {
    override fun encode(): ByteArray {
        return service.encode()
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is ServiceData) return false

        return service == other.service
    }
}
