package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeList
import io.forge.jam.safrole.preimage.PreimageHash
import kotlinx.serialization.Serializable

@Serializable
class ServiceData(
    val service: ServiceInfo,
    val preimages: List<PreimageHash>? = null
) : Encodable {
    override fun encode(): ByteArray {
        val preimageBytes = if (preimages != null) encodeList(preimages) else ByteArray(0)
        return service.encode() + preimageBytes
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is ServiceData) return false

        return service == other.service
    }

    override fun hashCode(): Int {
        return javaClass.hashCode()
    }
}
