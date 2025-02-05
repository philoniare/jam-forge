package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeFixedWidthInteger
import kotlinx.serialization.Serializable

@Serializable
data class PreimageAccount(
    val id: Long,
    val data: AccountInfo
) : Encodable {
    override fun encode(): ByteArray {
        val idBytes = encodeFixedWidthInteger(id, 4, false)
        return idBytes + data.encode()
    }
}
