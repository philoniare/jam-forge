package io.forge.jam.safrole.authorization

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class Auth(
    val core: Long,
    @Serializable(with = JamByteArrayHexSerializer::class)
    @SerialName("auth_hash")
    val authHash: JamByteArray
) : Encodable {
    companion object {
        const val SIZE = 34 // 2 bytes for core + 32 bytes for authHash

        fun fromBytes(data: ByteArray, offset: Int = 0): Auth {
            val core = decodeFixedWidthInteger(data, offset, 2, false)
            val authHash = JamByteArray(data.copyOfRange(offset + 2, offset + 2 + 32))
            return Auth(core, authHash)
        }
    }

    override fun encode(): ByteArray {
        val coreBytes = encodeFixedWidthInteger(core, 2, false)
        return coreBytes + authHash.bytes
    }
}
