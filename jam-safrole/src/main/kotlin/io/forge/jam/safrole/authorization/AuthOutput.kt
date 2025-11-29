package io.forge.jam.safrole.authorization

import io.forge.jam.core.Encodable
import kotlinx.serialization.Serializable

@Serializable
data class AuthOutput(val id: Long) : Encodable {
    override fun encode(): ByteArray {
        // NULL type encodes to empty byte array
        return ByteArray(0)
    }
}
