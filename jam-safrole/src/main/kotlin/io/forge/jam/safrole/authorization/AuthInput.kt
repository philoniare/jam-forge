package io.forge.jam.safrole.authorization

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeCompactInteger
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.encodeList
import kotlinx.serialization.Serializable

@Serializable
data class AuthInput(
    val slot: Long,
    val auths: List<Auth>
) : Encodable {
    override fun encode(): ByteArray {
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        // Vec<CoreAuthorizer> uses compact integer for length prefix
        val authsLengthBytes = encodeCompactInteger(auths.size.toLong())
        val authsBytes = encodeList(auths, includeLength = false)
        return slotBytes + authsLengthBytes + authsBytes
    }
}
