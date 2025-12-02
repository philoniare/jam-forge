package io.forge.jam.safrole.authorization

import io.forge.jam.core.Encodable
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.core.encodeCompactInteger
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.encodeList
import kotlinx.serialization.Serializable

@Serializable
data class AuthInput(
    val slot: Long,
    val auths: List<Auth>
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<AuthInput, Int> {
            var currentOffset = offset

            val slot = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4

            val (authsLength, authsLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += authsLengthBytes
            val auths = mutableListOf<Auth>()
            for (i in 0 until authsLength.toInt()) {
                auths.add(Auth.fromBytes(data, currentOffset))
                currentOffset += Auth.SIZE
            }

            return Pair(AuthInput(slot, auths), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        // Vec<CoreAuthorizer> uses compact integer for length prefix
        val authsLengthBytes = encodeCompactInteger(auths.size.toLong())
        val authsBytes = encodeList(auths, includeLength = false)
        return slotBytes + authsLengthBytes + authsBytes
    }
}
