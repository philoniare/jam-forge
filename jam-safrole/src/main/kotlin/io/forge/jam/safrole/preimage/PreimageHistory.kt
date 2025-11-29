package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeFixedWidthInteger
import kotlinx.serialization.Serializable

@Serializable
data class PreimageHistory(val key: PreimageHistoryKey, val value: List<Long>) : Encodable {
    override fun encode(): ByteArray {
        val lengthBytes = encodeFixedWidthInteger(value.size, 1, false)
        val valueBytes = value.fold(byteArrayOf()) { acc, v ->
            acc + encodeFixedWidthInteger(v, 4, false)
        }
        return key.encode() + lengthBytes + valueBytes
    }
}
