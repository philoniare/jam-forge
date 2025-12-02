package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.core.encodeFixedWidthInteger
import kotlinx.serialization.Serializable

@Serializable
data class PreimageAccount(
    val id: Long,
    val data: AccountInfo
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<PreimageAccount, Int> {
            var currentOffset = offset
            val id = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4
            val (accountInfo, accountInfoBytes) = AccountInfo.fromBytes(data, currentOffset)
            currentOffset += accountInfoBytes
            return Pair(PreimageAccount(id, accountInfo), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val idBytes = encodeFixedWidthInteger(id, 4, false)
        return idBytes + this.data.encode()
    }
}
