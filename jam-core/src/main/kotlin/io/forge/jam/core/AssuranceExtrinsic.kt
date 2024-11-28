package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class AssuranceExtrinsic(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val anchor: JamByteArray,        // 32-byte anchor (hash)
    @Serializable(with = JamByteArrayHexSerializer::class)
    val bitfield: JamByteArray,      // Bitfield indicating core assurance
    @SerialName("validator_index")
    val validatorIndex: Int,      // Validator index (encoded as two bytes)
    @Serializable(with = JamByteArrayHexSerializer::class)
    val signature: JamByteArray      // Ed25519 signature of the assurance
) : Encodable {
    override fun encode(): ByteArray {
        // Encode validatorIndex as 2 bytes (little-endian)
        val validatorIndexBytes = ByteArray(2)
        validatorIndexBytes[0] = (validatorIndex and 0xFF).toByte()
        validatorIndexBytes[1] = ((validatorIndex shr 8) and 0xFF).toByte()

        // Concatenate anchor, bitfield, validatorIndexBytes, signature
        return anchor.bytes + bitfield.bytes + validatorIndexBytes + signature.bytes
    }
}
