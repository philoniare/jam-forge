package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class GuaranteeSignature(
    @SerialName("validator_index")
    val validatorIndex: Long,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val signature: JamByteArray
) : Encodable {
    override fun encode(): ByteArray {
        val validatorIndexBytes = encodeFixedWidthInteger(validatorIndex, 2, false)
        val signatureBytes = signature.bytes
        return validatorIndexBytes + signatureBytes
    }
}
