package io.forge.jam.core

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class GuaranteeSignature(
    @SerialName("validator_index")
    val validatorIndex: Long,
    @Serializable(with = ByteArrayHexSerializer::class)
    val signature: ByteArray
) : Encodable {
    override fun encode(): ByteArray {
        val validatorIndexBytes = encodeFixedWidthInteger(validatorIndex, 2, false)
        val signatureBytes = signature
        return validatorIndexBytes + signatureBytes
    }
}
