package io.forge.jam.core

import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerializationException
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder

object SafroleErrorCodeSerializer : KSerializer<SafroleErrorCode> {
    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("JamErrorCode", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: SafroleErrorCode) {
        val serialName = value.name.lowercase().replace('_', '-')
        encoder.encodeString(serialName)
    }

    override fun deserialize(decoder: Decoder): SafroleErrorCode {
        val string = decoder.decodeString()
        return SafroleErrorCode.values().find { errorCode ->
            val hyphenName = errorCode.name.lowercase().replace('_', '-')
            val underscoreName = errorCode.name.lowercase().replace('-', '_')
            string == hyphenName || string == underscoreName
        } ?: throw SerializationException("Unknown error code: $string")
    }
}
