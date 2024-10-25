package io.forge.jam.core

import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerializationException
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder

object JamErrorCodeSerializer : KSerializer<JamErrorCode> {
    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("JamErrorCode", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: JamErrorCode) {
        val serialName = value.name.lowercase().replace('_', '-')
        encoder.encodeString(serialName)
    }

    override fun deserialize(decoder: Decoder): JamErrorCode {
        val string = decoder.decodeString()
        return JamErrorCode.values().find { errorCode ->
            val hyphenName = errorCode.name.lowercase().replace('_', '-')
            val underscoreName = errorCode.name.lowercase().replace('-', '_')
            string == hyphenName || string == underscoreName
        } ?: throw SerializationException("Unknown error code: $string")
    }
}
