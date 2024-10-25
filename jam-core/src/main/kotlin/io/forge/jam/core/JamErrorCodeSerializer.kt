package io.forge.jam.core

import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerializationException
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder

class JamErrorCodeSerializer : KSerializer<JamErrorCode> {
    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("JamErrorCode", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: JamErrorCode) {
        encoder.encodeString(value.name)
    }

    override fun deserialize(decoder: Decoder): JamErrorCode {
        val string = decoder.decodeString()
        return JamErrorCode.values().find { it.name == string }
            ?: throw SerializationException("Unknown error code: $string")
    }
}
