package io.forge.jam.core.serializers

import io.forge.jam.core.JamByteArray
import kotlinx.serialization.KSerializer
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder

object JamByteArrayListHexSerializer : KSerializer<List<JamByteArray>> {
    private val listSerializer = ListSerializer(JamByteArrayHexSerializer)

    override val descriptor: SerialDescriptor = listSerializer.descriptor

    override fun deserialize(decoder: Decoder): List<JamByteArray> {
        return listSerializer.deserialize(decoder)
    }

    override fun serialize(encoder: Encoder, value: List<JamByteArray>) {
        listSerializer.serialize(encoder, value)
    }
}
