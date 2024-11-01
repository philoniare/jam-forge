package io.forge.jam.core.serializers

import io.forge.jam.core.ByteArrayList
import kotlinx.serialization.KSerializer
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder

object ByteArrayListSerializer : KSerializer<ByteArrayList> {
    private val listSerializer = ListSerializer(ByteArrayHexSerializer)

    override val descriptor: SerialDescriptor = listSerializer.descriptor

    override fun serialize(encoder: Encoder, value: ByteArrayList) {
        listSerializer.serialize(encoder, value.toList())
    }

    override fun deserialize(decoder: Decoder): ByteArrayList {
        val list = listSerializer.deserialize(decoder)
        return ByteArrayList().apply {
            list.forEach { add(it) }
        }
    }
}
