package io.forge.jam.core.serializers

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.JamByteArrayList
import kotlinx.serialization.KSerializer
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder


object JamByteArrayListSerializer : KSerializer<JamByteArrayList> {
    private val listSerializer = ListSerializer(JamByteArrayHexSerializer)

    override val descriptor: SerialDescriptor = listSerializer.descriptor

    override fun deserialize(decoder: Decoder): JamByteArrayList {
        val byteArrayList = listSerializer.deserialize(decoder)

        val result = JamByteArrayList()
        byteArrayList.forEach { byteArray ->
            result.add(byteArray)
        }
        return result
    }

    override fun serialize(encoder: Encoder, value: JamByteArrayList) {
        val byteArrayList = value.map { JamByteArray(it.bytes) }
        listSerializer.serialize(encoder, byteArrayList)
    }
}
