package io.forge.jam.core.serializers

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.hexToJamBytes
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerializationException
import kotlinx.serialization.descriptors.buildClassSerialDescriptor
import kotlinx.serialization.descriptors.listSerialDescriptor
import kotlinx.serialization.encoding.CompositeDecoder
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.encoding.decodeStructure

object NullableJamByteArrayListSerializer : KSerializer<List<JamByteArray?>> {
    private object NullableJamByteArraySerializer : KSerializer<JamByteArray?> {
        override val descriptor = buildClassSerialDescriptor("JamByteArray?")

        @OptIn(ExperimentalSerializationApi::class)
        override fun serialize(encoder: Encoder, value: JamByteArray?) {
            when (value) {
                null -> encoder.encodeNull()
                else -> encoder.encodeString(value.toHex())
            }
        }

        @OptIn(ExperimentalStdlibApi::class)
        override fun deserialize(decoder: Decoder): JamByteArray? {
            return try {
                decoder.decodeString().hexToJamBytes()
            } catch (e: SerializationException) {
                null
            }
        }
    }

    @OptIn(ExperimentalSerializationApi::class)
    override val descriptor = listSerialDescriptor(NullableJamByteArraySerializer.descriptor)

    @OptIn(ExperimentalSerializationApi::class)
    override fun serialize(encoder: Encoder, value: List<JamByteArray?>) {
        encoder.beginCollection(descriptor, value.size).apply {
            value.forEachIndexed { index, bytes ->
                encodeNullableSerializableElement(
                    descriptor,
                    index,
                    NullableJamByteArraySerializer,
                    bytes
                )
            }
            endStructure(descriptor)
        }
    }

    @OptIn(ExperimentalSerializationApi::class)
    override fun deserialize(decoder: Decoder): List<JamByteArray?> {
        return decoder.decodeStructure(descriptor) {
            val list = ArrayList<JamByteArray?>()
            while (true) {
                val index = decodeElementIndex(descriptor)
                if (index == CompositeDecoder.DECODE_DONE) break

                list.add(
                    decodeNullableSerializableElement(
                        descriptor,
                        index,
                        NullableJamByteArraySerializer
                    )
                )
            }
            list
        }
    }
}
