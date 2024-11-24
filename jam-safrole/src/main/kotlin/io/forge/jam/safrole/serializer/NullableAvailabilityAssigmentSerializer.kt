package io.forge.jam.safrole.serializer

import io.forge.jam.core.encodeOptionalByteArray
import io.forge.jam.safrole.AvailabilityAssignment
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerializationException
import kotlinx.serialization.descriptors.buildClassSerialDescriptor
import kotlinx.serialization.descriptors.listSerialDescriptor
import kotlinx.serialization.encoding.CompositeDecoder
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.encoding.decodeStructure

object NullableAvailabilityAssignmentListSerializer : KSerializer<List<AvailabilityAssignment?>> {
    private object NullableAvailabilityAssignmentSerializer : KSerializer<AvailabilityAssignment?> {
        override val descriptor = buildClassSerialDescriptor("AvailabilityAssignment?")

        @OptIn(ExperimentalSerializationApi::class)
        override fun serialize(encoder: Encoder, value: AvailabilityAssignment?) {
            when (value) {
                null -> encoder.encodeNull()
                else -> {
                    encoder.beginStructure(descriptor).apply {
                        encodeOptionalByteArray(value.report.encode())
                        encodeLongElement(descriptor, 1, value.timeout)
                        endStructure(descriptor)
                    }
                }
            }
        }

        @OptIn(ExperimentalStdlibApi::class)
        override fun deserialize(decoder: Decoder): AvailabilityAssignment? {
            return try {
                decoder.decodeSerializableValue(AvailabilityAssignment.serializer())
            } catch (e: SerializationException) {
                null
            }
        }
    }

    @OptIn(ExperimentalSerializationApi::class)
    override val descriptor = listSerialDescriptor(NullableAvailabilityAssignmentSerializer.descriptor)

    @OptIn(ExperimentalSerializationApi::class)
    override fun serialize(encoder: Encoder, value: List<AvailabilityAssignment?>) {
        encoder.beginCollection(descriptor, value.size).apply {
            value.forEachIndexed { index, assignment ->
                encodeNullableSerializableElement(
                    descriptor,
                    index,
                    NullableAvailabilityAssignmentSerializer,
                    assignment
                )
            }
            endStructure(descriptor)
        }
    }

    @OptIn(ExperimentalSerializationApi::class)
    override fun deserialize(decoder: Decoder): List<AvailabilityAssignment?> {
        return decoder.decodeStructure(descriptor) {
            val list = ArrayList<AvailabilityAssignment?>()
            while (true) {
                val index = decodeElementIndex(descriptor)
                if (index == CompositeDecoder.DECODE_DONE) break

                list.add(
                    decodeNullableSerializableElement(
                        descriptor,
                        index,
                        NullableAvailabilityAssignmentSerializer
                    )
                )
            }
            list
        }
    }
}
