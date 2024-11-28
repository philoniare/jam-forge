package io.forge.jam.core.serializers

import io.forge.jam.core.JamByteArray
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonTransformingSerializer

object ByteArrayNestedListSerializer : JsonTransformingSerializer<List<List<JamByteArray>>>(
    ListSerializer(JamByteArrayListHexSerializer)
) {
    override fun transformDeserialize(element: JsonElement): JsonElement {
        return element
    }
}
