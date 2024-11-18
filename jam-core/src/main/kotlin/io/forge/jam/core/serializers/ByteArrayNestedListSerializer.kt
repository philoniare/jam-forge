package io.forge.jam.core.serializers

import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonTransformingSerializer

object ByteArrayNestedListSerializer : JsonTransformingSerializer<List<List<ByteArray>>>(
    ListSerializer(ByteArrayListHexSerializer)
) {
    override fun transformDeserialize(element: JsonElement): JsonElement {
        return element
    }
}
