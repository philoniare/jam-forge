package io.forge.jam.safrole.report

import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.builtins.PairSerializer
import kotlinx.serialization.builtins.serializer
import kotlinx.serialization.json.*

object ServiceListSerializer : JsonTransformingSerializer<List<Pair<Long, Service>>>(
    ListSerializer(PairSerializer(Long.serializer(), Service.serializer()))
) {
    override fun transformDeserialize(element: JsonElement): JsonElement {
        return JsonArray(element.jsonArray.map { array ->
            buildJsonObject {
                put("first", array.jsonArray[0])
                put("second", array.jsonArray[1])
            }
        })
    }
}
