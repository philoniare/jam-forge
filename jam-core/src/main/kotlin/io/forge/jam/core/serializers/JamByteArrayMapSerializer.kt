package io.forge.jam.core.serializers

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.hexToJamBytes
import kotlinx.serialization.KSerializer
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.descriptors.buildClassSerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.*

object JamByteArrayMapSerializer : KSerializer<Map<JamByteArray, JamByteArray>> {
    override val descriptor: SerialDescriptor = buildClassSerialDescriptor("ByteArrayMap")

    override fun deserialize(decoder: Decoder): Map<JamByteArray, JamByteArray> {
        require(decoder is JsonDecoder)
        val jsonObject = decoder.decodeJsonElement().jsonObject

        return jsonObject.entries.associate { (key, value) ->
            key.hexToJamBytes() to value.toString().trim('"').hexToJamBytes()
        }
    }

    override fun serialize(encoder: Encoder, value: Map<JamByteArray, JamByteArray>) {
        require(encoder is JsonEncoder)
        val jsonObject = JsonObject(value.map { (key, value) ->
            byteArrayToHex(key) to encoder.json.encodeToJsonElement(byteArrayToHex(value))
        }.toMap())
        encoder.encodeJsonElement(jsonObject)
    }

    // Helper function to convert ByteArray to hex string
    private fun byteArrayToHex(bytes: JamByteArray): String {
        return bytes.bytes.joinToString("") { byte -> "%02x".format(byte) }
    }
}
