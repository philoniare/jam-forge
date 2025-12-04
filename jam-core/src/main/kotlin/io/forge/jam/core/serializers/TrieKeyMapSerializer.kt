package io.forge.jam.core.serializers

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.hexToJamBytes
import kotlinx.serialization.KSerializer
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.descriptors.buildClassSerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.*

/**
 * Serializer for trie key-value maps where keys are truncated to 31 bytes.
 * Test vectors store 32-byte hex keys, but the JAM protocol uses 31-byte keys.
 */
object TrieKeyMapSerializer : KSerializer<Map<JamByteArray, JamByteArray>> {
    override val descriptor: SerialDescriptor = buildClassSerialDescriptor("TrieKeyMap")

    override fun deserialize(decoder: Decoder): Map<JamByteArray, JamByteArray> {
        require(decoder is JsonDecoder)
        val jsonObject = decoder.decodeJsonElement().jsonObject

        return jsonObject.entries.associate { (key, value) ->
            // Truncate key to first 31 bytes (62 hex chars)
            val keyBytes = key.hexToJamBytes()
            val truncatedKey = if (keyBytes.size > 31) {
                JamByteArray(keyBytes.bytes.copyOfRange(0, 31))
            } else {
                keyBytes
            }
            truncatedKey to value.toString().trim('"').hexToJamBytes()
        }
    }

    override fun serialize(encoder: Encoder, value: Map<JamByteArray, JamByteArray>) {
        require(encoder is JsonEncoder)
        val jsonObject = JsonObject(value.map { (key, value) ->
            byteArrayToHex(key) to encoder.json.encodeToJsonElement(byteArrayToHex(value))
        }.toMap())
        encoder.encodeJsonElement(jsonObject)
    }

    private fun byteArrayToHex(bytes: JamByteArray): String {
        return bytes.bytes.joinToString("") { byte -> "%02x".format(byte) }
    }
}
