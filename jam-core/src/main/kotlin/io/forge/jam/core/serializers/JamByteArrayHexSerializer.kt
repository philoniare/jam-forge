package io.forge.jam.core.serializers

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.hexToJamBytes
import kotlinx.serialization.KSerializer
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder

object JamByteArrayHexSerializer : KSerializer<JamByteArray> {
    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("HexByteArray", PrimitiveKind.STRING)

    // Deserialize hex string to ByteArray
    override fun deserialize(decoder: Decoder): JamByteArray {
        val hexString = decoder.decodeString()
        return hexString.hexToJamBytes()
    }

    // Serialize ByteArray to hex string
    override fun serialize(encoder: Encoder, value: JamByteArray) {
        encoder.encodeString(byteArrayToHex(value))
    }

    // Helper function to convert ByteArray to hex string
    private fun byteArrayToHex(bytes: JamByteArray): String {
        return bytes.bytes.joinToString("") { byte -> "%02x".format(byte) }
    }
}

