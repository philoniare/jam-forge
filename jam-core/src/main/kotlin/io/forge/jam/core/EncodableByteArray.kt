package io.forge.jam.core

import kotlinx.serialization.Serializable

@Serializable
data class EncodableByteArray(val bytes: ByteArray) : Encodable {
    override fun encode(): ByteArray = bytes
}
