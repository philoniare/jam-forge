package io.forge.jam.safrole.stats

import io.forge.jam.core.Encodable
import kotlinx.serialization.Serializable

@Serializable
data class StatOutput(val id: Long) : Encodable {
    override fun encode(): ByteArray {
        return byteArrayOf(0)
    }
}
