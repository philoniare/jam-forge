package io.forge.jam.safrole.stats

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeList
import kotlinx.serialization.Serializable

@Serializable
data class StatPi(
    val current: List<StatCount>,
    val last: List<StatCount>
) : Encodable {
    override fun encode(): ByteArray {
        return encodeList(current) + encodeList(last)
    }
}
