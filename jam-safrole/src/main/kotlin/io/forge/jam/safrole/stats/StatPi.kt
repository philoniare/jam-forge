package io.forge.jam.safrole.stats

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeList
import kotlinx.serialization.Serializable

@Serializable
data class StatPi(
    var current: List<StatCount>,
    var last: List<StatCount>
) : Encodable {
    fun copy() = StatPi(
        current = current.map { it.copy() },
        last = last.map { it.copy() }
    )

    override fun encode(): ByteArray {
        return encodeList(current) + encodeList(last)
    }
}
