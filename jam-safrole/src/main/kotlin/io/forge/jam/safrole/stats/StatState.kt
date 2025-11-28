package io.forge.jam.safrole.stats

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.encodeList
import io.forge.jam.safrole.ValidatorKey
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class StatState(
    @SerialName("vals_curr_stats")
    var valsCurrStats: List<StatCount>,
    @SerialName("vals_last_stats")
    var valsLastStats: List<StatCount>,
    val slot: Long,
    @SerialName("curr_validators")
    val currValidators: List<ValidatorKey>,
) : Encodable {
    fun copy() = StatState(
        valsCurrStats = valsCurrStats.map { it.copy() },
        valsLastStats = valsLastStats.map { it.copy() },
        slot = slot,
        currValidators = currValidators.map { it.copy() }
    )

    override fun encode(): ByteArray {
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        return encodeList(valsCurrStats) + encodeList(valsLastStats) + slotBytes + encodeList(currValidators)
    }
}
