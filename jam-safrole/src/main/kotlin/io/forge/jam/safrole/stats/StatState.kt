package io.forge.jam.safrole.stats

import io.forge.jam.core.Encodable
import io.forge.jam.core.decodeFixedWidthInteger
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
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0, validatorsCount: Int): Pair<StatState, Int> {
            var currentOffset = offset

            // valsCurrStats - fixed size list (validatorsCount)
            val valsCurrStats = mutableListOf<StatCount>()
            for (i in 0 until validatorsCount) {
                valsCurrStats.add(StatCount.fromBytes(data, currentOffset))
                currentOffset += StatCount.SIZE
            }

            // valsLastStats - fixed size list (validatorsCount)
            val valsLastStats = mutableListOf<StatCount>()
            for (i in 0 until validatorsCount) {
                valsLastStats.add(StatCount.fromBytes(data, currentOffset))
                currentOffset += StatCount.SIZE
            }

            // slot - 4 bytes
            val slot = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4

            // currValidators - fixed size list (validatorsCount)
            val currValidators = mutableListOf<ValidatorKey>()
            for (i in 0 until validatorsCount) {
                currValidators.add(ValidatorKey.fromBytes(data, currentOffset))
                currentOffset += ValidatorKey.SIZE
            }

            return Pair(StatState(valsCurrStats, valsLastStats, slot, currValidators), currentOffset - offset)
        }
    }

    fun copy() = StatState(
        valsCurrStats = valsCurrStats.map { it.copy() },
        valsLastStats = valsLastStats.map { it.copy() },
        slot = slot,
        currValidators = currValidators.map { it.copy() }
    )

    override fun encode(): ByteArray {
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        return encodeList(valsCurrStats, false) + encodeList(valsLastStats, false) + slotBytes + encodeList(currValidators, false)
    }
}
