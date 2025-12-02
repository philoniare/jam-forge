package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.encodeList
import io.forge.jam.safrole.accumulation.ServiceStatisticsEntry
import kotlinx.serialization.Serializable

@Serializable
data class PreimageState(
    var accounts: List<PreimageAccount>,
    val statistics: List<ServiceStatisticsEntry> = emptyList()
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<PreimageState, Int> {
            var currentOffset = offset
            // accounts - variable size list
            val (accountsLength, accountsLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += accountsLengthBytes
            val accounts = mutableListOf<PreimageAccount>()
            for (i in 0 until accountsLength.toInt()) {
                val (account, accountBytes) = PreimageAccount.fromBytes(data, currentOffset)
                accounts.add(account)
                currentOffset += accountBytes
            }
            // statistics - variable size list
            val (statisticsLength, statisticsLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += statisticsLengthBytes
            val statistics = mutableListOf<ServiceStatisticsEntry>()
            for (i in 0 until statisticsLength.toInt()) {
                val (entry, entryBytes) = ServiceStatisticsEntry.fromBytes(data, currentOffset)
                statistics.add(entry)
                currentOffset += entryBytes
            }
            return Pair(PreimageState(accounts, statistics), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        return encodeList(accounts) + encodeList(statistics)
    }
}
