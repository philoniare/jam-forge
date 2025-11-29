package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeCompactInteger
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class CoreStatisticsRecord(
    @SerialName("da_load")
    val daLoad: Long = 0,
    val popularity: Long = 0,
    val imports: Long = 0,
    @SerialName("extrinsic_count")
    val extrinsicCount: Long = 0,
    @SerialName("extrinsic_size")
    val extrinsicSize: Long = 0,
    val exports: Long = 0,
    @SerialName("bundle_size")
    val bundleSize: Long = 0,
    @SerialName("gas_used")
    val gasUsed: Long = 0
) : Encodable {
    override fun encode(): ByteArray {
        // All statistics use compact integer encoding
        return encodeCompactInteger(daLoad) +
            encodeCompactInteger(popularity) +
            encodeCompactInteger(imports) +
            encodeCompactInteger(extrinsicCount) +
            encodeCompactInteger(extrinsicSize) +
            encodeCompactInteger(exports) +
            encodeCompactInteger(bundleSize) +
            encodeCompactInteger(gasUsed)
    }
}
