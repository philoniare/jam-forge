package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.decodeCompactInteger
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
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<CoreStatisticsRecord, Int> {
            var currentOffset = offset
            val (daLoad, daLoadBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += daLoadBytes
            val (popularity, popularityBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += popularityBytes
            val (imports, importsBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += importsBytes
            val (extrinsicCount, extrinsicCountBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += extrinsicCountBytes
            val (extrinsicSize, extrinsicSizeBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += extrinsicSizeBytes
            val (exports, exportsBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += exportsBytes
            val (bundleSize, bundleSizeBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += bundleSizeBytes
            val (gasUsed, gasUsedBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += gasUsedBytes
            return Pair(CoreStatisticsRecord(daLoad, popularity, imports, extrinsicCount, extrinsicSize, exports, bundleSize, gasUsed), currentOffset - offset)
        }
    }

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
