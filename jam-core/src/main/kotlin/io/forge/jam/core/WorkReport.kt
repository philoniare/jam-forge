package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class WorkReport(
    @SerialName("package_spec")
    val packageSpec: PackageSpec,
    val context: Context,
    @SerialName("core_index")
    val coreIndex: Long,
    @SerialName("authorizer_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val authorizerHash: JamByteArray,
    @SerialName("auth_output")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val authOutput: JamByteArray,
    @SerialName("segment_root_lookup")
    val segmentRootLookup: List<SegmentRootLookup>,
    val results: List<WorkResult>
) : Encodable {
    override fun encode(): ByteArray {
        val packageSpecBytes = packageSpec.encode()
        val contextBytes = context.encode()
        val coreIndexBytes = encodeFixedWidthInteger(coreIndex, 2, false)
        val authorizerHashBytes = authorizerHash.bytes
        // Auth output - 1 byte length prefix
        val authOutputLengthBytes = encodeCompactInteger(authOutput.size.toLong())
        val authOutputBytes = authOutput.bytes
        // Segment root lookup - 1 byte length prefix
        val segmentRootLookupBytes = encodeFixedWidthInteger(segmentRootLookup.size.toLong(), 1, false)
        val segmentRootLookupListBytes = encodeList(segmentRootLookup, false)
        

        // Results - 1 byte length prefix
        val resultsLengthBytes = encodeFixedWidthInteger(results.size.toLong(), 1, false)
        val resultsListBytes = encodeList(results, false)
        return packageSpecBytes +
            contextBytes +
            coreIndexBytes +
            authorizerHashBytes +
            authOutputLengthBytes + authOutputBytes +
            segmentRootLookupBytes + segmentRootLookupListBytes +
            resultsLengthBytes + resultsListBytes
    }
}
