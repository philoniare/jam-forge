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
    @SerialName("auth_gas_used")
    val authGasUsed: Long,
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
        // CoreIndex uses compact integer encoding
        val coreIndexBytes = encodeCompactInteger(coreIndex)
        val authorizerHashBytes = authorizerHash.bytes
        // Auth gas used - compact integer encoding
        val authGasUsedBytes = encodeCompactInteger(authGasUsed)
        // Auth output is ByteSequence - variable length with compact integer length prefix
        val authOutputLengthBytes = encodeCompactInteger(authOutput.size.toLong())
        val authOutputBytes = authOutput.bytes
        // Segment root lookup is SEQUENCE OF - variable length with compact integer length prefix
        val segmentRootLookupBytes = encodeCompactInteger(segmentRootLookup.size.toLong())
        val segmentRootLookupListBytes = encodeList(segmentRootLookup, false)

        // Results is SEQUENCE (SIZE(1..16)) - variable size, needs compact integer length
        val resultsLengthBytes = encodeCompactInteger(results.size.toLong())
        val resultsListBytes = encodeList(results, false)
        return packageSpecBytes +
            contextBytes +
            coreIndexBytes +
            authorizerHashBytes +
            authGasUsedBytes +
            authOutputLengthBytes + authOutputBytes +
            segmentRootLookupBytes + segmentRootLookupListBytes +
            resultsLengthBytes + resultsListBytes
    }
}
