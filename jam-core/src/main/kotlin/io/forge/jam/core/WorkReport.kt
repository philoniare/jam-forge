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
        val discriminatorBytes = byteArrayOf(69, 0)
        val coreIndexBytes = encodeFixedWidthInteger(coreIndex, 2, false)
        val authorizerHashBytes = authorizerHash.bytes
        val authOutputLengthBytes = encodeFixedWidthInteger(authOutput.size, 1, false)
        val authOutputBytes = authOutput.bytes
        val segmentRootLookupBytes = encodeList(segmentRootLookup)
        val resultsBytes = encodeList(results)
        return packageSpecBytes + discriminatorBytes + contextBytes + coreIndexBytes + authorizerHashBytes + authOutputLengthBytes +
            authOutputBytes + segmentRootLookupBytes + resultsBytes
    }
}
