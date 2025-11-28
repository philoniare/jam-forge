package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class WorkPackage(
    @SerialName("auth_code_host")
    val authCodeHost: Long,
    @SerialName("auth_code_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val authCodeHash: JamByteArray,
    val context: Context,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val authorization: JamByteArray,
    @SerialName("authorizer_config")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val authorizerConfig: JamByteArray,
    val items: List<WorkItem>,
) : Encodable {
    override fun encode(): ByteArray {
        val authCodeHostBytes = encodeFixedWidthInteger(authCodeHost, 4, false)
        val authorizationLengthBytes = encodeCompactInteger(authorization.size.toLong())
        val authorizerConfigLengthBytes = encodeCompactInteger(authorizerConfig.size.toLong())
        val workItemsBytes = encodeList(items)
        return authCodeHostBytes + authCodeHash.bytes + context.encode() + authorizationLengthBytes + authorization.bytes + authorizerConfigLengthBytes + authorizerConfig.bytes + workItemsBytes
    }
}

