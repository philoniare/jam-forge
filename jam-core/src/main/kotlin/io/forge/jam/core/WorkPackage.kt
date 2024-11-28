package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class WorkPackage(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val authorization: JamByteArray,
    @SerialName("auth_code_host")
    val authCodeHost: Long,
    val authorizer: WorkAuthorizer,
    val context: Context,
    val items: List<WorkItem>,
) : Encodable {
    override fun encode(): ByteArray {
        val authorizationLengthBytes = encodeFixedWidthInteger(authorization.size, 1, false)
        val workItemsBytes = encodeList(items)
        val authCodeHostBytes = encodeFixedWidthInteger(authCodeHost, 4, false)
        return authorizationLengthBytes + authorization.bytes + authCodeHostBytes + authorizer.encode() + context.encode() + workItemsBytes
    }
}

