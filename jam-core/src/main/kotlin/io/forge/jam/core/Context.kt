package io.forge.jam.core

import io.forge.jam.core.serializers.ByteArrayHexSerializer
import io.forge.jam.core.serializers.ByteArrayListHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class Context(
    @Serializable(with = ByteArrayHexSerializer::class)
    val anchor: ByteArray,
    @Serializable(with = ByteArrayHexSerializer::class)
    @SerialName("state_root")
    val stateRoot: ByteArray,
    @Serializable(with = ByteArrayHexSerializer::class)
    @SerialName("beefy_root")
    val beefyRoot: ByteArray,
    @Serializable(with = ByteArrayHexSerializer::class)
    @SerialName("lookup_anchor")
    val lookupAnchor: ByteArray,
    @SerialName("lookup_anchor_slot")
    val lookupAnchorSlot: Long,
    @Serializable(with = ByteArrayListHexSerializer::class)
    val prerequisites: List<EncodableByteArray>
) : Encodable {
    override fun encode(): ByteArray {
        val anchorBytes = anchor
        val stateRootBytes = stateRoot
        val beefyRootBytes = beefyRoot
        val lookupAnchorBytes = lookupAnchor
        val lookupAnchorSlotBytes = encodeFixedWidthInteger(lookupAnchorSlot, 4, false)
        val prerequisiteBytes = encodeList(prerequisites)
        return anchorBytes + stateRootBytes + beefyRootBytes + lookupAnchorBytes + lookupAnchorSlotBytes + prerequisiteBytes
    }
}
