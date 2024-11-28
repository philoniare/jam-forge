package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class WorkItemImportSegment(
    @Serializable(with = JamByteArrayHexSerializer::class)
    @SerialName("tree_root")
    val treeRoot: JamByteArray,
    val index: Long
) : Encodable {
    override fun encode(): ByteArray {
        val indexBytes = encodeFixedWidthInteger(index, 2, false)
        return treeRoot.bytes + indexBytes
    }
}
