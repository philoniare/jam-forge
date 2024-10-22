package io.forge.jam.core

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class WorkItemImportSegment(
    @Serializable(with = ByteArrayHexSerializer::class)
    @SerialName("tree_root")
    val treeRoot: ByteArray,
    val index: Long
) : Encodable {
    override fun encode(): ByteArray {
        val indexBytes = encodeFixedWidthInteger(index, 2, false)
        return treeRoot + indexBytes
    }
}
