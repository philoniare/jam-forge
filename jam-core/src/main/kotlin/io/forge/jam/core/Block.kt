package io.forge.jam.core

import kotlinx.serialization.Serializable

@Serializable
data class Block(
    val header: Header,
    val extrinsic: Extrinsic
) {
    fun encode(): ByteArray {
        val headerBytes = header.encode()
        val extrinsicBytes = extrinsic.encode()
        return headerBytes + extrinsicBytes
    }
}
