package io.forge.jam.pvm

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class PageMap(
    val address: UInt,
    val length: UInt,
    @SerialName("is-writable") val isWritable: Boolean,
)
