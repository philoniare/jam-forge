package io.forge.jam.pvm

import kotlinx.serialization.Serializable

@Serializable
data class Memory(val address: UInt, val contents: List<UInt>)
