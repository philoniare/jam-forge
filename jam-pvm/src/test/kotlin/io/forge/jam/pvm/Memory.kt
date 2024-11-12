package io.forge.jam.pvm

import kotlinx.serialization.Serializable

@Serializable
data class Memory(val address: Int, val contents: UByteArray)
