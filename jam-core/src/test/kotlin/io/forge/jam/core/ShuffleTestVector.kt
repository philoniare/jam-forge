package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class ShuffleTestVector(
    val input: Int,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val entropy: JamByteArray,
    val output: List<Int>
)
