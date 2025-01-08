package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import io.forge.jam.core.serializers.JamByteArrayMapSerializer
import kotlinx.serialization.Serializable

@Serializable
data class TrieTestVector(
    @Serializable(with = JamByteArrayMapSerializer::class)
    val input: Map<JamByteArray, JamByteArray> = emptyMap(),
    @Serializable(with = JamByteArrayHexSerializer::class)
    val output: JamByteArray
)
