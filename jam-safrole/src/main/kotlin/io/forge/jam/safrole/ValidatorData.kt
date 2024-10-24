package io.forge.jam.safrole

import io.forge.jam.core.ByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
class ValidatorData(
    @Serializable(with = ByteArrayHexSerializer::class)
    val bandersnatch: ByteArray,
    @Serializable(with = ByteArrayHexSerializer::class)
    val ed25519: ByteArray,
    @Serializable(with = ByteArrayHexSerializer::class)
    val bls: ByteArray,
    @Serializable(with = ByteArrayHexSerializer::class)
    val metadata: ByteArray
)
