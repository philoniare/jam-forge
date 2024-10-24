package io.forge.jam.safrole

import io.forge.jam.core.ByteArrayHexSerializer
import io.forge.jam.core.Ticket
import kotlinx.serialization.Serializable

@Serializable
data class SafroleInput(
    val slot: Long,
    @Serializable(with = ByteArrayHexSerializer::class)
    val entropy: ByteArray,
    val extrinsic: List<Ticket>
)
