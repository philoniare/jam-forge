package io.forge.jam.safrole

import io.forge.jam.core.JamErrorCode
import io.forge.jam.core.JamErrorCodeSerializer
import kotlinx.serialization.Serializable

@Serializable
data class SafroleOutput(
    val ok: OutputMarks? = null,
    @Serializable(with = JamErrorCodeSerializer::class)
    val err: JamErrorCode? = null
)
