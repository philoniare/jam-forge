package io.forge.jam.safrole

import kotlinx.serialization.Serializable

@Serializable
data class SafroleOutput(
    val ok: OutputMarks? = null,
    val err: CustomErrorCode? = null
)
