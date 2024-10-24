package io.forge.jam.safrole

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class SafroleCase(
    val input: SafroleInput,
    @SerialName("pre_state")
    val preState: SafroleState,
    val output: SafroleOutput,
    @SerialName("post_state")
    val postState: SafroleState
)
