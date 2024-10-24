package io.forge.jam.safrole

import io.forge.jam.core.ByteArrayHexSerializer
import io.forge.jam.core.ByteArrayListHexSerializer
import io.forge.jam.core.Ticket
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class SafroleState(
    val tau: Long,
    @Serializable(with = ByteArrayListHexSerializer::class)
    val eta: List<ByteArray>,
    val lambda: List<ValidatorData>,
    val kappa: List<ValidatorData>,
    @SerialName("gamma_k")
    val gammaK: List<ValidatorData>,
    val iota: List<ValidatorData>,
    @SerialName("gamma_a")
    val gammaA: List<Ticket>,
    @SerialName("gamma_s")
    val gammaS: TicketsOrKeys,
    @SerialName("gamma_z")
    @Serializable(with = ByteArrayHexSerializer::class)
    val gammaZ: ByteArray
)
