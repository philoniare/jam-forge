package io.forge.jam.safrole

import io.forge.jam.core.ByteArrayHexSerializer
import io.forge.jam.core.ByteArrayListHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class SafroleState(
    // Current timeslot
    var tau: Long,

    @Serializable(with = ByteArrayListHexSerializer::class)
    // Entropy accumulator
    val eta: MutableList<ByteArray>,

    // Previous epoch validators
    var lambda: List<ValidatorKey>,

    // Current validators
    var kappa: List<ValidatorKey>,

    // Next epoch validators
    @SerialName("gamma_k")
    var gammaK: List<ValidatorKey>,

    // Queued validators
    val iota: List<ValidatorKey>,

    // Ticket accumulator
    @SerialName("gamma_a")
    var gammaA: List<TicketBody>,

    // Current sealing sequence
    @SerialName("gamma_s")
    var gammaS: TicketsOrKeys,

    // Bandersnatch ring root
    @SerialName("gamma_z")
    @Serializable(with = ByteArrayHexSerializer::class)
    var gammaZ: ByteArray
)
