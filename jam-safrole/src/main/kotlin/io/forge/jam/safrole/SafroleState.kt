package io.forge.jam.safrole

import io.forge.jam.core.serializers.ByteArrayHexSerializer
import io.forge.jam.core.serializers.ByteArrayListHexSerializer
import io.forge.jam.safrole.serializer.NullableAvailabilityAssignmentListSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class SafroleState(
    // Current timeslot
    var tau: Long,

    @Serializable(with = ByteArrayListHexSerializer::class)
    // Entropy accumulator
    val eta: MutableList<ByteArray> = mutableListOf(),

    // Previous epoch validators
    var lambda: List<ValidatorKey> = emptyList(),

    // Current validators
    var kappa: List<ValidatorKey> = emptyList(),

    // Next epoch validators
    @SerialName("gamma_k")
    var gammaK: List<ValidatorKey> = emptyList(),

    // Queued validators
    val iota: List<ValidatorKey> = emptyList(),

    // Ticket accumulator
    @SerialName("gamma_a")
    var gammaA: List<TicketBody> = emptyList(),

    // Current sealing sequence
    @SerialName("gamma_s")
    var gammaS: TicketsOrKeys = TicketsOrKeys(),

    // Bandersnatch ring root
    @SerialName("gamma_z")
    @Serializable(with = ByteArrayHexSerializer::class)
    var gammaZ: ByteArray = ByteArray(0),


    @Serializable(with = NullableAvailabilityAssignmentListSerializer::class)
    var rho: MutableList<AvailabilityAssignment?>? = null,
    var psi: Psi? = null
) {
    fun deepCopy(): SafroleState {
        return SafroleState(
            tau = tau,
            eta = eta.map { it.clone() }.toMutableList(),
            lambda = lambda.map { it.copy() },
            kappa = kappa.map { it.copy() },
            gammaK = gammaK.map { it.copy() },
            iota = iota.map { it.copy() },
            gammaA = gammaA.map { it.copy() },
            gammaS = gammaS.copy(),
            gammaZ = gammaZ.clone(),
            rho = rho?.map { it?.copy() }?.toMutableList(),
            psi = psi?.copy()
        )
    }
}
