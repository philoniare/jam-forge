package io.forge.jam.safrole.safrole

import io.forge.jam.core.*
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import io.forge.jam.safrole.*
import io.forge.jam.safrole.serializer.NullableAvailabilityAssignmentListSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class SafroleState(
    // Current timeslot
    var tau: Long,

    // Entropy accumulator
    val eta: JamByteArrayList = JamByteArrayList(),

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
    @Serializable(with = JamByteArrayHexSerializer::class)
    var gammaZ: JamByteArray = JamByteArray(ByteArray(0)),

    @Serializable(with = NullableAvailabilityAssignmentListSerializer::class)
    var rho: MutableList<AvailabilityAssignment?>? = null,
    var psi: Psi? = null
) : Encodable {
    fun deepCopy(): SafroleState {
        return SafroleState(
            tau = tau,
            eta = eta.clone(),
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

    override fun encode(): ByteArray {
        val tauBytes = encodeFixedWidthInteger(tau, 4, true)
        val etaBytes = encodeList(eta.toList())
        val lambdaBytes = encodeList(lambda)
        val kappaBytes = encodeList(kappa)
        val gammaKBytes = encodeList(gammaK)
        val iotaBytes = encodeList(iota)
        val gammaABytes = encodeList(gammaA)
        val gammaSBytes = gammaS.encode()
        val gammaZBytes = gammaZ.bytes
        val rhoBytes = rho?.let { encodeList(it) } ?: byteArrayOf(0)
        val psiBytes = psi?.encode() ?: byteArrayOf(0)
        return tauBytes + etaBytes + lambdaBytes + kappaBytes + gammaKBytes + iotaBytes + gammaABytes + gammaSBytes + gammaZBytes + rhoBytes + psiBytes
    }
}
