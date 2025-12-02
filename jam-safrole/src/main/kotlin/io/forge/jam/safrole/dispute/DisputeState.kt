package io.forge.jam.safrole.safrole

import io.forge.jam.core.Encodable
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.encodeList
import io.forge.jam.core.encodeOptionalList
import io.forge.jam.safrole.AvailabilityAssignment
import io.forge.jam.safrole.Psi
import io.forge.jam.safrole.ValidatorKey
import io.forge.jam.safrole.serializer.NullableAvailabilityAssignmentListSerializer
import kotlinx.serialization.Serializable

@Serializable
data class DisputeState(
    var psi: Psi? = null,
    @Serializable(with = NullableAvailabilityAssignmentListSerializer::class)
    var rho: List<AvailabilityAssignment?> = emptyList(),
    // Current timeslot
    var tau: Long,

    // Current validators
    var kappa: List<ValidatorKey> = emptyList(),
    // Previous epoch validators
    var lambda: List<ValidatorKey> = emptyList(),

    ) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0, coresCount: Int, validatorsCount: Int): Pair<DisputeState, Int> {
            var currentOffset = offset

            // psi - always decode as Psi (test vectors have psi as object, never null in JSON)
            val (psi, psiBytes) = Psi.fromBytes(data, currentOffset)
            currentOffset += psiBytes

            // rho - fixed size outer (coresCount), optional inner items
            val rho = mutableListOf<AvailabilityAssignment?>()
            for (i in 0 until coresCount) {
                val optionByte = data[currentOffset].toInt() and 0xFF
                currentOffset += 1
                if (optionByte == 0) {
                    rho.add(null)
                } else {
                    val (assignment, assignmentBytes) = AvailabilityAssignment.fromBytes(data, currentOffset)
                    currentOffset += assignmentBytes
                    rho.add(assignment)
                }
            }

            // tau - 4 bytes
            val tau = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4

            // kappa - fixed size (validatorsCount)
            val kappa = mutableListOf<ValidatorKey>()
            for (i in 0 until validatorsCount) {
                kappa.add(ValidatorKey.fromBytes(data, currentOffset))
                currentOffset += ValidatorKey.SIZE
            }

            // lambda - fixed size (validatorsCount)
            val lambda = mutableListOf<ValidatorKey>()
            for (i in 0 until validatorsCount) {
                lambda.add(ValidatorKey.fromBytes(data, currentOffset))
                currentOffset += ValidatorKey.SIZE
            }

            return Pair(DisputeState(psi, rho, tau, kappa, lambda), currentOffset - offset)
        }
    }
    fun deepCopy(): SafroleState {
        return SafroleState(
            psi = psi?.copy(),
            rho = rho.map { it?.copy() }.toMutableList(),
            tau = tau,
            kappa = kappa.map { it.copy() },
            lambda = lambda.map { it.copy() },
        )
    }

    override fun encode(): ByteArray {
        val psiBytes = psi?.encode() ?: byteArrayOf(0)
        val rhoBytes = encodeOptionalList(rho, false)
        val tauBytes = encodeFixedWidthInteger(tau, 4, false)
        val kappaBytes = encodeList(kappa, false)
        val lambdaBytes = encodeList(lambda, false)
        return psiBytes + rhoBytes + tauBytes + kappaBytes + lambdaBytes
    }
}
