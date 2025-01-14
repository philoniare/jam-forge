package io.forge.jam.safrole.stats

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.encodeList
import io.forge.jam.safrole.ValidatorKey
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class StatState(
    val pi: StatPi,
    val tau: Long,
    @SerialName("kappa_prime")
    val kappaPrime: List<ValidatorKey>,
) : Encodable {
    override fun encode(): ByteArray {
        val tauBytes = encodeFixedWidthInteger(tau, 4, false)
        return pi.encode() + tauBytes + encodeList(kappaPrime)
    }
}
