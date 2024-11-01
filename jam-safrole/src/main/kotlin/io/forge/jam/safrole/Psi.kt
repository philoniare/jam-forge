package io.forge.jam.safrole

import io.forge.jam.core.ByteArrayList
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class Psi(
    // Good/Valid Reports
    @SerialName("psi_g")
    val psiG: ByteArrayList = ByteArrayList(),

    // Bad/Invalid Reports
    @SerialName("psi_b")
    val psiB: ByteArrayList = ByteArrayList(),

    // Wonky/Unknown Reports
    @SerialName("psi_w")
    val psiW: ByteArrayList = ByteArrayList(),

    // Offending validators
    @SerialName("psi_o")
    val psiO: ByteArrayList = ByteArrayList(),
) {

    fun copy(): Psi = Psi(
        psiG = ByteArrayList().apply { addAll(psiG) },
        psiB = ByteArrayList().apply { addAll(psiB) },
        psiW = ByteArrayList().apply { addAll(psiW) },
        psiO = ByteArrayList().apply { addAll(psiO) }
    )
}
