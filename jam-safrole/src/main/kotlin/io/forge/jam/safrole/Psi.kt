package io.forge.jam.safrole

import io.forge.jam.core.serializers.ByteArrayListHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class Psi(
    @SerialName("psi_g")
    @Serializable(with = ByteArrayListHexSerializer::class)
    val psiG: List<ByteArray>,

    @SerialName("psi_b")
    @Serializable(with = ByteArrayListHexSerializer::class)
    val psiB: List<ByteArray>,

    @SerialName("psi_w")
    @Serializable(with = ByteArrayListHexSerializer::class)
    val psiW: List<ByteArray>,

    @SerialName("psi_o")
    @Serializable(with = ByteArrayListHexSerializer::class)
    val psiO: List<ByteArray>,
)
