package io.forge.jam.safrole.stats

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeFixedWidthInteger
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class StatCount(
    val blocks: Long,
    val tickets: Long,
    @SerialName("pre_images")
    val preImages: Long,
    @SerialName("pre_images_size")
    val preImagesSize: Long,
    val guarantees: Long,
    val assurances: Long
) : Encodable {
    override fun encode(): ByteArray {
        val blocksBytes = encodeFixedWidthInteger(blocks, 4, false)
        val ticketsBytes = encodeFixedWidthInteger(tickets, 4, false)
        val preImagesBytes = encodeFixedWidthInteger(preImages, 4, false)
        val preImagesSizeBytes = encodeFixedWidthInteger(preImagesSize, 4, false)
        val guaranteesBytes = encodeFixedWidthInteger(guarantees, 4, false)
        val assurancesBytes = encodeFixedWidthInteger(assurances, 4, false)
        return blocksBytes + ticketsBytes + preImagesBytes + preImagesSizeBytes + guaranteesBytes + assurancesBytes
    }
}
