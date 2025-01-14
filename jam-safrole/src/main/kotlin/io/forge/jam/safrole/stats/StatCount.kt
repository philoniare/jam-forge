package io.forge.jam.safrole.stats

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeFixedWidthInteger
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class StatCount(
    var blocks: Long = 0,
    var tickets: Long = 0,
    @SerialName("pre_images")
    var preImages: Long = 0,
    @SerialName("pre_images_size")
    var preImagesSize: Long = 0,
    var guarantees: Long = 0,
    var assurances: Long = 0
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
