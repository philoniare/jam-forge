package io.forge.jam.safrole.report

import io.forge.jam.core.serializers.ByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class Service(
    @SerialName("code_hash")
    @Serializable(with = ByteArrayHexSerializer::class)
    val codeHash: ByteArray,
    @SerialName("min_item_gas")
    val minItemGas: Long,
    @SerialName("min_memo_gas")
    val minMemoGas: Long,
    @SerialName("balance")
    val balance: Long,
    @SerialName("code_size")
    val codeSize: Long,
    @SerialName("items")
    val items: Long,
)
