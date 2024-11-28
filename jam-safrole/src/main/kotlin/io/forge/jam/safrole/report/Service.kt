package io.forge.jam.safrole.report

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class Service(
    @SerialName("code_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val codeHash: JamByteArray,
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
) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || this::class != other::class) return false

        other as Service

        if (!codeHash.contentEquals(other.codeHash)) return false

        if (minItemGas != other.minItemGas) return false
        if (minMemoGas != other.minMemoGas) return false
        if (balance != other.balance) return false
        if (codeSize != other.codeSize) return false
        if (items != other.items) return false

        return true
    }

    override fun hashCode(): Int {
        var result = codeHash.contentHashCode()
        result = 31 * result + minItemGas.hashCode()
        result = 31 * result + minMemoGas.hashCode()
        result = 31 * result + balance.hashCode()
        result = 31 * result + codeSize.hashCode()
        result = 31 * result + items.hashCode()
        return result
    }

    override fun toString(): String {
        return "Service(codeHash=${codeHash.contentToString()}, minItemGas=$minItemGas, minMemoGas=$minMemoGas, balance=$balance, codeSize=$codeSize, items=$items)"
    }
}
