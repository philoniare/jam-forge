package io.forge.jam.safrole.report

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
class ServiceInfo(
    @SerialName("code_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val codeHash: JamByteArray,

    val balance: Long,
    @SerialName("min_item_gas")
    val minItemGas: Long,
    @SerialName("min_memo_gas")
    val minMemoGas: Long,
    val bytes: Long,
    val items: Int
) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is ServiceInfo) return false

        return codeHash == other.codeHash &&
            balance == other.balance &&
            minItemGas == other.minItemGas &&
            minMemoGas == other.minMemoGas &&
            bytes == other.bytes &&
            items == other.items
    }

    override fun hashCode(): Int {
        var result = codeHash.hashCode()
        result = 31 * result + balance.hashCode()
        result = 31 * result + minItemGas.hashCode()
        result = 31 * result + minMemoGas.hashCode()
        result = 31 * result + bytes.hashCode()
        result = 31 * result + items.hashCode()
        return result
    }

    override fun toString(): String {
        return "ServiceInfo(codeHash=$codeHash, balance=$balance, minItemGas=$minItemGas, minMemoGas=$minMemoGas, bytes=$bytes, items=$items)"
    }

    fun copy(
        codeHash: JamByteArray = this.codeHash,
        balance: Long = this.balance,
        minItemGas: Long = this.minItemGas,
        minMemoGas: Long = this.minMemoGas,
        bytes: Long = this.bytes,
        items: Int = this.items
    ): ServiceInfo {
        return ServiceInfo(codeHash, balance, minItemGas, minMemoGas, bytes, items)
    }
}
