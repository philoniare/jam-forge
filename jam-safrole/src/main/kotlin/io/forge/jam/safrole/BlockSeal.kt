package io.forge.jam.safrole

enum class SealType {
    Ticket,
    Fallback
}

data class BlockSeal(
    val signature: ByteArray,
    val sealType: SealType
)
