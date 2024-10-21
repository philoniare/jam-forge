package io.forge.jam.core

// Convert ByteArray to hex string
fun ByteArray.toHex(): String = joinToString("") { "%02x".format(it) }

// Convert hex string to ByteArray
fun String.hexToBytes(): ByteArray {
    val len = length
    require(len % 2 == 0) { "Hex string must have even length" }
    return ByteArray(len / 2) { i ->
        substring(2 * i, 2 * i + 2).toInt(16).toByte()
    }
}

