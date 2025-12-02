package io.forge.jam.safrole.report

import io.forge.jam.core.*
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import io.forge.jam.safrole.preimage.PreimageHash
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class StorageMapEntry(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val key: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val value: JamByteArray
) : Encodable {
    override fun encode(): ByteArray {
        val keyBytes = encodeCompactInteger(key.bytes.size.toLong()) + key.bytes
        val valueBytes = encodeCompactInteger(value.bytes.size.toLong()) + value.bytes
        return keyBytes + valueBytes
    }
}

@Serializable
data class PreimageStatusKey(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val hash: JamByteArray,
    val length: Int
)

@Serializable
data class PreimagesStatusMapEntry(
    val key: PreimageStatusKey,
    val value: List<Long>
) : Encodable {
    // Convenience accessors
    val hash: JamByteArray get() = key.hash
    val length: Int get() = key.length
    val status: List<Long> get() = value

    override fun encode(): ByteArray {
        // Encode: hash (32 bytes) + length (4 bytes LE) + status count (1 byte) + status values (4 bytes each LE)
        val lengthBytes = encodeFixedWidthInteger(key.length, 4, false)
        val statusBytes = value.flatMap { encodeFixedWidthInteger(it, 4, false).toList() }.toByteArray()
        return key.hash.bytes + lengthBytes + byteArrayOf(value.size.toByte()) + statusBytes
    }
}

/**
 * ServiceData for Reports STF - only contains service metadata.
 */
@Serializable
data class ServiceData(
    val service: ServiceInfo
) : Encodable {
    override fun encode(): ByteArray {
        return service.encode()
    }
}

/**
 * ServiceData for Accumulation STF - contains full account data.
 * Used by AccumulationState where the ASN schema defines Account with
 * service, storage, preimages-blob, and preimages-status.
 */
@Serializable
data class AccumulationServiceData(
    val service: ServiceInfo,
    val storage: List<StorageMapEntry> = emptyList(),
    @SerialName("preimage_blobs")
    val preimages: List<PreimageHash> = emptyList(),
    @SerialName("preimage_requests")
    val preimagesStatus: List<PreimagesStatusMapEntry> = emptyList()
) : Encodable {
    override fun encode(): ByteArray {
        val storageBytes = encodeList(storage)
        val preimagesBytes = encodeList(preimages)
        val statusBytes = encodeList(preimagesStatus)
        return service.encode() + storageBytes + preimagesBytes + statusBytes
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is AccumulationServiceData) return false

        return service == other.service &&
            storage == other.storage &&
            preimages == other.preimages &&
            preimagesStatus == other.preimagesStatus
    }

    override fun hashCode(): Int {
        var result = service.hashCode()
        result = 31 * result + storage.hashCode()
        result = 31 * result + preimages.hashCode()
        result = 31 * result + preimagesStatus.hashCode()
        return result
    }
}
