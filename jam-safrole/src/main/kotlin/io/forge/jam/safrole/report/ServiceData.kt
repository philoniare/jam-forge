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
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<StorageMapEntry, Int> {
            var currentOffset = offset

            // key - compact length + bytes
            val (keyLength, keyLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += keyLengthBytes
            val key = JamByteArray(data.copyOfRange(currentOffset, currentOffset + keyLength.toInt()))
            currentOffset += keyLength.toInt()

            // value - compact length + bytes
            val (valueLength, valueLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += valueLengthBytes
            val value = JamByteArray(data.copyOfRange(currentOffset, currentOffset + valueLength.toInt()))
            currentOffset += valueLength.toInt()

            return Pair(StorageMapEntry(key, value), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val keyBytes = encodeCompactInteger(key.bytes.size.toLong()) + key.bytes
        val valueBytes = encodeCompactInteger(value.bytes.size.toLong()) + value.bytes
        return keyBytes + valueBytes
    }
}

/**
 * Status entry for preimages - used in accumulation state.
 * Contains hash and status list (timeslots) per ASN schema.
 */
@Serializable
data class PreimagesStatusMapEntry(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val hash: JamByteArray,
    val status: List<Long>
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<PreimagesStatusMapEntry, Int> {
            var currentOffset = offset

            // hash - 32 bytes
            val hash = JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32))
            currentOffset += 32

            // status count - compact integer
            val (statusCount, statusCountBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += statusCountBytes

            // status values - 4 bytes each
            val statusValues = mutableListOf<Long>()
            for (i in 0 until statusCount.toInt()) {
                statusValues.add(decodeFixedWidthInteger(data, currentOffset, 4, false))
                currentOffset += 4
            }

            return Pair(PreimagesStatusMapEntry(hash, statusValues), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        // Encode: hash (32 bytes) + status count (compact) + status values (4 bytes each LE)
        val statusCountBytes = encodeCompactInteger(status.size.toLong())
        val statusBytes = status.flatMap { encodeFixedWidthInteger(it, 4, false).toList() }.toByteArray()
        return hash.bytes + statusCountBytes + statusBytes
    }
}

/**
 * ServiceData for Reports STF - only contains service metadata.
 */
@Serializable
data class ServiceData(
    val service: ServiceInfo
) : Encodable {
    companion object {
        const val SIZE = ServiceInfo.SIZE // 89 bytes

        fun fromBytes(data: ByteArray, offset: Int = 0): ServiceData {
            val service = ServiceInfo.fromBytes(data, offset)
            return ServiceData(service)
        }
    }

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
    @SerialName("preimages_blob")
    val preimages: List<PreimageHash> = emptyList(),
    @SerialName("preimages_status")
    val preimagesStatus: List<PreimagesStatusMapEntry> = emptyList()
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<AccumulationServiceData, Int> {
            var currentOffset = offset

            // service - fixed size (89 bytes)
            val service = ServiceInfo.fromBytes(data, currentOffset)
            currentOffset += ServiceInfo.SIZE

            // storage - compact length + variable-size items
            val (storageLength, storageLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += storageLengthBytes
            val storage = mutableListOf<StorageMapEntry>()
            for (i in 0 until storageLength.toInt()) {
                val (entry, entryBytes) = StorageMapEntry.fromBytes(data, currentOffset)
                storage.add(entry)
                currentOffset += entryBytes
            }

            // preimages - compact length + variable-size items
            val (preimagesLength, preimagesLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += preimagesLengthBytes
            val preimages = mutableListOf<PreimageHash>()
            for (i in 0 until preimagesLength.toInt()) {
                val (preimage, preimageBytes) = PreimageHash.fromBytes(data, currentOffset)
                preimages.add(preimage)
                currentOffset += preimageBytes
            }

            // preimagesStatus - compact length + variable-size items
            val (statusLength, statusLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += statusLengthBytes
            val preimagesStatus = mutableListOf<PreimagesStatusMapEntry>()
            for (i in 0 until statusLength.toInt()) {
                val (status, statusBytes) = PreimagesStatusMapEntry.fromBytes(data, currentOffset)
                preimagesStatus.add(status)
                currentOffset += statusBytes
            }

            return Pair(AccumulationServiceData(service, storage, preimages, preimagesStatus), currentOffset - offset)
        }
    }

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
