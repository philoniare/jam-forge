package io.forge.jam.safrole.traces

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.blakeHash

/**
 * State key encoding/decoding for JAM protocol.
 *
 * State keys are 31 bytes and identify which state component a value belongs to.
 * The first byte identifies the component type, with remaining bytes used for
 * component-specific indexing.
 */
object StateKeys {
    // State component prefixes (byte 0)
    const val CORE_AUTHORIZATION_POOL: Byte = 1   // φc - core authorizations
    const val AUTHORIZATION_QUEUE: Byte = 2       // φ queue
    const val RECENT_HISTORY: Byte = 3            // β - recent blocks
    const val SAFROLE_STATE: Byte = 4             // γ - safrole gamma state
    const val JUDGEMENTS: Byte = 5                // ψ - judgements
    const val ENTROPY_POOL: Byte = 6              // η - entropy accumulator
    const val VALIDATOR_QUEUE: Byte = 7           // ι - pending validators
    const val CURRENT_VALIDATORS: Byte = 8        // κ - current validators
    const val PREVIOUS_VALIDATORS: Byte = 9       // λ - previous validators
    const val REPORTS: Byte = 10                  // ρ - pending reports
    const val TIMESLOT: Byte = 11                 // τ - current timeslot
    const val PRIVILEGED_SERVICES: Byte = 12      // χ - privileged services
    const val ACTIVITY_STATISTICS: Byte = 13      // activity stats
    const val ACCUMULATION_QUEUE: Byte = 14       // accumulation queue
    const val ACCUMULATION_HISTORY: Byte = 15     // accumulation history
    const val LAST_ACCUMULATION_OUTPUTS: Byte = 16 // last accumulation outputs
    const val SERVICE_STATISTICS: Byte = 17       // service statistics
    const val SERVICE_ACCOUNT: Byte = -1          // 255 (0xFF) - service account details (δ)

    /**
     * Identifies the state component from a 31-byte key.
     */
    fun getComponent(key: JamByteArray): StateComponent {
        require(key.size == 31) { "State key must be 31 bytes, got ${key.size}" }
        val firstByte = key.bytes[0]

        return when (firstByte) {
            CORE_AUTHORIZATION_POOL -> StateComponent.CoreAuthorizationPool
            AUTHORIZATION_QUEUE -> StateComponent.AuthorizationQueue
            RECENT_HISTORY -> StateComponent.RecentHistory
            SAFROLE_STATE -> StateComponent.SafroleState
            JUDGEMENTS -> StateComponent.Judgements
            ENTROPY_POOL -> StateComponent.EntropyPool
            VALIDATOR_QUEUE -> StateComponent.ValidatorQueue
            CURRENT_VALIDATORS -> StateComponent.CurrentValidators
            PREVIOUS_VALIDATORS -> StateComponent.PreviousValidators
            REPORTS -> StateComponent.Reports
            TIMESLOT -> StateComponent.Timeslot
            PRIVILEGED_SERVICES -> StateComponent.PrivilegedServices
            ACTIVITY_STATISTICS -> StateComponent.ActivityStatistics
            ACCUMULATION_QUEUE -> StateComponent.AccumulationQueue
            ACCUMULATION_HISTORY -> StateComponent.AccumulationHistory
            LAST_ACCUMULATION_OUTPUTS -> StateComponent.LastAccumulationOutputs
            SERVICE_STATISTICS -> StateComponent.ServiceStatistics
            SERVICE_ACCOUNT -> StateComponent.ServiceAccount(extractServiceIndex255(key))
            else -> {
                // Service storage/preimage keys have service ID interleaved
                val serviceIndex = extractServiceIndexInterleaved(key)
                StateComponent.ServiceData(serviceIndex)
            }
        }
    }

    /**
     * Creates a simple state key with just the component prefix.
     */
    fun simpleKey(component: Byte): JamByteArray {
        val data = ByteArray(31)
        data[0] = component
        return JamByteArray(data)
    }

    /**
     * Creates a service account key (prefix 255).
     * Service index bytes are interleaved at positions 1, 3, 5, 7.
     */
    fun serviceAccountKey(serviceIndex: Int): JamByteArray {
        val data = ByteArray(31)
        data[0] = SERVICE_ACCOUNT
        // Service index in little-endian, interleaved at positions 1, 3, 5, 7
        data[1] = (serviceIndex and 0xFF).toByte()
        data[3] = ((serviceIndex shr 8) and 0xFF).toByte()
        data[5] = ((serviceIndex shr 16) and 0xFF).toByte()
        data[7] = ((serviceIndex shr 24) and 0xFF).toByte()
        return JamByteArray(data)
    }

    /**
     * Creates a service storage key.
     * Uses UInt32.MAX as discriminator, with hash of data.
     */
    fun serviceStorageKey(serviceIndex: Int, storageKey: JamByteArray): JamByteArray {
        return serviceDataKey(serviceIndex, UInt.MAX_VALUE, storageKey)
    }

    /**
     * Creates a service preimage key.
     * Uses UInt32.MAX - 1 as discriminator, with hash of preimage.
     */
    fun servicePreimageKey(serviceIndex: Int, preimageHash: JamByteArray): JamByteArray {
        return serviceDataKey(serviceIndex, UInt.MAX_VALUE - 1u, preimageHash)
    }

    /**
     * Creates a service preimage info key.
     * Uses preimage length as discriminator, with hash of preimage.
     */
    fun servicePreimageInfoKey(serviceIndex: Int, length: Int, preimageHash: JamByteArray): JamByteArray {
        return serviceDataKey(serviceIndex, length.toUInt(), preimageHash)
    }

    /**
     * Creates a service data key with discriminator and data hash.
     * Structure: interleave service index bytes with hash bytes, then append remaining hash.
     */
    private fun serviceDataKey(serviceIndex: Int, discriminator: UInt, data: JamByteArray): JamByteArray {
        // Encode discriminator as compact integer + data
        val discriminatorBytes = encodeUInt32LE(discriminator)
        val combined = discriminatorBytes + data.bytes
        val hash = blakeHash(combined)

        // Build 31-byte key
        val stateKey = ByteArray(31)

        // Interleave first 4 bytes of service index with first 4 bytes of hash
        val serviceBytes = encodeUInt32LE(serviceIndex.toUInt())
        stateKey[0] = serviceBytes[0]
        stateKey[1] = hash[0]
        stateKey[2] = serviceBytes[1]
        stateKey[3] = hash[1]
        stateKey[4] = serviceBytes[2]
        stateKey[5] = hash[2]
        stateKey[6] = serviceBytes[3]
        stateKey[7] = hash[3]

        // Append remaining 23 bytes of hash (bytes 4-26)
        System.arraycopy(hash, 4, stateKey, 8, 23)

        return JamByteArray(stateKey)
    }

    /**
     * Extracts service index from a key with prefix 255.
     * Service bytes at positions 1, 3, 5, 7.
     */
    private fun extractServiceIndex255(key: JamByteArray): Int {
        val bytes = key.bytes
        return (bytes[1].toInt() and 0xFF) or
            ((bytes[3].toInt() and 0xFF) shl 8) or
            ((bytes[5].toInt() and 0xFF) shl 16) or
            ((bytes[7].toInt() and 0xFF) shl 24)
    }

    /**
     * Extracts service index from a service data key.
     * Service bytes at positions 0, 2, 4, 6.
     */
    private fun extractServiceIndexInterleaved(key: JamByteArray): Int {
        val bytes = key.bytes
        return (bytes[0].toInt() and 0xFF) or
            ((bytes[2].toInt() and 0xFF) shl 8) or
            ((bytes[4].toInt() and 0xFF) shl 16) or
            ((bytes[6].toInt() and 0xFF) shl 24)
    }

    /**
     * Encodes a UInt32 as little-endian bytes.
     */
    private fun encodeUInt32LE(value: UInt): ByteArray {
        return byteArrayOf(
            (value and 0xFFu).toByte(),
            ((value shr 8) and 0xFFu).toByte(),
            ((value shr 16) and 0xFFu).toByte(),
            ((value shr 24) and 0xFFu).toByte()
        )
    }

    /**
     * Checks if a key belongs to a specific service.
     */
    fun isServiceKey(key: JamByteArray, serviceIndex: Int): Boolean {
        if (key.size != 31) return false
        val firstByte = key.bytes[0]

        return if (firstByte == SERVICE_ACCOUNT) {
            // Check bytes at 1, 3, 5, 7
            extractServiceIndex255(key) == serviceIndex
        } else {
            // Check bytes at 0, 2, 4, 6
            extractServiceIndexInterleaved(key) == serviceIndex
        }
    }
}

/**
 * Represents the type of state component a key refers to.
 */
sealed class StateComponent {
    object CoreAuthorizationPool : StateComponent()
    object AuthorizationQueue : StateComponent()
    object RecentHistory : StateComponent()
    object SafroleState : StateComponent()
    object Judgements : StateComponent()
    object EntropyPool : StateComponent()
    object ValidatorQueue : StateComponent()
    object CurrentValidators : StateComponent()
    object PreviousValidators : StateComponent()
    object Reports : StateComponent()
    object Timeslot : StateComponent()
    object PrivilegedServices : StateComponent()
    object ActivityStatistics : StateComponent()
    object AccumulationQueue : StateComponent()
    object AccumulationHistory : StateComponent()
    object LastAccumulationOutputs : StateComponent()
    object ServiceStatistics : StateComponent()
    data class ServiceAccount(val serviceIndex: Int) : StateComponent()
    data class ServiceData(val serviceIndex: Int) : StateComponent()
}
