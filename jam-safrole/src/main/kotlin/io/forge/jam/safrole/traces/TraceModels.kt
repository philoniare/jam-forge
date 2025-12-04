package io.forge.jam.safrole.traces

import io.forge.jam.core.Block
import io.forge.jam.core.Header
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.encodeCompactInteger
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

/**
 * A key-value pair representing a storage entry.
 * Keys are exactly 31 bytes as per the schema.
 */
@Serializable
data class KeyValue(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val key: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val value: JamByteArray
) {
    companion object {
        const val KEY_SIZE = 31

        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<KeyValue, Int> {
            val key = JamByteArray(data.copyOfRange(offset, offset + KEY_SIZE))
            val (valueLength, valueLengthBytes) = decodeCompactInteger(data, offset + KEY_SIZE)
            val value = JamByteArray(data.copyOfRange(offset + KEY_SIZE + valueLengthBytes, offset + KEY_SIZE + valueLengthBytes + valueLength.toInt()))
            return Pair(KeyValue(key, value), KEY_SIZE + valueLengthBytes + valueLength.toInt())
        }
    }

    fun encode(): ByteArray {
        // Key is fixed 31 bytes, value is length-prefixed
        return key.bytes + encodeCompactInteger(value.size.toLong()) + value.bytes
    }
}

/**
 * Raw state representation containing state root and key-value pairs.
 */
@Serializable
data class RawState(
    @SerialName("state_root")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val stateRoot: JamByteArray,
    val keyvals: List<KeyValue>
) {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<RawState, Int> {
            var currentOffset = offset
            val stateRoot = JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32))
            currentOffset += 32

            val (keyvalsLength, keyvalsLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += keyvalsLengthBytes

            val keyvals = mutableListOf<KeyValue>()
            for (i in 0 until keyvalsLength.toInt()) {
                val (kv, kvBytes) = KeyValue.fromBytes(data, currentOffset)
                keyvals.add(kv)
                currentOffset += kvBytes
            }

            return Pair(RawState(stateRoot, keyvals), currentOffset - offset)
        }
    }

    fun encode(): ByteArray {
        val stateRootBytes = stateRoot.bytes
        val keyvalsLengthBytes = encodeCompactInteger(keyvals.size.toLong())
        val keyvalsBytes = keyvals.fold(byteArrayOf()) { acc, kv -> acc + kv.encode() }
        return stateRootBytes + keyvalsLengthBytes + keyvalsBytes
    }
}

/**
 * A single trace step representing a block import operation.
 * Contains pre-state, the block to import, and expected post-state.
 */
@Serializable
data class TraceStep(
    @SerialName("pre_state")
    val preState: RawState,
    val block: Block,
    @SerialName("post_state")
    val postState: RawState
) {
    companion object {
        fun fromBytes(
            data: ByteArray,
            offset: Int = 0,
            validatorCount: Int = TinyConfig.VALIDATORS_COUNT,
            epochLength: Int = TinyConfig.EPOCH_LENGTH,
            coresCount: Int = TinyConfig.CORES_COUNT,
            votesPerVerdict: Int = TinyConfig.VALIDATORS_COUNT
        ): Pair<TraceStep, Int> {
            var currentOffset = offset

            val (preState, preStateBytes) = RawState.fromBytes(data, currentOffset)
            currentOffset += preStateBytes

            val (block, blockBytes) = Block.fromBytes(data, currentOffset, validatorCount, epochLength, coresCount, votesPerVerdict)
            currentOffset += blockBytes

            val (postState, postStateBytes) = RawState.fromBytes(data, currentOffset)
            currentOffset += postStateBytes

            return Pair(TraceStep(preState, block, postState), currentOffset - offset)
        }
    }

    fun encode(): ByteArray {
        return preState.encode() + block.encode() + postState.encode()
    }
}

/**
 * Genesis state for a trace, containing initial header and state.
 */
@Serializable
data class Genesis(
    val header: Header,
    val state: RawState
) {
    companion object {
        fun fromBytes(
            data: ByteArray,
            offset: Int = 0,
            validatorCount: Int = TinyConfig.VALIDATORS_COUNT,
            epochLength: Int = TinyConfig.EPOCH_LENGTH
        ): Pair<Genesis, Int> {
            var currentOffset = offset

            val (header, headerBytes) = Header.fromBytes(data, currentOffset, validatorCount, epochLength)
            currentOffset += headerBytes

            val (state, stateBytes) = RawState.fromBytes(data, currentOffset)
            currentOffset += stateBytes

            return Pair(Genesis(header, state), currentOffset - offset)
        }
    }

    fun encode(): ByteArray {
        return header.encode() + state.encode()
    }
}

/**
 * Configuration for tiny chain spec used by all traces.
 * Uses ChainConfig.TINY for standard parameters.
 */
object TinyConfig {
    val CONFIG = io.forge.jam.core.ChainConfig.TINY
    val VALIDATORS_COUNT = CONFIG.validatorCount
    val EPOCH_LENGTH = CONFIG.epochLength
    val CORES_COUNT = CONFIG.coresCount
    val PREIMAGE_EXPUNGE_DELAY = CONFIG.preimageExpungePeriod
    val MAX_TICKET_ATTEMPTS = CONFIG.ticketsPerValidator

    // Safrole-specific parameters
    const val TICKET_CUTOFF = 10
    const val RING_SIZE = 6
}
