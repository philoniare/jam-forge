package io.forge.jam.safrole.authorization

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.encodeCompactInteger
import io.forge.jam.core.serializers.ByteArrayNestedListSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

const val AUTH_QUEUE_SIZE = 80 // Fixed size for authQueues inner list

@Serializable
data class AuthState(
    @SerialName("auth_pools")
    @Serializable(with = ByteArrayNestedListSerializer::class)
    var authPools: List<List<JamByteArray>>,
    @SerialName("auth_queues")
    @Serializable(with = ByteArrayNestedListSerializer::class)
    val authQueues: List<List<JamByteArray>>
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0, coresCount: Int): Pair<AuthState, Int> {
            var currentOffset = offset

            // authPools - fixed size outer (coresCount), variable inner (compact length + 32-byte hashes)
            val authPools = mutableListOf<List<JamByteArray>>()
            for (i in 0 until coresCount) {
                val (poolLength, poolLengthBytes) = decodeCompactInteger(data, currentOffset)
                currentOffset += poolLengthBytes
                val pool = mutableListOf<JamByteArray>()
                for (j in 0 until poolLength.toInt()) {
                    pool.add(JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32)))
                    currentOffset += 32
                }
                authPools.add(pool)
            }

            // authQueues - fixed size outer (coresCount), fixed size inner (80 x 32-byte hashes)
            val authQueues = mutableListOf<List<JamByteArray>>()
            for (i in 0 until coresCount) {
                val queue = mutableListOf<JamByteArray>()
                for (j in 0 until AUTH_QUEUE_SIZE) {
                    queue.add(JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32)))
                    currentOffset += 32
                }
                authQueues.add(queue)
            }

            return Pair(AuthState(authPools, authQueues), currentOffset - offset)
        }
    }

    fun copy() = AuthState(
        authPools = authPools.map { it.toList() },
        authQueues = authQueues.map { it.toList() }
    )

    override fun encode(): ByteArray {
        // AuthPools: outer list is fixed-size (core-count), inner list is variable-size (0..8)
        // No outer length prefix, inner uses compact integer length prefix
        val authPoolsBytes = authPools.fold(ByteArray(0)) { acc, pool ->
            val innerLengthBytes = encodeCompactInteger(pool.size.toLong())
            val innerBytes = pool.fold(ByteArray(0)) { innerAcc, hash -> innerAcc + hash.bytes }
            acc + innerLengthBytes + innerBytes
        }

        // AuthQueues: outer list is fixed-size (core-count), inner list is fixed-size (80)
        // No length prefixes at all
        val authQueuesBytes = authQueues.fold(ByteArray(0)) { acc, queue ->
            val innerBytes = queue.fold(ByteArray(0)) { innerAcc, hash -> innerAcc + hash.bytes }
            acc + innerBytes
        }

        return authPoolsBytes + authQueuesBytes
    }
}
