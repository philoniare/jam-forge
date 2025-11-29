package io.forge.jam.safrole.authorization

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.encodeCompactInteger
import io.forge.jam.core.serializers.ByteArrayNestedListSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class AuthState(
    @SerialName("auth_pools")
    @Serializable(with = ByteArrayNestedListSerializer::class)
    var authPools: List<List<JamByteArray>>,
    @SerialName("auth_queues")
    @Serializable(with = ByteArrayNestedListSerializer::class)
    val authQueues: List<List<JamByteArray>>
) : Encodable {
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
