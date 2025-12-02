package io.forge.jam.safrole.dispute

import io.forge.jam.core.Encodable
import io.forge.jam.core.Verdict
import io.forge.jam.safrole.safrole.DisputeInput
import io.forge.jam.safrole.safrole.DisputeState
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class DisputeCase(
    val input: DisputeInput,
    @SerialName("pre_state")
    val preState: DisputeState,
    val output: DisputeOutput,
    @SerialName("post_state")
    val postState: DisputeState
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0, coresCount: Int, validatorsCount: Int): Pair<DisputeCase, Int> {
            var currentOffset = offset
            // votes per verdict = 2/3 * validators + 1
            val votesPerVerdict = Verdict.votesPerVerdict(validatorsCount)

            val (input, inputBytes) = DisputeInput.fromBytes(data, currentOffset, votesPerVerdict)
            currentOffset += inputBytes

            val (preState, preStateBytes) = DisputeState.fromBytes(data, currentOffset, coresCount, validatorsCount)
            currentOffset += preStateBytes

            val (output, outputBytes) = DisputeOutput.fromBytes(data, currentOffset)
            currentOffset += outputBytes

            val (postState, postStateBytes) = DisputeState.fromBytes(data, currentOffset, coresCount, validatorsCount)
            currentOffset += postStateBytes

            return Pair(DisputeCase(input, preState, output, postState), currentOffset - offset)
        }
    }
    override fun encode(): ByteArray {
        val inputBytes = input.encode()
        val preStateBytes = preState.encode()
        val outputBytes = output.encode()
        val postStateBytes = postState.encode()
        return inputBytes + preStateBytes + outputBytes + postStateBytes
    }
}

