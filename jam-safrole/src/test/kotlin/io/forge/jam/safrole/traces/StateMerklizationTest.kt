package io.forge.jam.safrole.traces

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.encoding.TestFileLoader
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import io.forge.jam.core.serializers.TrieKeyMapSerializer
import kotlinx.serialization.Serializable
import kotlin.test.Test
import kotlin.test.assertContentEquals

@Serializable
data class TrieTestVector(
    @Serializable(with = TrieKeyMapSerializer::class)
    val input: Map<JamByteArray, JamByteArray> = emptyMap(),
    @Serializable(with = JamByteArrayHexSerializer::class)
    val output: JamByteArray
)

class StateMerklizationTest {
    @Test
    fun testStateMerklize() {
        val testVectors = TestFileLoader.loadJsonFromTestVectors<List<TrieTestVector>>("trie", "trie")
        for ((index, vector) in testVectors.withIndex()) {
            val actualOutput = StateMerklization.stateMerklize(vector.input)
            assertContentEquals(
                vector.output.bytes,
                actualOutput.bytes,
                "Test vector $index: State merklization output does not match expected"
            )
        }
    }
}
