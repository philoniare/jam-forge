package io.forge.jam.core

import io.forge.jam.core.encoding.TestFileLoader
import kotlin.test.Test
import kotlin.test.assertContentEquals

class MerkleTreeTest {
    @Test
    fun testTrie() {
        val testVectors = TestFileLoader.loadJsonFromTestVectors<List<TrieTestVector>>("trie", "trie")
        for (vector in testVectors) {
            val actualOutput = merkle(vector.input)
            assertContentEquals(vector.output.bytes, actualOutput.bytes, "The trie bytes do not match")
        }
    }
}
