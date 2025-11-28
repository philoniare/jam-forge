package io.forge.jam.core

import io.forge.jam.core.encoding.TestFileLoader
import kotlin.test.Test

class ShuffleTest {
    @Test
    fun testShuffle() {
        val testVectors = TestFileLoader.loadJsonFromTestVectors<List<ShuffleTestVector>>("shuffle", "shuffle_tests")
        for (vector in testVectors) {
            val actualOutput = jamComputeShuffle(vector.input, vector.entropy)
            assert(vector.output == actualOutput)
        }
    }
}
