package io.forge.jam.core.encoding

import io.forge.jam.core.Block
import kotlin.test.Test
import kotlin.test.assertContentEquals

class BlockTest {

    private fun testEncodeBlock(configPath: String) {
        val (inputBlock, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<Block>(configPath, "block")

        assertContentEquals(
            expectedOutputBytes,
            inputBlock.encode(),
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeBlockTiny() {
        testEncodeBlock("codec/tiny")
    }

    @Test
    fun testEncodeBlockFull() {
        testEncodeBlock("codec/full")
    }
}
