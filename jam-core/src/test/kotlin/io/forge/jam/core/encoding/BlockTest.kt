package io.forge.jam.core.encoding

import io.forge.jam.core.Block
import kotlin.test.Test
import kotlin.test.assertContentEquals

class BlockTest {
    @Test
    fun testEncodeBlock() {
        // Load JSON data from resources using the class loader
        val (inputBlock, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<Block>("codec/tiny", "block")

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            inputBlock.encode(),
            "Encoded bytes do not match expected output"
        )
    }
}
