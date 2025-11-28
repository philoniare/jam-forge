package io.forge.jam.core.encoding

import io.forge.jam.core.Context
import kotlin.test.Test
import kotlin.test.assertContentEquals

class ContextTest {
    @Test
    fun testEncodeContext() {
        // Load JSON data from resources using the class loader
        val (inputContext, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<Context>("codec/tiny", "refine_context")

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            inputContext.encode(),
            "Encoded bytes do not match expected output"
        )
    }
}

