package io.forge.jam.core.encoding

import io.forge.jam.core.Context
import kotlin.test.Test
import kotlin.test.assertContentEquals

class ContextTest {

    private fun testEncodeContext(configPath: String) {
        val (inputContext, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<Context>(configPath, "refine_context")

        assertContentEquals(
            expectedOutputBytes,
            inputContext.encode(),
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeContextTiny() {
        testEncodeContext("codec/tiny")
    }

    @Test
    fun testEncodeContextFull() {
        testEncodeContext("codec/full")
    }
}
