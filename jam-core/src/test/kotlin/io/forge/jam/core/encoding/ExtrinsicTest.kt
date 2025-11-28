package io.forge.jam.core.encoding

import io.forge.jam.core.Extrinsic
import kotlin.test.Test
import kotlin.test.assertContentEquals

class ExtrinsicTest {

    private fun testEncodeExtrinsic(configPath: String) {
        val (inputExtrinsic, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<Extrinsic>(configPath, "extrinsic")

        assertContentEquals(
            expectedOutputBytes,
            inputExtrinsic.encode(),
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeExtrinsicTiny() {
        testEncodeExtrinsic("codec/tiny")
    }

    @Test
    fun testEncodeExtrinsicFull() {
        testEncodeExtrinsic("codec/full")
    }
}
