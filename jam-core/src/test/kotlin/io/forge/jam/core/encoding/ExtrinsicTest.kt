package io.forge.jam.core.encoding

import io.forge.jam.core.Extrinsic
import kotlin.test.Test
import kotlin.test.assertContentEquals

class ExtrinsicTest {
    @Test
    fun testEncodeExtrinsic() {
        // Load JSON data from resources using the class loader
        val (inputExtrinsic, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<Extrinsic>("codec/tiny", "extrinsic")

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            inputExtrinsic.encode(),
            "Encoded bytes do not match expected output"
        )
    }
}
