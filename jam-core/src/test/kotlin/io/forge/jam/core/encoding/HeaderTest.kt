package io.forge.jam.core.encoding

import io.forge.jam.core.Header
import kotlin.test.Test
import kotlin.test.assertContentEquals

class HeaderTest {
    @Test
    fun testEncodeHeaderEpochMark() {
        // Load JSON data from resources using the class loader
        val (inputHeader, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<Header>("codec/tiny", "header_0")

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            inputHeader.encode(),
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeHeaderTicketsMark() {
        // Load JSON data from resources using the class loader
        val (inputHeader, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<Header>("codec/tiny", "header_1")

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            inputHeader.encode(),
            "Encoded bytes do not match expected output"
        )
    }
}
