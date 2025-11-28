package io.forge.jam.core.encoding

import io.forge.jam.core.Header
import kotlin.test.Test
import kotlin.test.assertContentEquals

class HeaderTest {

    private fun testEncodeHeaderEpochMark(configPath: String) {
        val (inputHeader, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<Header>(configPath, "header_0")

        assertContentEquals(
            expectedOutputBytes,
            inputHeader.encode(),
            "Encoded bytes do not match expected output"
        )
    }

    private fun testEncodeHeaderTicketsMark(configPath: String) {
        val (inputHeader, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<Header>(configPath, "header_1")

        assertContentEquals(
            expectedOutputBytes,
            inputHeader.encode(),
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeHeaderEpochMarkTiny() {
        testEncodeHeaderEpochMark("codec/tiny")
    }

    @Test
    fun testEncodeHeaderEpochMarkFull() {
        testEncodeHeaderEpochMark("codec/full")
    }

    @Test
    fun testEncodeHeaderTicketsMarkTiny() {
        testEncodeHeaderTicketsMark("codec/tiny")
    }

    @Test
    fun testEncodeHeaderTicketsMarkFull() {
        testEncodeHeaderTicketsMark("codec/full")
    }
}
