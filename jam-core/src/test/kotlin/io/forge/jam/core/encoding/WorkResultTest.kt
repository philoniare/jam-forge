package io.forge.jam.core.encoding

import io.forge.jam.core.WorkResult
import kotlin.test.Test
import kotlin.test.assertContentEquals

class WorkResultTest {
    @Test
    fun testEncodeWorkResultOk() {
        // Load JSON data from resources using the class loader
        val (inputWorkResult, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<WorkResult>("codec/tiny", "work_result_0")

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            inputWorkResult.encode(),
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeWorkResultPanic() {
        // Load JSON data from resources using the class loader
        val (inputWorkResult, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<WorkResult>("codec/tiny", "work_result_1")

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            inputWorkResult.encode(),
            "Encoded bytes do not match expected output"
        )
    }
}


